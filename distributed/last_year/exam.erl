-module(exam).
-compile(export_all).

-define(my_server, {server, 'srv@127.0.0.1'}).

start() ->
    register(server, spawn(?MODULE, loop, [#{}])).

stop() ->
    ?my_server ! stop.

send_money(FromRef, {UserName, AccountNumber}, Amount) ->
    ?my_server !
        {add_amount_from_this_ref_to_this_guy, FromRef, UserName, AccountNumber, Amount, self()},
    receive
        {transaction_denied, FromRef, ToUserAccount, Reason} ->
            {transaction_denied, FromRef, ToUserAccount, Reason};
        {transaction_approved, NewBalance} ->
            {transaction_approved, NewBalance}
    end.

login(UserName, AccountNumber) ->
    ?my_server ! {login_req, UserName, AccountNumber, self()},
    receive
        {login_success, Ref} -> {login_success, Ref};
        {login_failed, Reason} -> {login_failed, Reason}
    end.

logout(Ref) ->
    ?my_server ! {logout_req, Ref, self()},
    receive
        {logout_success, Ref} -> {logout_success, Ref};
        {logout_failed, ref_not_found} -> {logout_failed, ref_not_found}
    end.

deposit(Ref, Amount) ->
    ?my_server ! {deposit_req, Ref, Amount, self()},
    receive
        {deposit_success, Ref, NewBalance} -> {deposit_success, Ref, NewBalance};
        {deposit_failed, {ref_not_found, Ref}} -> {deposit_failed, {ref_not_found, Ref}}
    end.

balance(Ref) ->
    ?my_server ! {balance_inquiry, Ref, self()},
    receive
        {current_balance, Ref, UserBalance} -> {current_balance, Ref, UserBalance};
        {balance_failed, {ref_not_found, Ref}} -> {balance_failed, {ref_not_found, Ref}}
    end.

loop(State) ->
    receive
        {login_req, UserName, AccountNumber, UserPid} ->
            Ref = make_ref(),
            NewState = maps:put(Ref, {UserName, AccountNumber, 0}, State),
            UserPid ! {login_success, Ref},
            loop(NewState);
        {logout_req, Ref, UserPid} ->
            case maps:get(Ref, State, ref_not_found) of
                ref_not_found ->
                    UserPid ! {logout_failed, ref_not_found};
                _UserState ->
                    NewState = maps:remove(Ref, State),
                    UserPid ! {logout_success, Ref},
                    loop(NewState)
            end;
        {deposit_req, Ref, Amount, UserPid} ->
            case maps:get(Ref, State, ref_not_found) of
                ref_not_found ->
                    UserPid ! {deposit_failed, {ref_not_found, Ref}};
                {UserName, UserIndex, UserBalance} ->
                    NewUserBalance = UserBalance + Amount,
                    NewState = maps:update(Ref, {UserName, UserIndex, NewUserBalance}, State),
                    UserPid ! {deposit_success, Ref, NewUserBalance},
                    loop(NewState)
            end;
        {balance_inquiry, Ref, UserPid} ->
            case maps:get(Ref, State, ref_not_found) of
                ref_not_found ->
                    UserPid ! {balance_failed, {ref_not_found, Ref}};
                {_UserName, _UserIndex, UserBalance} ->
                    UserPid ! {current_balance, Ref, UserBalance},
                    loop(State)
            end;
        {add_amount_from_this_ref_to_this_guy, FromRef, ToUserName, ToAccountNumber, Amount,
            UserPid} ->
            % Verifying From Ref exists
            case maps:get(FromRef, State, ref_not_found) of
                ref_not_found ->
                    UserPid ! {transaction_denied, FromRef, ToAccountNumber, ref_not_found},
                    loop(State);
                {UserName, UserIndex, UserBalance} ->
                    % Verifying ToUserAccount exist
                    case
                        containsNameAndAccount(maps:keys(State), ToUserName, ToAccountNumber, State)
                    of
                        false ->
                            UserPid !
                                {transaction_denied, FromRef, ToAccountNumber,
                                    to_user_account_not_found},
                            loop(State);
                        {ToRef, ToUserName, ToAccountNumber, ToBalance} ->
                            % Check sender has enough balance
                            if
                                UserBalance < Amount ->
                                    UserPid !
                                        {transaction_denied, FromRef, ToAccountNumber,
                                            not_enough_balance},
                                    loop(State);
                                true ->
                                    NewSenderBalance = UserBalance - Amount,
                                    NewReceiverBalance = ToBalance + Amount,
                                    % update sender
                                    MidState = maps:update(
                                        FromRef, {UserName, UserIndex, NewSenderBalance}, State
                                    ),
                                    FinalState = maps:update(
                                        ToRef,
                                        {ToUserName, ToAccountNumber, NewReceiverBalance},
                                        MidState
                                    ),
                                    UserPid ! {transaction_approved, NewSenderBalance},
                                    loop(FinalState)
                            end
                    end
            end;
        stop ->
            io:format("Banking server stopped with state:~p~n", [State]),
            stop
    end.

containsNameAndAccount([], _ToUserName, _ToAccountNumber, _State) ->
    false;
containsNameAndAccount([Ref | Refs], ToUserName, ToAccountNumber, State) ->
    case maps:get(Ref, State) of
        {ToUserName, ToAccountNumber, Amount} -> {Ref, ToUserName, ToAccountNumber, Amount};
        _SomethingElse -> containsNameAndAccount(Refs, ToUserName, ToAccountNumber, State)
    end.
