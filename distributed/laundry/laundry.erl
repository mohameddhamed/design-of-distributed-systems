-module(laundry).
-compile(export_all).
% Grade 2: Distributed laundry
% Define a distributed server that implements the behaviour of a simplified laundry.

% The required interface of the laundry:

%  - the start/2 function to start it

%  - an insert_coins/1 function to insert the coins into the washing machine

%  - a select_program/1 function to select the element of the washing program

% The internal representation of the server state is not declared, you are free to choose.

% The behaviour of the server interface is the following:

% start/2
% The server is initiated with the price of the washing and the washing program option list. The example shows a list to store the program elements, but you can choose a different container.

% (laundry@localhost)2> laundry1:start(100, [{prewash, [true, false]}, {type, [white, dark, eco]}, {dry, [true, false]}]).

% insert_coins/1
% The function takes a number, the inserted money, as an argument. It sends it to the server and returns either the list of program elements and the corresponding types, or the insufficient_funds atom if the inserted money is not enough.

% The server checks the inserted amount and waits for the selection of the client. The server does not accept messages from other clients until the selection is finished.

% This function takes a program element and a type as an argument and returns the next program element to be selected. If the atom start is given as an argument and all the elements of the program were provided, it returns the atom started and the returned money.

% Only valid program elements and types need to be handled in the basic implementation.

% The server accepts the program element selection, sends back the rest of the program element list and waits for the next selection. If all program elements were selected, the start message starts the washing program (send a message to the client the washing is started) and lets the washing machine process the messages from other clients.

-define(PZR, {server, 'laundry@127.0.0.1'}).

start(Price, OptionList) ->
    register(server, spawn(laundry, loop, [Price, OptionList])).

insert_coins(InsertedMoney) ->
    ?PZR ! {inserted_money, InsertedMoney, self()},
    receive
        insufficient_funds -> insufficient_funds;
        {sufficient_funds, OptionList} -> OptionList
    end.

select_program(start) ->
    ?PZR ! {can_i_start, self()},
    receive
        {you_can_start, ReturnedMoney} -> {started, {returned_money, ReturnedMoney}};
        {you_cannot_start, ReturnedMoney} -> {money_returned, ReturnedMoney}
    end;
select_program({ProgramElement, Type}) ->
    ?PZR ! {latest_select, ProgramElement, Type, self()},
    receive
        {success_select, UpdatedOptionList} -> {next, UpdatedOptionList}
    end.
%           type           dark
checkMatch(ProgramElement, Type, CurrentNextOption) ->
    % [{prewash, [true, false]}, {type, [white, dark, eco]}, {dry, [true, false]}]
    % {type, [white, dark, eco]}
    {Elem, Types} = CurrentNextOption,
    Elem =:= ProgramElement andalso lists:member(Type, Types).

handle_select(Price, OptionList, RemainingOptions, ReturnedMoney, KnownFrom) ->
    receive
        {latest_select, ProgramElement, Type, KnownFrom} ->
            [CurrentNextOption | NewRemainingOptions] = RemainingOptions,
            ValidInput = checkMatch(ProgramElement, Type, CurrentNextOption),

            case ValidInput of
                true ->
                    KnownFrom ! {success_select, NewRemainingOptions},
                    handle_select(Price, OptionList, NewRemainingOptions, ReturnedMoney, KnownFrom);
                false ->
                    KnownFrom ! unsucessful_select,
                    loop(Price, OptionList)
            end;
        {can_i_start, From} ->
            case length(RemainingOptions) of
                0 -> From ! {you_can_start, ReturnedMoney};
                _ -> From ! {you_cannot_start, ReturnedMoney}
            end,
            loop(Price, OptionList)
    end.

loop(Price, OptionList) ->
    receive
        {inserted_money, InsertedMoney, From} ->
            Difference = InsertedMoney - Price,
            if
                Difference >= 0 ->
                    From ! {sufficient_funds, OptionList},
                    handle_select(Price, OptionList, OptionList, Difference, From);
                Difference < 0 ->
                    From ! insufficient_funds,
                    loop(Price, OptionList)
            end
        % {latest_select, ProgramElement, Type, From} ->
        %     [CurrentNextOption | RemainingOptions] = OptionList,
        %     ValidInput = CheckMatch(ProgramElement, Type, CurrentNextOption),
        %     case ValidInput of
        %         true ->
        %             OptionList = [next, RemainingOptions],
        %             From ! {success_select, OptionList},
        %             loop(_,_);
        %         false ->
        %             From ! unsucessful_select,
        %             loop(_,_)
        %     end;

        % {can_i_start, From} ->
        %     case length(RemainingOptions) of
        %         0 -> From ! {you_can_start, ReturnedMoney};
        %         _ -> From ! {you_cannot_start, ReturnedMoney}
        %     end,
        %     loop(_, _)
    end.
