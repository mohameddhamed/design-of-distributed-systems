-module(ptest).
-compile(export_all).

% Task 1: Parallel iterate  (20 points)
% Define a function iterate/3 that takes a function F, a list L and a number N as arguments.
% The function iterates function F on the elements of LN-times in a pipeline style.

% The process evaluating iterate/3 will be our main process (the master).
% It starts as many worker processes as the given number N. Each worker process evaluates function F.
% After this, the master sends all the elements of the list L to the first process **one by one**
% (takes the first element of the list and sends it, then takes the second element and sends it, and so on...)
% and waits for the calculated results from the last process. The master accepts messages only from the last process,
% so it should perform a check when a message arrives who is the sender.

% The worker processes are waiting (recursively) for an element, performing their job on the received element,
% and passing the result to the next process. So the first worker waits for a message from the main process
% and sends the calculated result to the second worker, the second worker waits for messages from the first worker,
% calculates F and sends the result to the third process, and so on...

% wait_calc_send(Fun, Dest) ->
%     receive
%         {data, Value} ->
%             Dest ! {data, Fun(Value)},
%             wait_calc_send(Fun, Dest)
%     end.

% start_workers(0, _, Pid) ->
%     Pid;
% start_workers(N, F, Pid) ->
%     Worker_Pid = spawn(ptest, wait_calc_send, [F, Pid]),
%     start_workers(N - 1, F, Worker_Pid).

% send_list_to_pid([], _) ->
%     ok;
% send_list_to_pid([H | T], Pid) ->
%     Pid ! {data, H},
%     send_list_to_pid(T, Pid).

% receive_all(0) ->
%     [];
% receive_all(N) ->
%     receive
%         {data, Value} -> [Value | receive_all(N - 1)]
%     end.

% iterate(_, L, 0) ->
%     L;
% iterate(F, L, N) ->
%     First_Process = start_workers(N, F, self()),
%     % send_list_to_pid(L, First_Process),
%     [First_Process ! {data, X} || X <- L],
%     receive_all(length(L)).

% Test cases:
% -----------

% ptest:iterate(fun(X) -> X + 1 end, [5,8,23], 11) == [16,19,34].

% ptest:iterate(fun(X) -> X + 1 end, lists:seq(1,10), 15) == [16,17,18,19,20,21,22,23,24,25].

% ptest:iterate(fun(X) -> X + 1 end, lists:seq(1,20), 15) == [16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35].

% ptest:iterate(fun(X) -> X + 1 end, lists:seq(1,20), 1) == [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21].

% ptest:iterate(fun(X) -> X ++ "0" end, ["a", "b", "c"], 1) == ["a0","b0","c0"].

% ptest:iterate(fun(X) -> X ++ "0" end, ["a", "b", "c"], 5) ==["a00000","b00000","c00000"].

% ptest:iterate(fun(X) -> X ++ "0" end, [], 5) == [].

% ptest:iterate(fun(X) -> X ++ "0" end, ["a", "b", "c"], 0) == ["a","b","c"].

% Task 2: Error handling for Task1 (7 points)
% Handle the errors that may occur in the evaluation of iterate/3.
% When F raises a runtime error, the master process has to terminate immediately and
% return the string "Failing operation".

% **The errors need to be handled with process-related error handling (link or monitor)!**

% wait_calc_send(Fun, Dest) ->
%     receive
%         {data, Value} ->
%             Dest ! {data, Fun(Value)},
%             wait_calc_send(Fun, Dest)
%     end.

% start_workers(0, _, Pid) ->
%     Pid;
% start_workers(N, F, Pid) ->
%     Worker_Pid = spawn_link(ptest, wait_calc_send, [F, Pid]),
%     start_workers(N - 1, F, Worker_Pid).

% receive_all(Res, 0) ->
%     lists:reverse(Res);
% receive_all(Res, N) ->
%     receive
%         {'EXIT', _, _} -> "Failing operation";
%         {data, Value} -> receive_all([Value | Res], N - 1)
%     end.

% iterate(_, L, 0) ->
%     L;
% iterate(F, L, N) ->
%     process_flag(trap_exit, true),
%     First_Process = start_workers(N, F, self()),
%     [First_Process ! {data, X} || X <- L],
%     receive_all([], length(L)).

% Test cases:
% -----------
% ptest:iterate(fun(X) -> X + 0 end, [2,a,3], 1) == "Failing operation"

% wait_calc_send(Fun, Dest) ->
%     receive
%         {data, Value} ->
%             Dest ! {data, Fun(Value)},
%             wait_calc_send(Fun, Dest)
%     end.

% start_workers(0, _, Pid) ->
%     Pid;
% start_workers(N, F, Pid) ->
%     Worker_Pid = spawn_link(ptest, wait_calc_send, [F, Pid]),
%     start_workers(N - 1, F, Worker_Pid).

% receive_all(Res, 0, _, _) ->
%     lists:reverse(Res);
% receive_all(Res, N, Ref, Pid) ->
%     receive
%         {'DOWN', Ref, process, Pid, _} -> "Failing operation";
%         {data, Value} -> receive_all([Value | Res], N - 1, Ref, Pid)
%     end.

% iterate(_, L, 0) ->
%     L;
% iterate(F, L, N) ->
%     process_flag(trap_exit, true),
%     {Sink, Ref} = spawn_monitor(ptest, wait_calc_send, [F, self()]),
%     Source = start_workers(N - 1, F, Sink),
%     [Source ! {data, X} || X <- L],
%     receive_all([], length(L), Ref, Sink).

% Stop the worker processes. You get 5 points if you stop the started workers in case of the normal execution
% and 5 more points if you stop them when an error occurred during the computation.

wait_calc_send(Fun, Dest) ->
    receive
        {data, Value} ->
            Dest ! {data, Fun(Value)},
            wait_calc_send(Fun, Dest);
        ok ->
            Dest ! ok
    end.

start_workers(0, _, Pid) ->
    Pid;
start_workers(N, F, Pid) ->
    Worker_Pid = spawn_link(ptest, wait_calc_send, [F, Pid]),
    start_workers(N - 1, F, Worker_Pid).

receive_all(Res, 0, _, _) ->
    receive
        ok -> lists:reverse(Res)
    end;
receive_all(Res, N, Ref, Pid) ->
    receive
        {'DOWN', Ref, process, Pid, _} -> "Failing operation";
        {data, Value} -> receive_all([Value | Res], N - 1, Ref, Pid)
    end.

iterate(_, L, 0) ->
    L;
iterate(F, L, N) ->
    process_flag(trap_exit, true),
    {Sink, Ref} = spawn_monitor(ptest, wait_calc_send, [F, self()]),
    Source = start_workers(N - 1, F, Sink),
    [Source ! {data, X} || X <- L],
    Source ! ok,
    receive_all([], length(L), Ref, Sink).
