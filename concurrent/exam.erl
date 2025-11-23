-module(exam).
-compile(export_all).

% Write a function any_order_doubler(List) that takes a list of integers.

% For every integer, spawn a process that doubles the number.
% The process should send the result back to the parent (you).
% The parent must collect all results into a list.
% Constraint: The order of the output list does not matter (e.g., if input is [1, 2, 3], output can be [6, 2, 4]).

double_and_send(I, Pid) ->
    timer:sleep(rand:uniform(100)),
    Pid ! {self(), I * 2}.

spawn_list([H | T]) ->
    spawn(exam, double_and_send, [H, self()]),
    spawn_list(T);
spawn_list([]) ->
    [].

any_order_doubler(L) ->
    spawn_list(L),
    receive_list(L).

receive_list([_ | T]) ->
    receive
        {_, Doubled} -> [Doubled | receive_list(T)]
    end;
receive_list([]) ->
    [].

% Write a function ordered_doubler(List) that takes a list of integers.

% Spawn a process for every integer to double it.
% Constraint: The result must be in the exact same order as the input.
% (Input: [1, 2, 3], Output must be [2, 4, 6]).
% The "Pid Trick" Hint (Crucial for your exam):
% If you just use receive Result -> ..., you get the fastest one first (wrong order).
% However, when you spawn a process, you get a Pid.
% Instead of waiting for any message,
% can you iterate through your list of Pids and wait for a message from specific Pids in order?

% code
% Erlang
% Logic visualization:
% 1. Spawn Process A (for number 1) -> Save PidA
% 2. Spawn Process B (for number 2) -> Save PidB
% 3. Wait specifically for PidA.
% 4. Wait specifically for PidB.

create_pid_list([H | T]) -> [spawn(exam, double_and_send, [H, self()]) | create_pid_list(T)];
create_pid_list([]) -> [].

process_pids([H | T]) ->
    receive
        {H, Doubled} -> [Doubled | process_pids(T)]
    end;
process_pids([]) ->
    [].

ordered_doubler(L) ->
    process_pids(create_pid_list(L)).

% Task 1: Parallel apply all (10 points)
% We have a list of functions FS and some values LS. Define a function
% that applies each function to all the elements of the
% list in a separate process. The order of the resulted values matters!

% For example:

% If the input is:
% FS = [f,g,h]
% LS = [a,b,c,d]

% The result has to be:
% [f(a), f(b), f(c), f(d), g(a), g(b), g(c), g(d), h(a), h(b), h(c), h(d)]
% %% the order is the same as it would be evaluated sequentially

% You have two options for the level of parallelization.
% You can do the parallelization based on the number of functions or per item.

% IMPORTANT: The solution has to do the computation in parallel.
% First, all the spawning has to be done, and after it, the results have to be collected!
% "Sequential" solutions are not acceptable!
% For example, the solution that starts a process and immediately waits for the result,
% then starts the second process only if the first is already finished, etc., will be rejected.

% applyAllPar(FS :: list(function()), LS :: list()) -> Result :: list()

apply_and_send(Fct, Arg, Pid) -> Pid ! {self(), apply(Fct, [Arg])}.

create_pid_list(Fct, [H | T]) ->
    [spawn(exam, apply_and_send, [Fct, H, self()]) | create_pid_list(Fct, T)];
create_pid_list(_, []) ->
    [].

collect_pid_list([H | T], LS) ->
    create_pid_list(H, LS) ++ collect_pid_list(T, LS);
collect_pid_list([], _) ->
    [].

applyAllPar(FS, LS) ->
    receive_all(collect_pid_list(FS, LS)).

receive_all([H | T]) ->
    receive
        {H, Value} -> [Value | receive_all(T)]
    end;
receive_all([]) ->
    [].

% Test cases (all should evaluate to `true`)

% exam:applyAllPar([fun(A) -> A+1 end, fun(A) -> A*2 end], [1,2,3,4]) == [2,3,4,5,2,4,6,8].
% exam:applyAllPar([fun(A) -> A+2 end], []) == [].
% exam:applyAllPar([], [apple, pear]) == [].
% exam:applyAllPar([fun erlang:is_list/1], [apple, pear]) == [false,false].
% exam:applyAllPar([fun erlang:is_list/1], [apple, pear, []]) == [false,false,true].

%%%

% Prep Task 1: The Safe Worker (Error Handling)
% Standard Erlang processes crash if they encounter an error.
% For this exam task, you need a worker that "catches" the crash and sends a safe error message instead.

% Task:
% Write a function safe_spawn(Func, Arg) that spawns a process.

% The process should try to run Func(Arg).
% If it works, send {ok, Result, self()} to the parent.
% If it crashes (e.g., badarith or function_clause), send {error, crash, self()} to the parent.
% Hint: Use try ... catch ... end.
% Example:

% Pid = safe_spawn(fun(X) -> 10/X end, 0).
% % Should not crash the shell. Should send {error, crash, Pid} to you.

tryFun(Fun, Args, Pid) ->
    try
        Pid ! {ok, apply(Fun, Args), self()}
    catch
        _:_ -> Pid ! {error, crash, self()}
    end.

safe_spawn(Fun, Arg) ->
    spawn(exam, tryFun, [Fun, Arg, self()]).

% Prep Task 2: The "Referee" (Stateful Collection)
% This simulates the logic of "Waiting for everyone, but remembering the winner."

% Task:
% Write a function referee(N).

% It should wait for N messages.
% The messages can be {ok, Val} or {error, Reason}.
% It must keep listening until it has received exactly N messages.
% Return Value:
% It should return the value of the first {ok, Val} message it received.
% If it received only {error, ...} messages (no successes), return no_proper_result.
% Hint:
% You need a recursive function that keeps track of two things:

% How many messages are left to receive (N).
% What is the best result I have seen so far? (CurrentBest).

referee(N) -> loop(N, no_proper_result).
loop(0, BestValue) ->
    BestValue;
loop(N, BestValue) ->
    receive
        {ok, Val} when BestValue == no_proper_result -> loop(N - 1, Val);
        {ok, _} -> loop(N - 1, BestValue);
        {error, _} -> loop(N - 1, BestValue)
    end.

% Task 2: Speculative evaluation (15 points)

% We have a list of functions and a list of values (which might be of different sizes).
% We want to evaluate all the functions in parallel as follows,
% the first function should be evaluated on the first data, the second on the second, etc.

% Your task is to start all the processes at the same time and wait for the quickest response.
% The result of the evaluation must be the quickest result.

% If your solution handles the possible errors and waits for all the processes to finish
% (hint: maintain a list), then returns the quickest result, your solution is worth 15 points.

% speculativeEval(FS :: list(function()), LS :: list()) -> Result

try_apply_send(Fun, Data, Pid) ->
    try
        Pid ! {self(), apply(Fun, [Data])}
    catch
        _:_ -> Pid ! {self(), no_proper_result}
    end.

create_pids([Fun | FS], [Data | LS]) ->
    [spawn(exam, try_apply_send, [Fun, Data, self()]) | create_pids(FS, LS)];
create_pids(_, []) ->
    [];
create_pids([], _) ->
    [].

receive_pids([_ | T], Best) ->
    receive
        {_, no_proper_result} -> receive_pids(T, Best);
        {_, Value} when Best == no_proper_result -> receive_pids(T, Value);
        {_, _} -> receive_pids(T, Best)
    end;
receive_pids([], Best) ->
    Best.

speculativeEval(Fs, Ls) ->
    receive_pids(create_pids(Fs, Ls), no_proper_result).

% For testing, add the following definition to your module, and be sure you export it.

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

% Test cases

% exam:speculativeEval([], []) == no_proper_result.
% exam:speculativeEval([fun exam:fib/1, fun exam:fib/1,fun exam:fib/1, fun exam:fib/1], []) == no_proper_result.
% exam:speculativeEval([], [10,20,21,22]) == no_proper_result.
% exam:speculativeEval([fun exam:fib/1, fun exam:fib/1,fun exam:fib/1, fun exam:fib/1], [10,20,21,22]) == 55. %% very probably
% exam:speculativeEval([fun exam:fib/1, fun exam:fib/1,fun exam:fib/1, fun exam:fib/1], [20,10,21,22]) == 55. %% very probably
% exam:speculativeEval([fun exam:fib/1, fun exam:fib/1,fun exam:fib/1, fun exam:fib/1], [22,24,20,10,21,22]) == 55.
% exam:speculativeEval([fun exam:fib/1, fun exam:fib/1,fun exam:fib/1, fun exam:fib/1], [apple, pear, 22, plum, foo, bar]) == 17711.
% exam:speculativeEval([fun exam:fib/1, fun exam:fib/1,fun exam:fib/1, fun exam:fib/1], [apple, pear, 22, plum, foo, bar]) == 17711.
% exam:speculativeEval([fun exam:fib/1, fun exam:fib/1,fun exam:fib/1, fun exam:fib/1], [apple, pear, 22, plum, 23, bar]) == 17711.
% exam:speculativeEval([fun exam:fib/1, fun exam:fib/1,fun exam:fib/1, fun exam:fib/1], [apple, pear, plum, orange, foo, bar]) == no_proper_result.

% Task 1: Parallel mergesort (10 points)
% Define a function that sorts the list with a parallel divide-and-conquer-style algorithm:

% - Implement a function merge_sort/1
% - It takes a list as an argument L
% - The return value is the sorted L
% - The input list needs to be divided into two sublists: L1 and L2 (first half of the list and second half of the list)
% - Sort L1 and L2 in parallel with the merge_sort/1 function.
% - The sorted sublists need to be merged
% - The parallelization must be carried out in a divide and conquer style.

% Hint: list:split and list:merge can be used in the divide and in the conquer phase.

% merge_sort(L::lists()) -> SortedResult::list()

sort_n_send(Pid, L) ->
    Pid ! {self(), merge_sort(L)}.

spawn_n_sort(L, Dest) ->
    spawn(exam, sort_n_send, [Dest, L]).

merge_sort([]) ->
    [];
merge_sort([V]) ->
    [V];
merge_sort(L) ->
    {L1, L2} = lists:split(trunc(length(L) / 2), L),

    Pid1 = spawn_n_sort(L1, self()),
    Pid2 = spawn_n_sort(L2, self()),

    SortedL1 =
        receive
            {Pid1, _L1} -> _L1
        end,
    SortedL2 =
        receive
            {Pid2, _L2} -> _L2
        end,
    lists:merge(SortedL1, SortedL2).

% Test cases

% test:merge_sort([111,11,1,12,13,23,2,3,31,22,253,4,221]) == [1,2,3,4,11,12,13,22,23,31,111,221,253]
% test:merge_sort([5,4,3]) == [3,4,5]
% test:merge_sort([5,4,3,5,1,3,2]) == [1,2,3,3,4,5,5]
% test:merge_sort([5,4,3,1,2]) == [1,2,3,4,5]
% test:merge_sort([5,4,3,1,2, 0, -3, -211]) == [-211,-3,0,1,2,3,4,5]

% Task 2: Parallel map-filter-map (20 points)
% Define a function pmfm/4 that takes three functions (F, G, H) and a list L as arguments.
% pmfm/4 will be our main process (the master).  It starts three worker processes that
% evaluate functions F, G and H accordingly. After this, the master sends all the elements
% of the list L to the first process **one by one** and waits for the calculated results
% from the last process. The master accepts messages only from the last process
% (the one evaluating function H), so it should perform a check when a message arrives who is the sender.

% The worker processes are waiting (recursively) for an element, performing their job
% on the received element, and passing the result to the next process. So the process
% evaluating F sends messages to the process evaluating G, and the process evaluating G
% sends messages to the process evaluating H. Finally, the process evaluating H sends the
% data back to the master process.

% The functions:

% - F is a function that maps a value to another value.
% - G is a boolean function that returns true or false for the given element.
% - H is a transformation function that has to be only applied on the elements where G returned true.

% For example, for list L = [1, apple, 3` the F = fun(X) -> X end, H = fun is_atom/1, G = fun atom_to_list/1 applied with pmfm/4 should return ["apple"].

% loop_F(F, Dest) ->
%     receive
%         {data, Element} ->
%             Dest ! {data, apply(F, [Element])},
%             loop_F(F, Dest);
%         {done} ->
%             Dest ! {done}
%     end.

% loop_G(G, Dest) ->
%     receive
%         {data, Element} ->
%             case apply(G, [Element]) of
%                 true ->
%                     Dest ! {data, Element},
%                     loop_G(G, Dest);
%                 false ->
%                     loop_G(G, Dest)
%             end;
%         {done} ->
%             Dest ! {done}
%     end.

% loop_H(H, Dest) ->
%     receive
%         {data, Element} ->
%             Dest ! {self(), apply(H, [Element])},
%             loop_H(H, Dest);
%         {done} ->
%             Dest ! {self(), done}
%     end.

% send_L([H | T], Pid) ->
%     Pid ! {data, H},
%     send_L(T, Pid);
% send_L([], Pid) ->
%     Pid ! {done}.

% receive_L(Pid) ->
%     receive
%         {Pid, done} -> [];
%         {Pid, Element} -> [Element | receive_L(Pid)]
%     end.

% pmfm(F, G, H, L) ->
%     PidH = spawn(exam, loop_H, [H, self()]),
%     PidG = spawn(exam, loop_G, [G, PidH]),
%     PidF = spawn(exam, loop_F, [F, PidG]),
%     send_L(L, PidF),
%     receive_L(PidH).

% Write a function risky_div(A, B).

% Setup: The main process should turn on exit trapping (process_flag(trap_exit, true)).
% Spawn: It should spawn_link a worker process.
% The Worker: Calculates A / B.
% If B is 0, this will naturally crash the worker with badarith.
% If valid, the worker sends {ok, Result} back to the parent.
% The Receive Loop:
% If you get {ok, Result}, return the result.
% If you get an {'EXIT', Pid, Reason} message, return the atom failing_operation.

div_n_send(A, B, Pid) -> Pid ! {data, (A / B)}.

risky_div(A, B) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(exam, div_n_send, [A, B, self()]),
    receive
        {'EXIT', Pid, _} -> failing_operation;
        {data, Value} -> Value
    end.

% Test cases:
% -----------

% test:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:atom_to_list/1, []) ==[]

% test:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:atom_to_list/1, [1, apple, 2])==["apple"]

% test:pmfm(fun(X)-> X*2 end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [1, 2, 3, 4, 5, 6]) ==[1, 2, 3, 4, 5, 6]

% test:pmfm(fun(X)-> X end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [11, 12, 13, 14, 15, 16]) == [6,7,8]

% Task 3: Error handling for Task2 (10 points)
% Handle the errors that may occur in the evaluation of pmfm/4.
% When either F, G or H raises a runtime error, the master process has to
% terminate immediately and return the string "Failing operation".

% **Although the errors can be handled with try and catch expressions,
% the maximum point can be achieved by process-related error handling only (link or monitor)!**

loop_F(F, Dest) ->
    receive
        {data, Element} ->
            Dest ! {data, apply(F, [Element])},
            loop_F(F, Dest);
        {done} ->
            Dest ! {done}
    end.

loop_G(G, Dest) ->
    receive
        {data, Element} ->
            case apply(G, [Element]) of
                true ->
                    Dest ! {data, Element},
                    loop_G(G, Dest);
                false ->
                    loop_G(G, Dest)
            end;
        {done} ->
            Dest ! {done}
    end.

loop_H(H, Dest) ->
    receive
        {data, Element} ->
            Dest ! {self(), apply(H, [Element])},
            loop_H(H, Dest);
        {done} ->
            Dest ! {self(), done}
    end.

send_L([H | T], Pid) ->
    Pid ! {data, H},
    send_L(T, Pid);
send_L([], Pid) ->
    Pid ! {done}.

receive_L(Pid, Res) ->
    receive
        {'EXIT', _, _} -> "Failing operation";
        {Pid, done} -> lists:reverse(Res);
        {Pid, Element} -> receive_L(Pid, [Element | Res])
    end.

pmfm(F, G, H, L) ->
    process_flag(trap_exit, true),
    PidH = spawn_link(exam, loop_H, [H, self()]),
    PidG = spawn_link(exam, loop_G, [G, PidH]),
    PidF = spawn_link(exam, loop_F, [F, PidG]),
    send_L(L, PidF),
    receive_L(PidH, []).

% Test cases:
% -----------

% test:pmfm(fun(X)-> X*2 end, fun(X)-> X rem 2 == 0 end, fun(X)-> X div 2 end, [1, apple, 6]).
% "Failing operation"
% test:pmfm(fun(X)-> X end, fun erlang:is_atom/1, fun erlang:list_to_atom/1, [1, apple, 2]).
% "Failing operation"
