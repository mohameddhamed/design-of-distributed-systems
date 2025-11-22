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
