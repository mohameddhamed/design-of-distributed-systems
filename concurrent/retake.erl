-module(retake).
-compile(export_all).

% Task 1: Find elements  (10 points)
% Define a function find/3 that takes a function F, and two lists L1 and L2 as arguments.
% The function calculates the lowest position N (the lowest index from the matching positions)
% in the list where function F(E1, E2) evaluates to true,
% where E1 is the N-th element of L1 and E2 is the N-th element of L2.
% It is enough to handle the cases when length(L1) = length(L2).

% The process evaluating find/3 will be our main process (the master).
% It starts as many worker processes as the length of the input list.
% After starting the processes, the master sends the input to the workers
% and waits for the result of the computation:

%  - once the first matching position is found by a worker,
% the main process does not wait for further messages (so it is lazy)

%  - if no match is found, it returns the atom not_found.

% Each worker process waits for some data, evaluates function F,
% and notifies the main process about the result.

% Only those solutions will be accepted where the worker evaluates receive expressions
% to get the data from the master and the master sends the required data as messages.
% So, do not give the data as arguments to the workers in the initialisation.

% Solutions, where the master receives the answers from all the workers and filters out sequentially the true values,
% will not be accepted.

% The solution needs to follow the parallel elementwise processing scheme (parallel map),
% but be aware of the required restrictions.

wait_n_match(F, N, Dest) ->
    receive
        {data, E1, E2} ->
            case F(E1, E2) of
                true -> Dest ! {found, N};
                false -> Dest ! not_found
            end
    end.

spawn_workers(_, 0, _) ->
    [];
spawn_workers(F, N, Pid) ->
    [spawn(retake, wait_n_match, [F, N, Pid]) | spawn_workers(F, N - 1, Pid)].

send_to_workers([], [], []) ->
    ok;
send_to_workers([H_Worker | T], [H1 | T1], [H2 | T2]) ->
    H_Worker ! {data, H1, H2},
    send_to_workers(T, T1, T2).

receive_match(0) ->
    not_found;
receive_match(N) ->
    receive
        {found, Index} -> Index;
        not_found -> receive_match(N - 1)
    end.

find(F, L1, L2) ->
    Workers = spawn_workers(F, length(L1), self()),
    send_to_workers(lists:reverse(Workers), L1, L2),
    receive_match(length(L1)).

% Test cases:

% You can use this implementation for testing:

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).
% -----------
% retake:find(fun(A, B) -> A == B end, [apple,2,3], [apple,2,1]) == 1
% retake:find(fun(A, B) -> A == B end, [a,2,3], [apple,2,1]) == 2
% retake:find(fun(A, B) -> A == B end, [a,2,3,2], [apple,2,1,2]) == 2
% retake:find(fun(A, B) -> retake:fib(A) == retake:fib(B) end, [39,2,3], [39,2,1]) == 1
% retake:find(fun(A, B) -> retake:fib(A) == retake:fib(B) end, [], []) == not_found
% retake:find(fun(A, B) -> retake:fib(A) == retake:fib(B) end, [2], [3]) == not_found
