-module(more).
-compile(export_all).

% Task 1: Parallel multiprocessing (5 points)
% ================================

% Implement a function multi/3 that processes the elements of two lists simultaneously.
% The function multi takes a function F, a list L1 and an other list L2 as arguments.
% It takes an element from L1 and an other element from L2 and applies F on them in parallel.
% Then it does the same with the rest of list L1 and L2. (Note: the length of the two list may differ).
% Then multi waits for the calculated values and returns them in the order of the original order of the arguments.

% multi(F::function(), L1::list(), L2::list()) -> Result::list()

% If the type of the function does not match to the type of the elements of the list,
% then return a proper error. You may use process link, monitor, try catch, whatever you like.

% Test cases:

wait_run_send(F, Dest) ->
    receive
        {elements, E1, E2} -> Dest ! {data, F(E1, E2)}
    end.

send_elements_to_workers(_, _, []) ->
    ok;
send_elements_to_workers(_, [], _) ->
    ok;
send_elements_to_workers([Pid | T], [H1 | T1], [H2 | T2]) ->
    Pid ! {elements, H1, H2},
    send_elements_to_workers(T, T1, T2).

create_workers(_, 0, _) ->
    [];
create_workers(F, N, Dest) ->
    [spawn_link(more, wait_run_send, [F, Dest]) | create_workers(F, N - 1, Dest)].

receive_all([], Acc) ->
    lists:reverse(Acc);
receive_all([_ | T], Acc) ->
    receive
        {data, Value} -> receive_all(T, [Value | Acc]);
        {'EXIT', _, R} when R =/= normal -> {'EXIT', "Non matching types"}
    end.

multi(F, L1, L2) ->
    process_flag(trap_exit, true),
    Pids = create_workers(F, min(length(L1), length(L2)), self()),
    send_elements_to_workers(Pids, L1, L2),
    receive_all(Pids, []).

% multi(fun erlang:'+'/2, [], []) == []
% multi(fun erlang:'+'/2, [1,2], [3,4]) == [4,6]
% multi(fun erlang:'+'/2, [1,2,5], [3,4]) == [4,6]
% multi(fun erlang:'+'/2, [1,2,5], [3,4, 8, 9]) == [4,6,13]
% multi(fun(X, Y) -> {X, Y, math:pow(X, Y)} end, [1,2,5], [3,4, 8, 9]) == [{1,3,1.0},{2,4,16.0},{5,8,390625.0}]
% multi(fun erlang:'+'/2, [1], [apple]) == {'EXIT',"Non matching types"}

%  Implement a function bucket_sort/1
% - It takes a list as an argument L
% - The return value is the sorted L
% - There are only two buckets. You put an element into the first
% bucket if it is smaller than the mean of the list, and put the
% element into the second bucket otherwise.
% - The buckets need to be sorted with the bucket_sort/1 function.
% - The input list is a unique list (there are no duplicates in it).
% - The parallelization must be carried out in a divide and conquer style.

% bucket_sort(L::lists()) -> SortedResult::list()

% "The buckets need to be sorted with the bucket_sort/1 function."
% -- if you use additional sorting functions, your solution will not be accepted!

divide([], _, Bucket1, Bucket2) ->
    {Bucket1, Bucket2};
divide([H | T], Mean, Bucket1, Bucket2) ->
    if
        H < Mean -> divide(T, Mean, [H | Bucket1], Bucket2);
        H >= Mean -> divide(T, Mean, Bucket1, [H | Bucket2])
    end.

bucket_sort_n_send(L, Dest) ->
    Dest ! {sorted, self(), bucket_sort(L)}.

bucket_sort([]) ->
    [];
bucket_sort([Value]) ->
    [Value];
bucket_sort(L) ->
    Mean = (lists:sum(L)) / length(L),
    {B1, B2} = divide(L, Mean, [], []),
    Sorter1 = spawn(more, bucket_sort_n_send, [B1, self()]),
    Sorter2 = spawn(more, bucket_sort_n_send, [B2, self()]),
    receive
        {sorted, Sorter1, Sorted_B1} -> Sorted_B1
    end,
    receive
        {sorted, Sorter2, Sorted_B2} -> Sorted_B2
    end,
    Sorted_B1 ++ Sorted_B2.

% bucket_sort([111,11,1,12,13,23,2,3,31,22,253,4,221]) == [1,2,3,4,11,12,13,22,23,31,111,221,253]
% bucket_sort([5,4,3]) == [3,4,5]
% bucket_sort([5,4,3, 1,2]) == [1,2,3,4,5]
% bucket_sort([5,4,3, 1,2, 0, -3, -211]) == [-211,-3,0,1,2,3,4,5]

% Implement the behaviour of the lists:any/2 function in parallel.
% The pany/2 function takes two arguments: a function F and a list List.
% It evaluates F on the elements of the List in parallel.
% If any of the parallel processes returns true, pany/2 immediately
% returns the {true, OrigElement} tuple (where OrigElement is the element of List where F evaluated to true).
% If all the parallel evaluations of F returns false, pany/2 returns false as well.

% Do not use lists:any, lists:all or lists:filter in the main function implementation of pany.
% If you use those, then you always wait all message to arrive in the implementation
% and will not satisfy the requirement that the function has to stop receiving messages when the first true arrives.

% pany(F::function(), List::lists()) -> Result:: {true, any()} | false

wait_eval_n_send(F, Dest) ->
    receive
        {data, Elem} ->
            case F(Elem) of
                true -> Dest ! Elem;
                false -> Dest ! false
            end
    end.

create_evaluaters(_, _, []) ->
    [];
create_evaluaters(F, Dest, [_ | T]) ->
    [spawn(more, wait_eval_n_send, [F, Dest]) | create_evaluaters(F, Dest, T)].

receive_all([]) ->
    false;
receive_all([_ | T]) ->
    receive
        false -> receive_all(T);
        Elem -> {true, Elem}
    end.
pany(F, L) ->
    Pids = create_evaluaters(F, self(), L),
    [Pid ! {data, X} || {X, Pid} <- lists:zip(L, Pids)],
    receive_all(Pids).

% Test cases:

% pany(fun(X) -> X > 6 end, [1,2,3]) == false
% pany(fun(X) -> X > 6 end, [11,12,13]) == {true, 11} or {true, 12} or {true, 13}
% pany(fun erlang:is_atom/1, [1,apple,2]) == {true, apple}

% Task 4: Speeding up the parallel fibonacci calculation (6 points)

% We have implemented several parallel version of the fibonacci calculation.
% You can use those and modify them to solve this task.

% Your task now is to solve the fibonacci calculation in parallel with an additional caching mechanism.
% You have to create a process in addition that takes care of a cache.
% Once we want to calculate a fibonacci number we ask this process whether it already knows the value.
% If somebody calculates a fibonacci value it notifies the cache process to store it.

fib_n_send(N, Master, Cache) ->
    Cache ! {get, self(), N},
    receive
        false ->
            Value = fib(N, Cache),
            Cache ! {set, N, Value},
            Value;
        {N, Value} ->
            Value
    end,
    Master ! {data, Value}.

fib(N, Cache) ->
    spawn(more, fib_n_send, [N - 1, self(), Cache]),
    spawn(more, fib_n_send, [N - 2, self(), Cache]),
    receive
        {data, Fib1} -> Fib1
    end,
    receive
        {data, Fib2} -> Fib2
    end,
    Fib1 + Fib2.

cache(L) ->
    receive
        {get, Pid, Num} ->
            Pid ! lists:keyfind(Num, 1, L),
            cache(L);
        {set, Key, Value} ->
            cache([{Key, Value} | L])
    end.

fib0(N) ->
    Cache = spawn(more, cache, [[{0, 0}, {1, 1}]]),
    fib(N, Cache).

% Test cases:

% fib(10) == 55

% fib(20) == 6765

% fib(30) == 832 040

% Implement a Parallel merge sort in Erlang using a master process and a pool of workers :

% - master/2 divide a list L in N parts and spawns N workers each with the order to sort a part of the list.
% Print on the console the sent lists togheter with the process that will be handling them.
% Than waits for the answer. After the master receive the result it kills all the workers.(No need for fancy implementatins,
% just make sure that no process is running at the end of the computation)

% - worker/0 is a generic function that receive a  message containing a Process id PidD(the dispatcher one)
% a Function and a List to apply the function to. It return the result to PidD and restart itself.

% - dispatcher/3 collects the sorted lists from the worker and send them back to be merged.
% The dispatcher takes as imputs:a list of workers processes Id, the length of the original list
% to be sorted and a Result variable keeping track of the sorted lists.
% Every time a worker send a sorted list, the dispatcher checks if it has at least two sorted
% lists available in Results and send 2 of them to the worker to merge them. Merged lists must be saved also
% into Results until there is just one list in the pool whose size is the length of the original list.
% At this point you're done: the dispatcher send the result to the master and dies. Master makes the workers stop and displays it.

% TIPS: As sort function you can use lists:sort, to merge 2 sorted lists you can use lists:merge,
% to divide the list you can use split/2: pass to it your list and N.

split(L, N) ->
    Len = length(L) div N,
    split(L, Len, N).
split(L, _, 1) ->
    [L];
split(L, Len, N) ->
    LN = lists:sublist(L, Len),
    [LN] ++ split(L -- LN, Len, N - 1).

% master(integer(),list()) -> list()
% worker()-> true
% dispatcher(list(pid())),integer(),list(list()))-> true

% wait_sort_n_send(Dest) ->
%     receive
%         {part_of_list, UnsortedList} -> lists:sort(UnsortedList), wait_sort_n_send(Dest);
%         ok -> ok
%     end.

% spawn_n_workers() ->
%     [spawn(more, wait_sort_n_send, [Dest]), spawn_n_workers(N-1)

% master(N, L) ->
%     SplitList = split(L, length(L), N),
%     spawn_n_workers()

% ~~~

% Test cases:
% -----------

% **Do not forget to change the name of the module!**

% ~~~

% test:master(3,[1,2,3,4,54,3,1,2,6,77,8,0]).
% Sent: [1,2,3,4] to <0.62.0>   
% Sent: [54,3,1,2] to <0.63.0>   
% Sent: [6,77,8,0] to <0.64.0>   
% [0,1,1,2,2,3,3,4,6,8,54,77]

% ~~~
