-module(exam).
-compile(export_all).

% Task 1: Difference of lists
% Compare two lists (position-by-position), and return the differences!
% The result should include the elements of the first list that differ from the elements of the second.
% The function should check all the elements of the first list.

% Function name

differences(L, []) -> L;
differences([], _) -> [];
differences([H1 | T1], [H2 | T2]) when H1 =/= H2 -> [H1 | differences(T1, T2)];
differences([_ | T1], [_ | T2]) -> differences(T1, T2).

% Test cases

% exam:differences("","") == [].
% exam:differences("","apple") == [].
% exam:differences("apple", "") == "apple".
% exam:differences("apple", "apple") == [].
% exam:differences("apple", "peach") == "apple".
% exam:differences("apple", "apfel") == "ple".
% exam:differences([1,2,3], [3,2,1]) == [1,3].

% Task 2: Apply all
% Define a function that takes a list of functions and a list of some elements as arguments.
% The function applies each function to all the elements of the list. The order of the evaluation matters.
% First, the first given function should be evaluated on all the elements of the list, then the second, etc.

% Function name

applyAll([F1 | T1], L) -> f_apply(F1, L) ++ applyAll(T1, L);
applyAll(_, []) -> [];
applyAll([], _) -> [].

f_apply(F, [H | T]) -> [F(H) | f_apply(F, T)];
f_apply(_, []) -> [].

% Test cases

% exam:applyAll([fun(A) -> A + 1 end, fun(A) -> A * 2 end], [1, 2, 3, 4]) == [2, 3, 4, 5, 2, 4, 6, 8].
% exam:applyAll([fun(A) -> A+2 end], []) == [].
% exam:applyAll([], [apple, pear]) == [].
% exam:applyAll([fun erlang:is_list/1], [apple, pear]) == [false,false].
% exam:applyAll([fun erlang:is_list/1], [apple, pear, []]) == [false,false,true].

% Task 3: Positions
% Define a function that returns the positions of a given element in a list! The indexing starts with `1`.

% Function name

% getPositions
getPositions(Elem, L) -> getPositions_1(Elem, L, 1).
getPositions_1(Elem, L, Start) ->
    case getPosition(Elem, L, Start) of
        {-1, _} ->
            [];
        {Index, T} ->
            [Index | getPositions_1(Elem, T, Index + 1)]
    end.
%     if Index == -1
%     getPositions(Elem, L)
% getPositions(Elem, L, Start) ->

getPosition(Elem, [H | T], Index) when Elem == H -> {Index, T};
getPosition(Elem, [_ | T], Index) -> getPosition(Elem, T, Index + 1);
getPosition(_, [], _) -> {-1, []}.

% Test cases

% exam:getPositions($e, "apple") == [5].
% exam:getPositions($p, "apple") == [2,3].
% exam:getPositions(1, []) == [].
% exam:getPositions(1, [1,3,2,1,2,34,21,1,1,4]) == [1,4,8,9].

% Task 4: Riffle shuffle
% Define the riffle shuffle algorithm on lists!
% The function takes a list as an argument, it splits the list into two "equal" parts
% (if the number of elements is odd, then the first part should be shorter),
% then merges the two sublists into a new list alternately (one from the first, one from the second repeatedly).

% Example:

%    [1,2,3,4,5]

%   [1,2] [3,4,5]

%    [1,3,2,4,5]

% Function name

riffleShuffle(L) -> shuffle(riffle(L)).

riffle(L) -> lists:split(length(L) div 2, L).

shuffle({[H1 | T1], [H2 | T2]}) -> [H1, H2 | shuffle({T1, T2})];
shuffle({[], L}) -> L;
shuffle({L, []}) -> L.

% Test cases

% exam:riffleShuffle([]) == [].
% exam:riffleShuffle([1]) == [1].
% exam:riffleShuffle([1,2]) == [1,2].
% exam:riffleShuffle([1,2,3]) == [1,2, 3].
% exam:riffleShuffle([1,2,3,4]) == [1,3,2,4].
% exam:riffleShuffle([1,2,3,4,5]) == [1,3,2,4,5].
% exam:riffleShuffle([1,2,3,4,5,6]) == [1,4,2,5,3,6].
% exam:riffleShuffle([1,2,3,4,5,6,7]) == [1,4,2,5,3,6,7].
% exam:riffleShuffle([1,4,2,5,3,6,7]) == [1,5,4,3,2,6,7].

% Task 5: Error handling
% Modify the definition of the applyAll/2  function to handle the runtime
% errors occurring during the evaluations of the function arguments on the elements.
% Put the atom bad_fun_argument into the returning list when a runtime error occurs.

applyAll2([F1 | T1], L) -> f_apply2(F1, L) ++ applyAll2(T1, L);
applyAll2(_, []) -> [];
applyAll2([], _) -> [].

f_apply2(F, [H | T]) ->
    Val =
        try
            F(H)
        catch
            _:_ -> bad_fun_argument
        end,
    [Val | f_apply2(F, T)];
f_apply2(_, []) ->
    [].

% Test cases

% exam:applyAll2([fun(A) -> A+2 end], [1,apple]) == [3,bad_fun_argument].
% exam:applyAll2([fun erlang:atom_to_list/1, fun(A) -> A*2 end], [1,apple,3, '12']) ==[bad_fun_argument, "apple", bad_fun_argument, "12", 2,  bad_fun_argument, 6, bad_fun_argument].
% exam:applyAll2([fun(A) -> A+1 end, fun(A) -> A*2 end], [1,2,3,4]) == [2,3,4,5,2,4,6,8].

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
