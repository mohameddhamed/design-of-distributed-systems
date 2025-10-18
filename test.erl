-module(test).
-compile(export_all).
% Sequential problems to solve
% Task 1: Search for the same elements (8 points)
% Define the function equals/2. The function takes two lists as arguments and returns the positions of the equal elements in the lists.

% equals([any()], [any()]) -> [integer()].
equals(L1, L2) -> equals(L1, L2, 1).
equals([H1 | T1], [H2 | T2], Index) when H1 == H2 -> [Index | equals(T1, T2, Index + 1)];
equals([_ | T1], [_ | T2], Index) -> equals(T1, T2, Index + 1);
equals([], _, _) -> [];
equals(_, [], _) -> [].

% Test cases:

% test:equals([],[]) == []
% test:equals([1],[]) == []
% test:equals([1, 1, 1], [1, 2, 1]) == [1, 3]
% test:equals([1,1,1,2,2,2,3,3,3],[1,2,1,1,2,1]) == [1,3,5]
% test:equals([1,1,1,2,2,2,3,3,3],[1,2,1,1,2,1,3,3,3,3,3,3,3]) == [1,3,5,7,8,9]
% test:equals([1,1,1,2,2,2,3,3,3],"abcdefg") == []
% test:equals([1,1,1,2,2,2,3,3,3],[a,b,c,d]) == []
% test:equals("firstlist", "secondlist") == []
% test:equals(" firstlist", "secondlist") == [7,8,9,10]

% Task 2: Alternating reduce (10 points)
% Define the function reduce_alter/1. The function calculates a value from the elements of the list
% by applying addition and multiplication operations in pairs:

% the function starts by taking the first and second elements of the list and adding them. 1
% then the function takes the previously calculated value and the third element of the list and multiplies them 2
% then the function takes the result of the previous multiplication and adds the fourth element of the list to it 3
% then the function takes the result of the previous step and multiplies it with the fifth element of the list 4
% and so on and so on...
% Example:

% [212,313,414,515,616] -> ((212+313)*414+515)*616

% reduce_alter([integer()]) -> integer() | not_defined
reduce_alter(L) -> reduce_alter_i(L, 0).
reduce_alter_i([First, Second | T], 0) -> reduce_alter_i([First + Second | T], 1);
reduce_alter_i([First, Second | T], 1) -> reduce_alter_i([First * Second | T], 0);
reduce_alter_i([Res], _) -> Res.

% % Test cases:

% test:reduce_alter([212,313,414,515,616]) == 134204840
% test:reduce_alter([]) == not_defined
% test:reduce_alter([212]) == 212
% test:reduce_alter([1,1,1,1,1,1]) == 4
% test:reduce_alter([1,2,1,2,1,2,1]) == 7
% test:reduce_alter([111,222,333,444]) == 111333
% test:reduce_alter([10,10,10,10,10,10,10]) == 21100

% % Task 3: Filtering by position (15 points) - without error handling 12 points

% % Define the function filter/3 that evaluates a predicate on the elements of two lists simultaneously
% and returns the last pairs where the predicate returned the same value for the elements (true or false).
% The return value is a two-element map: the key true is associated with the pairs of the elements
% where the predicate returned true, and the key false is associated with the pair where the predicate returned false.
% If no pair is found to the key false or true, not_found should be returned as a value.

% % The predicate might raise a runtime error during the evaluation. In this case, that pair should be omitted from the result.
% The solution is accepted without error handling as well for 12 points.

% % Help: using a map to store the return value helps you in the implementation because the keys are unique in a map,
% and once you want to add a value with the same key, the old value is replaced.

% filter(fun(any()) -> true | false end, [any()], [any()]) -> map()
% test:filter(fun erlang:is_integer/1, [3], [3]) == #{false => not_found,true => {3,3}}

filter(Fun, L1, L2) -> filter_m(Fun, L1, L2, #{true => not_found, false => not_found}).

filter_m(Fun, [H1 | L1], [H2 | L2], My_map) ->
    Res1 =
        try
            Fun(H1)
        catch
            _:_ -> error
        end,
    Res2 =
        try
            Fun(H2)
        catch
            _:_ -> error
        end,

    case {Res1, Res2} of
        {true, true} ->
            filter_m(Fun, L1, L2, My_map#{true => {H1, H2}});
        {false, false} ->
            filter_m(Fun, L1, L2, My_map#{false => {H1, H2}});
        _ ->
            filter_m(Fun, L1, L2, My_map)
    end;
filter_m(_, [], _, My_map) ->
    My_map;
filter_m(_, _, [], My_map) ->
    My_map.

% % Examples:
% Pred = fun erlang:is_integer/1
% List1 = [1,2]
% List2 = [33,44]
% Pred(1) = true, Pred(33) = true
% Pred(2) = true, Pred(44) = true
% % Both for the first element of L1 (1) and for the first element of L2 (33) the predicate returns true. 
% So the pair {1,33} is a possible value for the map key true: #{true => {1,33}}. 
% But in the next step, we check the second element of the list L1 (2) and L2 (44). 
% Both return true, so we need to record this as a value for the key true: #{true => {2,44}}. 
% We found no pairs where both values are false, so the final result is #{false => not_found,true => {2,44}}

% Pred = fun erlang:is_integer/1
% List1 = [1,a]
% List2 = [33,b]
% Pred(1) = true, Pred(33) = true
% Pred(a) = false, Pred(b) = false
% % Both for the first element of L1 (1) and for the first element of L2 (33) the predicate returns true. 
% So the pair {1,33} is a possible value for the map key true: #{true => {1,33}}.  
% In the next step, we check the second element of the list L1 (a) and L2 (b). 
% Both return false, so we need to record this as a value for the key false: #{false => {a,b}}. 
% So the final value is  #{false => {a,b},true => {1,33}}

% Pred = fun erlang:is_integer/1
% List1 = [1,2]
% List2 = [33,b]
% Pred(1) = true, Pred(33) = true
% Pred(a) = false, Pred(b) = false
% % Both for the first element of L1 (1) and for the first element of L2 (33) the predicate returns true. 
% So the pair {1,33} is a possible value for the map key true: #{true => {1,33}}.  
% In the next step, we check the second element of the list L1 (2) and L2 (b). 
% The first one returns true, and the second one returns false, so we skip these values. 
% We found no pairs where both values are false, so the final result is #{false => not_found,true => {1,33}}

% % Test cases:

% test:filter(fun erlang:is_integer/1, [3], [3]) == #{false => not_found,true => {3,3}}
% test:filter(fun erlang:is_integer/1, [0, 1,2,3, 6], [0, 1,2,3, 5]) == #{false => not_found,true => {6,5}}
% test:filter(fun erlang:is_integer/1, [0, 1,2,d, 3, 6], [0, 1,2,d, 3, 5])  == #{false => {d,d},true => {6,5}}
% test:filter(fun erlang:is_integer/1, [0, 1,2,d, 3, 6, 5], [0, 1,2,d, 3, 5, d])  == #{false => {d,d},true => {6,5}}
% test:filter(fun erlang:is_integer/1, [a], [a,d,f]) == #{false => {a,a},true => not_found}
% test:filter(fun erlang:is_integer/1, [a,d,f,g,g], [a,d,f]) == #{false => {f,f},true => not_found}
% test:filter(fun(X) -> X + 1 < 1 end, [a,d,f,g,g], [a,d,f])  == #{false => not_found,true => not_found}
% test:filter(fun(X) -> X + 1 < 11 end, [1,d,f,g,g], [1,d,f]) == #{false => not_found,true => {1,1}}
% test:filter(fun(X) -> X + 1 < 11 end, [1,11, d,f,g,g], [1,11,d,f]) == #{false => {11,11},true => {1,1}}
% test:filter(fun(X) -> X + 1 < 11 end, [1,11, d,f,g,g], [1,11, 12, d,f])  == #{false => {11,11},true => {1,1}}
% test:filter(fun(X) -> X + 1 < 11 end, [], []) == #{false => not_found,true => not_found}
% test:filter(fun(X) -> X + 1 < 11 end, [], [h])  == #{false => not_found,true => not_found}
% test:filter(fun(X) -> X + 1 < 11 end, [], [1]) == #{false => not_found,true => not_found}

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
