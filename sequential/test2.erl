-module(test2).
-compile(export_all).

% Task 1: Merging frequencies (10 points)
% Define the function merge_freq/2. The function takes two lists as arguments
% where both lists contain frequency pairs: {Elem::term(), Freq::integer()}.
% The function returns the merged frequency list. If an element appears in both lists the merged frequency is the sum of the frequencies.

% merge_freq([{Elem::term(), Freq::integer()}], [{Elem::term(), Freq::integer()}]) -> [{Elem::term(), Freq::integer()}].

getFreq(Target, [{Elem, Freq} | _]) when Target == Elem -> Freq;
getFreq(Target, [_ | T]) -> getFreq(Target, T);
getFreq(_, []) -> 0.

merge_freq([{Elem, Freq} | T], L) ->
    [{Elem, Freq + getFreq(Elem, L)}] ++ merge_freq(T, remove_Elem(Elem, L));
merge_freq([], L) ->
    L.

remove_Elem(Elem, [{E, _} | T]) when E == Elem -> T;
remove_Elem(Elem, [H | T]) -> [H] ++ remove_Elem(Elem, T);
remove_Elem(_, []) -> [].

% H1 : {a, 1}, Res : [{}, {b,1}, {a,2}]

% Test cases:

% The order of the elements in the return value is not specified, so the return value of the function is sorted in the following tests:

% lists:usort(test2:merge_freq([{a, 1}], [{c, 1}, {a, 12}])) == [{a, 13}, {c, 1}].
% lists:usort(test2:merge_freq([{a, 1}, {c, 1}, {d, 1}, {b, 2}], [{d, 1}, {c, 12}])) == [{a, 1}, {b, 2}, {c, 13}, {d, 2}].
% lists:usort(test2:merge_freq([{a, 1}, {c, 1}, {d, 1}, {g, 2}], [{c, 12}, {e, 2}, {f, 3}, {w, 2}])) == [{a, 1}, {c, 13}, {d, 1}, {e, 2}, {f, 3}, {g, 2}, {w, 2}].
% lists:usort(test2:merge_freq([{a, 1}, {32, 1}, {d, 13}, {"apple", 2}], [{d, 1}, {c, 12}])) ==
%     [{32, 1}, {a, 1}, {c, 12}, {d, 14}, {"apple", 2}].
% lists:usort(test2:merge_freq([{a, 1}, {32, 1}, {d, 13}, {"apple", 2}], [])) ==
%     [{32, 1}, {a, 1}, {d, 13}, {"apple", 2}].
% lists:usort(test2:merge_freq([{a, 1}, {"a", 2}], [])) == [{a, 1}, {"a", 2}].
% lists:usort(test2:merge_freq([], [{a, 1}, {"a", 2}])) == [{a, 1}, {"a", 2}].
% lists:usort(test2:merge_freq([], [])) == [].

% Task 2: Sum frequencies (10 points)
% Define the function sum_freq/2. The function takes a map and a list as arguments.
% The map contains frequencies of some Erlang terms and the list contains some Erlang terms.
% The function needs to calculate the sum of the frequencies of the elements listed in its list argument and returns a pair.
% The first element of the pair is the sum, and the second one is the map which is the original map argument extended with
% the Elem => not_defined association when an element from the list is not found in the map.

% sum_freq(map(), [any()]) -> {integer(), map()}
sum_freq(M, L) -> sum_freq0(M, L, 0).
sum_freq0(M, [H | T], Sum) ->
    Elem =
        try
            maps:get(H, M)
        catch
            _:_ -> error
        end,

    case Elem of
        Value when is_integer(Value) ->
            sum_freq0(M, T, Sum + Value);
        error ->
            sum_freq0(M#{H => not_found}, T, Sum)
    end;
sum_freq0(M, [], Sum) ->
    {Sum, M}.

% Test cases:

% test2:sum_freq(#{a => 12, b => 13}, [a]) == {12,#{a => 12,b => 13}}.
% test2:sum_freq(#{a => 12, b => 13}, [b,a]) == {25,#{a => 12,b => 13}}.
% test2:sum_freq(#{a => 12, b => 13}, [b, c, a]) == {25, #{a => 12, b => 13, c => not_found}}.
% test2:sum_freq(#{a => 12, b => 13}, []) == {0,#{a => 12,b => 13}}.
% test2:sum_freq(#{}, []) == {0,#{}}.
% test2:sum_freq(#{}, [a,b]) == {0,#{a => not_found,b => not_found}}.
% test2:sum_freq(#{a => 12, b => 13, c=>13}, [b, a]) == {25,#{a => 12,b => 13,c => 13}}.
% test2:sum_freq(#{a => 12, b => 13, c=>13}, []) == {0,#{a => 12,b => 13,c => 13}}.
% test2:sum_freq(#{a => 12, b => 13, c=>13}, [b, e]) == {13,#{a => 12,b => 13,c => 13,e => not_found}}.

% Task 3:  Apply (10 points) - without error handling 8 points
% Define the function apply_times/2 that takes a function F and a frequency list L as arguments.
% It evaluates F on the elements of a frequency list by applying the function on the element as many times as its frequency.

% If the evaluation of the function F fails on the element, you have to put the atom failing_operation to the result list.

% filter(fun(any()) -> any() end, [{any(), integer()}]) -> list()

% Test cases:

% test2:apply_times(fun(X) -> X + 1 end, [{1, 12}, {3, 3}, {4, 12}]) == [13,6,16].
% test2:apply_times(fun(X) -> X * 2 end, [{1, 12}, {3, 3}, {4, 12}]) == [4096,24,16384].
% test2:apply_times(fun(X) -> X + 1 end, [{u, 12}, {3, 3}, {s, 12}]) == [failing_operation,6,failing_operation].
% test2:apply_times(fun(X) -> X + 1 end, []) == [].
% test2:apply_times(apple, []) ==  [].
% test2:apply_times(apple,  [{1, 12}, {3, 3}, {4, 12}]) == [failing_operation,failing_operation,failing_operation].

apply_times(F, [{Elem, Freq} | T]) -> [f_apply(F, Elem, Freq) | apply_times(F, T)];
apply_times(_, []) -> [].

f_apply(F, Elem, Freq) ->
    try
        int_apply(F, Elem, Freq)
    catch
        _:_ -> failing_operation
    end.

% apply

int_apply(_, Elem, Freq) when Freq == 0 -> Elem;
int_apply(F, Elem, Freq) -> int_apply(F, F(Elem), Freq - 1).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%
