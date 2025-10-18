-module(warmup).
-compile(export_all).

% Goal: Calculate the factorial of a non-negative integer (e.g., 5! = 5 * 4 * 3 * 2 * 1).
% factorial(integer()) -> integer():
factorial(0) -> 1;
factorial(X) -> X * factorial(X - 1).

% Goal: Write a function my_length/1 that calculates the number of elements in a list.
% Signature: my_length([any()]) -> integer()

% my_length([_ | T]) -> 1 + my_length(T);
% my_length([]) -> 0.

your_length(My_List) -> your_length(My_List, 0).
your_length([_ | T], Len) -> your_length(T, Len + 1);
your_length([], Len) -> Len.

% warmup:my_length([]) should return 0.
% warmup:my_length([a, b, c]) should return 3.
% warmup:my_length([1, 1, 1, 1, 1]) should return 5.

% Goal: Write a function filter_positives/1 that takes a list of numbers and
% returns a new list containing only the numbers that are greater than 0.
% Signature: filter_positives([number()]) -> [number()]

filter_positives([H | T]) when H > 0 -> [H | filter_positives(T)];
filter_positives([_ | T]) -> filter_positives(T);
filter_positives([]) -> [].

% warmup:filter_positives([]) should return [].
% warmup:filter_positives([1, -2, 3, 0, -5, 6]) should return [1, 3, 6].
% warmup:filter_positives([-1, -2, -3]) should return [].

% Goal: Write a function my_zip/2 that takes two lists
% and combines them into a list of two-element tuples.
% The resulting list should be as long as the shortest input list.
% Signature: my_zip([any()], [any()]) -> [{any(), any()}]

my_zip([H1 | T1], [H2 | T2]) -> [{H1, H2} | my_zip(T1, T2)];
my_zip([], _) -> [];
my_zip(_, []) -> [].

% warmup:my_zip([a, b, c], [1, 2, 3]) should return [{a,1}, {b,2}, {c,3}].
% warmup:my_zip([a, b], [1, 2, 3, 4]) should return [{a,1}, {b,2}].
% warmup:my_zip([], [1, 2, 3]) should return [].
