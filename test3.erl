-module(test3).
-compile(export_all).

% Task 1: Repeat an operation while a certain condition holds (10 point)
% Define the higher-order function repeat_while/3.
% The function iteratively applies an operation on a value while a given condition holds for the value.

% The function takes three arguments:

% 1. The first argument is a predicate (a function with arity 1), that returns either true or false.

% 2. The second argument is a function with only one argument.
% This function is repeatable/iterable, that is the domain and the range/codomain are of the same type.

% 3. The value that has to be iteratively transformed with the given function to fulfil the given condition.

% The function returns a list of values, that involves the values of previous computations.

% repeat_while(fun((any()) -> boolean()), fun(any()) -> any()), any) -> any())
repeat_while(Pred, F, Elem) ->
    case Pred(Elem) of
        true -> [Elem | repeat_while(Pred, F, F(Elem))];
        false -> []
    end.

% Test cases:

% test3:repeat_while(fun(E) -> E > 10 end, fun(E) -> E - 1 end, 20) == [20,19,18,17,16,15,14,13,12,11]

% test3:repeat_while(fun(E) -> E > 65 end, fun(E) -> E - 1 end, 66) == "B"

% test3:repeat_while(fun(E) -> E > 65 end, fun(E) -> E - 1 end, 65) == []

% test3:repeat_while(fun(E) -> E > 600 end, fun(E) -> E - 1 end, 10) == []

% Task 2: Repeated elements at least *n* times (10 points)
% Define the function elems_repeated_at_least_ntimes/2.
% The function returns the elements of the given list that are repeated at least N times in the list.
% The resulted list should contain only unique elements.

% The arguments of the function are:

% 1. a number that is the threshold value for the minimum of repetition,

% 2. a list that has to be processed.

% elems_repeated_at_least_ntimes(integer(), [any()]) -> [any()]
elems_repeated_at_least_ntimes(Threshold, L) -> elems_tldr(Threshold, L, L, []).
elems_tldr(Threshold, [H | T], L, Res) ->
    N = occ(H, L, 1),
    case {N > Threshold, lists:member(H, Res)} of
        {true, false} -> elems_tldr(Threshold, T, L, [H | Res]);
        _ -> elems_tldr(Threshold, T, L, Res)
    end;
elems_tldr(_, [], _, Res) ->
    Res.

occ(Elem, [H | T], Occ) when H == Elem -> occ(Elem, T, Occ + 1);
occ(Elem, [_ | T], Occ) -> occ(Elem, T, Occ);
occ(_, [], Occ) -> Occ.

% Test cases:

% **Note:** It is not mandatory to return values in a sorted order!

% test3:elems_repeated_at_least_ntimes(0, [1,4,3]) == [1,3,4]

% test3:elems_repeated_at_least_ntimes(-3, [1,4,3]) == [1,3,4]

% test3:elems_repeated_at_least_ntimes(2, [1,4,3]) == []

% test3:elems_repeated_at_least_ntimes(2, [1, 4, 1, 3]) == [1]

% test3:elems_repeated_at_least_ntimes(2, [2,1,4,1,3,2]) == [1,2]

% test3:elems_repeated_at_least_ntimes(2, [2,1,4,1,3,2,2]) == [1,2]

% test3:elems_repeated_at_least_ntimes(2, "Mississippi") == "ips"

% test3:elems_repeated_at_least_ntimes(4, "Mississippi") == "is"

% Task 3: Evaluate Polynomial (10 points)
% Define the function eval_polynomial/2 that evaluates a polynomial given by its coefficients at a given point.

% The arguments of the functions are:

% 1. a list of coefficients,

% 2. a given point (numeric value).

% For example, the list [3,4,5,6,0,1] of coefficients stands for
% the polynomial 3*x^5 + 4*x^4  + 5*x^3 + 6*x^2 + 0*x^1 + 1*x^0.
% The former polynomial evaluated in point 3 gives the value 1243.0.

% eval_polynomial([number()], number()) -> number()
eval_polynomial([], _) -> 0;
eval_polynomial([H | T], Point) -> pol(Point, H, length(T)) + eval_polynomial(T, Point).

pol(Point, H, Pow) -> H * math:pow(Point, Pow).

% Test cases:

% test3:eval_polynomial([3,4,5,6,0,1], 1) == 19.0

% test3:eval_polynomial([3,4,5,6,0,1], 3) == 1243.0

% test3:eval_polynomial([32,4,5,1,0], 1) == 42.0

% test3:eval_polynomial([32,4,5,1,0], 0) == 0.0

% test3:eval_polynomial([], 3) == 0

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
