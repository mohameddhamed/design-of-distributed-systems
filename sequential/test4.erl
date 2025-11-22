-module(test4).
-compile(export_all).

% ~~~~
% %% <Name>
% %% <Neptun ID>
% %% <DDS, TEST1>
% %% <22.03.2018.>
% %% This solution was submitted and prepared by <Name, Neptun ID> for the DDS Retake Test Sequential.
% %% I declare that this solution is my own work.
% %% I have not copied or used third party solutions.
% %% I have not passed my solution to my classmates, neither  made it public.
% %% Students’ regulation of Eötvös Loránd University (ELTE Regulations Vol. II. 74/C. § ) states that as long as a student presents another student’s work - or at least the significant part of it - as his/her own performance, it will count as a disciplinary fault. The most serious consequence of a disciplinary fault can be dismissal of the student from the University.
% ~~~~

% Task no. 1:
% =================================

% Ethiopian multiplication is a method of multiplying integers using only addition, doubling, and halving.

% Define three functions:
% 	one to halve an integer,
halve(N) -> N div 2.
% 	one to double an integer, and
double(N) -> N * 2.
% 	one to state if an integer is even.
is_even(N) -> (N rem 2) == 0.

% Use them to create multiplier(L,R), that takes the two numbers (Left and Rigth)
% and compute their product in the following way:

% 	Half the Left number until you get to 1, at the same time Double the Rigth number for the same amount of times.
% 	Discard any value from Rigth where the corresponding value in Left is even.
% 	Sum all number left in right to produce the result of L*R.

multiplier(_, 0) -> forbidden;
multiplier(Left, Right) -> lists:sum(multiplierf(Left, Right)).

multiplierf(0, _) ->
    [];
multiplierf(1, Right) ->
    [Right | multiplierf(0, Right)];
multiplierf(Left, Right) ->
    L = halve(Left),
    R = double(Right),
    case is_even(Left) of
        true -> multiplierf(L, R);
        false -> [Right | multiplierf(L, R)]
    end.

% multiplier(Left, Right) ->
%     ParityList = repeat_until(halve, Left, 1, []),
%     Right = repeat(double, Right, Steps, 0, ParityList),

% repeat_until(_, N, Cond, ParityList) when N == Cond -> ParityList ++ [is_even(N)];
% repeat_until(F, N, Cond, ParityList) -> repeat_until(F, F(N), Cond, ParityList) ++ [is_even(N)].

% repeat(_, _, []) -> [];
% repeat(F, N, [Parity | Tail]) when Parity -> [F(N) | repeat(F, F(N), Tail)];
% repeat(F, N, [_ | Tail]) -> [repeat(F, F(N), Tail)].

%  Ex.   17    34            discard 68,136 and 272
%         8    68 			34 + 544 == 17*34
%         4   136
%         2   272
%         1   544

% %%%%%%%%%%%%#1
% ~~~
% multiplier(integer(),integer()) -> integer()
% ~~~

% Test cases:
% -----------

% **Do not forget to change the name of the module!**

% ~~~
% test4:multiplier(12, 3) == (12 * 3).
% test4:multiplier(0,0)== forbidden.
% test4:multiplier(7,0)== forbidden.
% ~~~

% Task no. 2: is_numeric/1
% ==========================

% Create the function is_numeric(S), that check if a given string is composed only by numbers
% (Integer and Float is good), return true if the argument passed is either an integer or a float.
% **IMPORTANT:** you need to reimplement the function, do not use the built-in function is_number/1.

% ~~~
% is_numeric(S:list()) -> boolean()
is_numeric(L) ->
    num =
        try
            list_to_integer(L)
        catch
            error:badarg ->
                try
                    list_to_float(L)
                catch
                    error:badarg -> error
                end
        end,
    case num of
        error -> false;
        _ -> true
    end.

% ~~~

% Test cases:
% -----------

% **Do not forget to change the name of the module!**

% ~~~
test4:is_numeric("123") == true.
% test4:is_numeric("12.3")==true.
% test4:is_numeric(".12.3")==false.
% test4:is_numeric(".12.3.")==false.
% test4:is_numeric("123.")==false.
% test4:is_numeric("")==false.

% ~~~

% Task no. 3: replace/3
% ==============================

% Define the replace/3 function that takes three strings as arguments and
% replaces every other occurrence of the substring Old with New in Str.
% ~~~
% replace(Str::list(), Old::list(), New::list())-> list()
% ~~~

% Test cases:
% -----------

% **Do not forget to change the name of the module!**

% ~~~
% test4:replace("AppleAppleApple", "Apple", "Pear")=="PearApplePear"
% test4:replace("AppleAppleApple", "Apple", "")=="Apple"
% test4:replace("AppleAppleApple", "App", "Pear")=="PearleApplePearle"

% ~~~

% Task no. 4: partition/2
% =================

% Define the higher-order function partition/2, which will take a list and a boolean Predicate P and return two Lists: the first one that has the terms which satisfy P, and the second one a list for the others terms.
% **IMPORTANT:** Use recursion, you are not allowed to use the built in function lists:partition/2.

% ~~~
% partition(Predicate::fun(any() -> boolean()), List(E))->Result::list(E)
% ~~~
% test4:partition(fun(X)-> X>10 end ,[7,10,8,155,133] )==[[155,133],[7,10,8]]
% test4:partition(fun(X)-> X rem 2==0  end ,[7,8,2222,223] )==[[8,2222],[7,223]]
% test4:partition(fun(X)-> is_atom(X)  end ,[[1,2],[alma],hello] )==[[hello],[[1,2],[alma]]]

% ~~~
