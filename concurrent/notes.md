## Statement Separators in Erlang

Erlang uses **semicolons** and **commas** as separators, and they have specific meanings:

- **Comma (`,`)** - separates expressions that are **part of the same clause/sequence**. Think of it as "and then do this next thing"
- **Semicolon (`;`)** - separates **different clauses** (like different pattern matching cases or function clauses)
- **Period (`.`)** - ends a function definition

## Sequential Expressions in a Function Clause

When you have multiple expressions that should execute one after another in the same function clause, you need to connect them properly.

For example:
```erlang
some_function() ->
    first_thing(),
    second_thing(),
    third_thing().
```

All those expressions are part of the **same sequence** - they happen one after another.

## Try-Catch in Erlang

Erlang's error handling is based on the "let it crash" philosophy, but sometimes you need to catch errors. Here's the syntax:

## Basic Structure

```erlang
try
    Expression1,
    Expression2,
    ResultExpression
catch
    ErrorType:ErrorValue -> HandleError
end
```

## Error Types

Erlang has three main error types:
- **`throw`** - for expected, recoverable errors (user-controlled)
- **`error`** - for unexpected runtime errors (like `badarg`, `badarith`)
- **`exit`** - for process termination signals

## Common Patterns

**Catch any error:**
```erlang
try
    risky_operation()
catch
    _:_ -> default_value
end
```

**Catch specific error types:**
```erlang
try
    1/0
catch
    error:badarith -> {error, "Can't divide by zero"}
end
```

**Multiple catch clauses:**
```erlang
try
    some_function()
catch
    throw:custom_error -> handle_throw();
    error:badarg -> handle_badarg();
    exit:Reason -> handle_exit(Reason)
end
```

## The `of` Clause (Optional)

If you want to pattern match on the **successful result**:

```erlang
try
    Expression
of
    Pattern1 -> Result1;
    Pattern2 -> Result2
catch
    error:Error -> handle_error(Error)
end
```

## The `after` Clause (Optional)

Like `finally` in other languages - always executes:

```erlang
try
    open_file_and_process()
catch
    error:E -> log_error(E)
after
    close_file()  % Always runs
end
```

## Key Points

- The try block returns the value of its last expression (if successful) or the catch handler's result
- You can have multiple expressions in the try block separated by commas
- The catch patterns work like function clauses (separated by semicolons)
- Variables bound in `try` are available in `catch` and `after`
