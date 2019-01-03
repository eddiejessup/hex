Haskell Style Guide
===================

Formatting
----------

#### Line Length

Avoid lines over 80 characters.

#### Indentation

Avoid tabs. Indent code blocks by four spaces. Indent the `where` keyword by two spaces, then indent its definitions by two spaces. For example,

```haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```

#### Blank Lines

Separate top-level definitions by one blank line. Don't add blank lines between type signatures and function definitions. Separate long type class instance declaration functions by one blank line.

#### Whitespace

Surround binary operators with one space. Don't add space after a lambda (`\`).

Add a space after each comma in tuple or list literals. For example, prefer `('a', 'b')` to `('a','b')`.

#### Data Declarations

Align data type definitions' data constructors. Format like,

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

or, for long names,

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

Format records like,

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

#### List Declarations

Align list elements like,

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

or,

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

#### Pragmas

Add function pragmas immediately below the function, like,

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

Add data type definition pragmas before the type, like,

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

#### Hanging Lambdas

Decide whether to indent the code following a hanging lambda based on its context:

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
          putStrLn "Here comes a number!"
          print n

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

#### Export Lists

Format export lists like,

```haskell
module Data.Set
    (
      -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , member
    ) where
```

#### If-then-else clauses

Prefer guards and case expressions to if-then-else clauses. Put short cases on one line.

Indent and align the `then` and `else` keywords, for example,

```haskell
foo = do
    someCode
    if condition
        then someMoreCode
        else someAlternativeCode
```

```haskell
foo = bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
```

Format nested `do` blocks like,

```haskell
foo = do
    instruction <- decodeInstruction
    skip <- load Memory.skip
    if skip == 0x0000
        then do
            execute instruction
            addCycles $ instructionCycles instruction
        else do
            store Memory.skip 0x0000
            addCycles 1
```

#### Case expressions

Indent case expression alternatives like,

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

or like,

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

If expression alternatives vary widely in length, group them into similarly aligned blocks, for example,

```haskell
f :: Foo -> Int
f x =
    case x of
        (A _)   -> _
        (B _ _) -> _
        (C _)   -> _
        (VeryLongNameHere _ _ _) -> _
        (PrettyLong _ _ _)       -> _
        ...
```

Imports
-------

Group and order imports into,

1. Standard library
2. Related third-party
3. Local application/library specific

Separate each group with a blank line. Sort each group's imports alphabetically by module name.

Use explicit import lists or `qualified` imports for standard and third-party libraries, except the prelude.

Comments
--------

#### Punctuation

Write proper sentences; start with a capital letter and use proper punctuation.

#### Top-Level Definitions

Comment all top level functions, particularly exported functions, and provide a type signature. Use Haddock syntax, for example,

```haskell
-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

Document a function such that one can use the function without inspecting its definition.

Comment all exported data types, for example,

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

Format fields requiring longer comments like,

```haskell
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: !Text

      -- | This is a second very very very long comment that is split
      -- over multiple lines.
    , field2 :: !Int
    }
```

#### End-of-Line Comments

Separate end-of-line comments from code with 2 spaces. Align comments for data type definitions. For example,

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

#### Links

Link to an API name for the first occurrence of the API name in the comment, if the user might actually click on it.

Naming
------

Name functions with camel-case, for example, `functionName`. Name data types with Pascal-case, for example, `DataType`.

To aid readability, don't capitalize all letters of an abbreviation; for example, write `HttpServer` instead of `HTTPServer`. Make an exception for two-letter abbreviations such as `IO`.

#### Modules

Name modules in the singular; for example, write `Data.Map` and `Data.ByteString.Internal` instead of `Data.Maps` and `Data.ByteString.Internals`.

Laziness
--------

Use strict data types and lazy functions by default.

#### Data types

Use strict constructor fields unless there's a reason to make them lazy. This reduces problems caused by too much laziness and eases reasoning about evaluation order.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Consider unpacking simple fields to improve performance and memory usage. Do this with a pragma, for example,

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

or by adding a flag to the top of the file,

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

Add this flag to the module instead of the Cabal file, so the option will be applied even if the file is compiled by other means.

`-funbox-strict-fields` applies to all strict fields, not just small fields such as `Double` or `Int`. Use the `NOUNPACK` pragma to opt out of the unpacking enabled by the flag.

#### Functions

Use lazy function arguments unless there's a reason to make them strict. The most common reason to need strict function arguments is when using recursion with an accumulator, for example,

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

Point-free style
----------------

Avoid point-free style when it harms readability. For example, avoid definitions like,

```haskell
f = (g .) . h
```

Case expressions versus multiple equations
------------------------------------------

Use multiple equations for short functions with few cases, for example,

```haskell
f :: Maybe Int -> Int
f Nothing  = 0
f (Just n) = n
```

A case expression would add a line, and require a variable binding.

As the number of cases, and the function name's length grows, prefer case expressions. Compare,

```haskell
freeVars :: Exp -> Set Var
freeVars (VarE v)        = ... short expr ...
freeVars (LitE _)        = ... short expr ...
freeVars (LitSymE _)     = ... short expr ...
freeVars (AppE _v _ ls)  = ... short expr ...
freeVars (PrimAppE _ ls) = ... short expr ...
...
```

with,

```haskell
freeVars :: Exp -> Set Var
freeVars e0 = case e0 of
    VarE v        -> ... short expr ...
    LitE _        -> ... short expr ...
    LitSymE _     -> ... short expr ...
    AppE _v _ ls  -> ... short expr ...
    PrimAppE _ ls -> ... short expr ...
    ...
```

The case style,

- Avoids distracting repetition of the identifier `freeVars`
- Eases refactoring to add `let` and `where` clauses
- Provides the original variable binding without an `@` pattern
- Reduces the need for parentheses

Revert to multiple equations for very long functions, to better orient the reader when the first definition line is off the screen:

```haskell
f :: Foo -> Int
f (A _) = ... > 1 page of code...

f (B _ _) = ... > 1 page of code...
```

(However, try to avoid such long functions at all.)

Warnings
--------

Write code that compiles with `-Wall -Werror`.
