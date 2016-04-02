Haskell Notes
=============


Compiling
---------

Make sure that `ghc` is on your path, and create a directory called `compiled`.

    ghc nnproblems.hs -odir compiled -hidir compiled -o compiled\nnproblems && compiled\nnproblems

Basics
------

\`Backticks\` make a function name into an infix operator:

    mod 19 3
    19 `mod` 3

`++` concatenates lists:

    [] ++ [False,True] ++ [True]
        [False,True,True]

`(:)` is the `cons` operator, which adds an element to the front of a list.

Haskell requires type names to start with an uppercase letter, and variable names must start with a lowercase letter.

`()` is a special type, 'unit', that is a tuple of zero elements, and has only one value: `()`. It's similar to C's `void`.

Tuples can contain mixed types. You can't have a tuple of 1 element.


Lambda Functions
----------------

Use `\` to create lambda functions (the backslash is supposed to look kind of like a lambda with the short leg missing)

    greaterThan100 :: [Integer] -> [Integer]
    greaterThan100 xs = filter (\x -> x > 100) xs

    main = print (greaterThan100 [1,9,349,6,907,98,105])

Here's one that takes multiple arguments:

    main = print ((\x y z -> [x,2*y,3*z]) 5 6 3)


Operator Sections
-----------------

`(>100)` is an operator section: if `?` is an operator, then `(?y)` is equivalent to the function `\x -> x ? y`, and `(y?)` is equivalent to `\x -> y ? x`. e.g.

    greaterThan100 :: [Integer] -> [Integer]
    greaterThan100 xs = filter (>100) xs

    main = print (greaterThan100 [1,9,349,6,907,98,105])


Function Composition
--------------------

Functions are composed with `(.)`.

If `f` and `g` are functions, then `f . g` is the function which does `g` first and then `f`.


Currying
--------

All functions in Haskell are automatically curried. That is, for any function arguments can be applied one at a time, each application producing a function which takes one fewer argument.

`curry` and `uncurry` allow you to convert between a function which takes a pair of arguments, and a function of two arguments that can be applied individually. Their definitions would look like:

    curry :: ((a,b) -> c) -> a -> b -> c
    curry f x y = f (x,y)

    uncurry :: (a -> b -> c) -> (a,b) -> c
    uncurry f (x,y) = f x y

`uncurry`, in particular, can be useful when you have a pair and want to apply a function to it. For example:

    uncurry (+) (2,3)

Note that Haskell doesn’t make it easy to partially apply to an argument other than the first. There is an art to deciding the order of arguments to a function to make partial applications of it as useful as possible: the arguments should be ordered from from "least to greatest variation", that is, arguments which will often be the same should be listed first, and arguments which will often be different should come last.


Point-free
----------

Point-free functions are functions that don't refer to their arguments, e.g.:

    foobar :: [Integer] -> Integer
    foobar = sum . map (\x -> 7*x + 2) . filter (>3)

    main = print (foobar [1,2,3,4,5])

You should strive for functions that are point-free, but not at the sake of
readability.


Folds
-----

Equivalent of reduce. Provided as `foldr` and `foldl`.

    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f z [a,b,c] == a `f` (b `f` (c `f` z))

    foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c

Typically, the second argument (the starting value) is the right-identity of the binary operator that is the first argument.

For example, to sum an array:

    sumArr = foldr (+) 0

In general, however, you should use `foldl'` from `Data.List` instead, which does the same thing as `foldl` but is more efficient.


Type Classes
------------

`Num`, `Eq`, `Ord`, and `Show` are type classes, and we say that `(==)`, `(<)`, and `(+)` are "type-class polymorphic" - they work on any type so long as it is an instace of the right type class(es). Type classes define 'interfaces' that a type must implement.

This is the `Eq` type class.

    class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool

Any type a which wants to be an instance of Eq must define two functions, `(==)` and `(/=)`, with the indicated type signatures.

    (==) :: Eq a   => a -> a -> Bool

The `Eq a` that comes before the `=>` in the definition of `(==)` is a _type class constraint_.

Functions can have multiple constraints:

    foo :: (Listable a, Ord a) => a -> a -> Bool

Here's an example of declaring an instance of `Eq` for a class (so `Foo` can now be used where a type supporting `Eq` is required):

    data Foo = F Int | G Char

    instance Eq Foo where
      (F i1) == (F i2) = i1 == i2

      (G c1) == (G c2) = c1 == c2

      _ == _ = False

We only need to declare one of `(==)` or `(/=)`, since the second is taken to just be logical-not the first.

In fact, the compiler can automatically generate instances of common type classes:

    data Foo' = F' Int | G' Char
      deriving (Eq, Ord, Show)


Standard Type Classes
---------------------

* `Ord` - Types whose elements can be totally ordered. It provides comparison operations and the `compare` function.
* `Num` - Numeric types. Support addition, subtraction, multiplication, ...
* `Show` - Converts values into strings.
* `Read` - Converts strings into values.
* `Integral` - Whole number types.


