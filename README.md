lambda
======

Lamda is an interpreter for a toy function language of the same name. It is written in Haskell, using Alex/Happy for parsing.

The language is based on polymorphic lambda calculus with Hindley-Milner type system. It allows no explicit type annotations, types are fully inferred and statically check. Lambda supports algebraic data types and simple pattern matching.

Limitations include: no type constructors other then built-in lists, no higher kinded types obviously, no typeclasses, no modules.

Example code
============

```
-- Function application.
f $ x = f x;

-- Function composition.
f . g = λx -> f (g x);

-- Flips arguments of a function.
flip f x y = f y x;

-- We can define "if" using pattern matching.
if cond th el =
    case cond of {
         True -> th;
         False -> el
    };

-- Absolute value.
abs x =
    if (x >= 0)
       x
       (0 - x);

-- Fibonacci!
fib x =
    if (x <= 1)
        1
        ((fib (x - 1)) + (fib (x - 2)));

-- Right fold on a list.
foldr k z =
    let go l = if (null l)
            z
            (k (head l) (go (tail l)))
    in go;

-- Concatenates two lists.
(++) = flip $ foldr (:);

-- String to int conversion.
indexOf e =
    let indexOf' i l =
        if ((head l) == e)
            i
            (indexOf' (i + 1) (tail l))
    in  indexOf' 0;
strToInt =
    let digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    in  foldr (λc n -> (10*n) + (indexOf c digits)) 0;

-- Trees!
data Tree = Leaf Int | Node Tree Tree;

exampleTree =
    Node
        (Node (Leaf 5) (Leaf 9))
        (Node (Node (Leaf 3) (Leaf 1)) (Leaf 9));

printTree t =
    case t of {
         Leaf i -> printInt i;
         Node t1 t2 -> 
            seq (printStr "(") $ 
            seq (printTree t1) $
            seq (printStr " ") $
            seq (printTree t2) $
                 printStr ")"
    };

-- Anything that main returns will be evaluated to WHNF (at least for now).
main = printTree exampleTree;

-- Some pattern matching and parsing tests.
printBool b =
    case b of {
         True -> printStr "True";
         False -> printStr "False"
    };

unitTest = case () of {
         () -> printInt 5;
         _ -> error "wtf!"
    };

-- Data types parsing tests.
data Test = Test Int [Int] [Bool] [[[Tree]]]
          | Test2 Double [Double]
          | Test3 () () () ()
          | Test4;
```

Usage
=====

```
Usage: lambda [INPUT FILE] [-o|--output FILE] [-a|--action ACTION]
  Lambda interpreter.

Available options:
  -h,--help                Show this help text
  -o,--output FILE         Output file.
  -a,--action ACTION       Compiler action. (default: Evaluate)
  
Possible actions:
  DumpAST                  Dumps AST without type annotations.
  DumpTypedAST             Dumps AST with type annotations.
  DumpObjects              Dumps objects.
  Evaluate                 Evaluates the "main" function.
```
