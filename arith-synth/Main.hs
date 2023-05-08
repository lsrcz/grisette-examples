{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where
 
import Grisette
import GHC.Generics
import Data.Proxy

--------------------------------------------------------------------------------
-- Symbolic programs
--------------------------------------------------------------------------------

-- We will synthesize single-input programs in this example. A single input
-- program will be \x -> E, where E is defined by the following grammar:
--
-- E -> c      -- constant
--    | x      -- value for input variable
--    | E + E  -- addition
--    | E * E  -- multiplication
--
-- The syntax defines how a concrete program is represented. In Grisette, to
-- synthesis a program, we need to define a symbolic program that represents a
-- whole space of program.

data SProgram
  -- `SConst` represents a constant in the syntax tree.
  --
  -- `SConst 1` is the constant 1, while `SConst "c1"` is a symbolic constant,
  -- and the solver can be used to find out what the concrete value should be.
  = SConst SymInteger
  -- `SInput` is very similar to the `SConst`, but is for inputs. We separate
  -- these two mainly for clarity.
  --
  -- We do not use a solver to solve for a input variable for synthesis tasks,
  -- and a program with symbolic program input is still considered as a concrete
  -- program. We will further show why we design it this way when we talk about
  -- pretty printing.
  | SInput SymInteger
  -- `SPlus` and `SMul` represent the addition and multiplication operators.
  --
  -- The children are **sets** of symbolic programs. Here `UnionM`s are such
  -- sets.
  --
  -- The solver will try to pick one concrete program from the set of programs.
  | SPlus (UnionM SProgram) (UnionM SProgram)
  | SMul (UnionM SProgram) (UnionM SProgram)
  -- `Generic` helps us derive other type class instances for `SProgram`.
  deriving stock (Generic, Show)
  -- Some type classes provided by Grisette for building symbolic evaluation
  -- tools. See the documentation for more details.
  deriving (Mergeable, EvaluateSym, ToCon SProgram)
    via (Default SProgram)

-- A template haskell procedure to help the construction of `SProgram` sets.
--
-- >>> SConst 1 :: SProgram
-- SConst 1
-- >>> mrgSConst 1 :: UnionM SProgram
-- UMrg (Single (SConst 1))
$(makeUnionWrapper "mrg" ''SProgram)

--------------------------------------------------------------------------------
-- Quick examples
--------------------------------------------------------------------------------

programSpace :: SymInteger -> UnionM SProgram
programSpace x = mrgSMul (mrgSInput x) (mrgSConst "c")
-- >>> executableProgramSpace 2
-- (* 2 c)
-- >>> executableProgramSpace "x"
-- (* x c)
executableProgramSpace :: SymInteger -> SymInteger
executableProgramSpace = interpretU . programSpace

quickExample :: IO ()
quickExample = do
  res <- solve (precise z3) (executableProgramSpace 2 ==~ 4)
  case res of
    Left err -> print err
    Right mo -> do
      print mo
      -- Model {c -> 2 :: Integer}
      let synthesizedProgram = evaluateSym False mo (programSpace "x")
      print synthesizedProgram
      -- UMrg (Single (SMul (UMrg (Single (SInput x))) (UMrg (Single (SConst 2)))))
      let executableSynthesizedProgram :: Integer -> Integer =
            evaluateSymToCon mo . executableProgramSpace . toSym
      print $ executableSynthesizedProgram 2
      -- 4

--------------------------------------------------------------------------------
-- Program spaces
--------------------------------------------------------------------------------

-- We then construct program spaces as symbolic programs. The `mrgIf` combinator
-- can be handy to let solver choose from different programs.

-- For example, the following program represents the program space:
-- \x -> x + {x or c}
-- or equivalently, (\x -> x + x) or (\x -> x + c)
--
-- The solver can choose from the two programs, and if the second program is
-- chosen the solver will also choose a value for the constant `c`.
--
-- We can call a solver to determine what `c` and `choice` should be. We write
-- and interpreter, interpret the symbolic program on the input 2, and construct
-- the constraint that the result is 5. The solver will then find out that `c`
-- should be 3 and `choice` should be `False`, which means that the program
-- should be \x -> x + 3.
--
-- >>> solve solverConfig $ interpretU (space1 2) ==~ 5
-- Right (Model {c -> 3 :: Integer, choice -> False :: Bool})
--
-- We will discuss this later in detail.
space1 :: SymInteger -> UnionM SProgram
space1 x =
  mrgSPlus
    (mrgSInput x)
    (mrgIf "choice" (mrgSInput x) (mrgSConst "c"))
  
-- Another program space:
-- \x -> x {+ or *} {x or 1}
-- or equivalently, (\x -> x + x) or (\x -> x * x) or (\x -> x + 1) or (\x -> x * 1).
space2 :: SymInteger -> UnionM SProgram
space2 x =
  mrgIf "choice1" (mrgSPlus lhs rhs) (mrgSMul lhs rhs)
  where
    lhs = mrgSInput x
    rhs = mrgIf "choice2" (mrgSInput x) (mrgSConst 1)

--------------------------------------------------------------------------------
-- The interpreter
--------------------------------------------------------------------------------

-- The most interesting feature of Grisette is that after you write an
-- interpreter for your DSL, you will automatically get symbolic
-- evaluation-based tools for your language, and you do not have to deal with
-- the low level details of symbolic evaluation.

-- The following interpreter interprets **all** trees represented by an
-- `SProgram`, and the result of the interpretation is a single symbolic formula
-- (an `SymInteger`) that represents the evaluation results of all trees.
interpret :: SProgram -> SymInteger
interpret (SConst c) = c
interpret (SInput c) = c
interpret (SPlus x y) = interpretU x + interpretU y
interpret (SMul x y) = interpretU x * interpretU y

-- The interpretU function is a lifted `interpret` function that works on
-- `UnionM` program sets.
interpretU :: UnionM SProgram -> SymInteger
interpretU = onUnion interpret

-- The following shows some interpretation results.
-- We can interpret the program spaces defined before with symbolic inputs.
-- The result would be a formula with regards to the symbolic variables in the
-- program space, and the symbolic inputs.

-- >>> result1
-- (+ x (ite choice x c))
result1 :: SymInteger
result1 = interpretU $ space1 "x"

-- >>> result2
-- (ite choice1 (+ x (ite choice2 x 1)) (ite choice2 (* x x) x))
result2 :: SymInteger
result2 = interpretU $ space2 "x"

-- We can also interpret the program spaces with concrete inputs. Some term
-- optimization would be performed.

-- >>> result2'
-- (ite choice1 2 1)
result2' :: SymInteger
result2' = interpretU $ space2 1

-- >>> result2''
-- (ite choice1 (ite choice2 6 4) (ite choice2 9 3))
result2'' :: SymInteger
result2'' = interpretU $ space2 3

--------------------------------------------------------------------------------
-- Synthesis from I/O pairs
--------------------------------------------------------------------------------

-- Now we can synthesize programs with the program spaces shown before. We will
-- synthesize from I/O pairs specifications. For example, a program in the
-- program space `space1` that return 5 when given 2 as input and 11 when given
-- 8 as input is \x -> x + 3.

synthesisExample :: IO ()
synthesisExample = do
  r1 <- ioPair space1 [(2, 5), (8, 11)]
  -- Printed result:
  -- Just (SPlus (UMrg (Single (SInput x))) (UMrg (Single (SConst 3))))
  -- Meaning:
  -- \x -> x + 3
  print r1
  r2 <- ioPair space2 [(2, 4), (8, 64)]
  -- Printed result:
  -- Just (SMul (UMrg (Single (SInput x))) (UMrg (Single (SConst x))))
  -- Meaning:
  -- \x -> x * x
  print r2

-- The solver configuration. We use the bounded reasoning mode with Boolector
-- for faster (but may be unsound) solving.
--
-- This assumes that every integer value in the system would not exceed the
-- range of a signed 16-bit integer, which is reasonable in our scenario.
-- The reasoning could be unsound when there is an overflow.
solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3

-- A function that synthesizes programs within the search space given some
-- input-output pairs.
ioPair :: (SymInteger -> UnionM SProgram) -> [(Integer, Integer)] -> IO (Maybe SProgram)
ioPair progSpace pairs = do
  -- Call the solver. The result may be an error or a model.
  -- Here we use the 'constraint' function to construct the constraint.
  -- The 'constraint' function takes a list of symbolic input-output pairs and
  -- we use the 'toSym' function to convert the concrete input-output pairs to
  -- symbolic ones.
  res <- solve solverConfig (constraint $ toSym pairs)
  case res of
    -- Print an error message if no solution was found in the space. 
    Left _ -> return Nothing
    Right model -> do
      -- Evaluate the program space to get the synthesized program.
      -- The toCon is used to convert `UnionM SProgram` to `SProgram`.
      return $ toCon $ evaluateSym False model (progSpace "x")
  where    
    -- make it top level since it's important 
    constraint :: [(SymInteger, SymInteger)] -> SymBool
    constraint [] = con True   -- 'con' type-converts from concrete to symbolic values 
    -- The '~' postfixed operators are the symbolic versions of the
    -- corresponding Haskell operators.
    constraint ((x, y) : xs) = interpretU (progSpace x) ==~ y &&~ constraint xs

--------------------------------------------------------------------------------
-- Modular program space construction
--------------------------------------------------------------------------------

-- We have seen how to construct program spaces manually. Now it is time to
-- construct program spaces in a more modular way.
-- This would help when we want to build very large candidate spaces.

-- An example function that generates a program space.
--
-- The result is maintained in the `Fresh` monad, which allows us to generate
-- fresh symbolic variables without explicitly provide their names. These fresh
-- symbolic variables will be instantiated with `runFresh`.
freshExpr :: [SProgram] -> Fresh (UnionM SProgram)
-- TODO: in the future, can we hide this under a rosette syntax-grammar like construct 
freshExpr terminals = do
  -- choose the left hand side operand
  l <- chooseFresh terminals
  -- choose the right hand side operand
  r <- chooseFresh terminals
  -- choose the operator
  chooseFresh [SPlus l r, SMul l r]
  
  -- TODO: the full code should create ASTs of a given depth k 
  -- say see full code for depth-k trees 

-- move this above freshExpr because it states the goal of our exercise here (it defiens the API). 
-- A program space:
-- \x -> {x or 1 or 2} {+ or *} {x or 1 or 2}
space :: SymInteger -> UnionM SProgram
space x = runFresh (freshExpr [SInput x, SConst 1, SConst 2]) "space"

-- TODO: give this first 
simple :: IO ()
simple = do
  -- Call the synthesizer. The printed result could be verbose.
  -- Grisette provides functionalities to convert it to easier-to-print
  -- programs via the 'ToCon' type class. Please check the documentation for 
  -- more details.
  v1 <- ioPair space [(1, 1), (2, 4), (3, 9)]
  print v1
  -- UMrg (Single (SMul (UMrg (Single (SInt x))) (UMrg (Single (SInt x))))) 
  -- The results means the program \x -> x * x
  v2 <- ioPair space [(1, 2), (2, 4), (3, 6)]
  print v2
  -- UMrg (Single (SPlus (UMrg (Single (SInt x))) (UMrg (Single (SInt x)))))
  -- The results means the program \x -> x + x

--------------------------------------------------------------------------------
-- Better Printing
--------------------------------------------------------------------------------

data PSProgram
  = PSConst Integer
  | PSInput SymInteger
  | PSPlus PSProgram PSProgram
  | PSMul PSProgram PSProgram
  deriving stock (Generic)
  deriving (ToCon SProgram) via (Default PSProgram)

instance Show PSProgram where
  showsPrec p e =
    case e of
      PSConst c -> shows c
      PSInput i -> shows i
      PSPlus l r -> showParen (p > 6) $ showsPrec 6 l . showString " + " . showsPrec 7 r
      PSMul l r -> showParen (p > 7) $ showsPrec 7 l . showString " * " . showsPrec 8 r

ioPairPS :: (SymInteger -> UnionM SProgram) -> [(Integer, Integer)] -> IO (Maybe PSProgram)
ioPairPS progSpace pairs = do
  res <- solve solverConfig (constraint $ toSym pairs)
  case res of
    Left _ -> return Nothing
    Right model -> do
      return $ toCon $ evaluateSym False model (progSpace "x")
  where    
    constraint :: [(SymInteger, SymInteger)] -> SymBool
    constraint [] = con True
    constraint ((x, y) : xs) = interpretU (progSpace x) ==~ y &&~ constraint xs

simplePS :: IO ()
simplePS = do
  v1 <- ioPairPS space [(1, 1), (2, 4), (3, 9)]
  print v1
  v2 <- ioPairPS space [(1, 2), (2, 4), (3, 6)]
  print v2

--------------------------------------------------------------------------------
-- More Advanced Program Spaces
--------------------------------------------------------------------------------

freshExpr' :: Int -> [SymInteger] -> Fresh (UnionM SProgram)
freshExpr' n inputs
  | n <= 0 = leaves
  | otherwise = do
    l <- freshExpr' (n - 1) inputs
    r <- freshExpr' (n - 1) inputs
    leafExpr <- leaves
    arithExpr <- chooseFresh [SPlus l r, SMul l r]
    chooseUnionFresh [leafExpr, arithExpr]
  where
    leaves = do
      c <- simpleFresh ()
      chooseFresh $ SConst c : [SInput x | x <- inputs]

space' :: Int -> SymInteger -> UnionM SProgram
space' depth x = runFresh (freshExpr' depth [x]) "space"

simple' :: IO ()
simple' = do
  v1 <- ioPairPS (space' 2) [(3,42), (4,56), (5,72)]
  print v1
  v2 <- ioPairPS (space' 3) [(3,342), (4,456), (5,572), (6,690)]
  print v2

main :: IO ()
main = do
  putStrLn "---- quick example ----"
  quickExample
  putStrLn "---- synthesis example ----"
  synthesisExample
  simple
  simplePS
  simple'
