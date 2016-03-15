module Cis194.Hw05UnitTest (hw05Tests) where

import           Cis194.Hw05_Calc
import           Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import           System.Exit (ExitCode(..), exitWith)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Cis194.StackVM  as S
import qualified Data.Map        as M    

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

hw05Tests :: IO Counts
hw05Tests =  runTestTT $ TestList
             [ TestList evalTests
             , TestList parseExpTests
             , TestList evalStrTests
             , TestList ex05Tests
             , TestList ex06Tests
             , TestList ex06'Tests
             ]


evalTests :: [Test]
evalTests = 
    [ testCase "eval" $
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @=? 20
    ]

parseExpTests :: [Test]
parseExpTests = 
    [ testCase "evalStr 1" $
      parseExp Lit Add Mul "(2+3)*4" @=? Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    , testCase "evalStr 2" $
      parseExp Lit Add Mul "2+3*4" @=?   Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
    , testCase "evalStr nothing" $
      parseExp Lit Add Mul "2+3*" @=?  Nothing
    ]


evalStrTests :: [Test]
evalStrTests = 
    [ testCase "evalStr 1" $
      (evalStr "(2+3)*4") @=? (Just (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))))
    , testCase "evalStr 2" $
      (evalStr "2+3*4") @=?  (Just (eval (Add (Lit 2) (Mul (Lit 3) (Lit 4)))))
    , testCase "evalStr nothing" $
      (evalStr "2+3*") @=?   Nothing
    ]


ex05Tests :: [Test]
ex05Tests =
    [testCase "p0" $
     ((mul (add (lit 2) (lit 3)) (lit 4))::S.Program) @=?   [S.PushI 2,S.PushI 3,S.Add,S.PushI 4,S.Mul]
    , testCase "p1" $
     ((parseExp lit add mul "(3 * 5)")::Maybe S.Program) @=? (Just [S.PushI 3,S.PushI 5,S.Mul])
    , testCase "p2" $
     (compile  "(3 * 5)") @=?  (Just [S.PushI 3,S.PushI 5,S.Mul])
    , testCase "p3" $
     (exec (compile "(3 * 5)")) @=?  (Right (S.IVal 15))
    -- parser is not like Haskell - haskell returns 121 (first does mul then final add)
    , testCase "p4" $
     (exec (compile "(2*3)*(4*5)+1" )) @=?  (Right (S.IVal 126))
    -- haskell returns 126 on this one - so parser is adding before doing the middle mul
    , testCase "p5" $
     (exec (compile "(2*3)*((4*5)+1)"))  @=?  (Right (S.IVal 126))
    ]



ex06Tests :: [Test]
ex06Tests =
    [
     testCase "v0" $
     ((add (lit 3) (var "x")) :: VarExprT) @=? (VAdd (VLit 3) (VVar "x"))
    , testCase "v1" $
     ((add (lit 3) (var "x")) :: (HasVars a, Expr a) => a) @=? (VAdd (VLit 3) (VVar "x"))
    ]

              
type FM = M.Map String Integer -> Maybe Integer
type MI = Maybe Integer
mm :: M.Map String Integer
mm  = M.fromList [("x", 6), ("y", 10)]

ex06'Tests :: [Test]
ex06'Tests =
    [
     testCase "v2" $
     (withVars [("x", 6)] $ var "x")  @=?  (Just  6)
    , testCase "v3" $
     (withVars [("x", 6)] $ add (lit 3) (var "x")) @=? (Just  9)
    -- abandoned equational reasoning - too many steps
    , testCase "v40" $
     (withVars [("x", 6), ("y", 10)] $ mul (add (lit 3) (var "x")) (var "y")) @=?     (Just 90)
    , testCase "v41"  $
     ((mul (add (lit 3) (var "x")) (var "y")::FM) $ mm)       @=?                    (Just 90)
    , testCase "v42" $
     ((do x <- ((add (lit 3) (var "x")) mm); y <- ((var "y") mm); return (x*y))::MI) @=?  (Just 90)
     -- shorter example instead
    , testCase "v50" $
     (withVars [("x", 6)] $ add (lit 3) (var "x")) @=? (Just 9)
    , testCase "v51" $
     ((add (lit 3) (var "x")::FM) $ mm) @=? (Just 9)
    , testCase "v52" $
     ((do x <- ((lit 3) mm); y <- ((var "x") mm) ; return (x+y))::MI) @=? (Just 9)
    , testCase "v53" $
     ((do x <-  Just 3; y <- ((var "x") mm) ; return (x+y))::MI) @=? (Just 9)
    , testCase "v54" $
     ((do x <-  Just 3; y <- M.lookup "x" mm; return (x+y))::MI) @=? (Just 9)
    , testCase "v55" $
     ((do x <-  Just 3; y <- Just 6; return (x+y))::MI) @=?  (Just 9)
    , testCase "v56" $
     (return (3+6) ::MI) @=?  (Just 9)
    , testCase "v57" $
     (Just (3+6::Int))   @=?  (Just 9)
    ]
