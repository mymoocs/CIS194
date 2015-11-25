module Cis194.Hw02UnitTest ( hw02Tests ) where

import           Cis194.Log
import           Cis194.Hw02_LogAnalysis
import           Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import           System.Exit (ExitCode(..), exitWith)
import           System.IO.Unsafe (unsafePerformIO)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

hw02Tests :: IO Counts
hw02Tests =  runTestTT $ TestList
             [ TestList insertTests
             , TestList parseTests
             ]

 
parseTests :: [Test]
parseTests =
    [ testCase "E" $
      parseMessage "E 2 562 help help" @=? LogMessage (Error 2) 562 "help help"
    , testCase "I" $
      parseMessage "I 29 la la la" @=?  LogMessage Info 29 "la la la"
    , testCase "U" $
      parseMessage "This is not in the right format" @=? Unknown "This is not in the right format"
    , testCase "W" $
      parseMessage "W 1654 warning:message" @=? LogMessage Warning 1654 "warning:message"
    , testCase "p" $
      parse "E 2 562 help help\nI 29 la la la\nThis is not in the right format"
      @=?
      [ LogMessage (Error 2) 562 "help help"
      , LogMessage Info 29 "la la la"
      , Unknown "This is not in the right format"
      ]
    , testCase "u" $
      unsafePerformIO (testParse parse 10 "src/Cis194/error.log")
      @=?
      [m0,m1,m2,m3,m4,m5,m6,m7,m8,m9]
    ]


insertTests :: [Test]
insertTests =
  [ testCase "i0" $
    i0 @=? Node Leaf m0 Leaf
  , testCase "i1" $   
    i1 @=? Node (Node Leaf m1 Leaf) m0 Leaf
  , testCase "i2" $
    i2 @=? Node (Node (Node Leaf m2 Leaf) m1 Leaf) m0 Leaf    
  , testCase "i3" $
    i3 @=? Node (Node (Node Leaf m2 (Node Leaf m3 Leaf)) m1 Leaf) m0 Leaf
  , testCase "i4" $
    i4 @=? Node (Node (Node Leaf m2 (Node Leaf m3 Leaf)) m1 (Node Leaf m4 Leaf)) m0 Leaf
  , testCase "i5" $
    i5 @=? Node (Node (Node (Node Leaf m5 Leaf) m2 (Node Leaf m3 Leaf))
                 m1 (Node Leaf m4 Leaf)) m0 Leaf
  , testCase "i6" $
    i6 @=? Node (Node (Node (Node Leaf m5 (Node Leaf m6 Leaf)) m2 (Node Leaf m3 Leaf))
                      m1 (Node Leaf m4 Leaf))
           m0 Leaf
  , testCase "i7" $
    i7 @=?  Node (Node (Node (Node Leaf m5 (Node Leaf  m6 Leaf))
                             m2 (Node (Node Leaf m7 Leaf) m3 Leaf))
                  m1 (Node Leaf m4 Leaf))
            m0 Leaf
  , testCase "i8" $ 
    i8 @=? Node (Node (Node (Node (Node Leaf m8 Leaf) m5 (Node Leaf m6 Leaf))
                            m2 (Node (Node Leaf m7 Leaf) m3 Leaf)) m1 (Node Leaf m4 Leaf))
           m0 Leaf
  , testCase "i9" $ 
   i9 @=? Node (Node (Node (Node (Node Leaf m8 Leaf) m5 (Node Leaf m6 Leaf))
                            m2 (Node (Node Leaf m7 (Node Leaf m9 Leaf)) m3 Leaf))
                m1 (Node Leaf m4 Leaf))
          m0 Leaf
  ]


m0,m1,m2,m3,m4,m5,m6,m7,m8,m9 :: LogMessage
m0 = LogMessage Info 5053 "pci_id: con ing!"
m1 = LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
m2 = LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
m3 = LogMessage Info 4076 "verse.'"
m4 = LogMessage Info 4764 "He trusts to you to set them free,"
m5 = LogMessage Info 858 "your pocket?' he went on, turning to Alice."
m6 = LogMessage Info 898 "would be offended again."
m7 = LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)"
m8 = LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And"
m9 = LogMessage Info 3899 "hastily."

i0,i2,i3,i4,i5,i6,i7,i8,i9 :: MessageTree
i0 = insert m0 Leaf
i1 = insert m1 i0
i2 = insert m2 i1
i3 = insert m3 i2
i4 = insert m4 i3
i5 = insert m5 i4
i6 = insert m6 i5
i7 = insert m7 i6
i8 = insert m8 i7
i9 = insert m9 i8

