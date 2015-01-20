module Main where

import Hw07_StringBuffer
import Hw07_Editor

-- for run run this command in ghci repl
--  :set -hide-package monads-tf

-- for run with ghc, e.g. runhaskell Hw07_StringBufEditor.hs
-- ghc-pkg hide monads-tf

main = runEditor editor $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
