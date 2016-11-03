-- http://community.haskell.org/~ndm/darcs/hlint/hlint.htm

import "hint" HLint.Default
import "hint" HLint.Dollar
-- import "hint" HLint.Generalise

-- ignore "Eta reduce" -- supress all eta reduction suggestions.
-- ignore "Eta reduce" = MyModule1 MyModule2 -q- supress eta reduction hints in the MyModule1 and MyModule2 modules.
-- ignore = MyModule.myFunction -- don't give any hints in the function MyModule.myFunction.
-- error = MyModule.myFunction -- any hint in the function MyModule.myFunction is an error.
-- error "Use concatMap" -- the hint to use concatMap is an error.
-- warn "Use concatMap" -- the hint to use concatMap is a warning.
