import Cis194.Hw02UnitTest
import Cis194.Hw03UnitTest    
import Cis194.Hw04UnitTest
import Cis194.Hw05UnitTest


main = do
  putStrLn "Cis194.Hw02UnitTest"
  _ <- hw02Tests
  putStrLn "Cis194.Hw03UnitTest"
  _ <- hw03Tests
  putStrLn "Cis194.Hw04UnitTest"
  _ <- hw04Tests
  putStrLn "Cis194.Hw05UnitTest"
  _ <- hw05Tests
  putStrLn "Cis194.Hw06UnitTest"
  hw06Tests
 



    
