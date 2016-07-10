import qualified System.IO
import qualified Data.Logic.Propositional as Prop

loopingInteraction :: String -> (String -> String) -> IO ()
loopingInteraction prompt interaction = do
  System.IO.putStr prompt
  System.IO.hFlush System.IO.stdout
  line <- System.IO.getLine
  if null line
    then return ()
    else do
      System.IO.putStrLn $ interaction line
      loopingInteraction prompt interaction

displayExpression :: String -> String
displayExpression input = case Prop.parseExpr "" input of
                       Left err -> "Failed to parse " ++ show err
                       Right expr -> "Parsed expression: " ++ show expr

main :: IO ()
main = do
  let prompt = "Please input a proposition: "
  loopingInteraction prompt displayExpression
