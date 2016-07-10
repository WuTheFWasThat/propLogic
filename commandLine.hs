import qualified System.IO
import qualified Data.Logic.Propositional as Prop

stringAsExpression :: String -> String
stringAsExpression input = case Prop.parseExpr "" input of
                             Left err -> "Failed to parse " ++ show err
                             Right expr -> show expr

main :: IO ()
main = do
  System.IO.putStr "Please enter a proposition: "
  System.IO.interact (unlines . map stringAsExpression . lines)
