import qualified System.IO
import qualified Data.Logic.Propositional as Prop

-- Account for lazy evaluation of input. This allows one to process a whole
-- line using `f`, as in an imperative language.
simpleInteract :: (String -> String) -> IO ()
simpleInteract f = System.IO.interact (unlines . map f . lines)

stringAsExpression :: String -> String
stringAsExpression input = case Prop.parseExpr "" input of
                       Left err -> "Failed to parse " ++ show err
                       Right expr -> "Parsed expression: " ++ show expr

main :: IO ()
main = do
  System.IO.putStr "Please input a proposition: "
  System.IO.hFlush System.IO.stdout
  simpleInteract stringAsExpression
