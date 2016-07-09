import qualified Data.Logic.Propositional as Prop
import qualified Data.List as List


data Sequent = Sequent {
  antecedent :: [Prop.Expr]
  , consequent :: [Prop.Expr]
} deriving (Show)

main :: IO ()
main =
  do
    putStrLn "Hello World"
    putStrLn . show $ Sequent [] []

