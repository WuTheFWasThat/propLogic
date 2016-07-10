import qualified Data.Logic.Propositional as Prop
import qualified Data.Sequence as Sequence
import Data.Foldable (toList)
import qualified Data.Maybe as Maybe
import Data.List (intercalate)

data Sequent = Sequent {
  antecedent :: Sequence.Seq Prop.Expr
  , consequent :: Sequence.Seq Prop.Expr
}

instance Show Sequent where
  show seq = let
      antecedentStr = intercalate " , " $ map show $ toList $ antecedent seq
      consequentStr = intercalate " , " $ map show $ toList $ consequent seq
    in
      antecedentStr ++ " |- " ++ consequentStr

class Rule r where
  apply :: r -> Sequent -> Maybe (Sequence.Seq Sequent)

-- reference: http://logitext.mit.edu/tutorial
--
-- notation:
-- A, B, C are propositions,
-- X, Y, Z are groups of them
--
-- terminates a branch:
--   X, A |- A, Y
--
-- RULES:
--   leftNot
--     X, ~A |- Y
--     X |- A, Y
--
--   rightNot
--     X |- ~A, Y
--     X, A |- Y
--
--   leftAnd
--     X, A /\ B |- Y
--     X, A, B |- Y
--
--   rightAnd
--     X |- A /\ B, Y
--     X |- A, Y
--     X |- B, Y
--
--   leftOr
--     X, A \/ B |- Y
--     X, A |- Y
--     X, B |- Y
--
--   rightOr
--     X |- A /\ B, Y
--     X |- A, B, Y
--
-- (optional rules, in classical logic.  but not in intuitionist?  i think)
--
--   leftImply
--     X, A -> B |- Y
--     X |- A, Y
--     X, B |- Y
--
--   rightImply
--     X |- A -> B, Y
--     X, A |- B, Y
--
-- cut rule!
--
--   cut
--     X, Y |- A, Z
--     X |- B
--     Y, B |- A, Z

data LeftOr = LeftOr {
  leftOrIndex :: Int
}

splitOr :: Prop.Expr -> Maybe (Prop.Expr, Prop.Expr)
splitOr (Prop.Disjunction x y) = Just (x, y)
splitOr _ = Nothing

-- NOTE: should make this less ugly
--       i know you're supposed to use the zipper pattern: http://learnyouahaskell.com/zippers
--       or maybe a lens: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
instance Rule LeftOr where
  apply leftOr seq =
    case (splitOr expr) of
      Nothing -> Nothing
      Just (leftExpr, rightExpr) -> Just $ Sequence.fromList
        [ (Sequent {
             antecedent=(Sequence.update index leftExpr (antecedent seq))
             , consequent=(consequent seq)
          })
        , (Sequent {
             antecedent=(Sequence.update index rightExpr (antecedent seq))
             , consequent=(consequent seq)
          })
        ]
    where
      index = leftOrIndex leftOr
      expr = Sequence.index (antecedent seq) index

-- TODO: make function to parse sequent from something like:
-- prop, prop2, prop3 |- prop4, prop5, prop6

main :: IO ()
main =
  do
    let s = Sequent {
        antecedent = Sequence.fromList
        [ Prop.Disjunction (Prop.Variable $ Prop.Var 'a') (Prop.Conjunction (Prop.Variable $ Prop.Var 'b') (Prop.Variable $ Prop.Var 'c'))
        , Prop.Negation (Prop.Variable $ Prop.Var 'x')
        ]
        , consequent = Sequence.fromList
        [ Prop.Disjunction (Prop.Variable $ Prop.Var 'x') (Prop.Variable $ Prop.Var 'a')
        ]
      } in do
        putStrLn "leftOr(0) application example:"
        putStrLn ""
        putStrLn . show $ s
        putStrLn "------------------------------------------------------"
        putStrLn $ intercalate "       " $ map show $ toList . Maybe.fromJust $ apply (LeftOr {leftOrIndex= 0}) s
