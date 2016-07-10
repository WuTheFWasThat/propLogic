import qualified Data.Logic.Propositional as Prop
import qualified Data.Sequence as Sequence
import Data.Foldable (toList)
import qualified Data.Maybe as Maybe
import Data.List (intercalate)

prop :: String -> Prop.Expr
prop = either (error "Failed to parse!") id . Prop.parseExpr ""

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
--            (the rules there have up and down flipped)
-- notation:
-- A, B, C are propositions,
-- X, Y, Z are groups of them
--
-- RULES:
--
--       termination      X, A |- A, Y
--                        ------------
--
--             X, ~A |- Y                      X |- ~A, Y
--  leftNot    ------------         rightNot   ------------
--             X |- A, Y                       X, A |- Y
--
--             X, A /\ B |- Y                  X |- A /\ B, Y
--  leftAnd    ----------------     rightAnd   ----------------
--             X, A, B |- Y                    X |- A, Y
--                                             X |- B, Y
--
--             X, A \/ B |- Y                  X |- A /\ B, Y
--  leftOr     ----------------     rightOr    ----------------
--             X, A |- Y                       X |- A, B, Y
--             X, B |- Y
--
-- (optional rules, in classical logic.  but not in intuitionist?  i think)
--
--             X, A -> B |- Y                   X |- A -> B, Y
--  leftImply  ----------------     rightImply ----------------
--             X |- A, Y                        X, A |- B, Y
--             X, B |- Y
--
-- optional (assuming cut elimination theorem applies, as it typically will)
--
--                        X, Y |- A, Z
--              cut!      ----------------
--                        X |- B
--                        Y, B |- A, Z

data LeftOr = LeftOr {
  leftOrIndex :: Int
}

-- NOTE: should make this less ugly
--       i know you're supposed to use the zipper pattern: http://learnyouahaskell.com/zippers
--       or maybe a lens: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
instance Rule LeftOr where
  apply leftOr seq =
    case expr of
      Prop.Disjunction leftExpr rightExpr -> Just $ Sequence.fromList
        [ (Sequent {
             antecedent=(Sequence.update index leftExpr (antecedent seq))
             , consequent=(consequent seq)
          })
        , (Sequent {
             antecedent=(Sequence.update index rightExpr (antecedent seq))
             , consequent=(consequent seq)
          })
        ]
      _ -> Nothing
    where
      index = leftOrIndex leftOr
      expr = Sequence.index (antecedent seq) index

main :: IO ()
main =
  do
    let s = Sequent {
        antecedent = Sequence.fromList [ prop "( a | ( b & c ))" , prop "~x" ]
        , consequent = Sequence.fromList [ prop "( x | a )" ]
      } in do
        putStrLn "leftOr(0) application example:"
        putStrLn ""
        putStrLn . show $ s
        putStrLn "------------------------------------------------------"
        putStrLn $ intercalate "       " $ map show $ toList . Maybe.fromJust $ apply (LeftOr {leftOrIndex= 0}) s
        putStrLn ""
        putStrLn ""
        putStrLn "leftOr(1) fails:"
        putStrLn ""
        putStrLn . show $ s
        putStrLn "------------------------------------------------------"
        putStrLn . show $ apply (LeftOr {leftOrIndex= 1}) s
