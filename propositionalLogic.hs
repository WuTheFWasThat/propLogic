import qualified Data.Logic.Propositional as Prop
import qualified Data.Sequence as Sequence
import Data.Foldable (toList)
import qualified Data.Maybe as Maybe
import Data.List (intercalate)

type ClauseSet = [Prop.Expr]

data Sequent = Sequent ClauseSet ClauseSet

instance Show Sequent where
  show (Sequent antecedent consequent) =
    let antecedentStr = intercalate " , " $ map show $ antecedent
        consequentStr = intercalate " , " $ map show $ consequent
    in antecedentStr ++ " |- " ++ consequentStr

type Rule = Sequent -> Maybe [Sequent]

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
leftNot :: Rule
leftNot (Sequent antecedent consequent) =
    case antecedent of
      (Prop.Negation a) : x -> Just [Sequent x (a : consequent)]
      _ -> Nothing

leftAnd :: Rule
leftAnd (Sequent antecedent consequent) =
    case antecedent of
      (Prop.Conjunction a b) : x -> Just [Sequent (a : b : x) consequent]
      _ -> Nothing

leftOr :: Rule
leftOr (Sequent antecedent consequent) =
    case antecedent of
      (Prop.Disjunction a b) : x -> Just [seq1, seq2]
        where seq1 = Sequent (a : x) consequent
              seq2 = Sequent (b : x) consequent
      _ -> Nothing

prop :: String -> Prop.Expr
prop = either (error "Failed to parse!") id . Prop.parseExpr ""

main :: IO ()
main = do
    let antecedent = [ prop "( a & ( b & c ))" , prop "~x" ]
    let consequent = [ prop "( x | a)" ]
    let s = Sequent antecedent consequent
    putStrLn "Sequent:"
    putStrLn . show $ s
    putStrLn ""
    putStrLn "leftOr application:"
    putStrLn ""
    putStrLn . show $ leftOr $ s
    putStrLn ""
    putStrLn "leftAnd application:"
    putStrLn ""
    putStrLn . show $ leftAnd $ s
