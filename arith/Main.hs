import Syntax(Term(..))
import Core(eval)

main :: IO ()
main = do
    print $ (TmIf TmTrue (TmSucc TmZero) (TmPred TmZero))
    print $ eval (TmIf TmTrue (TmSucc TmZero) (TmPred TmZero))
    print $ eval (TmIf TmFalse (TmSucc TmZero) (TmPred TmZero))
    print $ eval (TmPred (TmSucc (TmSucc TmZero)))
