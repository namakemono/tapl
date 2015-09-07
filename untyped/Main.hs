import Parser(parseArith)
import Core(eval)

main :: IO ()
main = do
    print $ eval $ parseArith "true"
    print $ eval $ parseArith "if false then true else false"
    print $ eval $ parseArith "0"
    print $ eval $ parseArith "succ (pred 0)"
    print $ eval $ parseArith "iszero (pred (succ (succ 0)))"
