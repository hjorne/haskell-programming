data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add ex1 ex2) = eval ex1 + eval ex2


printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add ex1 ex2) = printExpr ex1 ++ "+" ++ printExpr ex2



test = eval (Add (Add (Lit 1) (Lit 2)) (Lit 9001))
teststr = printExpr (Add (Add (Lit 1) (Lit 2)) (Lit 9001))