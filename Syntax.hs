module Syntax where
import Parser

-- extract all lambda expressions
lambdas :: Val -> [Val]
lambdas v@(Number n) = []
lambdas v@(Bool b) = []
lambdas (Atom id) = []
lambdas (List [Atom "quote", (List [])]) = []
lambdas (List [Atom "zero?", e0]) = lambdas e0
lambdas (List [Atom "empty?", e0]) = lambdas e0
lambdas (List [Atom "add1", e0]) = lambdas e0
lambdas (List [Atom "sub1", e0]) = lambdas e0
lambdas (List [Atom "if", e0, e1, e2]) = (lambdas e0) ++ (lambdas e1) ++ (lambdas e2)
lambdas (List [Atom "+", e0, e1]) = (lambdas e0) ++ (lambdas e1)
lambdas (List [Atom "-", e0, e1]) = (lambdas e0) ++ (lambdas e1)
lambdas (List [Atom "box", e0]) = lambdas e0
lambdas (List [Atom "unbox", e0]) = lambdas e0
lambdas (List [Atom "cons", e0, e1]) = (lambdas e0) ++ (lambdas e1)
lambdas (List [Atom "car", e0]) = lambdas e0
lambdas (List [Atom "cdr", e0]) = lambdas e0
lambdas (List [Atom "let", List [Atom x0, x1], expr]) = (lambdas x1) ++ (lambdas expr)
lambdas (List (Atom "letrec", bs, e0)) = (foldr1 (++) (map (lambdas . last) bs)) ++ (lambdas e0)
lambdas v@(List [Atom "λ", xs, l, e0]) = v:(lambdas e0)
lambdas (List (Atom f : e : Atom "." : es)) = (lambdas e) ++ (foldl1 (++) (map lambdas es))
lambdas (List (Atom f : e : es)) = (lambdas e) ++ (foldl1 (++) (map lambdas es))

--this adds a unique name for each lambda so we can track them 
label_lambda :: Val -> Compile_e Val 
{-
label_lambda v@(Number n) = (Number n)
label_lambda v@(Bool b) = Bool b
label_lambda (Atom id) = Atom id 
-}
label_lambda (List [Atom "quote" ,(List[])]) = return (List [Atom "quote", (List[])])
label_lambda (List [Atom "zero?", e0]) = return (List [Atom "zero?", (label_lambda e0)])
label_lambda (List [Atom "empty?", e0]) = return (List [Atom "empty?", (label_lambda e0)])
label_lambda (List [Atom "add1", e0]) = return (List [Atom "add1", (label_lambda e0)])
label_lambda (List [Atom "sub1", e0]) = return (List [Atom "sub1", (label_lambda e0)])
label_lambda (List [Atom "if", e0, e1, e2]) = return (List [Atom "if", (label_lambda e0), (label_lambda e1), (label_lambda e2)])
label_lambda (List [Atom "+", e0, e1]) = return (List [Atom "+", (label_lambda e0), (label_lambda e1)])
label_lambda (List [Atom "-", e0, e1]) = return (List [Atom "-", (label_lambda e0), (label_lambda e1)])
label_lambda (List [Atom "box", e0]) = return (List [Atom "box", (label_lambda e0)])
label_lambda (List [Atom "unbox", e0]) = return (List [Atom "unbox", (label_lambda e0)])
label_lambda (List [Atom "cons", e0, e1]) = return (List [Atom "cons", (label_lambda e0), (label_lambda e1)])
label_lambda (List [Atom "car", e0]) = return (List [Atom "car", (label_lambda e0)])
label_lambda (List [Atom "cdr", e0]) = return (List [Atom "cdr", (label_lambda e0)])
label_lambda (List [Atom "let", List [Atom x0, x1], expr]) = return 
label_lambda (List (Atom "letrec", bs, e0)) = 
    return (List (Atom "letrec", (List (map (\b -> [first b, label_lambda last b]) bs)), (label_lambda e0)))
label_lambda v@(List [Atom "λ", xs, e0]) = do
    name <- gensym
    (List [Atom "λ", xs, (gensymmod (show name)), (label_lambda e0)])
label_lambda (List (Atom f : e : Atom "." : es)) = (List (Atom f : (label_lambda e) : Atom "." : (mapM label_lambda es))
label_lambda (List (Atom f : e : es)) = (List (Atom f : (label_lambda e) : (mapM label_lambda es))
label_lamda = return

gensymmod :: String -> String
gensymmod s = "lambdafun_" ++ s

-- finds lambda free variables
fvs :: Val -> [String]
fvs v@(Number n) = []
fvs v@(Bool b) = []
fvs (List [Atom "quote" ,(List[])]) = []
fvs (Atom id) = pure id -- careful when its a char
fvs (List [Atom "zero?", e0]) = fvs e0
fvs (List [Atom "empty?", e0]) = fvs e0
fvs (List [Atom "add1", e0]) = fvs e0
fvs (List [Atom "sub1", e0]) = fvs e0
fvs (List [Atom "if", e0, e1, e2]) = (fvs e0) ++ (fvs e1) ++ (fvs e2) 
fvs (List [Atom "+", e0, e1]) = (fvs e0) ++ (fvs e1)
fvs (List [Atom "-", e0, e1]) = (fvs e0) ++ (fvs e1)
fvs (List [Atom "box", e0]) = fvs e0
fvs (List [Atom "unbox", e0]) = fvs e0
fvs (List [Atom "cons", e0, e1]) = (fvs e0) ++ (fvs e1)
fvs (List [Atom "car", e0]) = fvs e0
fvs (List [Atom "cdr", e0]) = fvs e0
fvs (List [Atom "let", List [Atom x0, x1], expr]) = (fvs x1) ++ (remq [x0] (fvs expr))
fvs (List (Atom "letrec", bs, e0 )) = (remq (map head bs) ((fvs e0) ++ (foldr1 (++) (map fvs (map last bs)))))
fvs v@(List [Atom "λ", xs, l, e0]) = (remq xs (fvs e0))
fvs (List (Atom f : e : Atom "." : es)) = (fvs e) ++ (foldr1 (++) (map fvs es))
fvs (List (Atom f : e : es)) = (fvs e) ++ (foldr1 (++) (map fvs es))

isImm :: Val -> Bool
isImm v@(Number _) -> True
isImm v@(Bool _) -> True
isImm (List [Atom "quote" ,(List[])]) = True
--isImm v@(Char _) -> True
isImm _ -> False

remq :: Eq a => [a] -> [a] -> [a]
remq fil lst= filter (\x -> (notElem x fil)) lst
