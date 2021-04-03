{-# LANGUAGE BinaryLiterals #-}
module Syntax where
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Bits
import Parser


--type Compile_e a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

type Asm = [Instruction]

data Instruction = Ret 
    | Mov Arg Arg
    | Add Arg Arg
    | Sub Arg Arg
-- | Mul Arg Arg
    | And Arg Arg
    | Or Arg Arg
    | Label String
    | Cmp Arg Arg
    | Jmp String --turn these to args? vvv
    | Je String
    | Jne String
    | Neg Arg
    | Call Arg
    | Push Reg
    | Xor Arg Arg
    | Lea Arg Arg
    deriving (Eq, Show)

data Arg 
    = I Int
    | R Reg
    | Offset Arg Int -- only Registers and Labels
    | L String
    -- | A Word32 -- addr
    deriving (Eq, Show)
    
data Label s = String s 


data Reg 
    = Rax 
    | Rbx
    | Rbp
    | Rsp
    | Rdi
    | Rcx
    | R8
    | R9
    deriving (Eq, Show)

rax :: Arg
rax = R Rax

rbx :: Arg
rbx = R Rbx

rsp :: Arg
rsp = R Rsp

rdi :: Arg
rdi = R Rdi

rcx :: Arg
rcx = R Rcx

r8 :: Arg
r8 = R R8

r9 :: Arg
r9 = R R9

type Env = [String]

imm_shift :: Int
imm_shift = (2 + result_shift)

result_shift :: Int
result_shift = 3

result_type_mask :: Int
result_type_mask = ((shift 1 result_shift) - 1)

imm_type_mask :: Int
imm_type_mask = ((shift 1 imm_shift) - 1)

type_imm :: Int
type_imm = 0b000
type_box :: Int
type_box = 0b001
type_pair :: Int
type_pair = 0b010
type_string :: Int 
type_string = 0b011
type_proc :: Int
type_proc = 0b100

imm_type_int :: Int
imm_type_int = (shift 0b00 result_shift)
imm_type_bool  :: Int
imm_type_bool = (shift 0b01 result_shift)
imm_type_char :: Int
imm_type_char = (shift 0b10 result_shift)
imm_val_empty :: Int
imm_val_empty = (shift 0b11 result_shift)

imm_val_false = imm_type_bool
imm_val_true = ((.|.) (shift 1 (imm_shift + 1))  imm_type_bool)


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
--lambdas (List [Atom "letrec", List bs, e0]) = (foldr1 (++) (map (lambdas . last) [bs])) ++ (lambdas e0)
lambdas v@(List [Atom "λ", xs, l, e0]) = v:(lambdas e0)
lambdas (List (Atom f : e : Atom "." : es)) = (lambdas e) ++ (foldl (++) [] (map lambdas es))
lambdas (List (Atom f : e : es)) = (lambdas e) ++ (foldl (++) [] (map lambdas es))

--TODO replace this with a state monad - its too awkward otherwise
{-
--this adds a unique name for each lambda so we can track them 
label_lambda :: Int -> Val -> (Val, Int)
label_lambda i v@(Number n) = ((Number n) ,i)
label_lambda i v@(Bool b) = (Bool b, i)
label_lambda i (Atom id) = (Atom id , i)
label_lambda i (List [Atom "quote" ,(List[])]) = ( (List [Atom "quote", (List[])]), i)
label_lambda i (List [Atom "zero?", e0]) = ( (List [Atom "zero?", fst (label_lambda i e0)]), i)
label_lambda i (List [Atom "empty?", e0]) = ( (List [Atom "empty?", fst (label_lambda i e0)]), i)
label_lambda i (List [Atom "add1", e0]) = ( (List [Atom "add1", fst (label_lambda i e0)]), i)
label_lambda i (List [Atom "sub1", e0]) = ( (List [Atom "sub1", fst (label_lambda i e0)]), i)
label_lambda i (List [Atom "if", e0, e1, e2]) = ( (List [Atom "if", fst (label_lambda i e0), fst (label_lambda i e1), fst (label_lambda i e2)]), i)
label_lambda i (List [Atom "+", e0, e1]) = ( (List [Atom "+", fst (label_lambda i e0), fst (label_lambda i e1)]), i)
label_lambda i (List [Atom "-", e0, e1]) = ( (List [Atom "-", fst (label_lambda i e0), fst (label_lambda i e1)]), i)
label_lambda i (List [Atom "box", e0]) = ( (List [Atom "box", fst (label_lambda i e0)]), i)
label_lambda i (List [Atom "unbox", e0]) = ( (List [Atom "unbox", fst (label_lambda i e0)]), i)
label_lambda i (List [Atom "cons", e0, e1]) = ( (List [Atom "cons", fst (label_lambda i e0), fst (label_lambda i e1)]), i)
label_lambda i (List [Atom "car", e0]) = ( (List [Atom "car", fst (label_lambda i e0)]), i)
label_lambda i (List [Atom "cdr", e0]) = ( (List [Atom "cdr", fst (label_lambda i e0)]), i)
label_lambda i (List [Atom "let", List [Atom x0, x1], expr]) =
     ((List [Atom "let", List [Atom x0, fst (label_lambda i x1)], fst (label_lambda i expr)]), i)
label_lambda i v@(List [Atom "λ", xs, e0]) = ((List [Atom "λ", xs, Atom (gensymmod (show i)), fst (label_lambda (i + 1) e0)]) , i + 1)
--TODO apply
--label_lambda i (List (Atom f : e : Atom "." : es)) = ( (List (Atom f : (fst (label_lambda i e)) : Atom "." : (map (label_lambda i) es))), i)
--label_lambda i (List (Atom f : e : es)) = ( (List (Atom f : (label_lambda i e) : (map (label_lambda i) es))), i)
label_lambda _ x = (error ("label_lambda error on: " ++ (show x))
--label_lambda i (List [Atom "letrec", bs, e0]) = 
--     (List [Atom "letrec", (List (map (\b -> [head b, (label_lambda i last b)]) bs)), (label_lambda i e0)])
--TODO
-}


label_lambda :: Val -> State Int Val
label_lambda v@(Number n) = return v
label_lambda v@(Bool b) = return v
label_lambda v@(Atom id) = return v
label_lambda v@(List [Atom "quote", (List [])]) = return v
label_lambda (List [Atom "zero?", e0]) = do
    c0 <- label_lambda e0
    return $ List [Atom "zero?", c0]
label_lambda (List [Atom "empty?", e0]) = do
    c0 <- label_lambda e0
    return $ List [Atom "emtpy?", c0]
label_lambda (List [Atom "add1", e0]) = do
    c0 <- label_lambda e0
    return (List [Atom "add1", c0]) 
label_lambda (List [Atom "sub1", e0]) = do
    c0 <- label_lambda e0
    return (List [Atom "sub1", c0]) 
label_lambda (List [Atom "if", e0, e1, e2]) = do
    c0 <- label_lambda e0
    c1 <- label_lambda e1
    c2 <- label_lambda e2
    return $ List [Atom "if", c0, c1, c2]
label_lambda (List [Atom "+", e0, e1]) = do
    c0 <- label_lambda e0
    c1 <- label_lambda e1
    return $ (List [Atom "+", c0, c1])
label_lambda (List [Atom "-", e0, e1]) = do
    c0 <- label_lambda e0
    c1 <- label_lambda e1
    return $ (List [Atom "-", c0, c1])
label_lambda (List [Atom "box", e0]) = do
    c0 <- label_lambda e0
    return $ List [Atom "box", c0]
label_lambda (List [Atom "unbox", e0]) = do
    c0 <- label_lambda e0
    return $ List [Atom "unbox", c0]
label_lambda (List [Atom "cons", e0, e1]) = do
    c0 <- label_lambda e0
    c1 <- label_lambda e1
    return $ List [Atom "cons", c0, c1]
label_lambda (List [Atom "car", e0]) = do
    c0 <- label_lambda e0
    return $ List [Atom "car", c0]
label_lambda (List [Atom "cdr", e0]) = do
    c0 <- label_lambda e0
    return $ List [Atom "cdr", c0]
label_lambda (List [Atom "let", List [Atom x0, x1], expr]) = do
    c0 <- label_lambda expr
    return $ List [Atom "let", List [Atom x0, x1], c0]
label_lambda (List [Atom "λ", xs, e0]) = do
    lb <- gensym'
    c0 <- label_lambda e0
    return $ List [Atom "λ", xs, Atom (gensymmod (show lb)), c0]
label_lambda (List (Atom f : e : es)) = do
    c <- label_lambda e
    ds <- mapM label_lambda es
    return $ List (Atom f : c : ds)
--TODO letrec

gensym' :: State Int Int
gensym' = do
    n <- get
    put (n + 1)
    return n 

gensymmod :: String -> String
gensymmod s = "lambdafun_" ++ s

fvs :: Val -> [String]
fvs v = nub (fvs_e v)

-- finds lambda free variables
fvs_e :: Val -> [String]
fvs_e v@(Number n) = []
fvs_e v@(Bool b) = []
fvs_e (List [Atom "quote" ,(List[])]) = []
fvs_e (Atom x) = pure x -- careful when its a char
fvs_e (List [Atom "zero?", e0]) = fvs e0
fvs_e (List [Atom "empty?", e0]) = fvs e0
fvs_e (List [Atom "add1", e0]) = fvs e0
fvs_e (List [Atom "sub1", e0]) = fvs e0
fvs_e (List [Atom "if", e0, e1, e2]) = (fvs e0) ++ (fvs e1) ++ (fvs e2) 
fvs_e (List [Atom "+", e0, e1]) = (fvs e0) ++ (fvs e1)
fvs_e (List [Atom "-", e0, e1]) = (fvs e0) ++ (fvs e1)
fvs_e (List [Atom "box", e0]) = fvs e0
fvs_e (List [Atom "unbox", e0]) = fvs e0
fvs_e (List [Atom "cons", e0, e1]) = (fvs e0) ++ (fvs e1)
fvs_e (List [Atom "car", e0]) = fvs e0
fvs_e (List [Atom "cdr", e0]) = fvs e0
fvs_e (List [Atom "let", List [Atom x0, x1], expr]) = (fvs x1) ++ (remq [x0] (fvs expr))
--fvs_e (List [Atom "letrec", List bs, e0]) = (remq (map head bs) ((fvs e0) ++ (foldr1 (++) (map fvs (map last bs)))))
fvs_e v@(List [Atom "λ", List xs, l, e0]) = (remq (lam_var xs) (fvs e0))
fvs_e (List (Atom f : e : Atom "." : es)) = (fvs e) ++ (foldr1 (++) (map fvs es))
fvs_e (List (Atom f : e : es)) = (fvs e) ++ (foldr1 (++) (map fvs es))

lam_var :: [Val] -> [String]
lam_var [] = []
lam_var [Atom x] = [x]
lam_var (Atom x : xs) = x : (lam_var xs)
lam_var _ = error "lambda has non atoms as vars"

isImm :: Val -> Bool
isImm v@(Number _) = True
isImm v@(Bool _) = True
isImm (List [Atom "quote" ,(List[])]) = True
--isImm v@(Char _) -> True
isImm _ = False

remq :: Eq a => [a] -> [a] -> [a]
remq fil lst= filter (\x -> (notElem x fil)) lst
