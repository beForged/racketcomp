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


type Compile_e a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

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
    | Jmp String
    | Je String
    | Jne String
    | Neg Arg
    | Call String -- label
    | Push Reg
    | Xor Arg Arg
    | Lea Arg Arg
    deriving (Eq, Show)

data Arg 
    = I Int
    | R Reg
    | Offset Reg Int
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
type_string = 0b011
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

gensym = do
    n <- get
    put (n + 1)
    return n

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
lambdas (List [Atom "letrec", bs, e0]) = (foldr1 (++) (map (lambdas . last) bs)) ++ (lambdas e0)
lambdas v@(List [Atom "λ", xs, l, e0]) = v:(lambdas e0)
lambdas (List (Atom f : e : Atom "." : es)) = (lambdas e) ++ (foldl1 (++) (map lambdas es))
lambdas (List (Atom f : e : es)) = (lambdas e) ++ (foldl1 (++) (map lambdas es))

--this adds a unique name for each lambda so we can track them 
label_lambda :: Val -> Val 
{-
label_lambda v@(Number n) = (Number n)
label_lambda v@(Bool b) = Bool b
label_lambda (Atom id) = Atom id 
-}
label_lambda (List [Atom "quote" ,(List[])]) =  (List [Atom "quote", (List[])])
label_lambda (List [Atom "zero?", e0]) =  (List [Atom "zero?", (label_lambda e0)])
label_lambda (List [Atom "empty?", e0]) =  (List [Atom "empty?", (label_lambda e0)])
label_lambda (List [Atom "add1", e0]) =  (List [Atom "add1", (label_lambda e0)])
label_lambda (List [Atom "sub1", e0]) =  (List [Atom "sub1", (label_lambda e0)])
label_lambda (List [Atom "if", e0, e1, e2]) =  (List [Atom "if", (label_lambda e0), (label_lambda e1), (label_lambda e2)])
label_lambda (List [Atom "+", e0, e1]) =  (List [Atom "+", (label_lambda e0), (label_lambda e1)])
label_lambda (List [Atom "-", e0, e1]) =  (List [Atom "-", (label_lambda e0), (label_lambda e1)])
label_lambda (List [Atom "box", e0]) =  (List [Atom "box", (label_lambda e0)])
label_lambda (List [Atom "unbox", e0]) =  (List [Atom "unbox", (label_lambda e0)])
label_lambda (List [Atom "cons", e0, e1]) =  (List [Atom "cons", (label_lambda e0), (label_lambda e1)])
label_lambda (List [Atom "car", e0]) =  (List [Atom "car", (label_lambda e0)])
label_lambda (List [Atom "cdr", e0]) =  (List [Atom "cdr", (label_lambda e0)])
label_lambda (List [Atom "let", List [Atom x0, x1], expr]) = 
     (List [Atom "let", List [Atom x0, (label_lambda x1)], (label_lambda expr)])
label_lambda (List [Atom "letrec", bs, e0]) = 
     (List (Atom "letrec", (List (map (\b -> [head b, (label_lambda last b)]) bs)), (label_lambda e0)))
--TODO
label_lambda v@(List [Atom "λ", xs, e0]) = (List [Atom "λ", xs, LambdaName, (label_lambda e0)]) 
label_lambda (List (Atom f : e : Atom "." : es)) =  (List (Atom f : (label_lambda e) : Atom "." : (mapM label_lambda es)))
label_lambda (List (Atom f : e : es)) =  (List (Atom f : (label_lambda e) : (mapM label_lambda es)))

gensymmod :: String -> String
gensymmod s = "lambdafun_" ++ s

fvs :: Val -> [String]
fvs v = nub (fvs_e v)

-- finds lambda free variables
fvs_e :: Val -> [String]
fvs_e v@(Number n) = []
fvs_e v@(Bool b) = []
fvs_e (List [Atom "quote" ,(List[])]) = []
fvs_e (Atom id) = pure id -- careful when its a char
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
fvs_e (List [Atom "letrec", bs, e0]) = (remq (map head bs) ((fvs e0) ++ (foldr1 (++) (map fvs (map last bs)))))
fvs_e v@(List [Atom "λ", xs, l, e0]) = (remq xs (fvs e0))
fvs_e (List (Atom f : e : Atom "." : es)) = (fvs e) ++ (foldr1 (++) (map fvs es))
fvs_e (List (Atom f : e : es)) = (fvs e) ++ (foldr1 (++) (map fvs es))

isImm :: Val -> Bool
isImm v@(Number _) = True
isImm v@(Bool _) = True
isImm (List [Atom "quote" ,(List[])]) = True
--isImm v@(Char _) -> True
isImm _ = False

remq :: Eq a => [a] -> [a] -> [a]
remq fil lst= filter (\x -> (notElem x fil)) lst
