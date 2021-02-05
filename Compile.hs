{-# LANGUAGE BinaryLiterals #-}
module Compile where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except
import Data.IORef
import Parser
import Syntax
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Maybe
import qualified Control.Monad.State as State
import Data.Bits
import Data.Hashable

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
-- data Mem m = String m

-- dont want to use this since its not pure or safe
-- type Env = IORef [(String, IORef Val)]

type Compile_e a = ReaderT Env (ExceptT String (State.StateT Integer Identity)) a

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



readExpr :: String -> Val
readExpr input = case parse (parseExpr) "lisp" input of
    Left err -> error ("bad parse" ++ (show err))
    Right x -> x

-- idk how to write the signature for this help
-- maybe need to lift $ return n ???
--gensym :: State.State Int Int
--gensym :: (Num s, State.MonadState s m) => 
gensym = do
    n <- State.get
    State.put (n + 1)
    return n

compile :: Val -> Asm
compile val = case fst (runCompile_e [] 0 (compile_entry val)) of
    Left er -> error (er)
    Right asm -> asm


-- primitive, assumes form (begin ((define ...) ...) e0)
{-
compile :: Val -> Compile_e Asm
compile (List (Atom "begin" : xs)) = do
    ds <- compile_defines (init xs)
    e0 <- compile_entry (last xs)
    return $ e0 ++ ds 
compile val = do
    compile_entry val
-}
{-
    e0 <- case fst (runCompile_e [] 0 (compile_entry (last xs))) of
        Left err -> error (err)
        Right asm -> asm
compile val = case fst (runCompile_e [] 0 (compile_entry val)) of
    Left err -> error (err)
    Right asm -> asm
-}

-- compile entry point 
-- fst ~~~ because i dont care about gensym val
{-
compile_entry :: Val -> Either String Asm
compile_entry val = case fst (runCompile_e [] 0 (compile_e val )) of
    Left er -> throwError ("error in entry: " ++ er)
    Right val -> Right $ 
        [(Label "entry")] ++ 
        val ++ 
        [Ret, Label "err", Push Rbp, Call "error", Ret]
-}

--desugar is useful
compile_entry :: Val -> Compile_e Asm
compile_entry val = do
    entry <- compile_tail_e le
    lambdas <- compile_lambdas_defs (lambdas le) 
    return $
        [(Label "entry")] ++ 
        entry ++ 
        [Ret] ++
        lambdas ++
        [Label "err", Push Rbp, Call "error", Ret]
    where le = label_lambda val
        

-- desugar :: Val -> Val
-- TODO

--compile labeled lambda into a func
compile_lambdas_def :: Val -> Compile_e Asm
compile_lambdas_def (List (Atom "λ", xs, f, e0)) = do
    c0 <- local (mappend (reverse (xs : fvs v))) $ compile_tail_e e0
    return $
        [Label f] ++ c0 ++ [Ret]


compile_lambdas_defs :: [Val] -> Compile_e Asm
compile_lambdas_defs ls = foldl1 (++) (map compile_lambdas_def ls)

-- executes the monads/transformers that wrap the compile funcs
runCompile_e :: Env -> Integer -> Compile_e a -> (Either String a, Integer)
runCompile_e env st ev = runIdentity (State.runStateT (runExceptT (runReaderT ev env)) st)

--if, let, call
--must have all the other duplicates becuase otherwise it will just match on call :(
compile_tail_e :: Val -> Compile_e Asm
compile_tail_e val@(Number n) = compile_integer n
compile_tail_e val@(Bool b) = compile_boolean b
compile_tail_e (Atom id) = compile_atom id
compile_tail_e (List [Atom "quote", (List [])]) = compile_empty
compile_tail_e (List [Atom "zero?", e0]) = compile_zero e0
compile_tail_e (List [Atom "empty?", e0]) = compile_emptyq e0
compile_tail_e (List [Atom "add1", e0]) = compile_add1 e0
compile_tail_e (List [Atom "sub1", e0]) = compile_sub1 e0
compile_tail_e (List [Atom "if", pred, res, alt]) = compile_tail_if pred res alt
compile_tail_e (List [Atom "+", e0, e1]) = compile_add e0 e1
compile_tail_e (List [Atom "-", e0, e1]) = compile_sub e0 e1
compile_tail_e (List [Atom "box", e0]) = compile_box e0
compile_tail_e (List [Atom "unbox", e0]) = compile_unbox e0
compile_tail_e (List [Atom "cons", e0, e1]) = compile_cons e0 e1
compile_tail_e (List [Atom "car", e0]) = compile_car e0
compile_tail_e (List [Atom "cdr", e0]) = compile_cdr e0
compile_tail_e e@(List [Atom "λ", xs, LambdaName, e0]) = do
    l <- gensym
    compile_lambda xs ("lambda_" ++ l)  (fvs e)
compile_tail_e (List [Atom "let", List [Atom x0, x1], expr]) = compile_tail_let x0 x1 expr
--compile_tail_e (List (Atom "letrec" : es)) =  --TODO init es, singular (last e0)
compile_tail_e (List (Atom f : Atom "." : es)) = compile_tail_call f es
compile_tail_e (List (Atom f : es)) = compile_tail_call f es 
compile_tail_e _ = throwError "compile_tail cannot match"

--actual compilation
compile_e ::  Val -> Compile_e Asm
compile_e val@(Number n) = compile_integer n
--compile_e (List [Atom "quote", ]) = compile_quote x
-- compile-e val@(String _) = 
compile_e val@(Bool b) = compile_boolean b
compile_e (Atom id) = compile_atom id
compile_e (List [Atom "quote", (List [])]) = compile_empty
compile_e (List [Atom "zero?", e0]) = compile_zero e0
compile_e (List [Atom "empty?", e0]) = compile_emptyq e0
compile_e (List [Atom "add1", e0]) = compile_add1 e0
compile_e (List [Atom "sub1", e0]) = compile_sub1 e0
compile_e (List [Atom "if", pred, res, alt]) = compile_if pred res alt
--single let - assume that x0 is a String
compile_e (List [Atom "let", List [Atom x0, x1], expr]) = compile_let x0 x1 expr
compile_e (List [Atom "+", e0, e1]) = compile_add e0 e1
compile_e (List [Atom "-", e0, e1]) = compile_sub e0 e1
compile_e (List [Atom "box", e0]) = compile_box e0
compile_e (List [Atom "unbox", e0]) = compile_unbox e0
compile_e (List [Atom "cons", e0, e1]) = compile_cons e0 e1
compile_e (List [Atom "car", e0]) = compile_car e0
compile_e (List [Atom "cdr", e0]) = compile_cdr e0
compile_e e@(List [Atom "λ", xs, LambdaName, e0]) = do
    l <- gensym
    compile_lambda xs ("lambda_" ++ l)  (fvs e)
--compile_e (List (Atom "letrec" : es)) =  --TODO init es, singular (last e0)
compile_e (List (Atom f : Atom "." : es)) = compile_call f es
compile_e (List (Atom f : es)) = compile_call f es
compile_e _ = throwError "compile cannot match"

compile_lambda :: [Val] -> String -> [Val] -> Compile_e Asm
compile_lambda xs f ys = do
    eh <- copy_env_heap ys 0
    return $
        --save label addr
        [Lea rax (Offset f 0), --TODO let offset typecheck here
        Mov (Offset Rdi 0) rax,
        --save env
        Mov r8 (length ys),
        Mov (Offset Rdi 1) r8,
        Mov r9 rdi,
        Add r9 16] ++ 
        eh ++
        --tag and return closure pointer
        [Mov rax rdi, Or rax imm_type_proc, Add rdi (8 * (2 + (length ys)))]

copy_env_heap :: [Val] -> Int -> Compile_e Asm
copy_env_heap [] i = return []
copy_env_heap (x:fvs) i = do
    ceh <- copy_env_heap fvs (i + 1)
    env <- ask
    return $
        [Mov r8 (Offset Rsp (negate (stackLookup x env))), -- pretty important that env is in the right order now
        Mov (Offset r9 i) r8] ++
        ceh

compile_integer :: Int -> Compile_e Asm
compile_integer i = return [Mov rax (I (shift i imm_shift))]

compile_boolean :: Bool -> Compile_e Asm
compile_boolean b = return [Mov rax (if b then (I imm_val_true) else (I imm_val_false))]

compile_empty :: Compile_e Asm
compile_empty = return [Mov rax (I imm_val_empty)]

compile_emptyq :: Val -> Compile_e Asm
compile_emptyq e0 = do
    c0 <- compile_e e0
    l0 <- gensym
    return $
        c0 ++
        [And rax (I imm_type_mask), 
        Cmp rax (I imm_val_empty),
        Mov rax (I imm_val_false),
        Jne (gensymToLabel (show l0)),
        Mov rax (I imm_val_true),
        Label (gensymToLabel (show l0))]
        

compile_add1 :: Val -> Compile_e Asm
compile_add1 e0 = do
    c0 <- compile_e e0
    return $
        c0 ++ assert_integer ++
        [Add rax (I (shift 1 imm_shift))]

compile_sub1 :: Val -> Compile_e Asm
compile_sub1 e0 = do
    c0 <- compile_e e0
    return $
        c0 ++ assert_integer ++
        [Sub rax (I (shift 1 imm_shift))]
-- TODO finish quote (also i think it takes a str not val?
--compile_quote :: Val -> Compile_e Asm
--compile_quote q = return [Ret]
    
compile_zero :: Val -> Compile_e Asm
compile_zero e0 = do
    c0 <- compile_e e0
    l0 <- gensym
    l1 <- gensym
    return $
        c0 ++ assert_integer ++ 
        [Cmp rax (I 0), Mov rax (I imm_val_false), -- #f
        Jne (gensymToLabel (show l0)), Mov rax (I imm_val_true),  -- #t
        Label (gensymToLabel (show l0))] 

compile_if :: Val -> Val -> Val -> Compile_e Asm
compile_if pred res alt = do
    c0 <- compile_e pred
    c1 <- compile_e res
    c2 <- compile_e alt
    l0 <- gensym
    l1 <- gensym
    return $
        c0 ++ 
        [Cmp rax (I imm_val_false), (Je (gensymToLabel (show l0)))] ++ -- #f
        c1 ++ 
        [Jmp (gensymToLabel (show l1)), (Label (gensymToLabel (show l0)))] ++ 
        c2 ++ 
        [Label (gensymToLabel(show l1))]

compile_tail_if :: Val -> Val -> Val -> Compile_e Asm
compile_tail_if pred res alt = do
    c0 <- compile_e pred
    c1 <- compile_tail_e res
    c2 <- compile_tail_e alt
    l0 <- gensym
    l1 <- gensym
    return $
        c0 ++ 
        [Cmp rax (I imm_val_false), (Je (gensymToLabel (show l0)))] ++ -- #f
        c1 ++ 
        [Jmp (gensymToLabel (show l1)), (Label (gensymToLabel (show l0)))] ++ 
        c2 ++ 
        [Label (gensymToLabel(show l1))]

compile_atom :: String -> Compile_e Asm
compile_atom id = do
    env <- ask
    case (stackLookup id env) of 
        Left err -> throwError err
        Right i -> return $
            [Mov rax (Offset Rsp (negate (i + 1)))]

compile_let :: String -> Val -> Val -> Compile_e Asm
compile_let x0 x1 expr= do
    c0 <- compile_e x1
    c1 <- local (mappend [x0]) $ compile_e expr
    env <- ask
    return $
        c0 ++
        [Mov (Offset Rsp (negate ((length env) + 1))) rax] ++
        c1

compile_tail_let :: String -> Val -> Val -> Compile_e Asm
compile_tail_let x0 x1 expr = do
    c0 <- compile_e x1
    c1 <- local (mappend [x0]) $ compile_tail_e expr
    env <- ask
    return $
        c0 ++ 
        [Mov (Offset Rsp (negate ((length env) + 1))) rax] ++
        c1

compile_add :: Val -> Val -> Compile_e Asm
compile_add e0 e1 = do
    c1 <- compile_e e1
    c0 <- local (mappend [""]) $ compile_e e0
    env <- ask
    return $
        c1 ++ assert_integer ++
        [Mov (Offset Rsp (negate ((length env) + 1))) rax] ++
        c0 ++ assert_integer ++
        [Add rax (Offset Rsp (negate ((length env) + 1)))]

compile_sub :: Val -> Val -> Compile_e Asm
compile_sub e0 e1 = do
    c1 <- compile_e e1
    c0 <- local (mappend [""]) $ compile_e e0
    env <- ask
    return $
        c1 ++ assert_integer ++
        [Mov (Offset Rsp (negate ((length env) + 1))) rax] ++
        c0 ++ assert_integer ++
        [Sub rax (Offset Rsp (negate ((length env) + 1)))]

-- compiles a pointer to a boxed value
compile_box :: Val -> Compile_e Asm
compile_box e0 = do
    c0 <- compile_e e0
    return $
        c0 ++
        [Mov (Offset Rdi 0) rax, Mov rax rdi, Or rax (I type_box), Add rdi (I 8)]

--unboxes value
compile_unbox :: Val -> Compile_e Asm
compile_unbox e0 = do
    c0 <- compile_e e0
    return $
        c0 ++ assert_box ++ 
        [Xor rax (I type_box), Mov rax (Offset Rax 0)]

--compile cons
compile_cons :: Val -> Val -> Compile_e Asm
compile_cons e0 e1 = do
    c0 <- compile_e e0
    c1 <- local (mappend [""]) $ compile_e e1
    env <- ask
    return $
        c0 ++
        [Mov (Offset Rsp (negate ((length env) + 1))) rax] ++
        c1 ++
        [Mov (Offset Rdi 0) rax, Mov rax (Offset Rsp (negate ((length env) + 1))), 
        Mov (Offset Rdi 1) rax, Mov rax rdi, Or rax (I type_pair), Add rdi (I 16)]

--car and cdr for pairs
compile_car :: Val -> Compile_e Asm
compile_car e0 = do
    c0 <- compile_e e0
    return $
        c0 ++ assert_pair ++
        [Xor rax (I type_pair), Mov rax (Offset Rax 1)]
        -- untag and then read
compile_cdr :: Val -> Compile_e Asm
compile_cdr e0 = do
    c0 <- compile_e e0
    return $
        c0 ++ assert_pair ++
        [Xor rax (I type_pair), Mov rax (Offset Rax 0)]
        -- untag and then read

--compile function call
compile_call :: String -> [Val] -> Compile_e Asm
compile_call f es = do
    cs <- local (mappend [""]) $ compile_es es
    env <- ask
    --stack_size <- (8 * (length env))
    return $
        cs ++
        [Sub rsp (I (8 * (length env))), Call (symbolToLabel f), Add rsp (I (8 * (length env)))] 

--lamda function call
compile_lambda_call :: Val -> [Val] -> Compile_e Asm
compile_lambda_call e0 es = do
    cs <- local (mappend [""]) $ compile_es es
    c0 <- compile_e e0 -- make sure that ^ local doesnt affect this one 
    env <- ask
    return $
        c0 ++
        [Mov (Offset Rsp i) rax] ++
        cs ++
        [Mov rax (Offset Rsp i)] ++
        assert_proc ++
        [Xor rax type_proc, Sub rsp (I stack_size)] ++
        copy_closure_stack (1 + (length es)) ++
        [Call (Offset Rax 0), Add rsp (I stack_size)]
    where i = (negate (1 + (length env)))
          stack_size = (8 * (length env))


--TODO
compile_tail_call :: String -> [Val] -> Compile_e Asm
compile_tail_call f es = do
    cs <- compile_es es
    env <- ask
    return $
        cs ++
        (mov_args (length es) (0 - (length env))) ++
        [Jmp (symbolToLabel f)]

copy_closure_stack :: Int -> Compile_e Asm
copy_closure_stack n = do
    cl <- gensym
    cd <- gensym
    return $
        [Mov rax (Offset Rax 1), Mov r9 rax, Mov r9 (I 16), Mov rcx rsp, 
        Add rcx (I (negate (8 * (n + 1)))), Label copy_loop, Cmp r8 (I 0),
        Je copy_done, Mov rbx (Offset r9 0), Mov (Offset Rcx 0) rbx, Sub r8 (I 1),
        Add r9 (I 8), Jmp copy_loop, Label copy_done]
    where copy_loop = "copy_closure" ++ (show cl)
          copy_done = "copy_done" ++ (show cd)

mov_args :: Int -> Int -> Asm
mov_args 0 off = []
mov_args i off = (mov_args (i - 1) off) ++ 
    [Mov rbx (Offset Rsp (off - i)), Mov (Offset Rsp (0 - i)) rbx]
    

--recursivley compiles args for function calls
compile_es :: [Val] -> Compile_e Asm
compile_es [] = return [] 
compile_es (v:vs) = do
    c0 <- compile_e v
    cs <- local (mappend [""]) $ compile_es vs
    env <- ask
    return $
        c0 ++
        [Mov (Offset Rsp (negate ((length env) + 1))) rax] ++
        cs
{-
copy_closure_stack :: Compile_e Asm
--TODO
--

compile_letrec :: [Val] -> [Val] -> Val -> Compile_e Asm
--TODO

compile_tail_letrec :: [Val] -> [Val] -> Val -> Compile_e Asm
--TODO

compile_letrec_lambdas :: [Val] -> Compile_e Asm
--TODO

compile_letrec_init :: [Val] -> [Val] -> Compile_e Asm
--TODO
---}



--map over all defines in the program
compile_defines :: [Val] -> Compile_e Asm
compile_defines xs = liftM concat $ mapM compile_define xs
    
--compile function definitions
--compile_define f xs e0
compile_define :: Val -> Compile_e Asm
--assume all xs are well formed as Atom "string" s and throw error otherwise 
compile_define (List [Atom "define", (List ((Atom f):xs)), e0]) = do
    c0 <- local (mappend (reverse xss)) $ compile_tail_e e0 
    --fname <- symbolToLabel f 
    return $
        [Label (symbolToLabel f)] ++ c0 ++ [Ret]
    where xss = map (\(Atom x) -> x) xs
    

-- this looks up variable name and returns either stack height to var 
-- or a error message
stackLookup:: String -> [String] -> Either String Int
stackLookup x env = case env of
        [] -> throwError "undefined var: "
        y:ys -> if x == y then Right (length ys) else (stackLookup x ys)

--generates valid nasm label 
--letters, numbers, _, $, # , @, ~, ., ?
symbolToLabel :: String -> String
symbolToLabel s = if (nasm_valid s) == [] then "label_" ++ (show (hash s)) else
    "label_" ++ (nasm_valid s) ++ "_" ++ (show (hash s))

nasm_valid :: String -> String
nasm_valid = filter (`elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_$#@~.?")) 

gensymToLabel :: String -> String
gensymToLabel s = "lbl" ++ s

--data Mask = int | box | pair
--emits code that asserts that data is a certain type
assert_type :: Int -> Int -> Asm
assert_type mask imm_type= 
    [Mov rbx rax, And rbx (I mask), Cmp rbx (I imm_type), Jne "err"]

assert_integer :: Asm
assert_integer = assert_type imm_type_mask imm_type_int

assert_box :: Asm
assert_box = assert_type result_type_mask type_box

assert_pair :: Asm
assert_pair = assert_type result_type_mask type_pair
