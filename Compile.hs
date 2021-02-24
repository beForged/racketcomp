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

-- dont want to use this since its not pure or safe
-- type Env = IORef [(String, IORef Val)]


type Compile_e a = ReaderT Env (ExceptT String (State.StateT Integer Identity)) a

readExpr :: String -> [Val]
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

--consider changing label_lambda return into a tuple and putting int val into here so
--its impossible to duplicate labels
compile :: [Val] -> Asm
compile val = case fst (runCompile_e [] 0 (compiler val)) of
    Left er -> error (er)
    Right asm -> asm

--TODO ^ make this work on [Val] where there may be multiple func declarations/defines
--for each define, compile and append. Then compile the rest (lambdas + 'main') 
--and place after entry label
--defines are then appended after with lambda defs
--need to find a way to determine if it is a func define or entry code that is executed as 'main'
--(also think about separate files, file could be only function declarations/defines)
--TODO

-- currently assumes only one non define s expression esists
-- how to determine otherwise (preprocess? statefully?)
--
-- compiler function will collect all separate defs and compile them into one
-- fold over on tuple
{-
compiler :: [Val] -> Compile_e Asm
compiler [] = throwError "no input to compiler"
compiler [List (Atom "define" : xs)] = --compile_define ([List (Atom "define": xs)]) -- these need to have lambda transforms
compiler (List (Atom "define" : xs) : code) = compiler (code : 
compiler (x : xs) = do --assume this is entry
    defs <- compiler xs
    entry <- compile_entry x
    return $
        entry ++ def ++ 
        [Label "err", Push Rbp, Call (L "error"), Ret]
-}

compiler :: [Val] -> Compile_e Asm
compiler lst = do
    let (defs, exec) = accum lst ([], [])
    let (ldef, lexec) = (label_lam ldef, label_lam exec)
    --let exec_f = foldr (++) lexec [] 
    --entry <- map compile_tail_e lexec --creates a list of compile_e val
    entry <- foldM fldr [] lexec 
    defs <- foldM fldr' [] ldef
    return $
        [(Label "entry")] ++
        entry ++
        [Ret] ++
        defs ++
        [Label "err", Push Rbp, Call (L "error"), Ret]
        
    --lambdafy both (map over?)
    --compile entry on each exec and put those together
    --compile all functions and append after entry
    
accum :: [Val] -> ([Val], [Val]) -> ([Val], [Val])
accum [] (d, e) = (d, f) where f = reverse e -- reverse e to preserve execution order
accum (List (Atom "define" : xs) : cds) (defs, exec) = accum cds (((List (Atom "define" :xs)) : defs), exec)
accum (x : xs) (defs, exec) = accum xs (defs, x : exec)

--can use runstate to we can extract the int value out of it
label_lam :: [Val] -> [Val]
label_lam ls = State.evalState (mapM label_lambda ls) 0
     
fldr :: Asm -> Val -> Compile_e Asm
fldr asm val = do
    ex <- compile_tail_e val
    return $ asm ++ ex

fldr' :: Asm -> Val -> Compile_e Asm
fldr' asm val = do
    ex <- compile_lambdas_defs . lambdas $ val
    return $ asm ++ ex
--want to probably write compile_define that outputs Compile_e Asm for a define 
--already written (btw)
--(code exists to do it, just write the helper)
--also want to write a 'sorter' to send code to either compile.
{-
compile_entry ::  Val -> Compile_e Asm
compile_entry defs val = do
    entry <- compile_tail_e le
    lambdas <- compile_lambdas_defs (lambdas le) 
    return $
        [(Label "entry")] ++ 
        entry ++ 
        [Ret] ++
        lambdas 
    where le = label_lambda 0 val --potentially move to before runstate
        
-}

-- desugar :: Val -> Val
-- TODO


--compile labeled lambda into a func
compile_lambdas_def :: Val -> Compile_e Asm
compile_lambdas_def v@(List [Atom "λ", xs,Atom f, e0]) = do 
        c0 <- local (mappend (reverse ((lstAtom xs) ++ (fvs v)))) $ compile_tail_e e0
        n <- gensym
        return $ 
            [Label f] ++ c0 ++ [Ret]


compile_lambdas_defs :: [Val] -> Compile_e Asm
compile_lambdas_defs ls = folder_tempname (map compile_lambdas_def ls) 

--TODO move these down
folder_tempname :: [Compile_e Asm] -> Compile_e Asm
folder_tempname [] =  return [] 
folder_tempname [x] = x
folder_tempname (x:xs) = do
    cs <- folder_tempname xs
    c <- folder_tempname [x]
    return $ c ++ cs

dummy :: Compile_e Asm
dummy = return []

lstAtom :: Val -> [String]
lstAtom (Atom f) = [f]
lstAtom (List (Atom f : fs)) = f : (lstAtoms fs)
lstAtom _ = error "lambda list not atoms1"

lstAtoms :: [Val] -> [String]
lstAtoms [] = []
lstAtoms [Atom f] = [f]
lstAtoms (Atom f : fs) = f : (lstAtoms fs)
lstAtoms _ = error "lambda list not atoms2"


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
compile_tail_e e@(List [Atom "λ", List xs, Atom f, e0]) = compile_lambda xs f  (fvs e)
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
compile_e e@(List [Atom "λ", List xs, Atom f, e0]) = compile_lambda xs f (fvs e)
--compile_e (List (Atom "letrec" : es)) =  --TODO init es, singular (last e0)
compile_e (List (Atom f : Atom "." : es)) = compile_call f es
compile_e (List (Atom f : es)) = compile_call f es
compile_e _ = throwError "compile cannot match"

compile_lambda :: [Val] -> String -> [String] -> Compile_e Asm
compile_lambda xs f ys = do
    eh <- copy_env_heap ys 0
    return $
        --save label addr
        [Lea rax (Offset (L f) 0), --TODO let offset typecheck here
        Mov (Offset rdi 0) rax,
        --save env
        Mov r8 (I (length ys)),
        Mov (Offset rdi 1) r8,
        Mov r9 rdi,
        Add r9 (I 16)] ++ 
        eh ++
        --tag and return closure pointer
        [Mov rax rdi, Or rax (I type_proc), Add rdi (I (8 * (2 + (length ys))))]

copy_env_heap :: [String] -> Int -> Compile_e Asm
copy_env_heap [] i = return []
copy_env_heap (x:fvs) i = do
    ceh <- copy_env_heap fvs (i + 1)
    env <- ask
    case stackLookup x env of
        Left err -> throwError err
        Right i -> return $
            [Mov r8 (Offset rsp (negate i)), -- pretty important that env is in the right order now
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
            [Mov rax (Offset rsp (negate (i + 1)))]

compile_let :: String -> Val -> Val -> Compile_e Asm
compile_let x0 x1 expr= do
    c0 <- compile_e x1
    c1 <- local (mappend [x0]) $ compile_e expr
    env <- ask
    return $
        c0 ++
        [Mov (Offset rsp (negate ((length env) + 1))) rax] ++
        c1

compile_tail_let :: String -> Val -> Val -> Compile_e Asm
compile_tail_let x0 x1 expr = do
    c0 <- compile_e x1
    c1 <- local (mappend [x0]) $ compile_tail_e expr
    env <- ask
    return $
        c0 ++ 
        [Mov (Offset rsp (negate ((length env) + 1))) rax] ++
        c1

compile_add :: Val -> Val -> Compile_e Asm
compile_add e0 e1 = do
    c1 <- compile_e e1
    c0 <- local (mappend [""]) $ compile_e e0
    env <- ask
    return $
        c1 ++ assert_integer ++
        [Mov (Offset rsp (negate ((length env) + 1))) rax] ++
        c0 ++ assert_integer ++
        [Add rax (Offset rsp (negate ((length env) + 1)))]

compile_sub :: Val -> Val -> Compile_e Asm
compile_sub e0 e1 = do
    c1 <- compile_e e1
    c0 <- local (mappend [""]) $ compile_e e0
    env <- ask
    return $
        c1 ++ assert_integer ++
        [Mov (Offset rsp (negate ((length env) + 1))) rax] ++
        c0 ++ assert_integer ++
        [Sub rax (Offset rsp (negate ((length env) + 1)))]

-- compiles a pointer to a boxed value
compile_box :: Val -> Compile_e Asm
compile_box e0 = do
    c0 <- compile_e e0
    return $
        c0 ++
        [Mov (Offset rdi 0) rax, Mov rax rdi, Or rax (I type_box), Add rdi (I 8)]

--unboxes value
compile_unbox :: Val -> Compile_e Asm
compile_unbox e0 = do
    c0 <- compile_e e0
    return $
        c0 ++ assert_box ++ 
        [Xor rax (I type_box), Mov rax (Offset rax 0)]

--compile cons
compile_cons :: Val -> Val -> Compile_e Asm
compile_cons e0 e1 = do
    c0 <- compile_e e0
    c1 <- local (mappend [""]) $ compile_e e1
    env <- ask
    return $
        c0 ++
        [Mov (Offset rsp (negate ((length env) + 1))) rax] ++
        c1 ++
        [Mov (Offset rdi 0) rax, Mov rax (Offset rsp (negate ((length env) + 1))), 
        Mov (Offset rdi 1) rax, Mov rax rdi, Or rax (I type_pair), Add rdi (I 16)]

--car and cdr for pairs
compile_car :: Val -> Compile_e Asm
compile_car e0 = do
    c0 <- compile_e e0
    return $
        c0 ++ assert_pair ++
        [Xor rax (I type_pair), Mov rax (Offset rax 1)]
        -- untag and then read
compile_cdr :: Val -> Compile_e Asm
compile_cdr e0 = do
    c0 <- compile_e e0
    return $
        c0 ++ assert_pair ++
        [Xor rax (I type_pair), Mov rax (Offset rax 0)]
        -- untag and then read

--compile function call
compile_call :: String -> [Val] -> Compile_e Asm
compile_call f es = do
    cs <- local (mappend [""]) $ compile_es es
    env <- ask
    --stack_size <- (8 * (length env))
    return $
        cs ++
        [Sub rsp (I (8 * (length env))), Call (L (symbolToLabel f)), Add rsp (I (8 * (length env)))] 

--lamda function call
compile_lambda_call :: Val -> [Val] -> Compile_e Asm
compile_lambda_call e0 es = do
    cs <- local (mappend [""]) $ compile_es es
    c0 <- compile_e e0 -- make sure that ^ local doesnt affect this one 
    env <- ask
    ccs <- copy_closure_stack (1 + (length es))
    let i = (negate (1 + length env))
    let stack_size = 8 * (length env)
    return $
        c0 ++
        [Mov (Offset rsp i) rax] ++
        cs ++
        [Mov rax (Offset rsp i)] ++
        assert_proc ++
        [Xor rax (I type_proc), Sub rsp (I stack_size)] ++
        ccs ++
        [Call (Offset rax 0), Add rsp (I stack_size)]


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
    let copy_loop = "copy_closure" ++ (show cl)
    let copy_done = "copy_done" ++ (show cd)
    return $
        [Mov rax (Offset rax 1), Mov r9 rax, Mov r9 (I 16), Mov rcx rsp, 
        Add rcx (I (negate (8 * (n + 1)))), Label copy_loop, Cmp r8 (I 0),
        Je copy_done, Mov rbx (Offset r9 0), Mov (Offset rcx 0) rbx, Sub r8 (I 1),
        Add r9 (I 8), Jmp copy_loop, Label copy_done]
          

mov_args :: Int -> Int -> Asm
mov_args 0 off = []
mov_args i off = (mov_args (i - 1) off) ++ 
    [Mov rbx (Offset rsp (off - i)), Mov (Offset rsp (0 - i)) rbx]
    

--recursivley compiles args for function calls
compile_es :: [Val] -> Compile_e Asm
compile_es [] = return [] 
compile_es (v:vs) = do
    c0 <- compile_e v
    cs <- local (mappend [""]) $ compile_es vs
    env <- ask
    return $
        c0 ++
        [Mov (Offset rsp (negate ((length env) + 1))) rax] ++
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

assert_proc :: Asm
assert_proc = assert_type result_type_mask type_proc
