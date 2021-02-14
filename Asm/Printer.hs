--need to import stuff
module Asm.Printer where
import Compile
import Parser
import Syntax
import Data.List
import System.Info

--folding instruction->String over itself
asmToString :: Asm -> String
asmToString = foldr (\i s -> (instrToString i) ++ s) "" 

--converting instructions to strings
instrToString :: Instruction -> String
--might need to adjust mov to arg arg?
instrToString (Mov a v) = "\tmov " ++ (argToString a) ++ ", " ++ 
    (argToString v) ++ "\n"
instrToString (Cmp a a') = "\tcmp " ++ (argToString a) ++ ", " ++ 
    (argToString a') ++ "\n"
instrToString (Add a a') = "\tadd " ++ (argToString a) ++ ", " ++
    (argToString a') ++ "\n"
instrToString (Sub a a') = "\tsub " ++ (argToString a) ++ ", " ++
    (argToString a') ++ "\n"
instrToString (And a a') = "\tand " ++ (argToString a) ++ ", " ++
    (argToString a') ++ "\n"
instrToString (Or a a') = "\tor " ++ (argToString a) ++ ", " ++
    (argToString a') ++ "\n"
instrToString (Xor a a') =  "\txor " ++ (argToString a) ++ ", " ++
    (argToString a') ++ "\n"
instrToString (Lea a a') =  "\tlea " ++ (argToString a) ++ ", " ++
    (argToString a') ++ "\n"
instrToString Ret = "\tret\n"
instrToString (Label s) = (lToString s) ++ ":\n"
instrToString (Jmp s) = "\tjmp " ++ (lToString s) ++ "\n"
instrToString (Je s) = "\tje " ++ (lToString s) ++ "\n"
instrToString (Jne s) = "\tjne " ++ (lToString s) ++ "\n"
instrToString (Neg a) = "\tneg " ++ (argToString a) ++ "\n"
instrToString (Push p) = "\tpush " ++ (regToString p) ++ "\n"
instrToString (Call c) = "\tcall " ++ (argToString c) ++ "\n"
--cmovl, xor, sal, sar



argToString :: Arg -> String
argToString (R reg) = regToString reg
argToString (I i) = show i
argToString (Offset arg i) = "[" ++ (argToString arg) ++ " + " ++ show (i * 8) ++ "]"
argToString (L s) = s

--turn args to string registers/values
regToString :: Reg -> String
regToString Rax = "rax"
regToString Rbx = "rbx"
regToString Rbp = "rbp"
regToString Rsp = "rsp"
regToString Rdi = "rdi"
regToString Rcx = "rcx"
regToString R8 = "r8"
regToString R9 = "r9"


--note that this is 
labelToString :: Instruction -> String
labelToString (Label s) = case System.Info.os of
    "macosx"    -> "_" ++ s
    _           -> s

--adds an _ for mac which is apparently needed
--this gets around making the gensym state monad really clunky
--might still need to be changed in compile.hs 
lToString :: String -> String
lToString s = --if ((s == "entry") || (s == "err") || (s == "error")) then labelToString (Label s) else
    case System.Info.os of
        "macosx"   ->   "_" ++   s
        _          ->   s

asmDisplay :: Asm -> String
asmDisplay a = 
    let g = find (\x -> case x of Label s-> True; _ -> False)  a
    in case g of Just g -> "\tglobal " ++ (labelToString g) ++ "\n" ++ "\tdefault rel\n" ++ "\textern " ++ (labelToString (Label "error")) ++ "\n" ++ "\tsection .text\n" ++ (asmToString a)
                 Nothing -> error "no entry label"



