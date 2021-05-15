{
module Lexer 
	( Token(..)
	, AlexPosn(..)
	, TokenClass(..)
	, unLex
	, Alex(..)
	, runAlex'
	, alexMonadScan'
	, alexError'
	)where

import Prelude hiding (lex)
import Control.Monad (liftM)
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [A-Za-z]

tokens :-
$white+				;
$digit+				{ lex (TokenNum . read) }
$alpha [$alpha $digit \_ \']* 	{ lex TokenId 		}
true				{ lex' (TokenBool True)	}
false				{ lex' (TokenBool False)	}
\'()				{ lex' TokenEmpty	}
\'				{ lex' TokenQuote	}
if				{ lex' TokenIf		}
\+				{ lex' TokenPlus	}
\-				{ lex' TokenMinus	}
\=				{ lex' TokenEq 		}
eq?				{ lex' TokenEq		}
box				{ lex' TokenBox		}
unbox				{ lex' TokenUnbox	}
cons				{ lex' TokenCons	}
car				{ lex' TokenCar		}
cdr				{ lex' TokenCdr		}
\\				{ lex' TokenLambda	}
let				{ lex' TokenLet		}
\(				{ lex' TokenLParen	}
\)				{ lex' TokenRParen	}

{
data AlexUserState = AlexUserState {
	filePath :: FilePath
	}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

data Token = Token AlexPosn TokenClass
	deriving (Show)


data TokenClass
	= TokenNum Int
	| TokenBool Bool
	| TokenId String
	| TokenEmpty
	| TokenQuote
	| TokenIf
	| TokenPlus
	| TokenMinus
	| TokenEq
	| TokenBox
	| TokenUnbox
	| TokenCons
	| TokenCar
	| TokenCdr
	| TokenLambda
	| TokenLet
	| TokenLParen
	| TokenRParen
	| TokenEOF
	deriving ( Show )

unLex :: TokenClass -> String
unLex (TokenNum n) = show n
unLex (TokenId s) = show s
unLex TokenQuote = "'"
unLex TokenIf = "if"
unLex TokenPlus = "+"
unLex TokenMinus = "-"
unLex TokenEq = "="
unLex TokenBox = "box"
unLex TokenUnbox = "unbox"
unLex TokenCons = "cons"
unLex TokenCar = "car"
unLex TokenCdr = "cdr"
unLex TokenLambda = "lambda"
unLex TokenLet = "let"
unLex TokenLParen = "("
unLex TokenRParen = "("
unLex TokenEOF = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
	(p,_,_,_) <- alexGetInput
	return $ Token p TokenEOF

lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

lex' :: TokenClass -> AlexAction Token
lex' = lex . const
	
alexMonadScan' :: Alex Token
alexMonadScan' = do
	inp <- alexGetInput
	sc <- alexGetStartCode
	case alexScan inp sc of
		AlexEOF -> alexEOF
		AlexError (p, _, _, s) -> 
			alexError' p ("lexer error at char '" ++ take 1 s ++ "'")
		AlexSkip inp' len -> do
			alexSetInput inp'
			alexMonadScan'
		AlexToken inp' len action -> do
			alexSetInput inp'
			action (ignorePendingBytes inp) len

alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
	fp <- getFilePath
	alexError (fp ++ " : " ++ show l ++ " : " ++ show c ++ " : " ++ msg)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)

}
