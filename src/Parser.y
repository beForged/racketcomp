{
module Parser( parseExp ) where

import Lexer
import Types

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } {Token _ TokenEOF }
%error { parseError }

%token
	num		{ Token _ (TokenNum $$) }
	id		{ Token _ (TokenId $$) }
	quote		{ Token _ TokenQuote }
	if		{ Token _ TokenIf }
	'+'		{ Token _ TokenPlus }
	'-'		{ Token _ TokenMinus }
	'='		{ Token _ TokenEq }
	box		{ Token _ TokenBox }
	unbox		{ Token _ TokenUnbox }
	cons		{ Token _ TokenCons }
	car		{ Token _ TokenCar }
	cdr		{ Token _ TokenCdr }
	lambda		{ Token _ TokenLambda }
	let		{ Token _ TokenLet }
	'('		{ Token _ TokenLParen }
	')'		{ Token _ TokenRParen }

%%

Sexp 	: '(' Expr ')' 	{ Expr $2 }
      	| num		{ Num $1 }
     	| bool		{ Bool $1 }
	| char		{ Char $1 }
	| empty		{ Empty }
     	
{- remember to add bool and char to lexer -}
Expr	: if Expr Expr Expr 	{ If $2 $3 $4 }
	| let Id Sexp		{ Let $2 $3 }
	{- | letrec '(' LetRec ')' Sexp	{ Letrec $3 $5 } -}
	| lambda '(' IdList ')' Sexp	{ Lambda $3 $5 }
	| quote Sexp		{ Quote $2 }
	| Binary 		{ Binary $1 }
	| Unary 		{ Unary $1 }

{- this uses constant stack space but produces reversed list -}
IdList 	: Id			{ [$1] }
	| IdList Id		{ $2 : $ 1 }

Unary 	: zero? Sexp		{ IsZero $2 }
	| empty? Sexp		{ IsEmpty $2 }
     	| box Sexp		{ Box $2 }
	| unbox Sexp		{ Unbox $2 }
	| car Sexp		{ Car $2 }
	| cdr Sexp		{ Cdr $2 }

Binary	: '+' Sexp Sexp		{ Plus $2 $3 }
        | '-' Sexp Sexp		{ Minus $2 $3 }
	| eq? Sexp Sexp		{ Eq $2 $3 }
	| '=' Sexp Sexp		{ Eq $2 $3 }
	| cons Sexp Sexp	{ Cons $2 $3 }
