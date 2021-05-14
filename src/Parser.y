{
module Parser( parseExp ) where

import Lexer

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } {Token _ TokenEOF }
%error { parseError }

%token
	num		{ Token _ (TokenNum $$) }
	id		{ Token _ (TokenId $$) }
	
	
