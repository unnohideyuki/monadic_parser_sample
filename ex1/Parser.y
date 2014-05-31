{
module Main where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
'('     { LParen _ }
')'     { RParen _ }
'+'     { Plus _ }
'-'     { Minus _ }
'*'     { Times _ }
'/'     { Div _ }
INT     { Int ($$, _) }

%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

%%
exp:    INT                     { $1 }
 |      '(' exp ')'             { $2 }
 |      '-' exp %prec UMINUS    { 0 - $2 }
 |      exp '+' exp             { $1 + $3 }
 |      exp '-' exp             { $1 - $3 }
 |      exp '*' exp             { $1 * $3 }
 |      exp '/' exp             { $1 `div` $3 }

{
parseError :: [Token] -> a
parseError [] = error "Parse error at EOF"
parseError (t:ts) = error $ "Parse error: " ++ show t

main :: IO ()
main = getContents >>= print . parse . alexScanTokens
}
