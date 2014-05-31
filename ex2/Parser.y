{
module Main where
import Lexer
import qualified Data.Map as Map
}

%name parser
%error { parseError }
%lexer { lexwrap } { Eof }
%monad { Alex }
%tokentype { Token }

%token
INT     { Int ($$, _) }
NAME    { Name ($$, _) }
OP0     { Op0 ($$, _) }
OP1     { Op1 ($$, _) }
INFIXL  { Infixl _ }
'('     { LParen _ }
')'     { RParen _ }
';'     { Semi _ }

%left OP0
%left OP1
%nonassoc FUN

%%
program :: { Integer }
program:        decs exp 	{ $2 }

decs:           dec ';' decs    { () }
 |              {- empty -}     { () }

dec:            INFIXL INT NAME NAME {% Alex (\s -> reg_infixl s $2 $3 $4) }

exp :: { Integer }
exp:    INT                     { $1 }
 |      '(' exp ')'             { $2 }
 |      exp OP0 exp             { binop $2 $1 $3 }
 |      exp OP1 exp             { binop $2 $1 $3 }
 |      NAME exp exp %prec FUN  { binop $1 $2 $3 }

{
binop :: String -> Integer -> Integer -> Integer
binop op lhs rhs = case op of
  "plus"   -> lhs + rhs
  "minus"  -> lhs - rhs
  "times"  -> lhs * rhs
  "divide" -> lhs `div` rhs

reg_infixl :: AlexState -> Integer -> String -> String 
              -> Either [Char] (AlexState, ())
reg_infixl s prec opname fname =
  case s of
    s@AlexState{alex_ust=AlexUserState{dict=dict}} ->
      let 
        insert_op =
          let
            dict' = Map.insert opname (fname, prec) dict
          in
            if fname == "plus" || fname == "minus" || 
               fname == "times" || fname == "divide" 
            then
              Right (s{alex_ust = AlexUserState{dict=dict'}}, ())
            else
              Left $ "undefined function: " ++ fname
      in
        case prec of
          0 -> insert_op
          1 -> insert_op
          _ -> Left $ "invalid precedence: " ++ show prec

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser

main :: IO ()
main = getContents >>= report . parse 

report ret = 
  case ret of
    Right i -> putStrLn $ show i
    Left s  -> putStrLn s
}

