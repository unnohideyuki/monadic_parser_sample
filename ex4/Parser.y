{
{- The goal of this code is to be a compact example to show the way to do 
   the Haskell layout using alex+happy monadic parser.
   So, rules are far from enough to analize the language.
-}
module Main where
import Lexer
}

%name parser
%error { parseError }
%lexer { lexwrap } { Eof }
%monad { Alex }
%tokentype { Token }

%token
TOKEN   { Token ($$, _) }
SP_TOK  { Token' ($$, _) }
"{"     { OBrace _ }
"}"     { CBrace _ }
vobrace { VOBrace _ }
vcbrace { VCBrace _ }
"("     { OParen _ }
")"     { CParen _ }

%%
tokens:    token tokens_t       { $1 ++ $2 }
 |         {- empty -}          { [] }

tokens_t:  token tokens_t       { $1 ++ $2 }
 |         {- empty -}          { [] }

token:     TOKEN                { [$1] }
 |         "(" tokens ")"       { "(" : ($2 ++ [")"]) }
 |         SP_TOK llist         { $1 : $2 }


llist:    "{" tokens "}"                { ["{"] ++ $2 ++ ["}"] }
 |         vobrace tokens vcbrace       { ["{"] ++ $2 ++ ["}"] }
 |         vobrace tokens               {% Alex (\s -> missing_vcbrace s $2) }


{
missing_vcbrace s@AlexState{alex_ust=t@AlexUserState{indent_levels=lvs}} toks =
  case lvs of
    [] -> Left $ "fatal error: " ++ show toks
    _:lvs' -> Right (s{alex_ust=t{indent_levels=lvs'}}, ["{"] ++ toks ++ ["}"])
  
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser

main :: IO ()
main = getContents >>= pp . parse 

pp (Right []) = putStr "\n"
pp (Right (x:xs)) = do
  if x == ";" || x == "{" then putStr "\n" else putStr " "
  putStr x
  pp $ Right xs
pp (Left s) = putStrLn s
}