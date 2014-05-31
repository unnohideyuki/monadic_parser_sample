{
module Lexer where
import qualified Data.Map as Map
}

%wrapper "monadUserState"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-

$white+				;

$digit+         		{ mkL LInt }
$alpha+         		{ mkL LName }

("+" | "-" | "*" | "/")+ 	{ mkL LOp }

"("    	     	   		{ mkL LLParen }
")"				{ mkL LRParen }

{
data LexemeClass = LInt | LName | LOp | LLParen | LRParen
                   deriving (Eq, Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len =
  let
    t = take len str
  in
    case c of
      LInt -> return $ Int (read t, pos)
      LName -> return $ Name (t, pos)
      LLParen -> return $ LParen pos
      LRParen -> return $ RParen pos
      LOp -> Alex $ (\s@AlexState{alex_ust=AlexUserState{dict=dict}} -> 
      	             Right (s, Op0 ("", pos))
             )

alexEOF :: Alex Token
alexEOF = return Eof

data Token =
       Int (Integer, AlexPosn)
     | Name (String, AlexPosn)
     | Op0 (String, AlexPosn)
     | Op1 (String, AlexPosn)
     | LParen AlexPosn
     | RParen AlexPosn
     | Eof

instance Show Token where
    show (Int  (i, pos)) = show i   ++ prettyAlexPosn pos
    show (Name (n, pos)) = "Name: " ++ show n ++ prettyAlexPosn pos
    show (Op0  (n, pos)) = "Infix0: " ++ show n ++ prettyAlexPosn pos
    show (Op1  (n, pos)) = "Infix1: " ++ show n ++ prettyAlexPosn pos
    show (LParen pos) = show "(" ++ prettyAlexPosn pos
    show (RParen pos) = show ")" ++ prettyAlexPosn pos
    show Eof          = "[EOF]"

prettyAlexPosn (AlexPn _ line col) = 
  "at line " ++ show line ++ ", col " ++ show col

data AlexUserState = AlexUserState { dict :: Map.Map String String }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { dict = Map.empty }

lookup_op = undefined
}
