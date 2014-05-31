{
module Lexer where
import qualified Data.Map as Map
}

%wrapper "monadUserState"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-

$white+                         ;

$digit+                         { mkL LInt }

"("                             { mkL LLParen }
")"                             { mkL LRParen }
";"                             { mkL LSemi }

[\+\-\*\/]+                     { mkL LOp }

"infixl"                        { mkL LInfixl }

$alpha+                         { mkL LName }

{
data LexemeClass = LInt | LName | LOp | LLParen | LRParen | LSemi | LInfixl
                   deriving (Eq, Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len =
  let
    t = take len str
  in
    case c of
      LInt -> return $ Int (read t, pos)
      LName -> return $ Name (t, pos)
      LInfixl -> return $ Infixl pos
      LLParen -> return $ LParen pos
      LRParen -> return $ RParen pos
      LSemi   -> return $ Semi pos
      LOp -> Alex $ (\s@AlexState{alex_ust=AlexUserState{dict=dict}} -> 
                     case Map.lookup t dict of
                       Just (name, 0) -> Right (s, Op0 (name, pos))
                       Just (name, 1) -> Right (s, Op1 (name, pos))
                       _              -> Right (s, Name (t, pos))
             )

alexEOF :: Alex Token
alexEOF = return Eof

data Token =
       Int (Integer, AlexPosn)
     | Name (String, AlexPosn)
     | Op0 (String, AlexPosn)
     | Op1 (String, AlexPosn)
     | Infixl AlexPosn
     | LParen AlexPosn
     | RParen AlexPosn
     | Semi AlexPosn
     | Eof

instance Show Token where
    show (Int  (i, pos)) = "Int: " ++ show i   ++ prettyAlexPosn pos
    show (Name (n, pos)) = "Name: " ++ show n ++ prettyAlexPosn pos
    show (Op0  (n, pos)) = "Infix0: " ++ show n ++ prettyAlexPosn pos
    show (Op1  (n, pos)) = "Infix1: " ++ show n ++ prettyAlexPosn pos
    show (Infixl pos) = show "infixl" ++ prettyAlexPosn pos
    show (LParen pos) = "LParen" ++ prettyAlexPosn pos
    show (RParen pos) = "RParen" ++ prettyAlexPosn pos
    show (Semi pos)   = "Semi" ++ prettyAlexPosn pos
    show Eof          = "[EOF]"

prettyAlexPosn (AlexPn _ line col) = 
  " at line " ++ show line ++ ", col " ++ show col

data AlexUserState = AlexUserState { dict :: Map.Map String (String, Integer) }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { dict = Map.empty }
}
