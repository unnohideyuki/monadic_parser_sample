{
module Lexer where
}

%wrapper "posn"
$digit = [0-9]

tokens :-

$white+         ;
$digit+         { \pos s -> Int (read s, pos) }

"("             { \pos _ -> LParen pos }
")"             { \pos _ -> RParen pos }
"+"             { \pos _ -> Plus pos }
"-"             { \pos _ -> Minus pos }
"*"             { \pos _ -> Times pos }
"/"             { \pos _ -> Div pos }

{
data Token =
       Int (Integer, AlexPosn)
     | LParen AlexPosn
     | RParen AlexPosn
     | Plus AlexPosn
     | Minus AlexPosn
     | Times AlexPosn
     | Div AlexPosn

instance Show Token where
    show (Int (i, pos)) = show i   ++ prettyAlexPosn pos
    show (LParen pos) = show "(" ++ prettyAlexPosn pos
    show (RParen pos) = show ")" ++ prettyAlexPosn pos
    show (Plus   pos) = show "+" ++ prettyAlexPosn pos
    show (Minus  pos) = show "-" ++ prettyAlexPosn pos
    show (Times  pos) = show "*" ++ prettyAlexPosn pos
    show (Div    pos) = show "/" ++ prettyAlexPosn pos

prettyAlexPosn (AlexPn _ line col) = 
  "at line " ++ show line ++ ", col " ++ show col
}
