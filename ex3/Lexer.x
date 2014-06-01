{
module Lexer where
}

%wrapper "monadUserState"

tokens :-

$white+                         ;

"{-"				{ mkL LOpenComment }
[^$white]*"-}"			{ mkL LCloseComment }

[^$white]+			{ mkL LToken }

{
data LexemeClass = LToken | LOpenComment | LCloseComment
                   deriving (Eq, Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len =
  let
    t = take len str
  in
    case c of
      LToken -> Alex $ 
                (\s@AlexState{alex_ust=AlexUserState{comment_depth=depth}} -> 
                  if depth == 0 then
                    Right (s, Token (t, pos))
                  else
                    case alexMonadScan of Alex f -> f s
                )
      LOpenComment -> Alex $
                      (\s@AlexState{alex_ust=ust@AlexUserState{comment_depth=depth}} -> 
                        case alexMonadScan of
                          Alex f -> f s{alex_ust=ust{comment_depth = depth + 1}}
                      )
      LCloseComment -> Alex $
                      (\s@AlexState{alex_ust=ust@AlexUserState{comment_depth=depth}} -> 
                        if depth > 0 then
                          case alexMonadScan of
                            Alex f -> f s{alex_ust=ust{comment_depth = depth - 1}}
                        else
			  Right (s, Token (t, pos))
                      )

alexEOF :: Alex Token
alexEOF = Alex $ 
          (\s@AlexState{alex_ust=ust@AlexUserState{comment_depth=depth}} -> 
	    if depth == 0 then
	      Right (s, Eof)
            else
	      Left "Comment reached to the EOF."
          )

data Token = Token (String, AlexPosn)
           | Eof

instance Show Token where
    show (Token (s, pos)) = show s ++ prettyAlexPosn pos
    show Eof              = "[EOF]"

prettyAlexPosn (AlexPn _ line col) = 
  " at line " ++ show line ++ ", col " ++ show col

data AlexUserState = AlexUserState { comment_depth :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { comment_depth = 0 }
}
