{
module Lexer where
}

%wrapper "monadUserState"

tokens :-

<0> $white+                             { white_space } 

<0,comment> "{-"                        { mkL LOpenComment }
<comment> [^$white]*"-}"                { mkL LCloseComment }
<comment> $white+                       { skip }
<comment> [^$white]+                    { skip }

<0> "where" | "let" | "do" | "of"       { mkL LLayoutKeyword }

<0,braced> "{"                          { mkL LOBrace }
<braced>   "}"                          { mkL LCBrace }
<braced>   $white                       { skip }
<braced>   [^$white]+                   { mkL LToken }

<0> [^$white]+                          { other_token }

{
data LexemeClass = LToken | LOpenComment | LCloseComment | LOBrace | LCBrace 
                 | LLayoutKeyword
                   deriving (Eq, Show)

-- white_space : white spaces with the startcode == 0
white_space :: AlexInput -> Int -> Alex Token
white_space (pos, _, _, _) len = Alex f
  where
    f s@AlexState{alex_ust=t@AlexUserState{pending_token=pend_tok}} =
      case pend_tok of
        Just tok -> Right (s{alex_ust=t{pending_token=Nothing}}, tok)
        Nothing  -> case alexMonadScan of Alex f -> f s

-- mkL
mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len =
  let
    t = take len str
  in
    case c of
      -- tokens with in explicit braces (startcode == braced)
      LToken -> Alex $ (\s -> Right (s, Token (t, pos)))

      -- "{-" starts a comment, and also behave as a valid white space, 
      -- outputs pending token if exists.
      LOpenComment -> Alex $
          (\s@AlexState{alex_ust=ust@AlexUserState{ comment_depth=depth
                                                  , pending_token = token }} -> 
              case token of
                Just tok -> Right (s{ alex_ust=ust{ comment_depth = depth + 1
                                                  , pending_token = Nothing}
                                    , alex_scd=comment
                                    },
                                   tok)
                Nothing -> case alexMonadScan of
                  Alex f -> f s{ alex_ust=ust{comment_depth = depth + 1}
                               , alex_scd=comment
                               }
          )

      -- "-}" ends a comment
      LCloseComment -> Alex $
                      (\s@AlexState{alex_ust=ust@AlexUserState{comment_depth=depth}} -> 
                        if depth > 0 then
                          case alexMonadScan of
                            Alex f -> f s{ alex_ust=ust{comment_depth = depth - 1}
                                         , alex_scd = if depth == 1 then 
                                                        0 
                                                      else 
                                                        comment
                                         }
                        else
                          Right (s, Token (t, pos))
                      )

      -- "{", explicit open brace starts `braced' state and 
      -- push a (-1) to the indente levels.
      LOBrace -> Alex $
       (\s@AlexState{ alex_ust = ust@AlexUserState{ indent_levels = lvs }} ->
         Right (s{ alex_ust=ust{ indent_levels = (-1):lvs }, alex_scd=braced },
                OBrace pos))

      -- "}", explicit close brace ends a `braced':
      -- pos a (-1) from the indente levels and chenge the startcode if necessary.
      LCBrace -> Alex $
       (\s@AlexState{ alex_ust = ust@AlexUserState{ indent_levels = lv:lvs }} ->
         let
           scd = case lvs of
                   (-1):xs -> braced
                   _ -> 0
         in
           Right (s{ alex_ust=ust{ indent_levels = lvs }, alex_scd=scd },
                  OBrace pos))

      -- where, let, do or of with the default startcode.
      LLayoutKeyword -> Alex $
        (\s@AlexState{ alex_ust=ust } ->
          Right (s{ alex_ust=ust{ morrow=True }}, Token (t, pos)))

-- other_token : all other strings with the startcode == 0 
other_token :: AlexInput -> Int -> Alex Token
other_token (pos, _, _, str) len =
  let
    t = take len str
    token = Token (t, pos)
    f s@AlexState{alex_ust=t@AlexUserState{ indent_levels = lv:lvs
                                          , morrow = morrow
                                          }} =
      case pos of
        AlexPn _ line col ->
          let
            (t', tok) = if morrow then
                            if col > lv then
                              (t{ indent_levels = col:lv:lvs
                                , morrow = False 
                                , pending_token = Just token},
                               VOBrace pos)
                            else
                              if col == lv then
                                (t{ morrow = False
                                  , pending_token = Just token},
                                 Token (";", pos))
                              else
                                (t{ indent_levels = lvs
                                  , morrow = False
                                  , pending_token = Just token},
                                 VCBrace pos)
                        else
                            if col > lv then
                              (t, token)
                            else
                              if col == lv then
                                (t{pending_token = Just token},
                                 Token (";", pos))
                              else
                                (t{indent_levels = lvs
                                  , pending_token = Just token},
                                 VCBrace pos)
          in
            Right (s{alex_ust=t'}, tok)
  in
    Alex f



alexEOF :: Alex Token
alexEOF = Alex $ 
          (\s@AlexState{alex_ust=ust@AlexUserState{comment_depth=depth}} -> 
            if depth == 0 then
              Right (s, Eof)
            else
              Left "unterminated `{-'"
          )

data Token = Token (String, AlexPosn)
           | OBrace AlexPosn
           | CBrace AlexPosn
           | VOBrace AlexPosn
           | VCBrace AlexPosn
           | Eof

instance Show Token where
    show (Token (s, pos)) = show s ++ prettyAlexPosn pos
    show Eof              = "[EOF]"

prettyAlexPosn (AlexPn _ line col) = 
  " at line " ++ show line ++ ", col " ++ show col

data AlexUserState = AlexUserState { comment_depth :: Int 
                                   , indent_levels :: [Int]
                                   , morrow :: Bool
                                   , pending_token :: Maybe Token
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { comment_depth = 0 
                                  , indent_levels = [0]
                                  , morrow = False
                                  , pending_token = Nothing
                                  }
}
