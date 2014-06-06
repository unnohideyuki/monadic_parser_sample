{
module Lexer where
}

%wrapper "monadUserState"

@tok = [^ \ \t \n \( \) \{ \}]+

tokens :-

<0> $white                              { white_space }

<0,braced,comment> "{-"                 { mkL LOpenComment }
<comment> [^$white]*"-}"                { mkL LCloseComment }
<comment> $white+                       { skip }
<comment> [^$white]+                    { skip }

<0> "where" | "let" | "do" | "of"       { mkL LLayoutKeyword }
<braced> "where" | "let" | "do" | "of"  { mkL LLayoutKeyword2 }

<0,braced> "{"                          { mkL LOBrace }
<braced>   "}"                          { mkL LCBrace }
<0,braced> "("                          { mkL LOParen }
<0,braced> ")"                          { mkL LCParen }
<braced>   $white                       { skip }
<braced>   @tok                         { mkL LToken }

<0> @tok                                { other_token }

{
data LexemeClass = LToken | LOpenComment | LCloseComment | LOBrace | LCBrace 
                 | LOParen | LCParen | LLayoutKeyword | LLayoutKeyword2
                   deriving (Eq, Show)

-- white_space : white spaces with the startcode == 0
white_space :: AlexInput -> Int -> Alex Token
white_space (pos, _, _, _) len = Alex f
  where
    f s@AlexState{alex_ust=t@AlexUserState{pending_tokens=pend_toks}, alex_pos=(AlexPn abs line col)} =
      case pend_toks of
        tok:toks -> Right (s{alex_ust=t{pending_tokens=toks}, alex_pos=(AlexPn abs line (col-1))}, tok)
        [] -> case alexMonadScan of Alex f -> f s

-- mkL
mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len =
  let
    t = take len str
  in
    case c of
      -- tokens with between explicit braces (startcode == braced)
      LToken -> Alex $ (\s -> Right (s, Token (t, pos)))
      
      -- parentheses
      LOParen -> Alex $ (\s -> Right (s, OParen pos))
      LCParen -> Alex $ (\s -> Right (s, CParen pos))

      -- "{-" starts a comment
      LOpenComment -> Alex $
          (\s@AlexState{ alex_ust=ust@AlexUserState{ comment_depth=depth 
                                                   , saved_scd=saved_scd
                                                   }
                       , alex_scd=scd
                       } -> 
                case alexMonadScan of
                  Alex f -> 
                    let
                      scd' = if depth == 0 then
                               scd
                             else
                               saved_scd
                    in
                     f s{ alex_ust=ust{ comment_depth = depth + 1
                                      , saved_scd = scd'
                                      }
                        , alex_scd=comment
                        }
          )

      -- "-}" ends a comment
      LCloseComment -> Alex $
                      (\s@AlexState{alex_ust=ust@AlexUserState{comment_depth=depth,saved_scd=saved_scd}} -> 
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
                  CBrace pos))

      -- where, let, do or of with the default startcode.
      LLayoutKeyword -> Alex 
         (\s@AlexState{ alex_ust=ust } ->
          Right (s{ alex_ust=ust{ morrow=True }}, Token' (t, pos)))

      -- where, let, do or of between explicit braces
      LLayoutKeyword2 -> Alex (\s-> Right (s, Token' (t, pos)))

-- other_token : all other strings with the startcode == 0 
other_token :: AlexInput -> Int -> Alex Token
other_token (pos, _, _, str) len =
  let
    vpos = case pos of (AlexPn abs line _) -> (AlexPn abs line (-1)) -- pos for virtual tokens
    
    tok_str = take len str
    token = Token (tok_str, pos)
    
    additional_pops col lvs pos = 
      let
        apop' col n [] ptoks = (n, [], ptoks)
        apop' col n (l:lvs) ptoks =
          if l == -1 || col > l then
            (n, l:lvs, ptoks)
          else
            if col == l then
              (n + 1, l:lvs, ptoks ++ [Token (";", vpos)])
            else {- col < l -}
              apop' col (n+1) lvs $ ptoks ++ [VCBrace vpos]
      in
       apop' col 0 lvs []
    
    f s@AlexState{alex_ust=t@AlexUserState{ indent_levels = lv:lvs
                                          , morrow = morrow
                                          , pending_tokens = ptoks
                                          },
                  alex_inp=inp
                 } =
      case pos of
        AlexPn _ line col ->
          let
            (t', tok, npend) = 
              if morrow then
                            if col > lv then
                              (t{ indent_levels = col:lv:lvs
                                , morrow = False 
                                , pending_tokens = token:ptoks},
                               VOBrace vpos,
                               1)
                            else
                              if col == lv then
                                (t{ morrow = False
                                  , pending_tokens = token:ptoks},
                                 Token (";", vpos),
                                 1)
                              else
                                let
                                  (n, lvs', ptoks') = additional_pops col lvs pos
                                in
                                 (t{ indent_levels = lvs'
                                   , morrow = False
                                   , pending_tokens = ptoks'++(token:ptoks)},
                                  VCBrace vpos,
                                  n+1)
                        else
                            if col > lv then
                              (t, token, 0)
                            else
                              if col == lv then
                                (t{pending_tokens = token:ptoks},
                                 Token (";", vpos),
                                 1)
                              else
                                let
                                  (n, lvs', ptoks') = additional_pops col lvs pos
                                in
                                 (t{ indent_levels = lvs'
                                   , pending_tokens = ptoks'++ (token:ptoks)},
                                  VCBrace vpos,
                                  n+1)           
                                 
            new_state npend ust' =
              let
                inp' = (take npend [' ',' '..]) ++ inp
              in
               s{alex_ust=t', alex_inp=inp'}
          in 
           Right (new_state npend t', tok)
  in
    Alex f



alexEOF :: Alex Token
alexEOF = Alex $ 
          (\s@AlexState{alex_ust=ust@AlexUserState{comment_depth=depth, pending_tokens=ptoks}} -> 
            if depth == 0 then
              if length ptoks == 0 then
                Right (s, Eof)
              else
                Left $ "fatal: pending tokens left: " ++ show ptoks
            else
              Left $ "unterminated `{-': " ++ show depth
          )

data Token = Token (String, AlexPosn)
           | Token' (String, AlexPosn)
           | OBrace AlexPosn
           | CBrace AlexPosn
           | VOBrace AlexPosn
           | VCBrace AlexPosn
           | OParen AlexPosn
           | CParen AlexPosn
           | Eof
             deriving (Show)

data AlexUserState = AlexUserState { comment_depth :: Int 
                                   , saved_scd :: Int
                                   , indent_levels :: [Int]
                                   , morrow :: Bool
                                   , pending_tokens :: [Token]
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { comment_depth = 0 
                                  , saved_scd = 0
                                  , indent_levels = [0]
                                  , morrow = False
                                  , pending_tokens = []
                                  }
}
