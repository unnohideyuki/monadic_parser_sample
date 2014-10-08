{
module Lexer where
}

%wrapper "monadUserState"

@tok = [^ \ \t \n \( \) \{ \}]+

tokens :-

<0> $white                              { white_space }

<0,braced,comment> "{-"                 { mkLOpenComment }
<comment> $white+                       { skip }
<comment> [^$white]*"-}"                { mkLCloseComment }
<comment> [^$white]+                    { skip }

<0> "where" | "let" | "do" | "of"       { mkLLayoutKeyword }
<braced> "where" | "let" | "do" | "of"  { mkLLayoutKeyword2 }

<0,braced> "{"                          { mkLOBrace }
<braced>   "}"                          { mkLCBrace }
<0,braced> "("                          { mkLOParen }
<0,braced> ")"                          { mkLCParen }
<braced>   $white                       { skip }
<braced>   @tok                         { mkLToken }

<0> @tok                                { other_token }

{
data LexemeClass = LToken | LOpenComment | LCloseComment | LOBrace | LCBrace 
                 | LOParen | LCParen | LLayoutKeyword | LLayoutKeyword2
                   deriving (Eq, Show)

-- State Utility
nop :: Monad m => m ()
nop = return ()

getPendingToks :: Alex [Token]
getPendingToks = Alex $ \st -> Right (st, pending_tokens $ alex_ust st)

setPendingToks :: [Token] -> Alex ()
setPendingToks toks = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{pending_tokens=toks}
    st' = st{alex_ust=ust'}
  in
    Right (st', ())

moveColumn :: Int -> Alex ()
moveColumn x = Alex $ \st ->
  let
    (AlexPn abs line col) = alex_pos st
  in
    Right(st{alex_pos=AlexPn abs line (col + x)}, ())

getCommentDepth :: Alex Int
getCommentDepth = Alex $ \st -> Right (st, comment_depth $ alex_ust st)

setCommentDepth :: Int -> Alex ()
setCommentDepth d = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{comment_depth=d}
    st' = st{alex_ust=ust'}
  in
    Right (st', ())

getSavedScd :: Alex Int
getSavedScd = Alex $ \st -> Right (st, saved_scd $ alex_ust st)

saveScd :: Int -> Alex ()
saveScd scd = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{saved_scd=scd}
    st' = st{alex_ust=ust'}
  in
    Right (st', ())

pushCtx :: Int -> Alex ()
pushCtx n = Alex $ \st ->
  let
    ust = alex_ust st
    ms = indent_levels ust
    ust' = ust{indent_levels=(n:ms)}
  in
    Right (st{alex_ust=ust'}, ())

popCtx :: Alex Int
popCtx = Alex $ \st ->
  let
    ust = alex_ust st
    ms' = indent_levels ust
  in
    case ms' of
      (m:ms) -> let ust' = ust{indent_levels=ms}
                    st' = st{alex_ust=ust'}
                in
                  Right (st', m)
      _ -> Left "empty layout context."

peepCtx :: Alex Int
peepCtx = Alex $ \st ->
  let
    ms' = indent_levels $ alex_ust st
  in
    case ms' of
      (m:ms) -> Right (st, m)
      _ -> Left "empty layout context."

setMorrow :: Alex ()
setMorrow = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{morrow=True}
    st' = st{alex_ust=ust'}
  in
    Right (st', ())

getMorrow :: Alex Bool
getMorrow = Alex $ \st -> Right (st, morrow $ alex_ust st)

-- white_space : white spaces with the startcode == 0
white_space _ _ = do
  ptoks <- getPendingToks
  case ptoks of
    tok:toks -> do setPendingToks toks
                   moveColumn (-1)
                   return tok
    [] -> alexMonadScan

-- mkL
mkLToken :: AlexAction Token
mkLToken (pos, _, _, str) len = return $ Token (take len str, pos)

mkLOParen :: AlexAction Token
mkLOParen (pos, _, _, _) _ = return $ OParen pos
mkLCParen :: AlexAction Token
mkLCParen (pos, _, _, _) _ = return $ CParen pos

mkLOpenComment :: AlexAction Token
mkLOpenComment _ _ = 
  do
    depth <- getCommentDepth
    scd <- alexGetStartCode
    setCommentDepth (depth + 1)
    if depth == 0 then 
      saveScd scd
    else
      nop
    alexSetStartCode comment
    alexMonadScan

mkLCloseComment :: AlexAction Token
mkLCloseComment (pos, _, _, str) len =
  do
    let token = Token (take len str, pos)
    depth <- getCommentDepth
    if depth > 0 then do
      setCommentDepth (depth - 1)
      scd <- getSavedScd
      if depth == 1 then
        alexSetStartCode scd
      else
        nop
      alexMonadScan
    else
      return token

mkLOBrace :: AlexAction Token
mkLOBrace (pos, _, _, _) _ = do
  pushCtx (-1)
  alexSetStartCode braced
  return (OBrace pos)

mkLCBrace :: AlexAction Token
mkLCBrace (pos, _, _, _) _ = do
  popCtx
  m <- peepCtx
  if m == -1 then 
    nop 
  else 
    alexSetStartCode 0
  return (CBrace pos)

mkLLayoutKeyword :: AlexAction Token
mkLLayoutKeyword (pos, _, _, str) len = do
  let token = Token' (take len str, pos)
  setMorrow
  return token

mkLLayoutKeyword2 :: AlexAction Token
mkLLayoutKeyword2 (pos, _, _, str) len = do
  let token = Token' (take len str, pos)
  return token

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
alexEOF = do
  depth <- getCommentDepth
  ptoks <- getPendingToks
  if depth == 0 then
    if length ptoks == 0 then
      return Eof
    else
      alexError $ "fatal: pending tokens left: " ++ show ptoks
  else
    alexError $ "unterminated `{-': " ++ show depth

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
