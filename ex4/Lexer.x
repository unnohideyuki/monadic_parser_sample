{
module Lexer where
-- TODO: "module" on the morrow
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

<0> @tok                                { layout_token }

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

pushToks :: [Token] -> Alex ()
pushToks ts = Alex $ \st ->
  let
    ust = alex_ust st
    toks = pending_tokens ust
    ust' = ust{pending_tokens=ts ++ toks}
    n = length ts
    inp = alex_inp st
    inp' = (take n [' ',' '..]) ++ inp
    st' = st{alex_ust=ust', alex_inp=inp'}
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

setMorrow :: Bool -> Alex ()
setMorrow b = Alex $ \st ->
  let
    ust = alex_ust st
    ust' = ust{morrow=b}
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
  setMorrow True
  return token

mkLLayoutKeyword2 :: AlexAction Token
mkLLayoutKeyword2 (pos, _, _, str) len = do
  let token = Token' (take len str, pos)
  return token

layout_token :: AlexAction Token
layout_token (pos@(AlexPn abs line col), _, _, str) len =
  do
    morrow <- getMorrow
    setMorrow False
    ctx <- peepCtx
    if morrow then
      if col > ctx then
        do pushCtx col
           pushToks [tok]
           return $ VOBrace vpos
      else
        do pushToks [VOBrace vpos]
           pushToks [VCBrace vpos]
           toks <- f []
           g (toks ++ [tok])
    else
      do toks <- f []
         g (toks ++ [tok])
  where
      tok = Token (take len str, pos)
      vpos = AlexPn abs line (-1)
      semi = Token (";", vpos)
      f xs = do
        ctx' <- peepCtx
        if col < ctx' then
          do popCtx
             f $ xs ++ [VCBrace vpos]
        else
          if col == ctx' then
            return $ xs ++ [semi]
          else
            return xs
      g (t:ts) = do
        pushToks ts
        return t

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
