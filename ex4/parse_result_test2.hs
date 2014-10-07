 main = do
{ s <- getContents
; print s }
Right [("main",AlexPn 41 3 1),("=",AlexPn 46 3 6),("do",AlexPn 48 3 8),("{",AlexPn 52 4 (-1)),("s",AlexPn 52 4 2),("<-",AlexPn 55 4 4),("getContents",AlexPn 58 4 7),(";",AlexPn 71 5 (-1)),("print",AlexPn 71 5 2),("s",AlexPn 78 5 8),("}",AlexPn 52 4 (-1))]
