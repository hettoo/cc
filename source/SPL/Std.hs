module SPL.Std where

stdSPL :: String
stdSPL = "\
\   Void _print([a] l) {\
\       print('[');\
\       if (!isEmpty(l)) {\
\           print(l.hd);\
\           l = l.tl;\
\       }\
\       while (!isEmpty(l)) {\
\           print(',');\
\           print(' ');\
\           print(l.hd);\
\           l = l.tl;\
\       }\
\       print(']');\
\   }\
\ "
