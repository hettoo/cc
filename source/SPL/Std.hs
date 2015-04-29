module SPL.Std where

stdSPL :: String
stdSPL = "\
\   Void _print(Bool b) {\
\       if (b) {\
\           print('T');\
\           print('r');\
\           print('u');\
\           print('e');\
\       } else {\
\           print('F');\
\           print('a');\
\           print('l');\
\           print('s');\
\           print('e');\
\       }\
\   }\

\   Void _print((a, b) t) {\
\       print('(');\
\       print(t.fst);\
\       print(',');\
\       print(' ');\
\       print(t.snd);\
\       print(')');\
\   }\

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

\   (a, b) _op_not((a, b) t) {\
\       return (!t.fst, !t.snd);\
\   }\

\   (a, b) _op_neg((a, b) t) {\
\       return (-t.fst, -t.snd);\
\   }\

\   (a, b) _op_add((a, b) t1, (a, b) t2) {\
\       return (t1.fst + t2.fst, t1.snd + t2.snd);\
\   }\
\ "
