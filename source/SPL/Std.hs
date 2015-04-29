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

\   Bool _op_eq((a, b) t1, (a, b) t2) {\
\       return t1.fst == t2.fst && t1.snd == t2.snd;\
\   }\

\   Bool _op_ne((a, b) t1, (a, b) t2) {\
\       return t1.fst != t2.fst || t1.snd != t2.snd;\
\   }\

\   Bool _op_lt((a, b) t1, (a, b) t2) {\
\       return t1.fst < t2.fst || t1.fst == t2.fst && t1.snd < t2.snd;\
\   }\

\   Bool _op_gt((a, b) t1, (a, b) t2) {\
\       return t1.fst > t2.fst || t1.fst == t2.fst && t1.snd > t2.snd;\
\   }\

\   Bool _op_le((a, b) t1, (a, b) t2) {\
\       return t1.fst < t2.fst || t1.fst == t2.fst && t1.snd <= t2.snd;\
\   }\

\   Bool _op_ge((a, b) t1, (a, b) t2) {\
\       return t1.fst > t2.fst || t1.fst == t2.fst && t1.snd >= t2.snd;\
\   }\

\   (a, b) _op_add((a, b) t1, (a, b) t2) {\
\       return (t1.fst + t2.fst, t1.snd + t2.snd);\
\   }\

\   (a, b) _op_sub((a, b) t1, (a, b) t2) {\
\       return (t1.fst - t2.fst, t1.snd - t2.snd);\
\   }\

\   (a, b) _op_mul((a, b) t1, (a, b) t2) {\
\       return (t1.fst * t2.fst, t1.snd * t2.snd);\
\   }\

\   (a, b) _op_div((a, b) t1, (a, b) t2) {\
\       return (t1.fst / t2.fst, t1.snd / t2.snd);\
\   }\

\   (a, b) _op_mod((a, b) t1, (a, b) t2) {\
\       return (t1.fst % t2.fst, t1.snd % t2.snd);\
\   }\

\   [a] _op_not([a] l) {\
\       if (isEmpty(l))\
\           return l;\
\       else\
\           return !l.hd : !l.tl;\
\   }\

\   [a] _op_neg([a] l) {\
\       if (isEmpty(l))\
\           return l;\
\       else\
\           return -l.hd : !l.tl;\
\   }\

\   Bool _op_eq([a] l1, [a] l2) {\
\       if (isEmpty(l1) && isEmpty(l2))\
\           return True;\
\       else if (isEmpty(l1) || isEmpty(l2))\
\           return False;\
\       else\
\           return l1.hd == l2.hd && l1.tl == l2.tl;\
\   }\

\   Bool _op_ne([a] l1, [a] l2) {\
\       if (isEmpty(l1) && isEmpty(l2))\
\           return False;\
\       else if (isEmpty(l1) || isEmpty(l2))\
\           return True;\
\       else\
\           return l1.hd != l2.hd && l1.tl != l2.tl;\
\   }\

\   Bool _op_lt([a] l1, [a] l2) {\
\       if (isEmpty(l1) && !isEmpty(l2))\
\           return True;\
\       else if (isEmpty(l1) || isEmpty(l2))\
\           return False;\
\       else\
\           return l1.hd < l2.hd || l1.hd == l2.hd && l1.tl < l2.tl;\
\   }\

\   Bool _op_gt([a] l1, [a] l2) {\
\       if (isEmpty(l2) && !isEmpty(l1))\
\           return True;\
\       else if (isEmpty(l2) || isEmpty(l1))\
\           return False;\
\       else\
\           return l1.hd > l2.hd || l1.hd == l2.hd && l1.tl > l2.tl;\
\   }\

\   Bool _op_le([a] l1, [a] l2) {\
\       if (isEmpty(l1))\
\           return True;\
\       else if (isEmpty(l2))\
\           return False;\
\       else\
\           return l1.hd < l2.hd || l1.hd == l2.hd && l1.tl <= l2.tl;\
\   }\

\   Bool _op_ge([a] l1, [a] l2) {\
\       if (isEmpty(l2))\
\           return True;\
\       else if (isEmpty(l1))\
\           return False;\
\       else\
\           return l1.hd > l2.hd || l1.hd == l2.hd && l1.tl >= l2.tl;\
\   }\

\   [a] _op_add([a] l1, [a] l2) {\
\       if (isEmpty(l1))\
\           return l2;\
\       else if (isEmpty(l1))\
\           return l1;\
\       else\
\           return l1.hd + l2.hd : l1.tl + l2.tl;\
\   }\

\   [a] _op_sub([a] l1, [a] l2) {\
\       if (isEmpty(l1))\
\           return l2;\
\       else if (isEmpty(l1))\
\           return l1;\
\       else\
\           return l1.hd - l2.hd : l1.tl - l2.tl;\
\   }\

\   [a] _op_mul([a] l1, [a] l2) {\
\       if (isEmpty(l1))\
\           return l2;\
\       else if (isEmpty(l1))\
\           return l1;\
\       else\
\           return l1.hd * l2.hd : l1.tl * l2.tl;\
\   }\

\   [a] _op_div([a] l1, [a] l2) {\
\       if (isEmpty(l1))\
\           return l2;\
\       else if (isEmpty(l1))\
\           return l1;\
\       else\
\           return l1.hd / l2.hd : l1.tl / l2.tl;\
\   }\

\   [a] _op_mod([a] l1, [a] l2) {\
\       if (isEmpty(l1))\
\           return l2;\
\       else if (isEmpty(l1))\
\           return l1;\
\       else\
\           return l1.hd % l2.hd : l1.tl % l2.tl;\
\   }\
\ "
