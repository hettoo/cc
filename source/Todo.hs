module Todo where

type Todo a = ([a], [a])

tnew :: Todo a
tnew = ([], [])

todo :: Eq a =>
    a -> Todo a -> Todo a
todo a (t, d) =
    if a `elem` d || a `elem` t then
        (t, d)
    else
        (a : t, d)

done :: Eq a =>
    a -> Todo a -> Todo a
done a (t, d) = (filter ((/=) a) t, if a `elem` d then d else a : d)

getTodo :: Eq a =>
    Todo a -> (Maybe a, Todo a)
getTodo p@(t, d) = case t of
    [] -> (Nothing, p)
    a : r -> (Just a, done a (r, d))
