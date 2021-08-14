module Problem1to10 where

myLast (x : []) = x
myLast (x : xs) = myLast xs

myButLast (x : y : []) = x
myButLast (x : xs)     = myButLast xs

elementAt (x : xs) 1 = x
elementAt (x : xs) n = elementAt xs (n -1)

myLength []       = 0
myLength (x : xs) = myLength xs + 1

myReverse []       = []
myReverse (x : xs) = (myReverse xs) ++ [x]

isPalindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]

flatten (Elem x) = [x]
flatten (List x) =
    case x of []     -> []
              (y:ys) -> flatten y ++ flatten (List ys)

compress [] = []
compress (x:[]) = [x]
compress (x:y:z)
    | x==y = compress (x:z)
    |otherwise = x: compress (y:z)

pack []     = []
pack (x:xs) = (x:z1) : pack z2
    where
        (z1,z2) = span (==x) xs

encode [] = []
encode (x:xs) = (length z1+1,x): encode z2
    where
        (z1,z2) = span (==x) xs
