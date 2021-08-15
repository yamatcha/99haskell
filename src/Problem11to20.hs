module Problem11to20 where

data D a = Single a|Multiple Int a deriving Show


decodeModified [] = []
decodeModified (x:xs) =
    case x of
        Single elm     -> elm: decodeModified xs
        Multiple n elm -> take n (repeat elm) ++ decodeModified xs

encodeDirect  [] = []
encodeDirect  (x:xs) =
    case length z1 of
        0 -> Single x: encodeDirect  z2
        _ -> Multiple (length z1+1) x : encodeDirect  z2
    where
        (z1,z2) = span (==x) xs

dupli []     =[]
dupli (x:xs) = x:x:dupli xs

-- repli [] n     = []
-- repli (x:xs) n = replicate n x ++ repli xs n

repli xs n = foldr ((++) . replicate n) [] xs

dropEvery [] n = []
dropEvery xs n = init z1++ dropEvery z2 n
    where
        (z1, z2) = splitAt n xs

split [] n = ([],[])
split xs 0 = ([],xs)
split (x:xs) n = (x:z1,z2)
    where
        (z1,z2) = split xs $ n-1

slice [] n1 n2 = []
slice xs n1 n2 = take (n2-n1+1) $ drop (n1-1) xs

rotate [] n = []
rotate xs n =
    if n>0 then drop n xs ++ take n xs else drop (length xs+n) xs ++take (length xs+n) xs

removeAt n []      = ([],[])
removeAt 1  (x:xs) = ([x], xs)
removeAt n (x:xs)  = (z1,x:z2)
    where
        (z1,z2) =  removeAt (n-1) xs
