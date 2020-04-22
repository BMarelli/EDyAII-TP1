module TP1 where

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
                |Leaf k v
                |E
                deriving (Show, Eq)

-- t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E) (Node 'o' (Just 2) (Leaf 'd' 9) E (Leaf 's' 4)) E) (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8) (Leaf 'n' 7) E) E)

-- Dado una clave [k] y un TTree, devuelve el valor asociado a la clave [k] 
search :: Ord k => [k] -> TTree k v -> Maybe v 
search _ E = Nothing
search [] _ = Nothing
search [c] (Leaf k v) | c == k     = Just v
                      | otherwise  = Nothing
search (c:cs) (Leaf k v) = Nothing
search a@(c:cs) (Node k v xs ys zs) | c == k     = (if cs /= [] then (search cs ys)
                                                    else v)
                                    | c < k      = (search a xs)
                                    | otherwise  = (search a zs)

-- Dado una clave [k], un valor v y un TTree, agrega un par (clave, valor) a un arbol.
-- Si la clave ya esta en el arbol, actualiza su valor
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert (c:cs) n E = if cs == [] then Leaf c n
                    else Node c Nothing E (insert cs n E) E
insert a@(c:cs) n (Leaf k v) | c == k     = case cs of
                                                [] -> Leaf k n
                                                otherwise -> Node k (Just v) E (insert cs n E) E
                             | c < k      = Node k (Just v) (insert a n E) E E
                             | otherwise  = Node k (Just v) E E (insert a n E)

insert a@(c:cs) n (Node k v xs ys zs) | c == k    = (if cs /= [] then Node k v xs (insert cs n ys) zs
                                                   else (Node k (Just n) xs ys zs))
                                      | c < k     = Node k v (insert a n xs) ys zs
                                      |otherwise  = Node k v xs ys (insert a n zs)

q = insert "cose" 1 E
w = insert "cosa" 2 q
e = insert "cosi" 3 w
r = insert "cosb" 4 e
t = insert "cosbe" 5 r
y = insert "cosc" 6 t
u = insert "cosd" 7 y
i = insert "cosde" 8 u

-- mergeMaximoHijo :: TTree k v -> TTree k v
-- mergeMaximoHijo nodo@(Node k v xs ys zs) = if ys == E then (Node k v xs (maximoHijo xs) zs)
--                                                       else (Node k v xs ys zs)
--     where maximoHijo xs@(Node _ _ _ _ zs) = case zs of
--                                                 E -> xs
--                                                 Leaf _ _ -> zs
--                                                 otherwise -> maximoHijo zs

-- delete :: Ord k => [k] -> TTree k v -> TTree k v
-- delete _ E = E
-- delete (c:cs) (Leaf k v) | c == k    = case cs of
--                                         [] -> E
--                                         otherwise -> Leaf k v
--                          | otherwise = Leaf k v
-- -- delete [c] (Node k v xs ys zs) 
-- delete a@(c:cs) (Node k v xs ys zs) | c < k       = Node k v (delete a xs) ys zs
--                                     | c > k       = Node k v xs ys (delete a zs)
--                                     | otherwise   = case cs of
--                                                         [] -> case zs of
--                                                                 E -> Node k Nothing xs ys zs
--                                                                 Node k v xs ys zs -> 
--                                                         otherwise -> delete cs ys

--Dado un arbol devuelve una lista ordenada con las claves del mismo.
keys :: TTree k v -> [[k]]
keys xs = keys_ xs []
 where keys_ E _ = []
       keys_ (Leaf k v) cs           = [cs ++ [k]]
       keys_ (Node k v xs ys zs) cs  = case v of
                                       Just _ -> (keys_ xs cs) ++ [cs ++ [k]] ++ (keys_ ys (cs ++ [k])) ++ (keys_ zs cs)
                                       otherwise -> (keys_ xs cs) ++ (keys_ ys (cs ++ [k])) ++ (keys_ zs cs)
