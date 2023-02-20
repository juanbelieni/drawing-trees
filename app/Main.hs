module Main where

import Graphics.Gloss

v = 60.0

data Tree a = Node a [Tree a]
  deriving Show

type Extent = [(Float, Float)]

moveTreeBy :: Tree (a, Float) -> Float -> Tree (a, Float)
moveTreeBy (Node (value, x) trees) x' = Node (value, (x + x')) trees

moveExtentBy :: Extent -> Float -> Extent
moveExtentBy ext x = map (\(r, l) -> (r + x, l + x)) ext

mergeExtents :: Extent -> Extent -> Extent
mergeExtents ls [] = ls
mergeExtents [] rs = rs
mergeExtents ((l, _) : ls) ((_, r) : rs) = (l, r) : (mergeExtents ls rs)

mergeExtentList :: [Extent] -> Extent
mergeExtentList exts = foldr mergeExtents [] exts

fitExtents :: Extent -> Extent -> Float
fitExtents ((_, r) : rs) ((l, _) : ls) = max (fitExtents rs ls) (r - l + v / 2)
fitExtents _ _ = 0.0

fitExtentListLeft :: [Extent] -> [Float]
fitExtentListLeft exts = aux [] exts
  where
    aux :: Extent -> [Extent] -> [Float]
    aux _ [] = []
    aux acc (ext : exts')
      = let x = fitExtents acc ext
        in x : aux (mergeExtents acc (moveExtentBy ext x)) exts'

fitExtentListRight :: [Extent] -> [Float]
fitExtentListRight exts = reverse (aux [] (reverse exts))
  where
    aux :: Extent -> [Extent] -> [Float]
    aux _ [] = []
    aux acc (ext : exts')
      = let x = - fitExtents ext acc
        in x : aux (mergeExtents (moveExtentBy ext x) acc) exts'

fitExtentList :: [Extent] -> [Float]
fitExtentList exts
  = map (\(x, y) -> (x + y) / 2)
        (zip (fitExtentListLeft exts) (fitExtentListRight exts))

design :: Tree a -> Tree (a, Float)
design tree = fst (design' tree)
  where
    design' :: Tree a -> (Tree (a, Float), Extent)
    design' (Node value subtrees)
      = let (subtrees', exts) = unzip (map design' subtrees)
            positions = fitExtentList exts
            subtrees'' = map (uncurry moveTreeBy) (zip subtrees' positions)
            exts' = map (uncurry moveExtentBy) (zip exts positions)
            extent' = (0.0, 0.0) : mergeExtentList exts'
            tree' = Node (value, 0.0) subtrees''
        in (tree', extent')

draw :: Tree (String, Float) -> Picture
draw tree = Pictures (draw' tree 0.0 0.0)
  where
    draw' :: Tree (String, Float) -> Float -> Float -> [Picture]
    draw' (Node (label, x') subtrees) x y
      = (translate (x + x') y (scale 0.12 0.12 (text label)))
      : concat (map (\tree' -> draw' tree' (x + x') (y - v)) subtrees)
      ++ map (\(Node (_, x'') _) -> line [(x + x' + 2, y - v * 1/8), (x + x' + x'' + 2, y - v * 5/8)]) subtrees


main :: IO ()
main = do
  let tree = Node "A" [
        Node "B" [
            Node "C" [
                Node "D" [],
                Node "E" [
                    Node "F" [
                        Node "G" [],
                        Node "H" [
                            Node "I" [],
                            Node "J" [],
                            Node "K" [],
                            Node "L" []],
                        Node "M" [],
                        Node "N" [
                            Node "O" []]]]],
            Node "P" [
                Node "Q" [],
                Node "R" []]],
        Node "S" [
            Node "T" [
                Node "U" [],
                Node "V" [],
                Node "W" [
                    Node "X" [
                        Node "Y" []],
                    Node "Z" [
                        Node "a" [],
                        Node "b" [],
                        Node "c" [],
                        Node "d" []]]],
              Node "e" [
                  Node "f" [
                      Node "g" []],
                  Node "i" [
                      Node "j" [
                          Node "k" [],
                          Node "l" [],
                          Node "m" [],
                          Node "n" []]]]],
        Node "o" [
            Node "p" [
                Node "q" [
                    Node "r" [],
                    Node "s" [],
                    Node "t" [],
                    Node "u" []],
                Node "v" [
                    Node "w" [],
                    Node "x" [
                        Node "y" [],
                        Node "z" []],
                    Node "0" [],
                    Node "1" []],
                Node "2" []]]]

  let designedTree = design tree
  print $ designedTree
  display (InWindow "Nice Window" (800, 600) (400, 400)) white (draw designedTree)
