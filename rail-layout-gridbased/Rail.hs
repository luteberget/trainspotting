module Main where

import SAT as S
import SAT.Val
import SAT.Term
import SAT.Equal
import SAT.Order
import SAT.Bool
import Data.List( transpose )

--------------------------------------------------------------------------------

-- this could be changed to another number representation, if that works better
type Number = Val Int

trivialNumber :: Number
trivialNumber = val 1

newNumber :: Solver -> Int -> IO Number
newNumber s n = newVal s [1..n]

isNumberOr :: Solver -> [Lit] -> Number -> Int -> IO ()
isNumberOr s xs numb k = addClause s ((numb .= k) : xs)

{-
-- Term works much worse
type Number = Term

trivialNumber :: Number
trivialNumber = fromList []

newNumber :: Solver -> Int -> IO Number
newNumber s n = newTerm s (fromIntegral (n-1))

isNumberOr :: Solver -> [Lit] -> Number -> Int -> IO ()
isNumberOr s xs numb k = equalOr s xs numb (number (fromIntegral (k-1)))
-}

--------------------------------------------------------------------------------

-- a cell maintains info on:
-- - what edges / \ _ are in it
-- - what color do they have
-- - the "point" in the lower-right corner
data Cell
  = Cell
  -- the edges in the cell
  { up      :: Lit -- /
  , down    :: Lit -- \
  , bot     :: Lit -- _

  , ccol    :: Number -- color of \ or /
  , bcol    :: Number -- color of _
  
  -- the point down-right
  , connect :: Lit -- is it connecting two edges?
  , thing   :: Lit -- is it one of the following features:
  , switchL :: Lit
  , switchR :: Lit
  , mergeL  :: Lit
  , mergeR  :: Lit
  , new     :: Lit
  , end     :: Lit
  }

newCell :: Solver -> Int -> IO Cell
newCell s nc =
  do x <- newLit s
     y <- newLit s
     z <- newLit s
     addClause s [neg x, neg y]
     c1 <- newNumber s nc
     c2 <- newNumber s nc
     
     cn <- newLit s
     th <- newLit s
     addClause s [neg cn, neg th]
     sL <- newLit s
     sR <- newLit s
     mL <- newLit s
     mR <- newLit s
     nw <- newLit s
     ed <- newLit s
     addClause s [neg th, sL, sR, mL, mR, nw, ed]
     return (Cell x y z c1 c2 cn th sL sR mL mR nw ed)

-- special cells at the border of the grid
newTopCell :: Solver -> Int -> IO Cell
newTopCell s nc =
  do z <- newLit s
     c2 <- newNumber s nc
     
     cn <- newLit s
     th <- newLit s
     addClause s [neg cn, neg th]
     sR <- newLit s
     mL <- newLit s
     nw <- newLit s
     ed <- newLit s
     addClause s [neg th, sR, mL, nw, ed]
     return (Cell false false z trivialNumber c2 cn th false sR mL false nw ed)

newLeftCell :: Solver -> IO Cell
newLeftCell s =
  do th <- newLit s
     nw <- newLit s
     addClause s [neg th, nw]
     return (Cell false false false trivialNumber trivialNumber
                  false th false false false false nw false)

-- used for bottom cells and right cells
emptyCell :: Cell
emptyCell =
  Cell false false false trivialNumber trivialNumber
       false false false false false false false false

--------------------------------------------------------------------------------

-- putting 4 cells next/above each other
cells4 :: Solver -> Int -> Cell -> Cell -> Cell -> Cell -> IO ()
cells4 s nc ul ur bl br =
  do -- exclude impossible configurations with too many edges
     addClause s [neg downL, neg upR]                       -- no \/
     addClause s [neg upL,   neg downR]                     -- no /\
     addClause s [neg downL, neg upL]                       -- no >
     addClause s [neg downR, neg upR]                       -- no <
     addClause s [neg downL, neg strtL, neg strtR, neg downR] -- no -\-
     addClause s [neg strtL, neg upL,   neg upR, neg strtR]   -- no -/-

     -- connect or thing when something is going on
     addClause s [neg downL, connect p, thing p]
     addClause s [neg strtL, connect p, thing p]
     addClause s [neg upL,   connect p, thing p]
     addClause s [neg downR, connect p, thing p]
     addClause s [neg strtR, connect p, thing p]
     addClause s [neg upR,   connect p, thing p]
     
     -- Connect
     addClause s [neg (connect p), downL, strtL, upL]
     addClause s [neg (connect p), neg downL, neg strtL]
     addClause s [neg (connect p), neg upL, neg strtL]
     addClause s [neg (connect p), downR, strtR, upR]
     addClause s [neg (connect p), neg downR, neg strtR]
     addClause s [neg (connect p), neg upR, neg strtR]

     -- connect makes sure that the edges have the same color
     col <- newNumber s nc
     equalOr s [neg (connect p), neg downL] col cdownL
     equalOr s [neg (connect p), neg strtL] col cstrtL
     equalOr s [neg (connect p), neg upL]   col cupL
     equalOr s [neg (connect p), neg downR] col cdownR
     equalOr s [neg (connect p), neg strtR] col cstrtR
     equalOr s [neg (connect p), neg upR]   col cupR

     -- New
     addClause s [neg (new p), neg downL]
     addClause s [neg (new p), neg strtL]
     addClause s [neg (new p), neg upL]
     addClause s [neg (new p), neg upR]
     addClause s [neg (new p), strtR]
     addClause s [neg (new p), neg downR]

     -- End
     addClause s [neg (end p), neg downL]
     addClause s [neg (end p), strtL]
     addClause s [neg (end p), neg upL]
     addClause s [neg (end p), neg upR]
     addClause s [neg (end p), neg strtR]
     addClause s [neg (end p), neg downR]

     -- SwitchL
     addClause s [neg (switchL p), strtL, downL]
     addClause s [neg (switchL p), strtR]
     addClause s [neg (switchL p), neg strtL, upR]
     addClause s [neg (switchL p), neg downL, downR]

     -- SwitchR
     addClause s [neg (switchR p), strtL, upL]
     addClause s [neg (switchR p), strtR]
     addClause s [neg (switchR p), neg strtL, downR]
     addClause s [neg (switchR p), neg upL, upR]

     -- MergeL
     addClause s [neg (mergeL p), strtR, downR]
     addClause s [neg (mergeL p), strtL]
     addClause s [neg (mergeL p), neg strtR, upL]
     addClause s [neg (mergeL p), neg downR, downL]

     -- MergeR
     addClause s [neg (mergeR p), strtR, upR]
     addClause s [neg (mergeR p), strtL]
     addClause s [neg (mergeR p), neg strtR, downL]
     addClause s [neg (mergeR p), neg upR, upL]
 where
  p     = ul
  
  downL = down ul
  strtL = bot ul
  upL   = up bl
  upR   = up ur
  strtR = bot ur
  downR = down br
  
  cdownL = ccol ul
  cstrtL = bcol ul
  cupL   = ccol bl
  cupR   = ccol ur
  cstrtR = bcol ur
  cdownR = ccol br

--------------------------------------------------------------------------------

type Grid = [[Cell]]

newGrid :: Solver -> Int -> (Int,Int) -> IO Grid
newGrid s nc (x,y) =
  do css <- sequence
            [ sequence [ if j == y || i == x then
                           return emptyCell
                         else if i == 0 then
                           newLeftCell s
                         else if j == 0 then
                           newTopCell s nc
                         else
                           newCell s nc
                       | i <- [0..x]
                       ]
            | j <- [0..y]
            ]
     sequence_
       [ cells4 s nc ul ur bl br
       | (cs1,cs2) <- css `zip` tail css
       , let ccss = cs1 `zip` cs2
       , ((ul,bl),(ur,br)) <- ccss `zip` tail ccss
       ]
     return css

--------------------------------------------------------------------------------

data Thing
  = SwitchL Int Int Int
  | SwitchR Int Int Int
  | MergeL Int Int Int
  | MergeR Int Int Int
  | New Int
  | End Int
 deriving ( Eq, Ord, Show )

inputs :: Thing -> [Int]
inputs (SwitchL x y z) = [x]
inputs (SwitchR x y z) = [x]
inputs (MergeL  x y z) = [x,y]
inputs (MergeR  x y z) = [x,y]
inputs (New x)         = []
inputs (End x)         = [x]

outputs :: Thing -> [Int]
outputs (SwitchL x y z) = [y,z]
outputs (SwitchR x y z) = [y,z]
outputs (MergeL  x y z) = [z]
outputs (MergeR  x y z) = [z]
outputs (New x)         = [x]
outputs (End x)         = []

colors :: Thing -> [Int]
colors t = inputs t ++ outputs t

example0 =
  [ New 1
  , SwitchL 1 1 2
  , MergeR 1 2 1 
  , End 1
  ]

example1 =
  [ New 1
  , SwitchL 1 2 3
  , SwitchR 2 1 2
  , SwitchR 3 3 4
  , MergeL 1 2 2
  , MergeR 2 3 3
  , MergeL 3 4 3
  , End 3
  ]

exampleBjornar =
  [ New 1
  , New 2
  , SwitchL 2 3 4
  , SwitchR 1 1 2
  , SwitchR 2 2 5
  , MergeL 5 3 3
  , New 7
  , SwitchR 1 1 5
  , MergeL 2 3 3
  , SwitchL 3 3 6
  , MergeR 5 7 2
  , MergeL 2 3 2
  , End 2
  , MergeL 6 4 2
  , MergeL 1 2 1
  , End 1
  ]

-- giving each edge a unique name (=color)
-- later, we can reuse colors when we're sure they won't mix
-- but only after adding ordering constraints
rename :: [Thing] -> [Thing]
rename ths = go 1 [] ths
 where
  go _ tab [] =
    if null tab then [] else error (show tab)
  
  go i tab (th:ths) =
    ren th (map (tab!) (inputs th) ++ outs)
      : go (i+n) ( (outputs th `zip` outs)
                ++ [ (x,y) | (x,y) <- tab, x `notElem` inputs th ]
                 ) ths
   where
    n = length (outputs th)

    outs
      | reuseOldNames = take n (filter (\i -> i `elem` map (tab!) (inputs th)
                                           || i `notElem` map snd tab) [1..])
      | otherwise     = take n [i,i+1 ..]

    -- reusing names (to reduce the number of different colors) seems like
    -- a good idea but makes the problem A LOT harder!
    reuseOldNames = False -- True

  ren (New _)         (i:_)     = New i
  ren (End _)         (i:_)     = End i
  ren (SwitchL _ _ _) (i:j:k:_) = SwitchL i j k
  ren (SwitchR _ _ _) (i:j:k:_) = SwitchR i j k
  ren (MergeL  _ _ _) (i:j:k:_) = MergeL i j k
  ren (MergeR  _ _ _) (i:j:k:_) = MergeR i j k

  tab!x = head $ [ y | (x',y) <- tab, x' == x ]
              ++ error (show tab ++ " ! " ++ show x)

-- adding constraints for a thing, adding the right colors
thingOr :: Solver -> [Lit] -> Thing -> Cell -> Cell -> Cell -> Cell -> IO ()
thingOr s pre th ul ur bl br =
  case th of
    New i ->
      do addClause s (new p : pre)
         isNumberOr s pre cstrtR i

    End i ->
      do addClause s (end p : pre)
         isNumberOr s pre cstrtL i

    SwitchL i j k ->
      do addClause s (switchL p : pre)
         isNumberOr s (neg strtL : pre) cstrtL i
         isNumberOr s (neg strtL : pre) cupR   j
         isNumberOr s (neg strtL : pre) cstrtR k
         isNumberOr s (neg downL : pre) cdownL i
         isNumberOr s (neg downL : pre) cstrtR j
         isNumberOr s (neg downL : pre) cdownR k

    SwitchR i j k ->
      do addClause s (switchR p : pre)
         isNumberOr s (neg strtL : pre) cstrtL i
         isNumberOr s (neg strtL : pre) cstrtR j
         isNumberOr s (neg strtL : pre) cdownR k
         isNumberOr s (neg upL   : pre) cupL   i
         isNumberOr s (neg upL   : pre) cupR   j
         isNumberOr s (neg upL   : pre) cstrtR k

    MergeL i j k ->
      do addClause s (mergeL p : pre)
         isNumberOr s (neg strtR : pre) cstrtR k
         isNumberOr s (neg strtR : pre) cstrtL i
         isNumberOr s (neg strtR : pre) cupL   j
         isNumberOr s (neg downR : pre) cdownR k
         isNumberOr s (neg downR : pre) cdownL i
         isNumberOr s (neg downR : pre) cstrtL j

    MergeR i j k ->
      do addClause s (mergeR p : pre)
         isNumberOr s (neg strtR : pre) cstrtR k
         isNumberOr s (neg strtR : pre) cdownL i
         isNumberOr s (neg strtR : pre) cstrtL j
         isNumberOr s (neg upR   : pre) cupR   k
         isNumberOr s (neg upR   : pre) cstrtL i
         isNumberOr s (neg upR   : pre) cupL   j
 where
  p     = ul
  
  downL = down ul
  strtL = bot ul
  upL   = up bl
  upR   = up ur
  strtR = bot ur
  downR = down br
  
  cdownL = ccol ul
  cstrtL = bcol ul
  cupL   = ccol bl
  cupR   = ccol ur
  cstrtR = bcol ur
  cdownR = ccol br

-- each point, if it is a thing, it must be a thing from the list
-- each thing from the list is represented exactly once
-- the things are in the right order
thingsGrid :: Solver -> [Thing] -> Grid -> IO ()
thingsGrid s ths css =
  do -- each point, if it's a thing, it needs to be in the list
     xlss <- sequence
              [ do ls <- sequence
                         [ do l <- newLit s
                              thingOr s [neg l] th ul ur bl br
                              return l
                         | th <- ths
                         ]
                   addClause s (neg (thing p) : ls)
                   return (x,ls)
              | (cs1,cs2) <- css `zip` tail css
              , let ccss = cs1 `zip` cs2
              , (x,((p@ul,bl),(ur,br))) <- [1..] `zip` (ccss `zip` tail ccss)
              ]
     -- each thing from the list exists exactly once
     sequence_
       [ do addClause s qs
            atMostOne s qs
       | qs <- transpose (map snd xlss)
       ]
     -- they appear in the same linear order
     sequence_
       [ addClause s (neg l : [ ls'!!(t-1) | (x',ls') <- xlss, x' <= x ])
       | (x,ls) <- xlss
       , (t,l)  <- [0..] `zip` ls
       , t > 0
       ]

--------------------------------------------------------------------------------

main :: IO ()
main =
  withNewSolver $ \s ->
    do putStrLn "--- generating grid..."
       putStrLn ("nc = " ++ show nc)
       css <- newGrid s nc (27,5)
       putStrLn "--- generating things..."
       sequence_ [ print t | t <- things ]
       thingsGrid s things css
       putStrLn "--- solving..."
       b <- solve s []
       if b then
         do n <- displayGrid s css
            putStrLn "--- optimizing diags..."
            q <- newTerm s (fromIntegral n)
            ls <- sequence
                  [ do l <- newLit s
                       addClause s [l, neg (down c)]
                       addClause s [l, neg (up c)]
                       return l
                  | cs <- css
                  , c <- cs
                  ]
            
            lessThanEqual s (fromList [(1,l) | l <- ls]) q
            a <- newLit s
            let loop a n =
                  do lessThanEqualOr s [neg a] q (number (fromIntegral n))
                     b <- solve s [a]
                     if b then
                       do n' <- displayGrid s css
                          loop a (n'-1)
                      else
                       do putStrLn "NO BETTER SOLUTIONS"
             in loop a (n-1)
        else
         do putStrLn "NO"
 where
  nc = maximum [ c | th <- things, c <- colors th ]

  --things = rename exampleBjornar
  --things = rename (exampleBjornar ++ exampleBjornar)
  things = rename $ init exampleBjornar ++ tail exampleBjornar

displayGrid :: Solver -> Grid -> IO Int
displayGrid s css =
  do bs <- sequence
                  [ do b1 <- S.modelValue s (down c)
                       b2 <- S.modelValue s (up c)
                       return (b1 || b2)
                  | cs <- css
                  , c <- cs
                  ]
     let n = length (filter id bs)
     putStrLn ("diags = " ++ show n ++ ":")
     sequence_
       [ do sequence_
              [ do a <- S.modelValue s (down c)
                   putStr (if a then "\\" else " ")
                   b <- S.modelValue s (up c)
                   putStr (if b then "/" else " ")
              | c <- cs
              ]
            putStrLn ""
            sequence_
              [ do d <- S.modelValue s (bot c)
                   b <- S.modelValue s (up c)
                   putStr (if b then "/" else if d then "_" else " ")
                   a <- S.modelValue s (down c)
                   putStr (if a then "\\" else if d then "_" else ".")

                   --th <- S.modelValue s (thing c)
                   --cn <- S.modelValue s (connect c)
                   --putStr (if th then "T" else if cn then "C" else " ")
              | c <- cs
              ]
            putStrLn ""
       | cs <- (init . tail) `fmap` init css
       ]
     return n


