module Thing where

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

--------------------------------------------------------------------------------

type Example = ((Int,Int),[Thing])

example0 :: Example
example0 =
  ( (6,2)
  , [ New 1
    , SwitchL 1 1 2
    , MergeR 1 2 1 
    , End 1
    ]
  )

example1 :: Example
example1 =
  ( (9,4)
  , [ New 1
    , SwitchL 1 2 3
    , SwitchR 2 1 2
    , SwitchR 3 3 4
    , MergeL 1 2 2
    , MergeR 2 3 3
    , MergeL 3 4 3
    , End 3
    ]
  )

example110 :: Example
example110 =
  ( (16,9)
  , [ New 1
    , New 2
    , New 3
    , New 4
    , New 5
    , New 6
    , New 7
    , New 8
    , New 9
    , MergeL 9 8 9
    , MergeL 7 6 7
    , MergeR 7 5 7
    , MergeL 9 7 9
    , MergeL 9 4 9
    , MergeL 3 2 3
    , MergeR 3 1 3
    , SwitchL 3 2 3
    , MergeL 9 2 2
    , SwitchL 2 2 5
    , SwitchR 2 4 2
    , End 4
    , End 2
    , MergeR 5 3 1
    , End 1
    ]
  )

exampleBjornar :: Example
exampleBjornar =
  ( (14,5)
  , [ New 1
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
  )

exampleBjornar2 :: Example
exampleBjornar2 =
  ( (27,5)
  , init things ++ tail things
  )
 where
  (_,things) = exampleBjornar

exampleBjornar2par :: Example
exampleBjornar2par =
  ( (14,10)
  , take k things2 `inter` drop k things2
  )
 where
  (_,things) = exampleBjornar
  things2    = rename (things ++ things)
  k          = length things

exampleBjornar3 :: Example
exampleBjornar3 =
  ( (27,10)
  , things' `inter` drop k things2
  )
 where
  (_,things') = exampleBjornar
  (_,things)  = exampleBjornar2
  things2     = rename (things ++ things)
  k           = length things

exampleBjornar4 :: Example
exampleBjornar4 =
  ( (27,10)
  , take k things2 `inter` drop k things2
  )
 where
  (_,things) = exampleBjornar2
  things2    = rename (things ++ things)
  k          = length things

(x:xs) `inter` ys = x : (ys `inter` xs)
[]     `inter` ys = ys

--------------------------------------------------------------------------------

