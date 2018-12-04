module Main where

import SAT as S
import SAT.Val as V
import SAT.Term as T
import SAT.Equal
import SAT.Order
import SAT.Bool
import SAT.Unary as U
import SAT.Optimize
import Control.Monad( when )
import Data.List( nub )

--------------------------------------------------------------------------------

type Number = Unary

newNumber :: Solver -> Int -> IO Number
newNumber s n = newUnary s n

lessThanMaybeEqualOr :: Solver -> [Lit] -> Lit -> Number -> Number -> IO ()
lessThanMaybeEqualOr s pre str a b =
  do lessThanEqualOr s pre a b
     lessThanOr s (neg str:pre) a b

--------------------------------------------------------------------------------

data Point
  = Point
  { col   :: Int
  , y     :: Number  -- what is my y-coordinate?
  , ahead :: Lit     -- am I ahead of the others?
  , from  :: Val Dir -- how did I get here?
  }

data Dir = FromAbove | Straight | FromBelow deriving ( Eq, Ord, Show )

newPoint :: Solver -> Int -> Int -> IO Point
newPoint s height c =
  do y <- newNumber s height
     newPointAt s y c

newPointAt :: Solver -> Number -> Int -> IO Point
newPointAt s y c =
  do ah <- newLit s
     fr <- newVal s [FromAbove, Straight, FromBelow]
     return (Point c y ah fr)

type Front = [Point]

stayOr :: Solver -> [Lit] -> Front -> Front -> IO ()
stayOr s pre fr0 fr1 =
  do sequence_
       [ do equalOr s pre (y p0)     (y p1)
            equalOr s pre (ahead p0) (ahead p1)
            equalOr s pre (from p0)  (from p1)
       | (p0,p1) <- fr0 `zip` fr1
       ]

order :: Solver -> Front -> IO ()
order s fr =
  sequence_
  [ lessThanEqual s (y p) (y q)
  | (p,q) <- fr `zip` tail fr
  ]

stepOr :: Solver -> [Lit] -> Front -> Front -> IO ()
stepOr s pre fr0 fr1 =
  do -- already ahead?
     sequence_
       [ do equalOr s (neg (ahead p0):pre) (y p0)    (y p1)
            equalOr s (neg (ahead p0):pre) (from p0) (from p1)
            addClause s (neg (ahead p1):pre)
       | (p0,p1) <- fr0 `zip` fr1
       ]
     -- not already ahead
     sequence_
       [ do -- no other points at the same y coordinate
            sequence_
              [ do notEqualOr s (ahead p0:ahead q0:pre) (y p0) (y q0)
              | q0 <- fr0
              , col q0 > col p0
              ]
            -- no \/ or /\
            addClause s (ahead p0:neg (from p0 .= FromAbove)
                                 :neg (from p1 .= FromBelow):pre)
            addClause s (ahead p0:neg (from p0 .= FromBelow)
                                 :neg (from p1 .= FromAbove):pre)
            -- from-direction corresponds to correct y-coordinate
            equalOr s (ahead p0:neg (from p1 .= FromBelow):pre)
                      (U.succ (y p0)) (y p1)
            equalOr s (ahead p0:neg (from p1 .= Straight):pre)
                      (y p0) (y p1)
            equalOr s (ahead p0:neg (from p1 .= FromAbove):pre)
                      (y p0) (U.succ (y p1))
       | (p0,p1) <- fr0 `zip` fr1
       ]

thing :: Solver -> Int -> Front -> Thing -> IO Front
thing s h fr0 th =
  case th of
    New r i ->
      do p <- newPoint s h i
         addClause s [ahead p]
         addClause s [from p .= Straight]
         sequence_
           [ notEqualOr s [ahead q] (y p) (y q)
           | q <- fr0
           ]
         return $
           if r == 0
             then p:fr0
             else [ g | f <- fr0, g <- if col f == r then [f,p] else [f] ]

    End i ->
      do addClause s [neg (ahead p)]
         addClause s [from p .= Straight]
         sequence_
           [ notEqualOr s [ahead q] (y p) (y q)
           | q <- fr0
           , col q /= i
           ]
         return [ p | p <- fr0, col p /= i ]
     where
      p = head [ p | p <- fr0, col p == i ]

    SwitchL i j k ->
      do -- precondition
         addClause s [neg (ahead p)]
         addClause s [neg (from p .= FromBelow)]
         sequence_
           [ notEqualOr s [ahead q] (y p) (y q)
           | q <- fr0
           , col q /= i
           ]
         -- post-condition
         p2 <- newPoint   s h k
         p1 <- newPointAt s (U.succ (y p2)) j
         addClause s [y p1 .<= h]
         addClause s [ahead p1]
         addClause s [ahead p2]
         -- slanted/or not
         equalOr   s [neg (from p .= Straight)]  (y p) (y p2)
         addClause s [neg (from p .= Straight), from p1 .= FromBelow]
         addClause s [neg (from p .= Straight), from p2 .= Straight]
         equalOr   s [neg (from p .= FromAbove)] (y p) (y p1)
         addClause s [neg (from p .= FromAbove), from p1 .= Straight]
         addClause s [neg (from p .= FromAbove), from p2 .= FromAbove]

         return [ g | f <- fr0, g <- if col f == i then [p2,p1] else [f] ]
     where
      p = head [ p | p <- fr0, col p == i ]

    SwitchR i j k ->
      do -- precondition
         addClause s [neg (ahead p)]
         addClause s [neg (from p .= FromAbove)]
         sequence_
           [ notEqualOr s [ahead q] (y p) (y q)
           | q <- fr0
           , col q /= i
           ]
         -- post-condition
         p2 <- newPoint   s h k
         p1 <- newPointAt s (U.succ (y p2)) j
         addClause s [y p1 .<= h]
         addClause s [ahead p1]
         addClause s [ahead p2]
         -- slanted/or not
         equalOr   s [neg (from p .= Straight)]  (y p) (y p1)
         addClause s [neg (from p .= Straight), from p1 .= Straight]
         addClause s [neg (from p .= Straight), from p2 .= FromAbove]
         equalOr   s [neg (from p .= FromBelow)] (y p) (y p2)
         addClause s [neg (from p .= FromBelow), from p1 .= FromBelow]
         addClause s [neg (from p .= FromBelow), from p2 .= Straight]

         return [ g | f <- fr0, g <- if col f == i then [p2,p1] else [f] ]
     where
      p = head [ p | p <- fr0, col p == i ]

    MergeL i j k ->
      do -- precondition
         addClause s [neg (ahead p1)]
         addClause s [neg (ahead p2)]
         equal s (y p1) (y p2)
         addClause s [neg (from p1 .= FromBelow)]
         addClause s [neg (from p2 .= FromAbove)]
         sequence_
           [ notEqualOr s [ahead q] (y p1) (y q)
           | q <- fr0
           , col q /= i
           , col q /= j
           ]
         -- postcondition
         p <- newPoint s h k
         addClause s [ahead p]
         addClause s [neg (from p .= FromBelow)]
         -- slanted/or not
         equalOr s [neg (from p .= Straight)]  (y p)          (y p1)
         equalOr s [neg (from p .= FromAbove)] (U.succ (y p)) (y p1)
         addClause s [neg (from p1 .= FromAbove), from p .= FromAbove]
         addClause s [neg (from p1 .= FromAbove), from p2 .= Straight]
         addClause s [neg (from p1 .= Straight), from p .= Straight]
         addClause s [neg (from p1 .= Straight), from p2 .= FromBelow]
         
         return [ g | f <- fr0, g <- if col f == i then [p]
                                     else if col f == j then []
                                     else [f] ]
     where
      p1 = head [ p | p <- fr0, col p == i ]
      p2 = head [ p | p <- fr0, col p == j ]

    MergeR i j k ->
      do -- precondition
         addClause s [neg (ahead p1)]
         addClause s [neg (ahead p2)]
         equal s (y p1) (y p2)
         addClause s [neg (from p1 .= FromBelow)]
         addClause s [neg (from p2 .= FromAbove)]
         sequence_
           [ notEqualOr s [ahead q] (y p1) (y q)
           | q <- fr0
           , col q /= i
           , col q /= j
           ]
         -- postcondition
         p <- newPoint s h k
         addClause s [ahead p]
         addClause s [neg (from p .= FromAbove)]
         -- slanted/or not
         equalOr s [neg (from p .= Straight)]  (y p)          (y p1)
         equalOr s [neg (from p .= FromBelow)] (y p) (U.succ (y p1))
         addClause s [neg (from p1 .= FromAbove), from p .= Straight]
         addClause s [neg (from p1 .= FromAbove), from p2 .= Straight]
         addClause s [neg (from p1 .= Straight), from p .= FromBelow]
         addClause s [neg (from p1 .= Straight), from p2 .= FromBelow]
         
         return [ g | f <- fr0, g <- if col f == i then [p]
                                     else if col f == j then []
                                     else [f] ]
     where
      p1 = head [ p | p <- fr0, col p == i ]
      p2 = head [ p | p <- fr0, col p == j ]

    Cross i j k l ->
      do -- precondition
         addClause s [neg (ahead p1)]
         addClause s [neg (ahead p2)]
         equal s (y p1) (y p2)
         addClause s [neg (from p1 .= Straight), neg (from p2 .= Straight)]
         addClause s [neg (from p1 .= FromBelow)]
         addClause s [neg (from p2 .= FromAbove)]
         sequence_
           [ notEqualOr s [ahead q] (y p1) (y q)
           | q <- fr0
           , col q /= i
           , col q /= j
           ]
         -- postcondition
         q1 <- newPoint s h k
         q2 <- newPoint s h l
         addClause s [ahead q1]
         addClause s [ahead q2]
         addClause s [neg (from q1 .= Straight), neg (from q2 .= Straight)]
         -- slanted/or not
         addClause s [neg (from p1 .= FromAbove), from q2 .= FromAbove]
         addClause s [neg (from p1 .= Straight),  from q2 .= Straight]
         addClause s [neg (from p2 .= FromBelow), from q1 .= FromBelow]
         addClause s [neg (from p2 .= Straight),  from q1 .= Straight]

         equalOr s [neg (from q1 .= Straight)]  (y q1) (y p1)
         equalOr s [neg (from q2 .= Straight)]  (y q2) (y p1)
         equalOr s [neg (from q1 .= FromBelow)] (y q1) (U.succ (y p1))
         equalOr s [neg (from q2 .= FromAbove)] (U.succ (y q2)) (y p1)
         equalOr s [neg (from q1 .= Straight)] (y q1) (y p1)
         
         return [ g | f <- fr0, g <- if col f == i then [q1]
                                     else if col f == j then [q2]
                                     else [f] ]
     where
      p1 = head [ p | p <- fr0, col p == i ]
      p2 = head [ p | p <- fr0, col p == j ]

--------------------------------------------------------------------------------

things :: Solver -> Int -> [Thing] -> IO ([(Lit,Front)],[Unary])
things s h ths = go [] ths
 where
  k = 3
  
  go fr [] =
    do return ([],[])
  
  go fr (th:ths) =
    do fr1 <- thing s h fr th
       frs <- sequence
              [ do fr <- sequence [ newPoint s h (col p) | p <- fr1 ]
                   order s fr
                   return fr
              | i <- [1..k]
              ]
       stp <- newUnary s k
       stayOr s [neg (stp .<= 0)] fr1 (head frs)
       sequence_
         [ stepOr s [neg (stp .>= i), neg (stp .<= i)] fr1 fr
         | (i,fr) <- [1..] `zip` frs
         ]
       sequence_
         [ stepOr s [neg (stp .>= i)] fr2 fr1
         | (i,(fr1,fr2)) <- [2..] `zip` (frs `zip` tail frs)
         ]
       (frs',us) <- go (head frs) ths
       return ( reverse [ (stp .>= i, fr) | (i,fr) <- [1..] `zip` frs ] ++ frs'
              , stp:us
              )
  
assert :: Monad m => Bool -> m ()
assert False = error "assertion failed"
assert True  = return ()

--------------------------------------------------------------------------------

main :: IO ()
main = withNewSolver $ \s ->
  do putStrLn "+++ generating problem..."
     (frs,us) <- things s h ths
     putStrLn "+++ solving..."
     b <- solve s []
     if b then
       do putStrLn "+++ SOLUTION"
          displaySolution s h frs
          minimizeAndCommitWidth s h frs us
          minimizeAndCommitDiags s h frs
          minimizeAndCommitHeight s h frs
      else
       do putStrLn "*** NO SOLUTION"
 where
  (h,ths) = exampleCross

{-
minimizeAndCommitDiags :: Solver -> Int -> [(Lit,Front)] -> IO ()
minimizeAndCommitDiags s h frs =
  do putStrLn "+++ minimizing diags..."
     as <- sequence
           [ do a <- newLit s
                addClause s [a, neg adv, from p .= Straight]
                return a
           | (adv,fr) <- frs
           , p <- fr
           ]
     ds <- sequence
           [ do a <- S.modelValue s adv
                d <- V.modelValue s (from p)
                return (a && d /= Straight)
           | (adv,fr) <- frs
           , p <- fr
           ]
     let n = length (filter id ds)
     cnt <- U.countUpTo s n as
     solveOptimize s [] cnt $ \_ ->
       do --displaySolution s h frs
          return True
     displaySolution s h frs
     n <- U.modelValue s cnt
     addClause s [cnt .<= n]
-}

minimizeAndCommitDiags :: Solver -> Int -> [(Lit,Front)] -> IO ()
minimizeAndCommitDiags s h frs =
  do putStrLn "+++ minimizing diags..."
     as <- sequence
           [ do a <- newLit s
                addClause s [a, neg adv, from p .= Straight]
                return a
           | (adv,fr) <- frs
           , p <- fr
           ]
     ds <- sequence
           [ do a <- S.modelValue s adv
                d <- V.modelValue s (from p)
                return (a && d /= Straight)
           | (adv,fr) <- frs
           , p <- fr
           ]
     let n = fromIntegral (length (filter id ds))
         q = fromList [(1,a)|a<-as]
     cnt <- newTerm s n
     lessThanEqual s q cnt
     a <- newLit s
     let loop n =
           do lessThanOr s [neg a] cnt (T.number (fromIntegral n))
              b <- solve s [a]
              if b then
                do displaySolution s h frs
                   n <- T.modelValue s q
                   loop n
               else
                do return ()
      in loop n
     addClause s [neg a]
     n <- T.modelValue s cnt
     lessThanEqual s cnt (T.number n)

minimizeAndCommitWidth :: Solver -> Int -> [(Lit,Front)] -> [Unary] -> IO ()
minimizeAndCommitWidth s h frs us =
  do putStrLn "+++ minimizing width..."
     cnt <- U.addList s us
     solveOptimize s [] cnt $ \_ ->
       do --displaySolution s h frs
          return True
     displaySolution s h frs
     n <- U.modelValue s cnt
     addClause s [cnt .<= n]

minimizeAndCommitHeight :: Solver -> Int -> [(Lit,Front)] -> IO ()
minimizeAndCommitHeight s h frs =
  do putStrLn "+++ minimizing height..."
     a <- newLit s
     let loop h =
           do sequence_
                [ addClause s [neg a, y p .< h]
                | (_,fr) <- frs
                , p <- fr
                ]
              b <- solve s [a]
              if b then
                do displaySolution s (h-1) frs
                   loop (h-1)
               else
                do return h
     h' <- loop h
     addClause s [neg a]
     sequence_
       [ addClause s [y p .<= h']
       | (_,fr) <- frs
       , p <- fr
       ]
     
displaySolution :: Solver -> Int -> [(Lit,Front)] -> IO ()
displaySolution s h frs =
  putStrLn "--- solution" >>
{-
  (do sequence_
          [ do b <- S.modelValue s adv
               putStr $ (if b then "XX " else "   ")
          | (adv,ps) <- frs
          ]
      putStrLn "") >> 
-}
  sequence_
  [ do sequence_
         [ do b <- S.modelValue s adv
              when b $
                do css <- sequence
                          [ do j <- U.modelValue s (y p)
                               d <- V.modelValue s (from p)
                               return $ [ '\\' | j == i, d == FromAbove ]
                                     ++ [ '_'  | j == i, d == Straight ]
                                     ++ [ '/'  | j == i+1, d == FromBelow ]
                          | p <- ps
                          ]
                   let cs = nub (concat css)
                   putStr $ (if '\\' `elem` cs then "\\" else " ") ++
                            (if '/'  `elem` cs then "/"  else " ")
         | (adv,ps) <- frs
         ]
       putStrLn ""
       sequence_
         [ do b <- S.modelValue s adv
              when b $
                do css <- sequence
                          [ do j <- U.modelValue s (y p)
                               d <- V.modelValue s (from p)
                               return $ [ '\\' | j == i, d == FromAbove ]
                                     ++ [ '_'  | j == i, d == Straight ]
                                     ++ [ '/'  | j == i+1, d == FromBelow ]
                          | p <- ps
                          ]
                   let cs = nub (concat css)
                   putStr $ (if '/'  `elem` cs then "/" else
                             if '_'  `elem` cs then "_" else " ") ++
                            (if '\\' `elem` cs then "\\" else
                             if '_'  `elem` cs then "_" else ".")
         | (adv,ps) <- frs
         ]
       putStrLn ""
  | i <- [h,h-1..0]
  ]

displaySolutionList :: Solver -> Int -> [(Lit,Front)] -> IO ()
displaySolutionList s h frs =
  putStrLn "--- solution list" >>
  sequence_
  [ do b <- S.modelValue s adv
       putStrLn (if b then "-- ADV" else "-- (no adv)")
       sequence_
         [ do y <- U.modelValue s (y p)
              a <- S.modelValue s (ahead p)
              d <- V.modelValue s (from p)
              putStrLn ( (if a then "     " else "")
                      ++ show (col p)
                      ++ ": "
                      ++ (if d == Straight then "-"
                          else if d == FromAbove then "\\"
                          else "/")
                      ++ " @"
                      ++ show y
                       ) 
         | p <- fr
         ]
  | (adv,fr) <- frs
  ]

--------------------------------------------------------------------------------

data Thing
  = SwitchL Int Int Int
  | SwitchR Int Int Int
  | MergeL Int Int Int
  | MergeR Int Int Int
  | Cross Int Int Int Int
  | New Int Int
  | End Int
 deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

type Example = (Int,[Thing]) -- (height guess, things)

example0 :: Example
example0 =
  ( 4
  , [ New 0 1
    , SwitchL 1 1 2
    , MergeR 1 2 1 
    , End 1
    ]
  )

example1 :: Example
example1 =
  ( 4
  , [ New 0 1
    , SwitchL 1 2 3
    , SwitchR 2 1 2
    , SwitchR 3 3 4
    , MergeL 1 2 2
    , MergeR 2 3 3
    , MergeL 3 4 3
    , End 3
    ]
  )

exampleCross :: Example
exampleCross =
  ( 7
  , [ New 0 1
    , New 1 2
    , New 2 3
    , New 3 4
    , New 3 12
    , End 12
    , Cross 2 1 5 6
    , Cross 4 3 7 8
    , MergeL 8 5 9
    , MergeR 7 9 10
    , MergeL 10 6 11
    , End 11
    ]
  )

example110 :: Example
example110 =
  ( 12
  , [ New 0 1
    , New 1 2
    , New 2 3
    , New 3 4
    , New 4 5
    , New 5 6
    , New 6 7
    , New 7 8
    , New 8 9
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
  ( 8
  , [ New 0 1
    , New 0 2
    , SwitchL 2 3 4
    , SwitchR 1 1 2
    , SwitchR 2 2 5
    , MergeL 5 3 3
    , New 2 7
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

exampleSteenwijk :: Example
exampleSteenwijk =
  ( 8
  , [ New 0 20
    , SwitchL 20 1 2
    , SwitchR 1 3 4
    , MergeR 4 2 5
    , SwitchR 5 6 7
    , SwitchL 3 8 9
    , New 8 10
    , SwitchL 8 11 12
    , MergeL 10 11 13
    , End 13
    , MergeL 6 7 14
    , MergeR 12 9 15
    , SwitchL 14 16 17
    , MergeL 15 16 18
    , MergeR 18 17 19
    , End 19 
    ]
  )

--------------------------------------------------------------------------------

