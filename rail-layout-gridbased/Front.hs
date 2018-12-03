module Main where

import SAT as S
import SAT.Val as V
import SAT.Term
import SAT.Equal
import SAT.Order
import SAT.Bool
import SAT.Unary as U
import SAT.Optimize
import Control.Monad( when )
import Data.List( nub )
import Thing

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
              [ notEqualOr s (ahead p0:ahead q:pre) (y p0) (y q)
              | q <- fr0
              , col q /= col p0
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

advance :: Solver -> Front -> Front -> IO Lit
advance s fr fr' =
  do adv <- newLit s
     stayOr s [adv] fr fr'
     stepOr s [neg adv] fr fr'
     return adv

thing :: Solver -> Int -> Front -> Thing -> IO Front
thing s h fr0 th =
  case th of
    New i ->
      do p <- newPoint s h i
         addClause s [ahead p]
         addClause s [from p .= Straight]
         sequence_
           [ notEqualOr s [ahead q] (y p) (y q)
           | q <- fr0
           ]
         return (p:fr0)

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

         return (p1:p2:[ p | p <- fr0, col p /= i ])
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

         return (p1:p2:[ p | p <- fr0, col p /= i ])
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
         
         return (p:[ p | p <- fr0, col p /= i, col p /= j ])
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
         
         return (p:[ p | p <- fr0, col p /= i, col p /= j ])
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
              [ sequence [ newPoint s h (col p) | p <- fr1 ]
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
  do (frs,us) <- things s h (rename ths)
     b <- solve s []
     if b then
       do putStrLn "+++ SOLUTION"
          displaySolutionList s h frs
          displaySolution s h frs
          putStrLn "+++ MINIMIZING"
          cnt <- U.addList s us
          solveOptimize s [] cnt $ \_ ->
            do displaySolution s h frs
               return True
          displaySolution s h frs
      else
       do putStrLn "*** NO SOLUTION"
 where
  --((_,h),ths) = ((10,3),[New 0, New 1, End 1, End 0])
  --((_,h),ths) = ((10,1),[New 0, SwitchL 0 1 2, MergeR 1 2 3, End 3])
  ((_,h),ths) = exampleBjornar

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

