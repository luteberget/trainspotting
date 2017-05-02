module Main where

import SAT
import SAT.Equal
import SAT.Order
import SAT.Term
import SAT.Optimize
import SAT.Value

--------------------------------------------------------------------------------

distMin, distMax :: Integer
distMin = 200
distMax = 400

--------------------------------------------------------------------------------

data Point
  = Point
  { dMin :: Term
  , dMax :: Term
  }

leftPoint, rightPoint :: Point
leftPoint  = Point (number distMin) (number 0)
rightPoint = Point (number 0)       (number distMax)

-- create a new point
newPoint :: Solver -> IO Point
newPoint s =
  do d1 <- newTerm s distMin
     d2 <- newTerm s distMax
     return (Point d1 d2)

-- create a new point, connected with an empty segment to another point
excludePoint :: Solver -> Point -> Integer -> IO Point
excludePoint s p l =
  do lessThanEqual s dMin' (number distMin)
     lessThanEqual s dMax' (number distMax)
     return (Point dMin' dMax')
 where
  dMin' = dMin p .+. number l
  dMax' = dMax p .+. number l

data Segment
  = Segment
  { len    :: Integer
  , numb   :: Term
  , dLeft  :: Term
  , dRight :: Term
  }

newSegment :: Solver -> Integer -> Point -> Point -> IO Segment
newSegment s l p q =
  do -- number of signals on this segment
     n' <- newTerm s (fromIntegral (nMax - nMin))
     let n = n' .+. number nMin
     n0 <- if nMin > 0 then
             return false
            else
             isEqual s n (number 0)
 
     -- distances of the left- and right-most signals to the endpoints of this segment
     dL <- newTerm s (l `min` distMax)
     dR <- if nMax <= 1 then
             -- an optimization for when we can have at most one point
             return (number l .-. dL)
            else
             newTerm s (l `min` distMax)

     -- internally, number of signals must match the distances
     lessThanEqualOr    s [n0] (dL .+. (distMin .* (n .-. number 1)) .+. dR) (number l)
     greaterThanEqualOr s [n0] (dL .+. (distMax .* (n .-. number 1)) .+. dR) (number l)
     
     -- connect to p on left-hand side
     greaterThanEqualOr s [n0] (dMin p .+. dL) (number distMin)
     lessThanEqualOr    s [n0] (dMax p .+. dL) (number distMax)
     
     -- connect to q on right-hand side
     greaterThanEqualOr s [n0] dR (dMin q)
     lessThanEqualOr    s [n0] dR (dMax p)

     -- connect through, if no signals exist here
     greaterThanEqualOr s [neg n0] (dMin p .+. number l) (dMin q)
     lessThanEqualOr    s [neg n0] (dMax p .+. number l) (dMax q)
     
     return (Segment l n dL dR)
 where
  nMin = l `div` distMax
  nMax = (l `div` distMin)+1

printSegment :: Solver -> String -> Segment -> IO ()
printSegment s name seg =
  do n  <- getValue s (numb seg)
     dL <- getValue s (dLeft seg)
     dR <- getValue s (dRight seg)
     putStrLn ( name
             ++ ": "
             ++ if n > 0 then
                  showr dL ++ " - " ++ show n ++ " signals -" ++ showr dR
                else
                  "-"
              )
 where
  showr x = let s = show x in replicate (5-length s) ' ' ++ s

--------------------------------------------------------------------------------

main :: IO ()
main =
  withNewSolver $ \s ->
    do p1  <- newPoint s
       p2' <- newPoint s
       p3  <- newPoint s
       
       p1'  <- excludePoint s p1  50
       p2   <- excludePoint s p2' 100
       p1'' <- excludePoint s p1  100
       p3'  <- excludePoint s p3  50
       p3'' <- excludePoint s p3  150
       
       sa  <- newSegment s 400 leftPoint p1
       sb  <- newSegment s 250 p1'       p2'
       sc  <- newSegment s 200 p2        rightPoint
       sd  <- newSegment s 150 p1''      p3
       se  <- newSegment s 200 p3'       rightPoint
       sf  <- newSegment s 0   p3''      p2
       let segs = [sa,sb,sc,sd,se,sf]
       
       -- set number of signals
       greaterThanEqual s (foldr (.+.) (number 0) (map numb segs)) (number 8)
       
       b <- solve s []
       if b then
         do putStrLn "+++ SOLUTION:"
            sequence_ [ printSegment s [c] seg
                      | (seg,c) <- segs `zip` ['A'..]
                      ]
        else
         do putStrLn "*** NO SOLUTION"

--------------------------------------------------------------------------------

