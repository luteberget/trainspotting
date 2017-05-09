module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad (forM, forM_)
import Data.Maybe (fromJust)

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

data Node
  = Node
  { dMin :: Term
  , dMax :: Term
  }

leftNode, rightNode :: Node
leftNode  = Node (number distMin) (number 0)
rightNode = Node (number 0)       (number distMax)

-- create a new node
newNode :: Solver -> IO Node
newNode s =
  do d1 <- newTerm s distMin
     d2 <- newTerm s distMax
     return (Node d1 d2)

-- create a new node, connected with an empty segment to another node
excludeNode :: Solver -> Node -> Integer -> IO Node
excludeNode s p l =
  do lessThanEqual s dMin' (number distMin)
     lessThanEqual s dMax' (number distMax)
     return (Node dMin' dMax')
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

newSegment :: Solver -> Integer -> Node -> Node -> IO Segment
newSegment s l p q =
  do -- number of signals on this segment
     n' <- newTerm s (fromIntegral (nMax - nMin))
     let n = n' .+. number nMin
     n0 <- if nMin > 0 then
             return false
            else
             isEqual s n (number 0)
 
     -- distances of the left- and right-most signals to the endnodes of this segment
     dL <- newTerm s (l `min` distMax)
     dR <- if nMax <= 1 then
             -- an optimization for when we can have at most one node
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
                  showr 5 dL ++ " - " ++ showr 3 n ++ " signals -" ++ showr 5 dR
                else
                  "-"
              )
 where
  showr l x = let s = show x in replicate (l-length s) ' ' ++ s

--------------------------------------------------------------------------------

termSum :: [Term] -> Term
termSum = foldr (.+.) (number 0)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let (nodeNames,edges) = (read contents) :: ([String],[(String,String,Float)])

  withNewSolver $ \s -> do

    -- Create a node for each node in the graph
    nodes <- forM nodeNames $ \nm -> do
      pt <- newNode s
      return (nm,pt)
    
    let findNode p = fromJust $ lookup p nodes
    segments <- forM edges $ \(a,b,l) -> do
       seg <- newSegment s (round l) (findNode a) (findNode b)
       return (a,b,seg)

    -- set number of signals
    greaterThanEqual s (termSum (map (\(a,b,seg) -> numb seg) segments)) (number 120)
    
    b <- solve s []
    if b then
      do putStrLn "+++ SOLUTION:"
         let colWidth = (maximum $ map length nodeNames) + 1
         let pad l x = x ++ (replicate (l - (length x)) ' ')
         let showName a b = (pad colWidth a) ++ " -- " ++ (pad colWidth b)

         forM_ segments $ \(a,b,seg) -> printSegment s (showName a b) seg
     else
      do putStrLn "*** NO SOLUTION"

-----------------------------------------------------------------------------

