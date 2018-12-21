module Main where

import SAT
import SAT.Unary
import SAT.Term
import SAT.Order
import SAT.Equal
import SAT.Val
import SAT.Optimize
import Control.Monad (forM, forM_)
import Data.Tuple (swap)
import Data.Set (Set)
import qualified Data.Set as Set

import Input
import EdgeOrder

--
-- SOLVER REPR
--

data EdgeShape = EUp | EStraight | EDown
  deriving (Show, Ord, Eq)
data PropagationDirection = PropForward | PropBackward
  deriving (Show, Ord, Eq)
type Solution = ()
type Section = [((EdgeRef,EdgeRef), Unary)]

beginSplit :: Solver -> [EdgeRef] -> [EdgeRef] -> IO (Section, Section)
beginSplit s before after = join [] [] (succPairs before) (succPairs after)
    -- for example before: [1,2,3]
    -- for example after:  [1,2,9,3] (begin)
    -- for example before: [1,2]
    -- for example after:  [1,2,9] (begin)
    -- for example before: []
    -- for example after:  [9] (begin)
    -- for example before: [1]
    -- for example after:  [9,1] (begin)
    -- for example before: [1,2,3]
    -- for example after:  [9,1,2,3] (begin)
  where
    join l r [] [] = return (l,r) 

    join l r (b:bs) (a:as) | b == a = do
      -- Share it
      x <- fmap SAT.Unary.succ (newUnary s 1)
      join (l ++ [(b,x)]) (r ++ [(a,x)]) bs as

    join l r ((b1,b2):bs) ((a1,a2):(a3,a4):as) | b1 == a1 && a4 == b2 = do
      -- Split it
      bx  <- return (SAT.Unary.number 2)
      ax1 <- fmap SAT.Unary.succ (newUnary s 1)
      ax2 <- fmap SAT.Unary.succ (newUnary s 1)
      join (l ++ [((b1,b2),bx)]) (r ++ [((a1,a2),ax1),((a3,a4),ax2)]) bs as

    join l r b (a:as) = do
      -- Exists only on one side
      x <- fmap SAT.Unary.succ (newUnary s 1)
      join l (r ++ [(a,x)]) b as

    join _ _ _ _ = error "Inconsistent beginSplit"


switchSplit :: Solver -> [EdgeRef] -> [EdgeRef] -> IO (Section,Section)
switchSplit s trunkSide branchSide = join [] [] (succPairs trunkSide) (succPairs branchSide)
    -- for example before: [1,2,3]                                           [(1,2),(2,3)]
    -- for example after:  [1,4,5,3] (switch, trunk=2 lower=4 upper=5)       [(1,4),(4,5),(5,3)]
    -- for example before: [1,0,2,3]                                         [(1,0),(0,2),(2,3)]
    -- for example after:  [1,0,4,5,3] (switch, trunk=2 lower=4 upper=5)     [(1,0),(0,4),(4,5),(5,3)]
    -- for example before: [1]                                               []
    -- for example after:  [4,5] (switch, trunk=2 lower=4 upper=5)           [(4,5)]
    -- for example before: [0,1]                                             [(0,1)]
    -- for example after:  [0,4,5] (switch, trunk=2 lower=4 upper=5)         [(0,4),(4,5)]
    -- for example before: [1,0]
    -- for example after:  [4,5,0] (switch, trunk=2 lower=4 upper=5)
  where
    join l r [] [] = return (l,r) 
    join l r [] (d:[]) = return (l, r ++ [(d,SAT.Unary.number 0)]) -- Just the switch remains

    join l r (t:ts) (b:bs) | t == b = do 
      -- Share it
      x <- fmap SAT.Unary.succ (newUnary s 1)
      join (l ++ [(t,x)]) (r ++ [(b,x)]) ts bs

    join l r ((t1,t2):(t3,t4):ts) ((b1,b2):(b3,b4):(b5,b6):bs) | t1 == b1 && t4 == b6 = do
      -- Split it, with shared over and under
      dyUnder <- fmap SAT.Unary.succ (newUnary s 1)
      dyOver <- fmap SAT.Unary.succ (newUnary s 1)
      join (l ++ [((t1,t2),dyUnder), ((t3,t4), dyOver)]) 
           (r ++ [((b1,b2),dyUnder), ((b3,b4), SAT.Unary.number 0), ((b5,b6), dyOver)])  ts bs

    join l r ((t1,t2):ts) ((b1,b2):(b3,b4):bs) | t1 == b1 = do 
      -- Split it, with shared under
      dyUnder <- fmap SAT.Unary.succ (newUnary s 1)
      join (l ++ [((t1,t2),dyUnder)])
           (r ++ [((b1,b2),dyUnder), ((b3,b4), SAT.Unary.number 0)])  ts bs

    join l r ((t1,t2):ts) ((b1,b2):(b3,b4):bs) | t2 == b4 = do 
      -- Split it, with shared over
      dyOver <- fmap SAT.Unary.succ (newUnary s 1)
      join (l ++ [((t1,t2), dyOver)]) 
           (r ++ [((b1,b2), SAT.Unary.number 0), ((b3,b4), dyOver)])  ts bs

    join _ _ _ _ = error "Switch edges inconsistent"


lessThanSaturatingOr :: Solver -> [Lit] -> Unary -> Unary -> IO ()
lessThanSaturatingOr s pre a b = do
  addClause s (pre ++ [b .>= 1])
  addClause s (pre ++ [neg (a .>= 1), b .>= 2])

findEdgeIdx :: [Edge] -> Int -> Port -> Int
findEdgeIdx edges ni port = head [ ei | (ei,((n1,p1),(n2,p2))) <- zip [0..] edges
                                      , (n1 == ni && p1 == port) || (n2 == ni && p2 == port) ]

layout :: [Node] -> [Edge] -> IO Solution
layout nodes edges = withNewSolver $ \s -> do 

  let edgeOrderRelation = Set.fromList (transitiveClosure (ltRel nodes edges))
  putStrLn $ "Edge order " ++ (show edgeOrderRelation)
  let sortEdges = relationSort edgeOrderRelation
  let edgesBetween n1 n2 = [ ei | (ei, ((x,_),(y,_))) <- zip [0..] edges
                                , x <= n1 && n2 <= y ]

  nodeSecs <- forM (zip [0..] nodes) $ \(ni,node) -> do
    let beforeEdges = sortEdges (edgesBetween (ni-1) ni)
    let afterEdges =  sortEdges (edgesBetween ni (ni+1))

    case node of
      BeginNode ->                    beginSplit  s beforeEdges afterEdges
      EndNode   ->         fmap swap (beginSplit  s afterEdges  beforeEdges)
      SwitchNode _ Up ->              switchSplit s beforeEdges afterEdges
      SwitchNode _ Down -> fmap swap (switchSplit s afterEdges  beforeEdges)

  columns <- forM (succPairs (zip [0..] nodes)) $ \((n1,_),(n2,_)) -> do
    let colEdges = sortEdges (edgesBetween n1 n2)
    let mkCol = do edges <- sequence [ do shape <- newVal s [EUp, EStraight, EDown]
                                          prop  <- newVal s [PropForward, PropBackward]
                                          return (ei,shape,prop)
                                     | ei <- colEdges ]
                   active <- newLit s
                   return (active, edges)
                              
    leftCol   <- mkCol
    middleSection <- sequence [ do x <- fmap SAT.Unary.succ (newUnary s 1); return ((e1,e2),x)
                              | (e1,e2) <- succPairs colEdges ]
    rightCol  <- mkCol
    return (leftCol, middleSection, rightCol)

  let (t1,t2,t3) = ((\(x,_,_) -> x), (\(_,x,_) -> x), (\(_,_,x) -> x))
  let sectionPairs = concat [ [ (snd (nodeSecs !! dni),   t1 (columns !! dni),  t2 (columns !! dni)),
                                (t2 (columns !! dni),     t3 (columns !! dni),  fst (nodeSecs !! (dni+1))) ]
                            | (dni,_) <- zip [0..] (succPairs nodes) ]
  let columnPairs = succPairs (concat [ [l,r] | (l,_,r) <- columns ])

  -- Representation consistency constraints:
  --  1. Delta Ys propagate across unused columns
  forM_ sectionPairs $ \(sec1,(active,edges),sec2) -> do
    sequence_ [ equalOr s [active] dy1 dy2 | (de1,dy1) <- sec1, (de2,dy2) <- sec2, de1 == de2 ]

  --  2. Edge shapes propagate across inactive columns according to prop direction
  forM_ columnPairs $ \((active1,edges1),(active2,edges2)) -> do
    --
    -- Propagate forward: if left column is inactive, and edge propagates forwards, right column inherits
    --
    sequence_ [ do equalOr s   [active1, neg (prop1 .= PropForward)] sh1 sh2
                   addClause s [active1, neg (prop1 .= PropForward), prop2 .= PropForward]
              | (ei1,sh1,prop1) <- edges1, (ei2,sh2,prop2) <- edges2, ei1 == ei2 ]
    --
    -- Propagate backward: if right column is inactive, and edge propagates backwards, left column inherits
    --
    sequence_ [ do equalOr s   [active2, neg (prop2 .= PropBackward)] sh1 sh2
                   addClause s [active2, neg (prop2 .= PropBackward), prop1 .= PropBackward]
              | (ei1,sh1,prop1) <- edges1, (ei2,sh2,prop2) <- edges2, ei1 == ei2 ]

  --  3. Edge shapes consistent with delta Ys

  sequence_ [ sequence_ [ do -- Going straight
                             sequence_ [ equalOr s [neg active, neg (sh1 .= sh), neg (sh2 .= sh)] dy1 dy2
                                       | sh <- [EUp, EStraight, EDown] ]

                             -- Diverging going forward
                             sequence_ [ lessThanSaturatingOr s [neg active, neg (sh1 .= sa), neg (sh2 .= sb)] dy1 dy2 
                                       | (sa,sb) <- [(EStraight, EUp),(EDown, EUp),(EDown,EStraight)]]
                             
                             -- Converging going forward
                             sequence_ [ lessThanSaturatingOr s [neg active, neg (sh1 .= sa), neg (sh2 .= sb)] dy2 dy1 
                                       | (sa,sb) <- [(EStraight,EDown),(EUp,EStraight),(EUp,EDown)]]

                        | ((ea1,ea2),dy1) <- sec1
                        , ((eb1,eb2),dy2) <- sec2
                        , ((ec1,sh1,_prop1),(ec2,sh2,_prop2)) <- succPairs edges
                        , ea1 == eb1 && eb1 == ec1 && ea2 == eb2 && eb2 == ec2 ]
            | (sec1, (active, edges), sec2) <- sectionPairs ]

  --
  -- Problem constraints:
  --  4. Switch shapes

  forM_ (zip [0..] nodes) $ \(ni,node) -> do
    let prevCol = t3 (columns !! (ni-1)) -- first node cannot be a switch, 
                                         -- so indexing by -1 here will not actually execute.
    let nextCol = t1 (columns !! (ni))   -- same for the last node, will not index by (n_nodes-1).
    let trunkEdge = findEdgeIdx edges ni PTrunk
    let leftEdge =  findEdgeIdx edges ni PLeft
    let rightEdge = findEdgeIdx edges ni PRight
    let isActive col@(active,edges) = active
    let find col ei = head [ (s,p) | (i,s,p) <- snd col, i == ei ]

    -- Set propagation direction
    case node of
      SwitchNode _ Up ->   do addClause s [isActive prevCol, snd (find prevCol trunkEdge) .= PropBackward ]
                              addClause s [isActive nextCol, snd (find nextCol leftEdge)  .= PropForward ]
                              addClause s [isActive nextCol, snd (find nextCol rightEdge) .= PropForward ]
      SwitchNode _ Down -> do addClause s [isActive nextCol, snd (find nextCol trunkEdge) .= PropForward ]
                              addClause s [isActive prevCol, snd (find prevCol leftEdge)  .= PropBackward ]
                              addClause s [isActive prevCol, snd (find prevCol rightEdge) .= PropBackward ]
      _ -> return ()

    -- Set the shape
    case node of
      SwitchNode SLeft Up -> do
        let (trunkShape,_) = find prevCol trunkEdge 
        let (leftShape,_)   = find nextCol leftEdge 
        let (rightShape,_) = find nextCol rightEdge 

        addClause s [trunkShape .= EStraight, trunkShape .= EDown] 
        addClause s [neg (trunkShape .= EStraight), leftShape .= EUp]
        addClause s [neg (trunkShape .= EStraight), rightShape .= EStraight]
        addClause s [neg (trunkShape .= EDown), leftShape .= EStraight]
        addClause s [neg (trunkShape .= EDown), rightShape .= EDown]

      SwitchNode SRight Up -> do
        let (trunkShape,_) = find prevCol trunkEdge 
        let (leftShape,_)   = find nextCol leftEdge 
        let (rightShape,_) = find nextCol rightEdge 

        addClause s [trunkShape .= EStraight, trunkShape .= EUp] 
        addClause s [neg (trunkShape .= EStraight), leftShape .= EStraight]
        addClause s [neg (trunkShape .= EStraight), rightShape .= EDown]
        addClause s [neg (trunkShape .= EUp), leftShape .= EUp]
        addClause s [neg (trunkShape .= EUp), rightShape .= EStraight]

      SwitchNode SLeft Down -> do
        let (trunkShape,_) = find nextCol trunkEdge 
        let (leftShape,_)   = find prevCol leftEdge 
        let (rightShape,_) = find prevCol rightEdge 

        addClause s [trunkShape .= EStraight, trunkShape .= EDown] 
        addClause s [neg (trunkShape .= EStraight), leftShape .= EDown]
        addClause s [neg (trunkShape .= EStraight), rightShape .= EStraight]
        addClause s [neg (trunkShape .= EDown), leftShape .= EStraight]
        addClause s [neg (trunkShape .= EDown), rightShape .= EDown]

      SwitchNode SRight Down -> do
        let (trunkShape,_) = find nextCol trunkEdge 
        let (leftShape,_)   = find prevCol leftEdge 
        let (rightShape,_) = find prevCol rightEdge 

        addClause s [trunkShape .= EStraight, trunkShape .= EUp] 
        addClause s [neg (trunkShape .= EStraight), leftShape .= EStraight]
        addClause s [neg (trunkShape .= EStraight), rightShape .= EDown]
        addClause s [neg (trunkShape .= EUp), leftShape .= EUp]
        addClause s [neg (trunkShape .= EUp), rightShape .= EStraight]

      _ -> return ()

  putStrLn =<< stats s

  b <- solve s []
  if b then do
    putStrLn "SAT"
  else do
    putStrLn "UNSAT"


  return ()


stats :: Solver -> IO String
stats s = do
  vars <- numVars s
  clauses <- numClauses s
  return ("SAT instance with " ++ (show vars) ++ " vars and " ++ (show clauses) ++ " clauses.")

