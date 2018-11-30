module Main where

import SAT
import SAT.Unary
import SAT.Term
import SAT.Order
import SAT.Equal
import SAT.Optimize
import Control.Monad (forM, forM_)
import Control.Applicative ((<|>))

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe
import Data.List (partition)


-- # Idea for optimizing with Unary
-- Summing a large number of Unarys takes up a lot of space.
-- We can instead do lexicographic optimizatino on each of them,
--   then sum them into a Unary with upper bound given by the 
--   result of the lexicographic optimization.
-- 1. Put in constraints
-- 2. Optimize each unary in turn, giving upper bound.
-- 3. Remove the constraints from (2)
-- 4. Sum the unarys up-to upper bound, optimize this number.

--
-- INPUT REPRESENTATION
--
data Side = SLeft | SRight
  deriving (Show)
data Dir = Up | Down
  deriving (Show)
data Port = PLeft | PRight | PTrunk | PBegin | PEnd
  deriving (Show, Eq)

type EdgeRef = Int
type NodeRef = Int

type PortRef = (NodeRef, Port)
type Edge = (PortRef, PortRef)
data Node = BeginNode | EndNode | SwitchNode Side Dir
  deriving (Show)

isSwitch :: Node -> Bool
isSwitch (SwitchNode _ _) = True
isSwitch _ = False

data SolverInput = SolverInput 
  { sNodes :: [Node]
  , sEdges :: [Edge]
  , sEdgeLT :: [(EdgeRef, EdgeRef)]
  }

example1 = SolverInput n e lt
  where n = [BeginNode, EndNode]
        e = [((0, PBegin),(1, PEnd))]
        lt = []

example2 = SolverInput n e lt
  where  n = [BeginNode, SwitchNode SLeft Up, EndNode, EndNode ]
         e = [((0, PBegin),(1,PTrunk)),
              ((1, PLeft),(2,PEnd)),
              ((1, PRight),(3,PEnd))]
         lt = [(2,1)]

example2b = SolverInput n e lt
  where  n = [BeginNode, SwitchNode SLeft Up, SwitchNode SRight Down, EndNode ]
         e = [((0, PBegin),(1,PTrunk)),
              ((1, PLeft),(2,PRight)),
              ((1, PRight),(2,PLeft)),
              ((2, PTrunk),(3,PEnd)) ]
         lt = [(2,1)]

example3 = SolverInput n e lt
  where  n = [BeginNode, BeginNode, SwitchNode SLeft Up, 
              SwitchNode SLeft Down,
              EndNode, EndNode ]
         e = [((0,PBegin),(2,PTrunk)), -- 0  edges:  1--4
              ((1,PBegin),(3,PRight)), -- 1           2
              ((2,PLeft),(3,PLeft)),   -- 2          0--3
              ((2,PRight),(4,PEnd)),   -- 3  nodes:   1   3 5
              ((3,PTrunk),(5,PEnd))]   -- 4           0 2   4
         lt = [(0, 2), (2, 1), (0, 1), (3,2),(3,4),(2,4)]

example4 = SolverInput n e lt
  where  n = [BeginNode, SwitchNode SLeft Up, SwitchNode SLeft Up, SwitchNode SRight Down, 
                SwitchNode SRight Down, EndNode]
         e = [((0,PBegin),(1,PTrunk)), -- 0
              ((1,PRight),(2,PTrunk)), -- 1
              ((2,PRight),(3,PLeft)), -- 2
              ((3,PTrunk),(4,PLeft)), -- 3
              ((4,PTrunk),(5,PEnd)), -- 4
              ((1,PLeft),(4,PRight)), -- 5
              ((2,PLeft),(3,PRight))] -- 6

---  
--
--        /---5----\
--       /  /-6-\   \
--    -0x-1x--2--x-3-x-4-
-- 15 26 65 35 (25)
--
             
         lt = [(1,5),(2,5),(3,5),(2,6),(5,6)]


example5 = SolverInput n e lt
  where  n = [BeginNode, SwitchNode SLeft Up, SwitchNode SRight Up, SwitchNode SLeft Down, 
                SwitchNode SRight Down, EndNode]
         e = [((0,PBegin),(1,PTrunk)), -- 0
              ((1,PLeft),(2,PTrunk)), -- 1
              ((1,PRight),(4,PLeft)), --2
              ((2,PLeft),(3,PRight)), --3
              ((2,PRight),(3,PLeft)), --4
              ((3,PTrunk),(4,PRight)), --5
              ((4,PTrunk),(5,PEnd))] --6
             
------
--      /-3--
--     ---4--\
--    1      5
-- -0---2-----\ -6
--
-- from 1-2  --> 1 < 2, 1 < 4, 
--
--
         lt = [(2,1),(2,4),(2,3),(2,5),(4,3)]

main = withNewSolver $ \s -> do
  let p = example5
  layout s (sNodes p) (sEdges p) 10
 


edgePath :: [Node] -> [Edge] -> (EdgeRef,EdgeRef) -> Maybe [(EdgeRef,EdgeSide,Dir)]
edgePath nodes edges (start,goal) = go [[(start,Begin,Down)],[(start,End,Up)]] (Set.fromList [(start,Begin),(start,End)])
  where
    next :: (EdgeRef, EdgeSide, Dir) -> [(EdgeRef,EdgeSide, Dir)]
    next (e,side,dir) = case (side,dir) of
                          (Begin,Down) -> nodeLinks
                          (End, Up) -> nodeLinks
                          _ -> (e, opposite side, dir)
      where
        edgeNodeIdx = fst ((if side == Begin then fst else snd) (edges !! e))
        nodeLinks = [ (ei,sd) | (ei,es) <- zip [0..] (fmap edgeLink edges), (sd,ni) <- es, ni == edgeNodeIdx ]

    edgeLink ((n1,p1),(n2,p2)) = [(Begin,n1), (End,n2)]

    opposite Begin = End
    opposite End = Begin

    other Up = Down
    other Down = Up

    go :: [[(EdgeRef,EdgeSide)]] -> Set (EdgeRef,EdgeSide) -> Maybe [(EdgeRef,EdgeSide)]
    go [] _ = Nothing 
    go paths visited = (listToMaybe [ p | p <- newPaths, fst (head p) == goal ]) <|> goNext
      where newPaths = [ (new:path) | path <- paths, new <- next (head path), not (new `Set.member` visited) ]
            goNext = go newPaths (Set.union visited $ Set.fromList [ head p | p <- newPaths ])

-- transitive reduction, remove any (a,z) s.t. exits b,c,d,...y and (a,b),(b,c),(c,d),...,(y,z)
-- Assumes acyclic relation! Reflexivity is ok.
minRel :: [(Int,Int)] -> [(Int,Int)]
minRel xs = [ (a,c) | (a,c) <- xs, not (reachable c [ b | (_,b) <- next a, b /= c ]) ]
  where 
    s = Set.fromList xs
    next x = Set.toList (setRangeInclusive s x x)
    reachable :: Int -> [Int] -> Bool
    reachable goal [] = False
    reachable goal (x:xs) | x == goal = True
    reachable goal (x:xs) = reachable goal ([ b | (_,b) <- next x ] ++ xs)

setRangeInclusive :: Set (Int,Int) -> Int -> Int -> Set (Int,Int)
setRangeInclusive s lo hi = justRight
  where (_, highEnough) = Set.split ((lo-1),2^29-1) s
        (justRight, _ ) = Set.split ((hi+1),-2^29) highEnough


type Prio = Set (Int, EdgeRef)
--
-- 2018-11-29: ported less-than relation function from Rust. Hm, that wasn't too bad.
--
ltRel :: [Node] -> [Edge] -> [(EdgeRef, EdgeRef)]
ltRel nodes edges = Set.toList $ Set.fromList allLt
  where
    (switchLeftEdge,switchRightEdge,switchTrunkEdge) = ((Map.!) switchLeftEdges, (Map.!) switchRightEdges, 
                                                        (Map.!) switchTrunkEdges)
    switchLeftEdges  = Map.fromList [ (ni, ei) | (ei,e) <- zip [0..] edges, (ni, PLeft)  <- [fst e, snd e] ]
    switchRightEdges = Map.fromList [ (ni, ei) | (ei,e) <- zip [0..] edges, (ni, PRight) <- [fst e, snd e] ]
    switchTrunkEdges = Map.fromList [ (ni, ei) | (ei,e) <- zip [0..] edges, (ni, PTrunk) <- [fst e, snd e] ]

    dirFactor :: Dir -> Int
    dirFactor Up = 1
    dirFactor Down = -1

    allLt = [ (under,over) | (i, (SwitchNode side dir)) <- zip [0..] nodes
                           , let (overs,unders) = findIt i dir
                           , over <- Set.toList overs, under <- Set.toList unders ]

    findIt :: Int -> Dir -> (Set EdgeRef, Set EdgeRef)
    findIt n dir = go (Set.singleton (edgeNode (top n)))
                      (Set.singleton (edgeNode (bottom n)))
                      (Set.singleton (f*(edgeNode (top n)), top n)) 
                      (Set.singleton (f*(edgeNode (bottom n)), bottom n))
                      Set.empty Set.empty
      where
        go :: Set NodeRef -> Set NodeRef -> Prio -> Prio -> Set EdgeRef -> Set EdgeRef -> (Set EdgeRef, Set EdgeRef)
        go oNodes uNodes oQueue uQueue oEdges uEdges | Set.null oQueue || Set.null uQueue || not (Set.null (Set.intersection oNodes uNodes))  = (rest oEdges oQueue, rest uEdges uQueue)
        go oNodes uNodes oQueue uQueue oEdges uEdges = if overPos < underPos then goOver else goUnder
          where 
            ((overPos,overEdge),(underPos,underEdge)) = (Set.elemAt 0 oQueue, Set.elemAt 0 uQueue)
            goOver  = go newONodes uNodes newOQueue uQueue (Set.insert overEdge oEdges) uEdges
            goUnder = go oNodes newUNodes oQueue newUQueue oEdges (Set.insert underEdge uEdges)
            newONodes = Set.union oNodes (Set.fromList [ edgeNode e | e <- nextEdges (edgeNode overEdge)  ])
            newUNodes = Set.union uNodes (Set.fromList [ edgeNode e | e <- nextEdges (edgeNode underEdge) ])
            newOQueue = Set.union (Set.deleteAt 0 oQueue) 
                          (Set.fromList [ (f*(edgeNode e), e) | e <- nextEdges (edgeNode overEdge)])
            newUQueue = Set.union (Set.deleteAt 0 uQueue)
                          (Set.fromList [ (f*(edgeNode e), e) | e <- nextEdges (edgeNode underEdge)])

        f = dirFactor dir

        top ni = case (dir, (nodes !! ni)) of 
                  (Up, SwitchNode _ Up) -> switchLeftEdge ni
                  (Down, SwitchNode _ Down) -> switchRightEdge ni
                  _ -> error "top ni"

        bottom ni = case (dir, (nodes !! ni)) of
                     (Up, SwitchNode _ Up) -> switchRightEdge ni
                     (Down, SwitchNode _ Down) -> switchLeftEdge ni
                     _ -> error "bottom ni"

        edgeNode ei = case dir of
                        Up   -> fst (snd (edges!!ei))
                        Down -> fst (fst (edges!!ei))

        nextEdges ni = case (dir, (nodes !! ni)) of
                              (Up, EndNode) -> []
                              (Up, SwitchNode _ Up) -> [switchLeftEdge ni, switchRightEdge ni]
                              (Up, SwitchNode _ Down) -> [switchTrunkEdge ni]
 
                              (Down, BeginNode) -> []
                              (Down, SwitchNode _ Down) -> [switchLeftEdge ni, switchRightEdge ni]
                              (Down, SwitchNode _ Up) -> [switchTrunkEdge ni]

                              _ -> error "nextEdges ni"

    rest :: Set EdgeRef -> Prio -> Set EdgeRef
    rest edges queue = Set.union edges (Set.map snd queue)


--
-- SOLVER REPRESENTATION
--

data EdgeSide = Begin | End
  deriving (Show, Ord, Eq)
data PortShape = PortShape {
  goUp :: Lit,  -- going upwards when moving left to right
  goStraight :: Lit,
  goDown :: Lit
} deriving (Show, Ord, Eq)

-- this function is the worst
mkPortShape :: EdgeSide -> Node -> Port -> Lit -> PortShape
mkPortShape Begin BeginNode                PBegin _ = PortShape false true false
mkPortShape End   EndNode                  PEnd   _ = PortShape false true false
mkPortShape Begin (SwitchNode SLeft  Up)   PLeft  s = PortShape (neg s) s false
mkPortShape Begin (SwitchNode SLeft  Up)   PRight s = PortShape false (neg s) s 
mkPortShape Begin (SwitchNode SRight Up)   PLeft  s = PortShape s (neg s) false
mkPortShape Begin (SwitchNode SRight Up)   PRight s = PortShape false s (neg s)
mkPortShape Begin (SwitchNode SLeft  Down) PTrunk s = PortShape false (neg s) s
mkPortShape Begin (SwitchNode SRight Down) PTrunk s = PortShape s (neg s) false
mkPortShape End   (SwitchNode SLeft  Down) PLeft  s = PortShape (neg s) s false
mkPortShape End   (SwitchNode SLeft  Down) PRight s = PortShape false (neg s) s
mkPortShape End   (SwitchNode SRight Down) PLeft  s = PortShape s (neg s) false
mkPortShape End   (SwitchNode SRight Down) PRight s = PortShape false s (neg s)
mkPortShape End   (SwitchNode SLeft  Up)   PTrunk s = PortShape false (neg s) s
mkPortShape End   (SwitchNode SRight Up)   PTrunk s = PortShape s (neg s) false
mkPortShape _ n p _ = error ("invalid edge shape " ++ (show n) ++ " " ++ (show p))

portShapeFactor :: PortShape -> Int
portShapeFactor (PortShape x _ _) | x == false = -1
portShapeFactor _ = 1

layout :: Solver -> [Node] -> [Edge] -> [(EdgeRef,EdgeRef)] -> Integer -> IO ()
layout s nodes edges edgeLt yBound = do

  -- REPRESENTATION
  node_delta_xs <- sequence [ newUnary s 2 | _ <- zip nodes (tail nodes) ]
  node_ys <- sequence [ newTerm s yBound | _ <- nodes ]
  edge_ys <- sequence [ newTerm s yBound | _ <- edges ]
  edge_short <- sequence [ do up <- newLit s ; down <- newLit s ; return (up,down) 
                         | _ <- edges ]
  slanted <- sequence [ if isSwitch x then newLit s else return false | x <- nodes ]

  -- CONSTRAINTS

  -- edges push nodes apart: sum(delta_x_a .. delta_x_b) >= 1
  forM_ edges $ \((n1,p1),(n2,p2)) -> do
    addClause s [ (node_delta_xs !! i) .>= 1 | i <- [(n1) .. (n2-1)] ]

  -- edge ordering
  forM_ edgeLt $ \(a,b) -> do
    let ((aShortUp,aShortDown),(bShortUp,bShortDown)) = (edge_short !! a, edge_short !! b)
    lessThanEqual s (edge_ys !! a) (edge_ys !! b)
    lessThanOr s [ aShortUp, bShortDown ] (edge_ys !! a) (edge_ys !! b)
    -- if short down from begin, then edge has same value as node_begin
    -- if short up   to end    , then edge has same value as node_end

  -- big bad edge iterator
  abs_dy <- forM (zip [0..] edges) $ \(ei, ((n1,p1),(n2,p2))) -> do
    let (shortUp,shortDown) = edge_short !! ei
    let eBegin = mkPortShape Begin (nodes !! n1) p1 (slanted !! n1)
    let eEnd   = mkPortShape End   (nodes !! n2) p2 (slanted !! n2)

    -- shortup/down => start and end both go up/down
    -- short can only be used when both ends of the edge go up or down
    addClause s [neg shortUp,   goUp   eBegin]
    addClause s [neg shortUp,   goUp   eEnd]
    addClause s [neg shortDown, goDown eBegin]
    addClause s [neg shortDown, goDown eEnd]

    -- going straight
    equalOr s [neg (goStraight eBegin)] (node_ys!!n1) (edge_ys!!ei)
    equalOr s [neg (goStraight eEnd)]   (edge_ys!!ei)   (node_ys!!n2)

    -- going down
    greaterThanOr s      [neg (goDown eBegin), shortDown] (node_ys!!n1) (edge_ys!!ei)
    greaterThanEqualOr s [neg (goDown eBegin)] (node_ys!!n1) (edge_ys!!ei)
    greaterThanOr s      [neg (goDown eEnd)]       (edge_ys!!ei) (node_ys!!n2)
      -- shortdown implies either dy1 or dy2 is gt 0 ... but we can fix it to be dy1
      -- if we want, I think.

    -- going up
    lessThanOr s [neg (goUp eBegin)] (node_ys!!n1) (edge_ys!!ei)
    lessThanOr s [neg (goUp eEnd), shortUp]   (edge_ys!!ei) (node_ys!!n2)
    lessThanEqualOr s [neg (goUp eEnd)]   (edge_ys!!ei) (node_ys!!n2)
    
    -- push X values apart
    -- they are already 1 apart (edge push constraints above)
    -- so we need to find out if they are 2 apart.
    --   x1 + abs(dy1) + abs(dy2) + notsamedir <= x2
    --   dx_gte_2 <- abs(dy1) + abs(dy2) + notsamedir > 1
    dx <- countUpTo s 2 $ concat [ [ (node_delta_xs !! i) .>= 1 ,
                                     (node_delta_xs !! i) .>= 2 ]
                                 | i <- [(n1) .. (n2-1)] ]

    -- shortup/shortdown are influenced by dx
    -- first, if is_short then dx must be 1
    addClause s [neg shortUp,   (dx .< 2)]
    addClause s [neg shortDown, (dx .< 2)]
    -- also, !is_short => x+2 <= x2 
    addClause s [shortDown, shortUp, (dx .>= 2)]

    -- Alternative for Unary representation:
    --let absdy1 = if portShapeFactor eBegin > 0 then [invert (node_ys!!n1), edge_ys!!ei ]
    --             else [ node_ys!!n1 , invert (edge_ys!!ei) ]
    --let absdy2 = if -1*(portShapeFactor eEnd) > 0 then [ invert (node_ys!!n2), edge_ys!!ei ]
    --             else [ node_ys!!n2, invert (edge_ys!!ei) ]

    putStrLn $ show (ei, portShapeFactor eBegin, portShapeFactor eEnd)
    let absdy1 = if portShapeFactor eBegin > 0 then ( (edge_ys!!ei) .-. (node_ys!!n1))
                 else ( (node_ys!!n1) .-. (edge_ys!!ei) )
    let absdy2 = if -1*(portShapeFactor eEnd) > 0 then (  (edge_ys!!ei) .-. (node_ys!!n2) )
                 else ( (node_ys!!n2) .-.  (edge_ys!!ei) )

    return (absdy1 .+. absdy2)

  let big_dy = (foldl (.+.) (SAT.Term.number 0) abs_dy)
  putStrLn =<< stats s
  putStrLn =<< fmap show (solve s [])

  let print = do node_x <- fmap (scanl (+) 0) $ sequence [ SAT.Unary.modelValue s x 
                                                         | x <- node_delta_xs ]
                 node_y <- sequence [ SAT.Term.modelValue s x | x <- node_ys ]
                 edge_y <- sequence [ SAT.Term.modelValue s x | x <- edge_ys ]
                 short <- sequence [ do a <- SAT.modelValue s x; b <- SAT.modelValue s y; return (a,b) | (x,y) <- edge_short ]
                 slants <- sequence [ SAT.modelValue s x | x <- slanted ]
                 putStrLn $ (show (zip node_x node_y))
                 putStrLn $ (show edge_y)
                 putStrLn $ (show short)
                 putStrLn $ (show (zip slanted slants))

  big_dy_val <- minimizeTerm s big_dy
  putStrLn $ "big dy val " ++ (show big_dy_val)
  print

stats :: Solver -> IO String
stats s = do
  vars <- numVars s
  clauses <- numClauses s
  return ("SAT instance with " ++ (show vars) ++ " vars and " ++ (show clauses) ++ " clauses.")

minimizeTerm :: Solver -> Term -> IO (Maybe Integer)
minimizeTerm s x = do 
  putStrLn =<< stats s
  ok <- solve s []
  if ok then do
    let opt minTry minReached | minReached > minTry =
         do let try = ((minReached+minTry) `div` 2)
            putStrLn $ "opt " ++ (show minTry) ++ " "  ++ (show minReached) ++ " " ++ (show try)
            constraint <- isLessThanEqual s x (SAT.Term.number try)
            putStrLn =<< stats s
            ok <- solve s [constraint]
            if ok then (SAT.Term.modelValue s x >>= \reached -> opt minTry reached)
            else opt (try+1) minReached
        opt v _ = return (Just v)
    max <- SAT.Term.modelValue s x
    opt (SAT.Term.minValue x) max
  else return Nothing
