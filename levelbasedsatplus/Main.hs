module Main where

import SAT
import SAT.Unary
import SAT.Order
import SAT.Equal
import SAT.Optimize
import Control.Monad (forM, forM_)

--
-- INPUT REPRESENTATION
--
data Side = SLeft | SRight
data Dir = Up | Down
data Port = PLeft | PRight | PTrunk | PBegin | PEnd

type EdgeRef = Int
type NodeRef = Int

type PortRef = (NodeRef, Port)
type Edge = (PortRef, PortRef)
data Node = BeginNode | EndNode | SwitchNode Side Dir (EdgeRef,EdgeRef) EdgeRef

isSwitch :: Node -> Bool
isSwitch (SwitchNode _ _ _ _) = True
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
  where  n = [BeginNode, SwitchNode SLeft Up (0,0) 0, EndNode, EndNode ]
         e = [((0, PBegin),(1,PTrunk)),
              ((1, PLeft),(2,PEnd)),
              ((1, PRight),(3,PEnd))]
         lt = [(1,2)]

main = withNewSolver $ \s -> do
  let p = example2
  layout s (sNodes p) (sEdges p) (sEdgeLT p) 5
 

--
-- SOLVER REPRESENTATION
--

data EdgeSide = Begin | End
data PortShape = PortShape {
  goUp :: Lit, -- direction north-east on dwg. when port is on east-side of node, corr. west.
  goStraight :: Lit,
  goDown :: Lit
} deriving (Show, Ord, Eq)

-- this function is the worst
mkPortShape :: EdgeSide -> Node -> Port -> Lit -> PortShape
mkPortShape Begin BeginNode                    PBegin _ = PortShape false true false
mkPortShape End   EndNode                      PEnd   _ = PortShape false true false
mkPortShape Begin (SwitchNode SLeft  Up _ _)   PLeft  s = PortShape (neg s) s false
mkPortShape Begin (SwitchNode SLeft  Up _ _)   PRight s = PortShape false (neg s) s 
mkPortShape Begin (SwitchNode SRight Up _ _)   PLeft  s = PortShape s (neg s) false
mkPortShape Begin (SwitchNode SRight Up _ _)   PRight s = PortShape false s (neg s)
mkPortShape Begin (SwitchNode SLeft  Down _ _) PTrunk s = PortShape false (neg s) s
mkPortShape Begin (SwitchNode SRight Down _ _) PTrunk s = PortShape s (neg s) false
mkPortShape End   (SwitchNode SLeft  Down _ _) PLeft  s = PortShape (neg s) s false
mkPortShape End   (SwitchNode SLeft  Down _ _) PRight s = PortShape false (neg s) s
mkPortShape End   (SwitchNode SRight Down _ _) PLeft  s = PortShape s (neg s) false
mkPortShape End   (SwitchNode SRight Down _ _) PRight s = PortShape false s (neg s)
mkPortShape End   (SwitchNode SLeft  Up   _ _) PTrunk s = PortShape false (neg s) s
mkPortShape End   (SwitchNode SRight Up   _ _) PTrunk s = PortShape s (neg s) false
mkPortShape _ _ _ _ = error "invalid edge shape"

portShapeFactor :: PortShape -> Int
portShapeFactor (PortShape x _ _) | x == false = -1
portShapeFactor _ = 1

layout :: Solver -> [Node] -> [Edge] -> [(EdgeRef,EdgeRef)] -> Int -> IO ()
layout s nodes edges edge_lt yBound = do

  -- REPRESENTATION
  node_delta_xs <- sequence [ newUnary s 2 | _ <- zip nodes (tail nodes) ]
  node_ys <- sequence [ newUnary s yBound | _ <- nodes ]
  edge_ys <- sequence [ newUnary s yBound | _ <- edges ]
  edge_short <- sequence [ do up <- newLit s ; down <- newLit s ; return (up,down) 
                         | _ <- edges ]
  slanted <- sequence [ if isSwitch x then return false else newLit s | x <- nodes ]

  -- CONSTRAINTS

  -- edges push nodes apart: sum(delta_x_a .. delta_x_b) >= 1
  forM_ edges $ \((n1,p1),(n2,p2)) -> do
    addClause s [ (node_delta_xs !! i) .>= 1 | i <- [(n1) .. (n2-1)] ]

  -- edge ordering
  forM_ edge_lt $ \(a,b) -> do
    let ((aShortUp,aShortDown),(bShortUp,bShortDown)) = (edge_short !! a, edge_short !! b)
    lessThanEqual s (edge_ys !! a) (edge_ys !! b)
    lessThanOr s [neg aShortDown] (edge_ys !! a) (edge_ys !! b)
    lessThanOr s [neg bShortUp  ] (edge_ys !! a) (edge_ys !! b)

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
    greaterThanOr s      [neg (goDown eBegin)] (node_ys!!n1) (edge_ys!!ei)
    greaterThanOr s      [neg (goDown eEnd), shortDown]       (edge_ys!!ei) (node_ys!!n2)
    greaterThanEqualOr s [neg (goDown eEnd), neg shortDown]   (edge_ys!!ei) (node_ys!!n2)
      -- shortdown implies either dy1 or dy2 is gt 0 ... but we can fix it to be dy1
      -- if we want, I think.

    -- going up
    lessThanOr s [neg (goUp eBegin)] (node_ys!!n1) (edge_ys!!ei)
    lessThanOr s [neg (goUp eEnd), shortUp] (edge_ys!!ei) (node_ys!!n2)
    lessThanEqualOr s [neg (goUp eEnd), neg shortUp] (edge_ys!!ei) (node_ys!!n2)
    
    -- push X values apart
    -- they are already 1 apart (edge push constains above)
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

    let absdy1 = if portShapeFactor eBegin > 0 then [invert (node_ys!!n1), edge_ys!!ei ]
                 else [ node_ys!!n1 , invert (edge_ys!!ei) ]
    let absdy2 = if portShapeFactor eEnd > 0 then [ invert (node_ys!!n2), edge_ys!!ei ]
                 else [ node_ys!!n2, invert (edge_ys!!ei) ]
    return (absdy1 ++ absdy2)

  big_dy <- addList s (concat abs_dy)
  big_nx <- addList s node_delta_xs
  putStrLn $ "big dy " ++ (show big_dy)
  putStrLn $ "big nx " ++ (show big_nx)

  putStrLn =<< stats s

  let print = do node_x <- fmap (scanl (+) 0) $ sequence [ SAT.Unary.modelValue s x 
                                                        | x <- node_delta_xs ]
                 node_y <- sequence [ SAT.Unary.modelValue s x | x <- node_ys ]
                 edge_y <- sequence [ SAT.Unary.modelValue s x | x <- edge_ys ]
                 putStrLn $ (show (zip node_x node_y))
                 putStrLn $ (show edge_y)

  ok1 <- solveMinimize s [] big_dy
  if ok1 then do
    dy <- SAT.Unary.modelValue s big_dy
    putStrLn $ "dy = " ++ (show dy)
    addClause s [big_dy .<= dy]
    print
    ok2 <- solveMinimize s [] big_nx
    if ok2 then do 
      nx <- SAT.Unary.modelValue s big_nx
      putStrLn $ "nx = " ++ (show nx)
      addClause s [big_nx .<= nx]
      print
    else putStrLn "opt nx failed"
  else putStrLn "opt dy failed"

  --ok1 <- solveMinimize s [] big_ey
  --if ok1 then do 
  --  ey <- SAT.Unary.modelValue s big_ey
  --  putStrLn $ "ey = " ++ (show ey)
  --  addClause s [big_ey .<= ey]
  --  ok2 <- solveMinimize s [] big_ny
  --  if ok2 then do
  --    ny <- SAT.Unary.modelValue s big_ny
  --    putStrLn $ "ny = " ++ (show ny)
  --    addClause s [big_ny .<= ny]
  --    ok3 <- solveMinimize s [] big_nx
  --    if ok3 then do 
  --      nx <- SAT.Unary.modelValue s big_nx
  --      putStrLn $ "nx = " ++ (show nx)
  --    else putStrLn "failed"
  --  else putStrLn "failed"
  --else putStrLn "failed"
     
        

  --ok <- solve s []
  --if ok then do
  --  putStrLn "solved"
  --  node_x <- fmap (scanl (+) 0) $ sequence [ SAT.Unary.modelValue s x | x <- node_delta_xs ]
  --  node_y <- sequence [ SAT.Unary.modelValue s x | x <- node_ys ]
  --  edge_y <- sequence [ SAT.Unary.modelValue s x | x <- edge_ys ]
  --  putStrLn $ (show (zip node_x node_y))
  --  putStrLn $ (show edge_y)
  --else do
  --  putStrLn "failed"


stats :: Solver -> IO String
stats s = do
  vars <- numVars s
  clauses <- numClauses s
  return ("SAT instance with " ++ (show vars) ++ " vars and " ++ (show clauses) ++ " clauses.")



