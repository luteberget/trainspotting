module UnaryY where

import SAT
import SAT.Bool
import SAT.Unary
import SAT.Order
import SAT.Equal
import SAT.Val
import SAT.Optimize

import Data.Maybe (listToMaybe, catMaybes)
import Control.Monad (forM, forM_, when)
import Data.Tuple (swap)
import Data.Set (Set)
import qualified Data.Set as Set


import Input
import EdgeOrder


data EdgeShape = EUp | EStraight | EDown
  deriving (Show,Ord,Eq)

data PropagationDirection = PropForward | PropBackward
  deriving (Show,Ord,Eq)

type Section = [(EdgeRef, Unary)]

beginSplit :: Solver -> Int -> [EdgeRef] -> [EdgeRef] -> IO (Section, Section)
beginSplit s h before after = do
  ys <- sequence [ newUnary s h | _ <- after ]
  --nforM_ (succPairs ys) $ \(y1,y2) -> lessThanOr s [] y1 y2
  let afterYs = zip after ys 
  let beforeYs = filter (\(i,x) -> i `elem` before) afterYs
  return (beforeYs, afterYs)

-- idea for less-than on edges vertically:
-- All DYs have dy >= 0.
--  - Begin/end have forced dy >= 1 around them.
--  - Switches have forced dy >= 1 around them.
--  - Each DY transferred across a column: 
--     has to be dy != zero, UNLESS it was previously (in prop dir) 
--                              AND column is off.
--
--      this is needed not on end separators for edges, but everywhere inbetween
--
--  IDEA 2
--    on edge ends:    
--      begin/end/switch have dy >=1 at the node
--    on edge internal:
--      internal separatorson between two ACTIVE columns has dy >=1 around itself
--
--  IDEA 3
--
--    on edge ends: explicit contraints
--    on internal edges: if (left is active or not propagate forward) AND (right is active or not prop backw), 
--                             the edge must separate from its neighbors

switchSplit :: Solver -> Int -> [EdgeRef] -> [EdgeRef] -> IO (Section,Section)
switchSplit s h trunkSide branchSide = do
  meet   <- newUnary s h
  trunk  <- sequence [ if e `elem` branchSide then (newUnary s h) else 
                         return meet  | e <- trunkSide ]

  trunkSep <- forM (succPairs trunk) $ \(a,b) -> if a `elem` trunk || b `elem` trunk then return true else newLit s

  let branch = [ if e `elem` trunkSide 
                   then head [ y | (e2,y) <- zip trunkSide trunk, e == e2 ] 
                   else meet | e <- branchSide ]

  return (zip trunkSide trunk,zip branchSide branch)

findEdgeIdx :: [Edge] -> Int -> Port -> Int
findEdgeIdx edges ni port = head [ ei | (ei,((n1,p1),(n2,p2))) <- zip [0..] edges
                                      , (n1 == ni && p1 == port) || (n2 == ni && p2 == port) ]

layout :: [Node] -> [Edge] -> IO [[Pt]]
layout nodes edges = withNewSolver $ \s -> do
  let h = 50
  let k = 20
  let edgeOrderRelation = Set.fromList (transitiveClosure (ltRel nodes edges))
  let sortEdges = relationSort edgeOrderRelation
  let edgesBetween n1 n2 = [ ei | (ei, ((x,_),(y,_))) <- zip [0..] edges
                                , x <= n1 && n2 <= y ]

  nodeSeps <- forM (zip [0..] nodes) $ \(ni,node) -> do
    let beforeEdges = sortEdges (edgesBetween (ni-1) (ni))
    let afterEdges  = sortEdges (edgesBetween (ni) (ni+1))
    case node of
      BeginNode ->                    beginSplit  s h beforeEdges afterEdges
      EndNode   ->         fmap swap (beginSplit  s h afterEdges  beforeEdges)
      SwitchNode _ Up ->              switchSplit s h beforeEdges afterEdges
      SwitchNode _ Down -> fmap swap (switchSplit s h afterEdges  beforeEdges)


  succNodeCols <- forM (succPairs (zip [0..] nodes)) $ \((n1,_),(n2,_)) -> do
    let colEdges = sortEdges (edgesBetween n1 n2)
    let mkCol = do edges <- sequence [ do shape <- newVal s [EUp, EStraight, EDown]
                                          prop <- newVal s [PropForward, PropBackward]
                                          return (ei, shape, prop)
                                     | ei <- colEdges ]
                   active <- newLit s
                   return (active, edges)

    let mkSep = do ys <- sequence [ newUnary s h | ei <- colEdges ]
                   --sequence_ [ lessThanOr s [] y1 y2 | (y1,y2) <- succPairs ys ]
                   return (zip colEdges ys)

    cols <- sequence [ mkCol | _ <- [1..k] ]
    seps <- sequence [ mkSep | _ <- succPairs cols]
    

    return (cols,seps)

  let allSeparatorPairs = concat [ zip cols (succPairs seps)
                          | ((cols,internal), ((_,l),(r,_))) <- (zip succNodeCols (succPairs nodeSeps)),
                          let seps = [l] ++ internal ++ [r] ]

  -- CONSISTENCY CONSTRAINTS:
  forM_ allSeparatorPairs $ \((active,edges),(sep1,sep2)) -> do
  -- 1. Ys propagate across deactivated columns
    sequence_ [ equalOr s [active] y1 y2 | ((_,y1),(_,y2)) <- zip sep1 sep2 ]
    
  -- 3. edge shapes consistent with Y values
    forM_ (zip3 edges sep1 sep2) $ \((ei,sh,prop),(ei1,y1),(ei2,y2)) -> do
      if ei /= ei1 || ei /= ei2 then do error "??" else return ()
      equalOr s [neg active, neg (sh .= EStraight)] y1 y2
      equalOr s [neg active, neg (sh .= EUp)] (SAT.Unary.succ y1) y2
      equalOr s [neg active, neg (sh .= EDown)] y1 (SAT.Unary.succ y2)

  -- 2. Edge shapes propagate acrsos deactivated columns
  forM_ (succPairs (concat [ cs | (cs,_seps) <- succNodeCols ])) $ \((active1,edges1),(active2,edges2)) -> do
    -- if left col is deactivated and edge prop forward, right column inherits
    sequence_ [ do case listToMaybe [ x | x@(ei2,_,_) <- edges2, ei1 == ei2 ] of
                     Just (ei2,sh2,prop2) -> do
                       equalOr s   [ active1, neg (prop1 .= PropForward)] sh1 sh2
                       addClause s [ active1, active2, neg (prop1 .= PropForward), prop2 .= PropForward ]
                     Nothing -> addClause s [ active1, neg (prop1 .= PropForward) ]
              | (ei1,sh1,prop1) <- edges1 ]
    -- if right col is deactivated and edge prop backward, left column inherits
    sequence_ [ do case listToMaybe [ x | x@(ei1,_,_) <- edges1, ei1 == ei2 ] of
                     Just (ei1,sh1,prop1) -> do
                       equalOr s   [ active2, neg (prop2 .= PropBackward)] sh2 sh1
                       addClause s [ active2, active1, neg (prop2 .= PropBackward), prop1 .= PropBackward ]
                     Nothing -> addClause s [ active2, neg (prop2 .= PropBackward) ]
              | (ei2,sh2,prop2) <- edges2 ]

    -- both columns deactivated => equal prop dir
    sequence_ [ do equalOr s [ active1, active2 ] sh1 sh2
                   equalOr s [ active1, active2 ] prop1 prop2
              | (ei1,sh1,prop1) <- edges1, (ei2,sh2,prop2) <- edges2, ei1 == ei2 ]

    -- no sharp angles
    sequence_ [ do sharp <- orl s =<< sequence [ andl s [ sh1 .= a, sh2 .= b ] | (a,b) <- [(EUp,EDown),(EDown,EUp)] ]
                   addClause s $ [ neg active1, active2, neg (prop2 .= PropForward), neg sharp]
                   addClause s $ [ neg active1, neg active2, neg sharp]
                   addClause s $ [ neg active2, active1, neg (prop1 .= PropBackward), neg sharp]
              | (ei1,sh1,prop1) <- edges1, (ei2,sh2,prop2) <- edges2, ei1 == ei2 ]

  -- PROBLEM CONSTRAINTS
  --
  -- 5. y coordinates sorted and distinct (except switch legs meeting at nodes)

  forM_ allSeparatorPairs $ \((active,edges),(sep1,sep2)) -> do
    -- (active) => 
    --

  -- 
  -- 4. switch shapes
  forM_ (zip [0..] nodes) $ \(ni,node) -> do
    let prevCol = last (fst (succNodeCols !! (ni-1)))
    let nextCol = head (fst (succNodeCols !! (ni  )))

    let trunkEdge = findEdgeIdx edges ni PTrunk
    let leftEdge =  findEdgeIdx edges ni PLeft
    let rightEdge = findEdgeIdx edges ni PRight
    let beginEdge = findEdgeIdx edges ni PBegin
    let endEdge = findEdgeIdx edges ni PEnd
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

      BeginNode ->         do addClause s [isActive nextCol, snd (find nextCol beginEdge) .= PropForward ]
      EndNode   ->         do addClause s [isActive prevCol, snd (find prevCol endEdge)   .= PropBackward ]


    -- Set the shape
    case node of
      BeginNode -> do
        let (sh,_) = find nextCol beginEdge
        addClause s [sh .= EStraight]
      EndNode -> do
        let (sh,_) = find prevCol endEdge
        addClause s [sh .= EStraight]

      SwitchNode SLeft Up -> do
        let (trunkShape,_) = find prevCol trunkEdge
        let (leftShape,_)   = find nextCol leftEdge
        let (rightShape,_) = find nextCol rightEdge

        addClause s [neg (trunkShape .= EUp)]
        addClause s [neg (trunkShape .= EStraight), leftShape .= EUp]
        addClause s [neg (trunkShape .= EStraight), rightShape .= EStraight]
        addClause s [neg (trunkShape .= EDown), leftShape .= EStraight]
        addClause s [neg (trunkShape .= EDown), rightShape .= EDown]

      SwitchNode SRight Up -> do
        let (trunkShape,_) = find prevCol trunkEdge
        let (leftShape,_)   = find nextCol leftEdge
        let (rightShape,_) = find nextCol rightEdge

        addClause s [neg (trunkShape .= EDown)]
        addClause s [neg (trunkShape .= EStraight), leftShape .= EStraight]
        addClause s [neg (trunkShape .= EStraight), rightShape .= EDown]
        addClause s [neg (trunkShape .= EUp), leftShape .= EUp]
        addClause s [neg (trunkShape .= EUp), rightShape .= EStraight]

      SwitchNode SLeft Down -> do
        let (trunkShape,_) = find nextCol trunkEdge
        let (leftShape,_)   = find prevCol leftEdge
        let (rightShape,_) = find prevCol rightEdge

        addClause s [neg (trunkShape .= EUp)]
        addClause s [neg (trunkShape .= EStraight), leftShape .= EUp]
        addClause s [neg (trunkShape .= EStraight), rightShape .= EStraight]
        addClause s [neg (trunkShape .= EDown), leftShape .= EStraight]
        addClause s [neg (trunkShape .= EDown), rightShape .= EDown]

      SwitchNode SRight Down -> do
        let (trunkShape,_) = find nextCol trunkEdge
        let (leftShape,_)   = find prevCol leftEdge
        let (rightShape,_) = find prevCol rightEdge

        addClause s [neg (trunkShape .= EDown)]
        addClause s [neg (trunkShape .= EStraight), leftShape .= EStraight]
        addClause s [neg (trunkShape .= EStraight), rightShape .= EDown]
        addClause s [neg (trunkShape .= EUp), leftShape .= EUp]
        addClause s [neg (trunkShape .= EUp), rightShape .= EStraight]


  putStrLn =<< stats s
  b <- solve s []
  if b then do
    putStrLn "SAT"

    -- WIDTH
    nCols <- SAT.Unary.count s [ active | ((active,_),_) <- allSeparatorPairs]
    putStrLn =<< stats s
    solveMinimize s [] nCols
    nColsVal <- SAT.Unary.modelValue s nCols
    addClause s [nCols .<= nColsVal]
    putStrLn =<< stats s

    -- DIAGS
    nDiag <- SAT.Unary.count s =<< sequence [ andl s [active, neg (sh .= EStraight)] | ((active,es),_) <- allSeparatorPairs, (_,sh,_) <- es ]
    putStrLn =<< stats s
    solveMinimize s [] nDiag
    nDiagVal <- SAT.Unary.modelValue s nDiag
    putStrLn =<< stats s
    addClause s [nDiag .<= nDiagVal]
    putStrLn =<< stats s

    -- HEIGHT
    height <- newUnary s h
    forM_ allSeparatorPairs $ \(_,(s1,s2)) -> do
      let ((_,y1),(_,y2)) = last (zip s1 s2) 
      lessThanEqualOr s [] y1 height
      lessThanEqualOr s [] y2 height
    putStrLn =<< stats s
    solveMinimize s [] height
    putStrLn =<< stats s

    let cols = [ (active,[(y1,y2) | ((_,y1),(_,y2)) <- zip sep1 sep2 ]) 
               | ((active,edges),(sep1,sep2)) <- allSeparatorPairs ]
    lines <- edgeLines s edges allSeparatorPairs
    forM_ (zip edges lines) $ \(e,l) -> do
      putStrLn $ (show e) ++ ":\n    " ++ (show (linesToPolyline l))
    dumpSol s h cols
    return (fmap linesToPolyline lines)
  else do
    error "UNSAT"


stats :: Solver -> IO String
stats s = do
  vars <- numVars s
  clauses <- numClauses s
  return ("SAT instance with " ++ (show vars) ++ " vars and " ++ (show clauses) ++ " clauses.")


main = do
  let (SolverInput n e _) = example4
  layout n e

linesToPolyline :: [Line] -> [Pt]
linesToPolyline [] = []
linesToPolyline ((a,b):[]) = [a,b]
linesToPolyline xs = (fmap fst xs) ++ [(snd (last xs))]

type Pt = (Int,Int)
type Line = (Pt, Pt)
edgeLines :: Solver -> [Edge] -> [((Lit,[(EdgeRef, Val EdgeShape, Val PropagationDirection)]),([(EdgeRef,Unary)],[(EdgeRef,Unary)]))] -> IO [[Line]]
edgeLines s edges cols = do
  activeCols <- forM cols $ \((active,_),_) -> SAT.modelValue s active
  let colLeftX = scanl (+) 0 [ if x then 1 else 0::Int | x <- activeCols ]
  edgeShapes <- forM (zip [0..] edges) $ \(ei,_) -> do
    lines <- forM (zip [0..] cols) $ \(ci,((active,eshs),(sep1,sep2))) -> do
      b <- SAT.modelValue s active
      if b then do
        case listToMaybe [ (sh,y1,y2) | ((e,sh,_),(_,y1),(_,y2)) <- zip3 eshs sep1 sep2, e == ei ] of
          Just (sh,y1,y2) -> do 
            y1val <- SAT.Unary.modelValue s y1
            y2val <- SAT.Unary.modelValue s y2
            let x = colLeftX !! ci
            return (Just ((x,y1val),(x+1,y2val)))
          Nothing -> return Nothing
      else return Nothing
    return (catMaybes lines)
  return edgeShapes

dumpSol :: Solver -> Int -> [(Lit,[(Unary,Unary)])] -> IO ()
dumpSol s h cols = forM_ (zip [0..] cols) $ \(i,(active,vars)) -> do
  b <- SAT.modelValue s active
  putStrLn $ "col " ++ (show i ) ++ ": " ++ (show b)
  when b $ do 
      forM_ vars $ \(y1var,y2var) -> do
        y1 <- SAT.Unary.modelValue s y1var
        y2 <- SAT.Unary.modelValue s y2var
        putStrLn $ "  " ++ (show y1) ++ "-"++(show y2)


displaySol :: Solver -> Int -> [(Lit,[(Unary,Unary)])] -> IO ()
displaySol s h cols = forM_ [h,(h-1)..0] $ \i -> do
  putStr $ (show i) ++ ": "
  forM_ cols $ \(active,vars) -> do
    b <- SAT.modelValue s active
    when b $ do
      forM_ vars $ \(y1var,y2var) -> do
        y1 <- SAT.Unary.modelValue s y1var
        y2 <- SAT.Unary.modelValue s y2var
        if y1 == i && y2 == i+1 then do putStr "/"
        else do
          if y1 == i && y2 == i then do putStr "-"
          else do 
            if y1 == i && y2 == i-1 then do  putStr "\\"
            else  putStr " "
  putStrLn ""
