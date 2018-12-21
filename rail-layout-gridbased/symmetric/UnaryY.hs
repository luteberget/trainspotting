module Main where

import SAT
import SAT.Unary
import SAT.Order
import SAT.Equal
import SAT.Val
import SAT.Optimize

import Data.Maybe (listToMaybe)
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
  forM_ (succPairs ys) $ \(y1,y2) -> lessThanOr s [] y1 y2
  let afterYs = zip after ys 
  let beforeYs = filter (\(i,x) -> i `elem` before) afterYs
  return (beforeYs, afterYs)

switchSplit :: Solver -> Int -> [EdgeRef] -> [EdgeRef] -> IO (Section,Section)
switchSplit s h trunkSide branchSide = do
  meet   <- newUnary s h
  trunk  <- sequence [ if e `elem` branchSide then (newUnary s h) else 
                         return meet  | e <- trunkSide ]

  let branch = [ if e `elem` trunkSide 
                   then head [ y | (e2,y) <- zip trunkSide trunk, e == e2 ] 
                   else meet | e <- branchSide ]

  forM_ (succPairs trunk) $ \(y1,y2) -> lessThanOr s [] y1 y2
  return (zip trunkSide trunk,zip branchSide branch)

findEdgeIdx :: [Edge] -> Int -> Port -> Int
findEdgeIdx edges ni port = head [ ei | (ei,((n1,p1),(n2,p2))) <- zip [0..] edges
                                      , (n1 == ni && p1 == port) || (n2 == ni && p2 == port) ]

layout :: [Node] -> [Edge] -> IO ()
layout nodes edges = withNewSolver $ \s -> do
  let h = 10
  let k = 4
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
                   sequence_ [ lessThanOr s [] y1 y2 | (y1,y2) <- succPairs ys ]
                   return (zip colEdges ys)

    cols <- sequence [ mkCol | _ <- [1..k] ]
    seps <- sequence [ mkSep | _ <- succPairs cols]
    return (cols,seps)

  let allSeparatorPairs = concat [ zip cols (succPairs seps)
                          | ((cols,internal), ((_,l),(r,_))) <- (zip succNodeCols (succPairs nodeSeps)),
                          let seps = [l] ++ internal ++ [r] ]

  -- CONSISTENCY CONSTRAINTS:
  forM_ allSeparatorPairs $ \((active,edges),(sep1,sep2)) -> do
    putStrLn (show (map (\(x,_,_) -> x) edges,map fst sep1,map fst sep2))
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
                       addClause s [ active1, neg (prop1 .= PropForward), prop2 .= PropForward ]
                     Nothing -> addClause s [ active1, neg (prop1 .= PropForward) ]
              | (ei1,sh1,prop1) <- edges1 ]
    -- if right col is deactivated and edge prop backward, left column inherits
    sequence_ [ do case listToMaybe [ x | x@(ei1,_,_) <- edges1, ei1 == ei2 ] of
                     Just (ei1,sh1,prop1) -> do
                       equalOr s   [ active2, neg (prop2 .= PropBackward)] sh2 sh1
                       addClause s [ active2, neg (prop2 .= PropBackward), prop1 .= PropBackward ]
                     Nothing -> addClause s [ active2, neg (prop2 .= PropBackward) ]
              | (ei2,sh2,prop2) <- edges2 ]

  -- PROBLEM CONSTRAINTS
  -- 4. switch shapes
  forM_ (zip [0..] nodes) $ \(ni,node) -> do
    let prevCol = last (fst (succNodeCols !! (ni-1)))
    let nextCol = head (fst (succNodeCols !! (ni  )))

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

    -- WIDTH
    nCols <- SAT.Unary.count s [ active | ((active,_),_) <- allSeparatorPairs]
    putStrLn =<< stats s
    solveMinimize s [] nCols
    nColsVal <- SAT.Unary.modelValue s nCols
    addClause s [nCols .<= nColsVal]
    putStrLn =<< stats s

    -- DIAGS
    nDiag <- SAT.Unary.count s [ neg (sh .= EStraight) | ((_,es),_) <- allSeparatorPairs, (_,sh,_) <- es ]
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
    dumpSol s h cols
    displaySol s h cols
  else do
    putStrLn "UNSAT"


  return ()


stats :: Solver -> IO String
stats s = do
  vars <- numVars s
  clauses <- numClauses s
  return ("SAT instance with " ++ (show vars) ++ " vars and " ++ (show clauses) ++ " clauses.")


main = do
  let (SolverInput n e _) = example4
  layout n e

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
