-- Visualization: 
-- * parse infrastructure file (NodeParser.hs)
-- * check if global up/down can be defined (later: find minimal set of up/down reversals to dag-ify)
-- * group linear parts
-- * solve grid (GridSolver.hs)
-- * ungroup linear parts
--
--

module GridVis where

import qualified NodeParser as P
import qualified GridSolver as S

import Data.Map (Map)
import qualified Data.Map as Map

convertInput :: [P.Statement] -> [S.Node]
convertInput stmts = [ conv dat | (P.NodeStmt _ dat) <- stmts]
  where
    nameToIdx = Map.fromList (zip [name | (P.NodeStmt name _) <- stmts] [0..])
    idx x = nameToIdx Map.! x
    
    conv :: P.NodeData -> S.Node
    conv (P.Start name) = S.startNode (idx name)
    conv (P.End name) = S.endNode (idx name)
    conv (P.Sw P.SwLeft  P.Incoming a (b,c)) = S.inLeftSw  (idx a) ((idx b),(idx c))
    conv (P.Sw P.SwRight P.Incoming a (b,c)) = S.inRightSw (idx a) ((idx b),(idx c))
    conv (P.Sw P.SwLeft  P.Outgoing a (b,c)) = S.outLeftSw (idx a) ((idx b),(idx c))
    conv (P.Sw P.SwRight P.Outgoing a (b,c)) = S.outRightSw (idx a) ((idx b),(idx c))

run = do
  (Right graph) <- P.parseFile "/home/bjlut/rolling/test"
  let x = convertInput graph
  putStrLn (show graph)
  putStrLn (show x)

