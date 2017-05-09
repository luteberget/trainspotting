-- Convert railML track infrastructure to graph representation, 
-- outputting a list of node names and a list of edges referring
-- to node names, also with length.
--
-- Assumes that all connections are track begin/end or two-way switches
-- (no three-way switches or track crossings)
--

module Main where

-- run: `cabal install xml`
import Text.XML.Light

import Control.Monad (join)
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import qualified Data.Map as Map
import Data.List ((\\), sort, nubBy, partition)
import Data.Tuple (swap)

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

(|>) x f = f x
infixl 9 |>
(.||.) f g = \x y -> (f x y) || (g x y)
for = flip map

type Loc = (String,Float) -- Track name + position

-- Takes a characteristic function for an equivalence relation
-- and groups a list accordingly (naive algorithm)
equivalenceClasses :: (a -> a -> Bool) -> [a] -> [[a]]
equivalenceClasses _     [] = []
equivalenceClasses equal xs = let (fg,rst) = split equal xs
                              in fg : equivalenceClasses equal rst
  where 
    split :: (a -> a -> Bool) -> [a] -> ([a],[a])
    split equal xs@(x:_) = partition (equal x) xs
    split _     []       = ([],[])


--
-- XML access
--

childElems = onlyElems . elContent

getAttrString :: Element -> String -> Maybe String
getAttrString e n = listToMaybe $ map attrVal $ filter (\x ->( qName $ attrKey x) == n) $ elAttribs e

(-|?>) = getAttrString
(-|>) e n = fromJust $ getAttrString e n

filterName n = filter (\x -> (qName $ elName x) == n)
subElem mult e n = listToMaybe $ filterName n $ mult e

(-:>)  e n = fromJust $ subElem childElems e n 
(-::>) e n = fromJust $ subElem id e n
(-:*>) e n = filter (\x -> (qName $ elName x) == n) $ childElems e
(-:?>) e n = subElem childElems e n
(-:**>) es n = join $ map (-:*> n) es

--
--

trackLength :: Element -> Float
trackLength e = endPos - beginPos
  where
    endPos   = read $ e -:> "trackTopology" -:> "trackEnd"   -|> "pos"
    beginPos = read $ e -:> "trackTopology" -:> "trackBegin" -|> "pos"


-- Extract all connections from a list of railML tracks.
connectionsMap :: [Element] -> Map.Map  (String) (String, String,String,String)
connectionsMap ts = Map.fromList (beginConnections ++ endConnections ++ switches)
  where
    endConnections =   trackNodeConnections "trackEnd"
    beginConnections = trackNodeConnections "trackBegin"

    trackNodeConnections name = catMaybes $ for ts $ \t -> do
      let e = t -:> "trackTopology" -:> name
      connection <- e -:?> "connection"
      return $ connectionInfo name t e connection

    switches = join $ for ts $ \t -> join $ map (switchConnectionss t) $ trackSwitches t
    switchConnectionss t sw = map (connectionInfo "switch" t sw) (sw -:*> "connection")
    trackSwitches t = t -:> "trackTopology" -:*> "connections" -:**> "switch"

    connectionInfo :: String -> Element -> Element -> Element -> (String,(String,String,String,String))
    connectionInfo b track object connection =
      (connection -|> "id", (track -|> "name", object -|> "pos", connection -|> "ref", b))

-- List doubly linked connections as pairs of Loc (track name + position),
-- where the order of the pair is such that the connection goes in the
-- direction of increasing position (the "up" direction).
upConnections :: Map.Map String (String,String,String,String) -> [(Loc,Loc)]
upConnections cs = catMaybes $ for (Map.toList cs) $ \(id,(track,pos,ref,typ)) -> do
  (othertrack, otherpos, otherref, othertyp) <- Map.lookup ref cs
  if isUp typ othertyp then return ((track,read pos),(othertrack,read otherpos))
  else Nothing
  where 
    isUp :: String -> String -> Bool
    isUp typ othertyp = (typ == "trackEnd" && othertyp == "switch") || 
                        (typ == "switch" && othertyp == "trackBegin")

-- Locs at begin/end of track
trackEnds :: Element -> [Loc]
trackEnds e = [(name,0.0),(name,end)]
  where name =  e-|> "name"
        end = trackLength e

diff :: Num a => [a] -> [(a,a,a)]
diff [] = []
diff ls = zip3 (ls) (tail ls) (zipWith (-) (tail ls) ls)

-- Split track into non-branching segments
trackSegments :: Element -> [Loc] -> [(Float,Float,Float)]
trackSegments element allConnections = allConnections |>
  filter (\(n,p) -> n == (element -|> "name"))  |>
  nubBy sameLoc |> map (\(n,p) -> p) |> sort |> diff

-- Same location on track (tolerance 0.5 mm)
sameLoc :: Loc -> Loc -> Bool
sameLoc (tx,px) (ty,py) = tx == ty && abs (px-py) < 0.5e-3

-- Convert list of tracks into a graph (edge list representation)
-- where node names are sets of track locations (Loc) and
-- edges refer to nodes by any element of the location set.
trackGraph :: [Element] -> ([[Loc]],[(Loc,Loc,Float)])
trackGraph tracks = (nodes, edges)
  where 
    allNodes :: [Loc]
    allNodes = (join $ map (\(x,y) -> [x,y]) connections) ++ 
               (join $ map trackEnds tracks)

    nodes :: [[Loc]]
    nodes = equivalenceClasses (sameLoc .||. isConnected) allNodes

    connections :: [(Loc,Loc)]
    connections = upConnections $ connectionsMap tracks

    edges :: [(Loc,Loc,Float)]
    edges = join $ for tracks $ \t -> for (trackSegments t allNodes) $ 
      \(a,b,x) -> ((t -|> "name" ,a),(t -|> "name" ,b),x)

    isConnected :: Loc -> Loc -> Bool
    isConnected x y = any (\(z,w) -> (x `sameLoc` z) && (y `sameLoc` w))
       (connections ++ map swap connections)

-- Flatten the graph representation from `trackGraph` above by picking
-- a name for each node (set of track locations) and
-- translating the edges' node references by searching through the location sets.
nameOutput :: ([[Loc]], [(Loc,Loc,Float)]) -> ([String], [(String,String,Float)])
nameOutput (nodes,edges) = (nodeNames, namedEdges)
  where
    nodeNames  = map snd namedClasses
    namedEdges = map (\(x,y,l) -> (findName x, findName y, l)) edges

    namedClasses = zip nodes names
      where  
        names = map (\(s,p) -> s ++ "_" ++ (show p)) $ zip trackNames pos
        trackNames = map (\x ->  fst $  x !! 0) nodes
        pos = map (\x -> snd $  x !! 0) nodes

    findName :: Loc -> String
    findName x = snd $ head $ filter (\k -> x `elem` (fst k)) namedClasses

-- Read railML from argument file name, print graph representation.
main :: IO ()
main = do
  args <- getArgs
  contents <- B.readFile (head args)
  let xml = onlyElems $ parseXML contents
  let tracks = xml -::> "railml" -:> "infrastructure" -:> "tracks" -:*> "track"
  let graph = nameOutput $ trackGraph tracks
  putStrLn $ show graph
