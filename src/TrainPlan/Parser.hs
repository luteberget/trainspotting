module TrainPlan.Parser where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import TrainPlan.Infrastructure
import TrainPlan.UsagePattern
import TrainPlan.ScreenCoords

type Output = (Infrastructure, UsagePattern, [ScreenCoords])

parseFile :: String -> IO (Either String Output)
parseFile fn = do
  contents <- readFile fn
  return (TrainPlan.Parser.parse fn contents)

parseStdin :: IO (Either String Output)
parseStdin = do
  contents <- getContents
  return (TrainPlan.Parser.parse "<stdin>" contents)

parse :: String -> String -> Either String Output
parse name contents = case MP.parse (sc >> some statement <* eof) name contents of
  (Left err) -> Left $ parseErrorPretty err
  (Right statements) -> Right $ toStructured statements

toStructured :: [Statement] -> Output
toStructured xs = foldl f emptyOutput xs
  where
    f :: Output -> Statement -> Output
    f ((Infrastructure ts ns cs), up, sc) 
      (TrackStmt t) = ((Infrastructure (t:ts) ns cs), up, sc)
    f ((Infrastructure ts ns cs), up, sc)
      (NodeStmt n) = ((Infrastructure ts (n:ns) cs), up, sc)
    f ((Infrastructure ts ns cs), up, sc)
      (ComponentStmt c) = ((Infrastructure ts ns (c:cs), up, sc))
    f (is, (UsagePattern vs ms ts), sc)
      (VehicleStmt v) = (is, (UsagePattern (v:vs) ms ts), sc)
    f (is, (UsagePattern vs ms ts), sc)
      (MovementStmt m) = (is, (UsagePattern vs (m:ms) ts), sc)
    f (is, (UsagePattern vs ms ts), sc)
      (TimingStmt t) = (is, (UsagePattern vs ms (t:ts)), sc)
    f (is, up, sc)
      (ScreenCoordsStmt s) = (is, up, (s:sc))

emptyOutput :: Output
emptyOutput = ((Infrastructure [] [] []),
               (UsagePattern [] [] []),
               [])

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

number :: Parser Double
number = lexeme L.float

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many bodyChar)
  where bodyChar = alphaNumChar <|> (char '-') <|> (char '_')

data Statement = 
    TrackStmt Track
  | NodeStmt Node
  | ComponentStmt Component
  | VehicleStmt Vehicle
  | MovementStmt MovementSpec
  | TimingStmt TimingSpec
  | ScreenCoordsStmt ScreenCoords
  deriving (Show)

statement :: Parser Statement
statement = trackStmt <|> nodeStmt <|> componentStmt  -- infrastructure
  <|> vehicleStmt <|> movementStmt <|> timingStmt -- usagepattern
  <|> screenCoordsStmt -- screen coordinates

list :: Parser a -> Parser [a]
list =  (between (symbol ("[")) (symbol ("]"))) . (\x -> sepBy x (symbol ","))

trackStmt :: Parser Statement
trackStmt = do
  symbol "track"
  name <- identifier
  l <- (number :: Parser Double)
  return (TrackStmt (Track name l))

nodeStmt :: Parser Statement
nodeStmt = do
  symbol "node"
  from <- list identifier
  to <- list identifier
  return (NodeStmt (Node from to))

componentStmt :: Parser Statement
componentStmt = signalStmt <|> detectorStmt

signalStmt :: Parser Statement
signalStmt = do
  symbol "signal"
  name <- identifier
  loc <- directionalLocation 
  return (ComponentStmt (Signal name loc))

detectorStmt :: Parser Statement
detectorStmt = do
  symbol "detector"
  loc <- location
  return (ComponentStmt (Detector loc))

location :: Parser Location 
location = do
  symbol "("
  ref <- identifier
  symbol ","
  l <- number
  symbol ")"
  return (Location ref l)

directionalLocation :: Parser (Location, Direction)
directionalLocation = do
  symbol "("
  ref <- identifier
  symbol ","
  l <- number
  symbol ","
  dir <- direction
  symbol ")"
  return ((Location ref l), dir)

direction :: Parser Direction
direction =     (symbol "up" >> return Up) 
            <|> (symbol "down" >> return Down)

vehicleStmt :: Parser Statement
vehicleStmt = vehicle >>= return . VehicleStmt

vehicle :: Parser Vehicle
vehicle = do
  symbol "vehicle"
  name <- identifier
  symbol "length"
  l <- number
  symbol "accel"
  a <- number
  symbol "brake"
  b <- number
  symbol "maxspeed"
  vmax <- number
  return (Vehicle name l a b vmax)

visit :: Parser (Maybe String, [DirectionalLocation], Maybe Double)
visit = do
  symbol "visit"
  name <- optional $ do 
     char '#'
     identifier
  locations <- list directionalLocation
  waittime <- optional $ do
    symbol "wait"
    number
  return (name, locations, waittime)

movementStmt :: Parser Statement
movementStmt = do
  symbol "movement"
  vehicle <- identifier
  symbol "{"
  enter <- enterExit "enter"
  visits <- some visit
  exit <- enterExit "exit"
  symbol "}"
  return (MovementStmt (MovementSpec vehicle enter visits exit))

enterExit :: String -> Parser ([DirectionalLocation], Maybe ConstVelocity)
enterExit n = do
  symbol n
  locations <- list directionalLocation
  velocity <- optional $ do
    symbol "velocity"
    number
  return (locations, velocity)

timingStmt :: Parser Statement
timingStmt = do
  symbol "timing"
  refA <- identifier
  refB <- identifier
  timeDiff <- number
  return (TimingStmt (TimingSpec refA refB timeDiff))

coords :: Parser (Double, Double)
coords = do
  symbol "("
  x <- number
  symbol ","
  y <- number
  symbol ")"
  return (x,y)

screenCoordsStmt :: Parser Statement
screenCoordsStmt = do
  symbol "coords"
  loc <- location
  c <- coords
  return (ScreenCoordsStmt (ScreenCoords loc c))

