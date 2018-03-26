module TrainPlan.Parser where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import TrainPlan.Routes
import TrainPlan.UsagePattern


-- Not used by planner, but recognized by parser
data SwitchPosition = SwLeft | SwRight | SwUnknown

parseRoutesFile :: String -> IO (Either String [Route])
parseRoutesFile fn = do
  contents <- readFile fn
  return (parseRoutes fn contents)

parseUsageFile :: String -> IO (Either String UsagePattern)
parseUsageFile fn = do
  contents <- readFile fn
  return (parseUsage fn contents)

parseRoutes :: String -> String -> Either String [Route]
parseRoutes name contents = case MP.parse (sc >> some route <* eof) name contents of
  (Left err) -> Left $ parseErrorPretty err
  (Right routes) -> Right $ routes

parseUsage :: String -> String -> Either String UsagePattern
parseUsage name contents = case MP.parse (sc >> some usageStatement <* eof) name contents of
  (Left err) -> Left $ parseErrorPretty err
  (Right stmts) -> Right $ usageToStructured stmts

usageToStructured :: [UsageStatement] -> UsagePattern
usageToStructured = foldl f (UsagePattern [] [] [])
  where
    f :: UsagePattern -> UsageStatement -> UsagePattern
    f (UsagePattern vs ms ts) (VehicleStmt v) = (UsagePattern (v:vs) ms ts)
    f (UsagePattern vs ms ts) (MovementStmt m) = (UsagePattern vs (m:ms) ts)
    f (UsagePattern vs ms ts) (TimingStmt t) = (UsagePattern vs ms (t:ts))

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

data UsageStatement = 
    VehicleStmt Vehicle
  | MovementStmt MovementSpec
  | TimingStmt TimingSpec
  deriving (Show)

usageStatement :: Parser UsageStatement
usageStatement = vehicleStmt <|> movementStmt <|> timingStmt

list :: Parser a -> Parser [a]
list =  (between (symbol ("[")) (symbol ("]"))) . (\x -> sepBy x (symbol ","))
vehicleStmt :: Parser UsageStatement
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

optname :: Parser (Maybe String)
optname = optional $ do 
  char '#'
  identifier 

visit :: Parser (Maybe String, [NodeRef], Maybe WaitTime)
visit = do
  symbol "visit"
  name <- optname
  locations <- list identifier
  waittime <- optional $ do
    symbol "wait"
    number
  return (name, locations, waittime)

movementStmt :: Parser UsageStatement
movementStmt = do
  symbol "movement"
  vehicle <- identifier
  symbol "{"
  visits <- some visit
  symbol "}"
  return (MovementStmt (MovementSpec vehicle visits))

enterExit :: String -> Parser (Maybe String, [NodeRef])
enterExit n = do
  symbol n
  name <- optname
  locations <- list identifier
  -- velocity <- optional $ do
  --   symbol "velocity"
  --   number
  return (name, locations)

timingStmt :: Parser UsageStatement
timingStmt = do
  symbol "timing"
  refA <- identifier
  refB <- identifier
  timeDiff <- optional number
  let dt = fromMaybe 0.0 timeDiff
  return (TimingStmt (TimingSpec refA refB dt))

swpos :: Parser SwitchPosition
swpos =     (symbol "left"  >> return SwLeft) 
        <|> (symbol "right" >> return SwRight)

release :: Parser Release
release = do
  symbol "release"
  symbol "{"
  symbol "length"
  length <- number
  symbol "trigger"
  trigger <- identifier
  symbol "resources"
  res <- list identifier
  symbol "}"
  return (Release length res)

routePoint :: Parser RoutePoint
routePoint = bdry <|> sig <|> end
  where
    bdry = do
      symbol "boundary"
      id <- identifier
      return (RoutePointBoundary id)
    sig = do
      symbol "signal"
      id <- identifier
      return (RoutePointSignal id)
    end = do
      symbol "trackend"
      return RoutePointTrackEnd

route :: Parser Route
route = modelEntry <|> modelExit <|> trainRoute

modelEntry :: Parser Route
modelEntry = do
  symbol "modelentry"
  name <- identifier
  symbol "from"
  bdry <- identifier
  symbol "{"
  symbol "exit"
  sig <- identifier
  symbol "length"
  length <- number
  symbol "}"
  let releases = []
  let contains = []
  return (Route name (RoutePointBoundary bdry)
                     (RoutePointSignal sig)
                length releases contains)

modelExit :: Parser Route
modelExit = do
  symbol "modelexit"
  name <- identifier
  symbol "to"
  bdry <- identifier
  symbol "{"
  symbol "entry"
  sig <- identifier
  optional $ do
    symbol "entrysection"
    entrysection <- identifier
    return ()
  symbol "length"
  length <- number
  symbol "}"
  let releases = []
  let contains = []
  return (Route name (RoutePointSignal sig)
                     (RoutePointBoundary bdry)
                length releases contains)

trainRoute :: Parser Route
trainRoute = do
  symbol "route"
  name <- identifier
  symbol "{"
  symbol "entry"
  entry <- identifier 
  symbol "exit"
  exit <- identifier
  optional $ do
    symbol "entrysection"
    entrysection <- identifier
    return ()
  symbol "length"
  length <- number
  sections <- optional $ do 
    symbol "sections"
    list identifier
  swpos <- optional $ do
    symbol "switches"
    list $ do
      symbol "("
      swref <- identifier
      symbol ","
      pos <- swpos
      symbol ")"
      return (swref,pos)
  releases <- many release
  contains <- optional $ do
     symbol "contains"
     list identifier
  symbol "}"
  return (Route name (RoutePointSignal entry)
                     (RoutePointSignal exit)
                           length releases (fromMaybe [] contains))
