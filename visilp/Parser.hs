module Parser where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (join)

type Identifier = String
type Mileage = Double

data Side = SwLeft | SwRight
  deriving (Show, Eq, Ord)

data Dir = Incoming | Outgoing
  deriving (Show, Eq, Ord)

data NodeType = NodeStart | NodeEnd
  | NodeSwitch Dir Side  | NodeVertical
  deriving (Show, Eq, Ord)

type PortRef = (Identifier, Port)

data Port = PortIn | PortOut | PortTrunk | PortLeft | PortRight | PortTop | PortBottom
  deriving (Show, Eq, Ord)

data Statement
 = NodeStmt Identifier NodeType Mileage 
 | EdgeStmt PortRef PortRef
  deriving (Show, Eq, Ord)

parseFile :: String -> IO (Either String [Statement])
parseFile fn = do
  contents <- readFile fn
  return (parseStmts fn contents)

parseStmts :: String -> String -> Either String [Statement]
parseStmts name contents = case MP.parse (sc >> some statement <* eof) name contents of
  (Left err) -> Left $ parseErrorPretty err
  (Right statements) -> Right statements

 
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

number :: Parser Double
number =  (try (lexeme L.float)) <|> (do x <- lexeme L.decimal ; return (fromIntegral x))

integer :: Parser Int
integer = lexeme L.decimal

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many bodyChar)
  where bodyChar = alphaNumChar <|> (char '_')

statement :: Parser Statement
statement = node <|> edge

node :: Parser Statement
node = do
  symbol "node"
  name <- identifier
  ty <- nodetype
  pos <- number
  return (NodeStmt name ty pos)

portref :: Parser PortRef
portref = do
  a <- identifier
  symbol "."
  a_port <- port
  return (a,a_port)

port :: Parser Port
port = (symbol "out" >> return PortOut) <|>
       (symbol "in" >> return PortIn) <|>
       (symbol "left" >> return PortLeft) <|>
       (symbol "right" >> return PortRight) <|>
       (symbol "trunk" >> return PortTrunk) <|>
       (symbol "top" >> return PortTop) <|>
       (symbol "bottom" >> return PortBottom)

nodetype :: Parser NodeType
nodetype = (symbol "start" >> return NodeStart) <|>
           (symbol "end" >> return NodeEnd) <|>
           (symbol "vertical" >>   (return NodeVertical)) <|>
           (symbol "inleftsw" >>   (return $ NodeSwitch Incoming SwLeft))   <|>
           (symbol "inrightsw" >>  (return $ NodeSwitch Incoming SwRight))  <|>
           (symbol "outleftsw" >>  (return $ NodeSwitch Outgoing SwLeft))   <|>
           (symbol "outrightsw" >> (return $ NodeSwitch Outgoing SwRight)) 

edge :: Parser Statement
edge = do
  symbol "edge"
  a <- portref
  b <- portref
  return (EdgeStmt a b)
