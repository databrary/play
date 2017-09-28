{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import Debug.Trace
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B

type Name = String
type Type = String
type Version = String

data Value = Value String deriving (Show, Eq)

data Argument = Argument Name Type deriving (Show, Eq)

data Timestamp = Timestamp String String String String deriving (Show, Eq)

data Cell = Cell Int Int [Value] deriving (Show, Eq)

data Column = Column Name Type [Argument] [Cell] deriving (Show, Eq)

data Spreadsheet = Spreadsheet Version [Column] deriving (Show, Eq)

timestampToMs :: Timestamp -> Int
timestampToMs (Timestamp h m s ms) = (read h :: Int) * 60^2 * 1000
                                      + (read m :: Int) * 60 * 1000
                                      + (read s :: Int) * 1000
                                      + (read ms :: Int)

timestampParser :: Parser Timestamp
timestampParser = do
    hours <- count 2 digit
    char ':'
    minutes <- count 2 digit
    char ':'
    seconds <- count 2 digit
    char ':'
    milliseconds <- count 3 digit
    trace(show milliseconds) char ','
    return $ Timestamp hours minutes seconds milliseconds

argParser :: Parser Argument
argParser = do
    Data.Attoparsec.Text.takeWhile (`elem` ",")
    name <- many $ satisfy (`notElem` "\n|")
    char '|'
    colType <- many $ satisfy (`notElem` ",\n")
    return $ Argument name colType

charParser :: Parser Char
charParser = do
    satisfy (`notElem` ",)\\\n") <|> char '\\' *> anyChar

valueParser :: Parser Value
valueParser = do
    value <- many $ charParser
    return $ Value value

cellValueParser :: Parser [Value]
cellValueParser = do
    char '('
    values <- sepBy1 valueParser $ char ','
    char ')'
    return values

cellParser :: Parser Cell
cellParser = do
    onset <- timestampParser
    offset <- timestampParser
    values <- cellValueParser
    return $ Cell (timestampToMs onset) (timestampToMs offset) values

versionParser :: Parser Version
versionParser = do
    version <- many $ notChar '\n'
    char '\n'
    return $ version

columnParser :: Parser Column
columnParser = do
    name <- many $ notChar ' '
    trace(name) char ' '
    char '('
    colType <- many $ notChar ','
    trace(colType) char ','
    many $ notChar ')'
    char ')'
    char '-'
    args <- many $ argParser
    trace(show args) char '\n'
    cells <- many $ cellParser <* endOfLine
    return $ Column name colType args cells

fileParser :: Parser Spreadsheet
fileParser = do
    version <- versionParser
    columns <- many $ columnParser
    return $ Spreadsheet version columns

----------------------
---------- MAIN --------
------------------------
main :: IO ()
main = do
    file <- T.readFile "/Users/jesse/Desktop/CrawlingObstacles-102-1/testing"
    print "Parsing"
    print $ parseOnly fileParser file
