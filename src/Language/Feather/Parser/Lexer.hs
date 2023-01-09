module Language.Feather.Parser.Lexer where
  import Text.Parsec.Token ( GenTokenParser )
  import Text.Parsec ( alphaNum, letter, ParsecT, oneOf, getPosition )
  import Language.Feather.CST.Literal ( Located(..), Position )
  import Text.Parsec.Expr ( Assoc )
  import Control.Monad.State ( StateT, evalStateT )
  import Control.Monad.Identity ( Identity(runIdentity) )

  import qualified Text.Parsec.Token as Token

  data FeatherState = FeatherState { 
    infixOperators :: [(String, Assoc)]
  }

  type Feather m a = ParsecT String () (StateT FeatherState m) (Located a)
  type Parser m a  = ParsecT String () (StateT FeatherState m) a

  runAssocT :: Monad m => StateT FeatherState m a -> m a
  runAssocT = flip evalStateT (FeatherState [])

  runAssoc :: StateT FeatherState Identity a -> a
  runAssoc = runIdentity . runAssocT

  languageDef :: Monad m => Token.GenLanguageDef String u m
  languageDef =
    Token.LanguageDef {  Token.commentStart    = "/*"
              , Token.commentEnd      = "*/"
              , Token.commentLine     = "//"
              , Token.identStart      = letter
              , Token.caseSensitive   = True
              , Token.nestedComments  = True
              , Token.opStart         = Token.opLetter languageDef
              , Token.opLetter        = oneOf "+-*/%<>=|"
              , Token.identLetter     = alphaNum
              , Token.reservedNames   = ["let", "where", "in", "do", "fun", "if", "then", "else", "match", "with"]
              , Token.reservedOpNames = ["+", "-", "*", "/", "%", "=", "|", ">", "<", "!", "$", "#", "&", "@", "^", ".", "?", ":", ","] }

  operators :: String
  operators = "<>!#$%&.?@^|~:-+*/=,"

  lexer :: Monad m => GenTokenParser String u m
  lexer = Token.makeTokenParser languageDef

  identifier :: Monad m => Parser m String
  identifier = Token.identifier lexer

  reserved :: Monad m => String -> Parser m ()
  reserved = Token.reserved lexer

  reservedOp :: Monad m => String -> Parser m ()
  reservedOp = Token.reservedOp lexer

  parens :: Monad m => Parser m a -> Parser m a
  parens = Token.parens lexer

  integer :: Monad m => Parser m Integer
  integer = Token.natural lexer

  whiteSpace :: Monad m => Parser m ()
  whiteSpace = Token.whiteSpace lexer

  comma :: Monad m => Parser m String
  comma = Token.comma lexer

  commaSep :: Monad m => Parser m a -> Parser m [a]
  commaSep = Token.commaSep lexer

  semi :: Monad m => Parser m String
  semi = Token.semi lexer

  getLoc :: Located a -> Position
  getLoc (_ :>: p) = p

  -- Locate is a function that takes a parser an returns the parser result
  -- with location information

  locate :: Monad m => Parser m a -> Feather m a
  locate p = do
    start <- getPosition
    r <- p
    end <- getPosition
    return (r :>: (start, end))