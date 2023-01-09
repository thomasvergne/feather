module Language.Feather.Parser.Modules.Literal where
  import Language.Feather.CST.Expression ( Expression(..) )
  import Control.Applicative ( Alternative((<|>)) )
  import Language.Feather.CST.Literal ( Literal(..) )

  import qualified Text.Parsec as P
  import qualified Language.Feather.Parser.Lexer as L
  import qualified Text.Parsec.Token as Token

  literal :: Monad m => L.Feather m Expression
  literal = L.locate $ ELiteral <$> literal'

  literal' :: Monad m => L.Parser m Literal
  literal' = P.try floatLiteral
          <|> intLiteral
          <|> stringLiteral
          <|> charLiteral

  intLiteral :: Monad m => L.Parser m Literal
  intLiteral = IntLit <$> L.integer
  
  floatLiteral :: Monad m => L.Parser m Literal
  floatLiteral = FloatLit <$> Token.float L.lexer

  stringLiteral :: Monad m => L.Parser m Literal
  stringLiteral = StringLit <$> Token.stringLiteral L.lexer

  charLiteral :: Monad m => L.Parser m Literal
  charLiteral = CharLit <$> Token.charLiteral L.lexer