module Main where
  import Language.Feather.Parser.Parser
  import Language.Feather.Pretty.Typed ()
  import Language.Feather.AST.Transform
  import Language.Feather.TypeChecker.Checker

  main :: IO ()
  main = do
    let name = "example/index.feather"
    x <- readFile name
    case parseFeather name x of
      Left err -> print err
      Right ast -> do
        let ast' = removeNilExpressions $ transformExpressions ast
        let ast'' = runChecker ast'
        case ast'' of
          Left err -> print err
          Right ast -> print ast