module Language.Feather.TypeChecker.Substitution where
  import Data.Map ( Map )
  import Data.Set ( Set )
  import Language.Feather.TypeChecker.Type ( Type )

  type Substitution = Map Int Type

  class Types a where
    free  :: a -> Set String
    apply :: Substitution -> a -> a