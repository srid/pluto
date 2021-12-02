{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusCore.Assembler.FFI (plutoProgram, plutoImport) where

import           Data.Data                      (Data, cast)
import qualified Data.Text                      as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     (Lift (lift), dataToExpQ)
import           PlutusCore.Assembler.App
import qualified PlutusCore.Assembler.Assemble  as Assemble
import qualified PlutusCore.Assembler.Build     as B
import qualified PlutusCore.Assembler.Evaluate  as E
import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Types.AST as AST
import           System.IO                      (FilePath)

-- | Embed the AST of a Pluto program into the current module.
plutoProgram :: FilePath -> Q Exp
plutoProgram =
  liftDataWithText <=< loadPlutoMod
  where
    loadPlutoMod :: (MonadIO m, MonadFail m) => FilePath -> m (AST.Program ())
    loadPlutoMod fp = do
      s <- liftIO $ readFile fp
      either (fail . show . ErrorParsing) (pure . void) $ Assemble.parseProgram "<TH>" (T.pack s)

-- | Import a top-level value (or function) from a Pluto program.
plutoImport ::
  -- | The pluto program to import from
  Name ->
  -- | The variable name of the top-level binding to import.
  String ->
  -- | Expected value type.
  Q Type ->
  Q [Dec]
plutoImport prog name qType = do
  type_ <- qType
  functionD name type_ $ \args ->
    [|
      E.evalToplevelBindingToHaskellValueMust
        (fromString name)
        $( listE $
              flip fmap args $ \arg -> do
                [|B.toPluto|] `appE` varE arg
          )
        $(varE prog)
      |]

-- | A simple function (or value) declaration
--
-- Type must be a simple type or a function (arrow) type.
functionD :: String -> Type -> ([Name] -> Q Exp) -> Q [Dec]
functionD name type_ bodyF = do
  args <- lambdaArgsFromType type_
  body <- bodyF args
  let sym = mkName name
  pure
    [ SigD sym type_,
      if null args
        then ValD (VarP sym) (NormalB body) []
        else FunD sym [Clause (VarP <$> args) (NormalB body) []]
    ]
  where
    lambdaArgsFromType :: Type -> Q [Name]
    lambdaArgsFromType = \case
      ConT _ -> do
        pure []
      AppT (AppT ArrowT (ConT _)) rest -> do
        n <- newName "_arg"
        (n :) <$> lambdaArgsFromType rest
      _ ->
        error "functionD: not a valid type"

-- Why? See https://gitlab.haskell.org/ghc/ghc/-/issues/12596#note_169275

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)
