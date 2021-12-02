{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusCore.Assembler.FFI (plutoProgram, plutoImport, processResult) where

import           Data.Data                      (Data, cast)
import qualified Data.Text                      as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     (Lift (lift), dataToExpQ)
import qualified PlutusCore                     as PLC
import           PlutusCore.Assembler.App
import qualified PlutusCore.Assembler.Assemble  as Assemble
import qualified PlutusCore.Assembler.Build     as B
import qualified PlutusCore.Assembler.Evaluate  as E
import           PlutusCore.Assembler.Prelude
import qualified PlutusCore.Assembler.Types.AST as AST
import           Prelude                        (Foldable (null),
                                                 MonadFail (fail), error, flip,
                                                 readFile)
import           System.IO                      (FilePath)
import qualified UntypedPlutusCore              as UPLC

-- | Embed the AST of a Pluto program into the current module.
plutoProgram :: FilePath -> Q Exp
plutoProgram =
  liftDataWithText <=< loadPlutoMod

-- | Import a top-level value (or function) from a Pluto program.
plutoImport ::
  -- | The pluto program to import from
  Name ->
  -- | The variable name of the top-level binding to import.
  String ->
  -- | Expected value type.
  Q Type
  -> Q [Dec]
plutoImport prog name qType = do
  type_ <- qType
  functionD name type_  $ \args ->
    [|
      processResult
        $ E.evalToplevelBinding
            (fromString name)
            $(listE $ flip fmap args $ \arg -> do
                [|B.toPluto|] `appE` varE arg)
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
  pure [ SigD sym type_
      , if null args
          then ValD (VarP sym) (NormalB body) []
          else FunD sym [Clause (VarP <$> args) (NormalB body) []]
      ]
  where
    lambdaArgsFromType :: Type -> Q [Name]
    lambdaArgsFromType = \case
      ConT _ -> do
        pure []
      AppT (AppT ArrowT (ConT _)) rest ->  do
        n <- newName "_arg"
        (n :) <$> lambdaArgsFromType rest
      _ ->
        error "functionD: not a valid type"


loadPlutoMod :: (MonadIO m, MonadFail m) => FilePath -> m (AST.Program ())
loadPlutoMod fp = do
  s <- liftIO $ readFile fp
  either (fail . show . ErrorParsing) (pure . void) $ Assemble.parseProgram "<TH>" (T.pack s)

processResult :: forall a. B.FromUPLC a => Either Error (UPLC.Term UPLC.Name PLC.DefaultUni PLC.DefaultFun ()) -> a
processResult = \case
  Left err -> error $ show err
  Right t -> case B.fromUPLC @a t
    of Nothing -> error "processResult: failed to convert term"
       Just x  -> x

-- Why? See https://gitlab.haskell.org/ghc/ghc/-/issues/12596#note_169275

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)
