{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module PlutusCore.Assembler.FFI (plutoProgram, plutoImport, processResult) where 

import Language.Haskell.TH
import PlutusCore.Assembler.Prelude
import System.IO (FilePath, putStrLn)
import Prelude (error, readFile, MonadFail (fail), Foldable (null), flip)
import qualified PlutusCore.Assembler.Assemble as Assemble
import qualified Data.Text as T
import PlutusCore.Assembler.App
import qualified PlutusCore.Assembler.Types.AST as AST
import Language.Haskell.TH.Syntax (Lift (lift), dataToExpQ)
import Data.Data (Data, cast)
import qualified PlutusCore.Assembler.Evaluate as E
import qualified PlutusCore.Assembler.Build as B
import qualified UntypedPlutusCore as UPLC
import qualified PlutusCore as PLC

-- | Embed the AST of a Pluto program into the current module.
plutoProgram :: FilePath -> Q Exp
plutoProgram fp = do
  plutoProg <- loadPlutoMod fp 
  liftDataWithText plutoProg

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
  args <- lambdaArgsFromType type_
  liftIO $ putStrLn "plutoImport: doing stuff"
  -- bFn <- [|AST.Name name|]
  args' <- listE $ flip fmap args $ \arg -> do
    [|B.toPluto|] `appE` (pure $ VarE arg)
  x <- [|E.evalToplevelBinding (fromString name) $(pure args') $(varE prog)|]
  let sig = 
        SigD (mkName name) type_
      body = 
        let 
          bRes = VarE 'processResult `AppE` x
        in bRes
      fun = if null args 
        then ValD (VarP (mkName name)) (NormalB body) []
        else FunD (mkName name) [Clause (VarP <$> args) (NormalB body) []]
  -- pure [psig, pfun, sig, fun]
  pure [sig, fun]

lambdaArgsFromType :: Type -> Q [Name]
lambdaArgsFromType = \case 
  ConT _ -> do
    pure []
  AppT (AppT ArrowT (ConT _)) rest ->  do
    n <- newName "_arg"
    (n :) <$> lambdaArgsFromType rest
  _ ->
    error "clausePatsFor: not a valid type"


loadPlutoMod :: (MonadIO m, MonadFail m) => FilePath -> m (AST.Program ())
loadPlutoMod fp = do
  s <- liftIO $ readFile fp
  either (fail . show . ErrorParsing) (pure . void) $ Assemble.parseProgram "<TH>" (T.pack s)

processResult :: forall a. B.FromUPLC a => Either Error (UPLC.Term UPLC.Name PLC.DefaultUni PLC.DefaultFun ()) -> a
processResult = \case
  Left err -> error $ show err
  Right t -> case B.fromUPLC @a t
    of Nothing -> error "processResult: failed to convert term"
       Just x -> x

-- Why? See https://gitlab.haskell.org/ghc/ghc/-/issues/12596#note_169275

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)