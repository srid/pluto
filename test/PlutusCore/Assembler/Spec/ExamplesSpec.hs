{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module PlutusCore.Assembler.Spec.ExamplesSpec (tests) where

import           Data.Function
import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range
import           PlutusCore.Assembler.FFI
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Spec.Prelude
import qualified PlutusCore.Assembler.Types.AST    as AST

-- FFIs must be declared before tests
hello :: AST.Program ()
hello = $(plutoProgram "examples/hello.pluto")
$(plutoImport 'hello
  "defaultGreeting" [t|String|])
$(plutoImport 'hello
  "greet" [t|String -> String -> String|])

tests :: TestTree
tests =
  testGroup
    "examples"
    [ helloTest
    ]

helloTest :: TestTree
helloTest =
  testGroup
    "hello.pluto"
    [ testProperty "accepts diverse greetings" . property $ do
        greeting <- forAll someText
        name <- forAll someText
        greet greeting name === greeting <> ", " <> name
    , testProperty "default greeting stays" . property $ do
        name <- forAll someText
        greet defaultGreeting name === "Hello" <> ", " <> name
    ]
  where
    someText = Gen.string (Range.linear 3 9) Gen.alpha

