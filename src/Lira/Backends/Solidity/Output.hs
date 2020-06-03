--- MIT License
--
-- Copyright (c) 2020 eToroX Labs
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lira.Backends.Solidity.Output where

import           Data.Aeson hiding (Array)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics

import           Lira.Version (versionText)

-- | Serialize a 'ToJSON' value as 'Text'.
encodeUtf8 :: ToJSON a => a -> Text
encodeUtf8 = decodeUtf8 . toStrict . encode

---------- solc output
data Contract = Contract
  { contractName              :: Text
  , contractAbi               :: Abi
  , contractMetadata          :: Text -- really, json-in-string
  , contractBytecode          :: Text -- "0x..."
  , contractDeployedBytecode  :: Text -- "0x..."
--  , contractSourceMap         :: Text -- "181:3084:3:-;;;389:202;8:9:..."
--  , contractDeployedSourceMap :: Text -- "..."
--  , contractSource            :: Text -- "..." Lira source code
--  , contractSourcePath        :: FilePath -- "/Users/sshine/...lir"
--  , contractAst               :: ...
--  , contractLegacyAst         :: ...
  , contractCompiler          :: Compiler
--  , contractNetworks          :: ...
--  , contractSchemaVersion     :: Version (3.1.0?)
--  , contractUpdatedAt         :: DateTime
--  , contractDevdoc            :: ...
--  , contractUserdoc           :: ...
  } deriving (Generic, Show)

data Compiler = Compiler
  { compilerName    :: Text
  , compilerVersion :: Text
  } deriving (Generic, Show)

-- TODO: Move out if/when this becomes standalone ABI library
liraCompiler :: Compiler
liraCompiler = Compiler
  { compilerName = "lira"
  , compilerVersion = versionText
  }

---------- ABI v0.6.6

-- https://solidity.readthedocs.io/en/v0.6.6/abi-spec.html#json

-- Constructor and fallback function never have name or outputs.
-- Fallback function doesn’t have inputs either.

data Abi = Abi
  { abiType            :: ContractType
  , abiName            :: Text
  , abiInputs          :: [ContractInputOutput]
  , abiOutputs         :: [ContractInputOutput]
  , abiStateMutability :: StateMutability
  } deriving (Generic, Show)

data ContractType
  = FunctionType
  | ConstructorType
  | ReceiveType
  | FallbackType
  deriving (Generic, Show)

data StateMutability
  = Pure
  | View
  | Nonpayable
  | Payable
  deriving (Generic, Show)

data ContractInputOutput = ContractInputOutput
  { contractInputOutputName       :: Text
  , contractInputOutputType       :: Type
  , contractInputOutputComponents :: [Component]
  } deriving (Generic, Show)

data Event = Event
  { eventType      :: EventType
  , eventName      :: Text
  , eventInputs    :: [EventInput]
  , eventAnonymous :: Bool
  } deriving (Generic, Show)

data Type
  = Uint256           -- uint256
  | Tuple [Component] -- tuple
  | Array Type        -- t[]
  deriving (Generic, Show)

data Component = Component
  { componentName :: Text
  , componentType :: Type
  } deriving (Generic, Show)

data EventType = EventType
  deriving (Generic, Show)

data EventInput = EventInput
  { eventInputName       :: Text
  , eventInputType       :: Type
  , eventInputComponents :: [Component]
  , eventInputIndexed    :: Bool
  } deriving (Generic, Show)

------------

instance ToJSON Abi where
  toJSON Abi{..} = object
    [ "type"            .= abiType
    , "name"            .= abiName
    , "inputs"          .= abiInputs
    , "outputs"         .= abiOutputs
    , "stateMutability" .= abiStateMutability
    ]

instance ToJSON ContractType where
  toJSON contractType = toJSON $ case contractType of
    FunctionType -> "function"
    ConstructorType -> "constructor"
    ReceiveType -> "receive"
    FallbackType -> "fallback" :: Text

instance ToJSON StateMutability where
  toJSON stateMutability = toJSON $ case stateMutability of
    Pure -> "pure"
    View -> "view"
    Nonpayable -> "nonpayable"
    Payable -> "payable" :: Text

instance ToJSON ContractInputOutput where
  toJSON ContractInputOutput{..} = object
    [ "name"       .= contractInputOutputName
    , "type"       .= contractInputOutputType
    , "components" .= contractInputOutputComponents
    ]

instance ToJSON Type where
  toJSON = toJSON . prettyType
    where
      prettyType :: Type -> Text
      prettyType Uint256 = "uint256"
      prettyType (Tuple _) = "tuple"
      prettyType (Array t) = prettyType t <> "[]"

instance ToJSON Component where
  toJSON Component{..} = object
    [ "name" .= componentName
    , "type" .= componentType
    ]

instance ToJSON Compiler where
  toJSON Compiler{..} = object
    [ "name"    .= compilerName
    , "version" .= compilerVersion
    ]
