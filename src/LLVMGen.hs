{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module LLVMGen where

import           Definition

import           Control.Monad.State
import           Data.Map
import           Data.Text           as T

import qualified LLVM
import           LLVM.AST
import           LLVM.AST.Global

emptyModule :: Text -> Module
emptyModule label = defaultModule { moduleName = fromText label }

appendDefn :: Definition -> State Module ()
appendDefn def = modify $ \s ->
  s { moduleDefinitions = moduleDefinitions s ++ [def] }

define ::  Type -> Text -> [(Type, Name)] -> [BasicBlock] -> State Module ()
define retT funcName argtys body =
  appendDefn $ GlobalDefinition
  functionDefaults { name        = genName funcName
                   , parameters  = (genParam <$> argtys, False)
                   , returnType  = retT
                   , basicBlocks = body
                   }

external ::  Type -> Text -> [(Type, Name)] -> State Module ()
external retT funcName argtys =
  appendDefn $ GlobalDefinition
  functionDefaults { name        =  genName funcName
                   , parameters  = (genParam <$> argtys, False)
                   , returnType  = retT
                   , basicBlocks = []
                   }


emptyBlock i = BlockState { index = i
                          , stack = []
                          , term = Nothing}

uniqueName :: Text -> Names -> (Name, Names)
uniqueName name names =
  maybe
  (genName name,  insert name 1 names)
  (\ix -> (genName $ name <> tShow ix
          , insert name (ix+1) names))
  $ names !? name

addBlock :: Text -> State CodegenState Name
addBlock blkname = do
  CodegenState { blocks = blocks
               , blockCount = blockCount
               , names = names } <- get
  let new = emptyBlock blockCount
      (mangledname, supply) = uniqueName blkname names
    in do
    modify $ \s -> s { blocks = insert mangledname (emptyBlock blockCount) blocks
                     , names = supply
                     , blockCount = blockCount + 1 }
    pure mangledname


genName :: Text -> Name
genName = Name . fromText

genParam :: (Type, Name) -> Parameter
genParam (ty, nm) = Parameter ty nm []
