
{-# LANGUAGE ExistentialQuantification, TypeApplications, Rank2Types #-} 

module R1CS.Algebra.Fields
  ( module R1CS.Algebra.Class
  , FieldChoice(..) , FieldProxy(..) , selectField , Proxy(..) , runWithField
  , F16 , F20 , F24
  )
  where

--------------------------------------------------------------------------------

import Data.Proxy

import R1CS.Algebra.Class

import R1CS.Algebra.TestField16
import R1CS.Algebra.TestField20
import R1CS.Algebra.TestField24

--------------------------------------------------------------------------------

data FieldChoice 
  = Field16
  | Field20
  | Field24
  deriving Show

data FieldProxy 
  = forall f. Field f => MkFieldProxy (Proxy f)

instance Show FieldProxy where
  show (MkFieldProxy proxy) = "FieldProxy<" ++ show (fieldName proxy) ++ ">"

selectField :: FieldChoice -> FieldProxy
selectField which = case which of
  Field16 -> MkFieldProxy (Proxy @F16)
  Field20 -> MkFieldProxy (Proxy @F20)
  Field24 -> MkFieldProxy (Proxy @F24)

--------------------------------------------------------------------------------

runWithField :: FieldChoice -> (forall field. Field field => Proxy field -> a) -> a
runWithField fieldchoice user = case selectField fieldchoice of
  MkFieldProxy proxy -> user proxy

--------------------------------------------------------------------------------
