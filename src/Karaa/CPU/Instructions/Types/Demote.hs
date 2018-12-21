{-# LANGUAGE TemplateHaskell #-}
module Karaa.CPU.Instructions.Types.Demote ( Demote(..), mkDemoteInstances ) where

import Data.Proxy
import Language.Haskell.TH

class Demote (a :: k) where
    demote :: proxy a -> k

instance Demote '() where
    demote _ = ()

instance (Demote a, Demote b) => Demote '(a, b) where
    demote (_ :: proxy '(a, b)) = (a, b)
        where
            a = demote (Proxy :: Proxy a)
            b = demote (Proxy :: Proxy b)

namesInCon :: Con -> Q [Name]
namesInCon (NormalC n [])     = return [n]
namesInCon (RecC n [])        = return [n]
namesInCon (ForallC _ _ c)    = namesInCon c
namesInCon (GadtC ns [] _)    = return ns
namesInCon (RecGadtC ns [] _) = return ns
namesInCon _                  = fail "Can't provide Demote instance for non-enumeration types."

mkDemoteInstance :: Name -> DecQ
mkDemoteInstance conN = head <$> [d| instance Demote $conT where demote _ = $conE |]
    where
        conE = return (ConE conN)
        conT = return (PromotedT conN)

mkDemoteInstances :: Name -> DecsQ
mkDemoteInstances name = reify name >>= \case
    TyConI (DataD _ _ _ _ cons _) -> concat <$> mapM go cons
    _ -> fail "Can't provide Demote instance for anything but a `data`-type."
    where
        go con = mapM mkDemoteInstance =<< namesInCon con