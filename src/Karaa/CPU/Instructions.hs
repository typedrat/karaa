module Karaa.CPU.Instructions ( module T ) where

import Data.Proxy
import Data.Word
import Fcf
import GHC.TypeLits
    ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
    ( Nat )
import qualified GHC.TypeNats as TN

import Karaa.CPU.Instructions.Instances
import Karaa.CPU.Instructions.Types as T

import Karaa.Monad

makeInst :: forall m as . (Demote m, DemoteArguments as, ValidInstruction m as) => Instruction m as
makeInst = Inst m as
    where
        m = demote (Proxy :: Proxy m)
        as = demoteArguments (Proxy :: Proxy as)

data AnyInstruction = AnyInstruction (forall m as. Instruction m as)

data (>>) :: Nat -> Nat -> Exp Nat
type instance Eval (a >> b) = TN.Div a (2 TN.^ b)

data Mod :: Nat -> Nat -> Exp Nat
type instance Eval (Mod a b) = TN.Mod a b

data ToOctal :: Nat -> Nat -> Exp Nat
type instance Eval (ToOctal n x) = TN.Mod (TN.Div x (2 TN.^ (3 TN.* n))) 8

type ToX = ToOctal 2
type ToY = ToOctal 1
type ToZ = ToOctal 0
type ToP = Flip (>>) 1 <=< ToY
type ToQ = Flip Mod  2 <=< ToY

data GetReg :: Nat -> Exp (Argument a)
type instance Eval (GetReg i) = RegImpl i

type family RegKind (i :: Nat)  where
    RegKind 0 = Argument (Register "B")

type family RegImpl (i :: Nat) = (r :: RegKind i) | r -> i where
    RegImpl 0 = Reg_8b B
    -- RegImpl 1 = Reg_8b C
    -- RegImpl 2 = Reg_8b D
    -- RegImpl 3 = Reg_8b E
    -- RegImpl 4 = Reg_8b H
    -- RegImpl 5 = Reg_8b L
    -- RegImpl 6 = Indirect (Reg_16b HL)
    -- RegImpl 7 = Reg_8b A

type (==) = TyEq

type (&) = Flip ($)

data FromJust :: ErrorMessage -> Maybe a -> Exp a
type instance Eval (FromJust _msg (Just a)) = a
type instance Eval (FromJust msg Nothing) = TypeError msg

type Cases (value :: k) (cases :: [(k -> Exp Bool, Exp b)]) = 
    Join (
            FromJust (MatchFailedError value cases)
        =<< Map Snd
        =<< Find ((&) value <=< Fst) cases
        )

type MatchFailedError (value :: k) (cases :: [(k -> Exp Bool, Exp b)]) =
        (Text "Type \"" :<>: ShowType value :<>: Text "\" failed to match any of the following cases: " :$$: ShowType cases)

