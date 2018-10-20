--
-- Copyright 2018 Includable.
-- Licensed under MIT license.
--

module AbsRholang where

newtype Var = Var String deriving (Eq, Ord, Show, Read)
data Contr = DContr Proc
  deriving (Eq, Ord, Show, Read)

data Proc
    = PNil
    | PValue Value
    | PDrop Chan
    | PLift Chan [Proc]
    | PInput [Bind] Proc
    | PChoice [CBranch]
    | PMatch Proc [PMBranch]
    | PNew [Var] Proc
    | PConstr Var [Proc]
    | PContr Var [CPattern] Proc
    | PPar Proc Proc
  deriving (Eq, Ord, Show, Read)

data Chan = CVar Var | CQuote Proc
  deriving (Eq, Ord, Show, Read)

data Bind
    = InputBind CPattern Chan
    | InputDBind CPattern Chan
    | CondInputBind CPattern Chan Proc
  deriving (Eq, Ord, Show, Read)

data PMBranch = PatternMatch PPattern Proc
  deriving (Eq, Ord, Show, Read)

data CBranch = Choice [Bind] Proc
  deriving (Eq, Ord, Show, Read)

data RhoBool = QTrue | QFalse
  deriving (Eq, Ord, Show, Read)

data Quantity
    = QBool RhoBool
    | QInt Integer
    | QDouble Double
    | QString String
    | QVar Var
    | QMap
    | QDot Quantity Var [Quantity]
    | QNeg Quantity
    | QMult Quantity Quantity
    | QDiv Quantity Quantity
    | QAdd Quantity Quantity
    | QMinus Quantity Quantity
    | QLt Quantity Quantity
    | QLte Quantity Quantity
    | QGt Quantity Quantity
    | QGte Quantity Quantity
    | QEq Quantity Quantity
    | QNeq Quantity Quantity
  deriving (Eq, Ord, Show, Read)

data Value = VQuant Quantity | EChar Char | ETuple [Proc]
  deriving (Eq, Ord, Show, Read)

data VarPattern = VarPtVar Var | VarPtWild
  deriving (Eq, Ord, Show, Read)

data PPattern = PPtVal ValPattern | PPtVar VarPattern
  deriving (Eq, Ord, Show, Read)

data CPattern
    = CPtVar VarPattern | CValPtrn ValPattern | CPtQuote PPattern
  deriving (Eq, Ord, Show, Read)

data ValPattern
    = VPtTuple [PPattern]
    | VPtTrue
    | VPtFalse
    | VPtInt Integer
    | VPtDbl Double
    | VPtNegInt Integer
    | VPtNegDbl Double
    | VPtStr String
  deriving (Eq, Ord, Show, Read)

