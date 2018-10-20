--
-- Copyright 2018 Includable.
-- Licensed under MIT license.
--

{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PrintRholang where

import AbsRholang
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Var where
  prt _ (Var i) = doc (showString i)
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Contr where
  prt i e = case e of
    DContr proc -> prPrec i 0 (concatD [prt 1 proc])

instance Print Proc where
  prt i e = case e of
    PNil -> prPrec i 4 (concatD [doc (showString "Nil")])
    PValue value -> prPrec i 4 (concatD [prt 0 value])
    PDrop chan -> prPrec i 3 (concatD [doc (showString "*"), prt 0 chan])
    PLift chan procs -> prPrec i 2 (concatD [prt 0 chan, doc (showString "!"), doc (showString "("), prt 0 procs, doc (showString ")")])
    PInput binds proc -> prPrec i 1 (concatD [doc (showString "for"), doc (showString "("), prt 0 binds, doc (showString ")"), doc (showString "{"), prt 0 proc, doc (showString "}")])
    PChoice cbranchs -> prPrec i 1 (concatD [doc (showString "select"), doc (showString "{"), prt 0 cbranchs, doc (showString "}")])
    PMatch proc pmbranchs -> prPrec i 1 (concatD [doc (showString "match"), prt 0 proc, doc (showString "with"), prt 0 pmbranchs])
    PNew vars proc -> prPrec i 1 (concatD [doc (showString "new"), prt 0 vars, doc (showString "in"), prt 1 proc])
    PConstr var procs -> prPrec i 1 (concatD [prt 0 var, doc (showString "("), prt 0 procs, doc (showString ")")])
    PContr var cpatterns proc -> prPrec i 1 (concatD [doc (showString "contract"), prt 0 var, doc (showString "("), prt 0 cpatterns, doc (showString ")"), doc (showString "="), doc (showString "{"), prt 0 proc, doc (showString "}")])
    PPar proc1 proc2 -> prPrec i 0 (concatD [prt 0 proc1, doc (showString "|"), prt 1 proc2])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Proc] where
  prt = prtList

instance Print Chan where
  prt i e = case e of
    CVar var -> prPrec i 0 (concatD [prt 0 var])
    CQuote proc -> prPrec i 0 (concatD [doc (showString "@"), prt 3 proc])

instance Print Bind where
  prt i e = case e of
    InputBind cpattern chan -> prPrec i 0 (concatD [prt 0 cpattern, doc (showString "<-"), prt 0 chan])
    InputDBind cpattern chan -> prPrec i 0 (concatD [prt 0 cpattern, doc (showString "<="), prt 0 chan])
    CondInputBind cpattern chan proc -> prPrec i 0 (concatD [prt 0 cpattern, doc (showString "<-"), prt 0 chan, doc (showString "if"), prt 0 proc])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [Bind] where
  prt = prtList

instance Print PMBranch where
  prt i e = case e of
    PatternMatch ppattern proc -> prPrec i 0 (concatD [prt 0 ppattern, doc (showString "=>"), doc (showString "{"), prt 0 proc, doc (showString "}")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [PMBranch] where
  prt = prtList

instance Print CBranch where
  prt i e = case e of
    Choice binds proc -> prPrec i 0 (concatD [doc (showString "case"), prt 0 binds, doc (showString "=>"), doc (showString "{"), prt 0 proc, doc (showString "}")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [CBranch] where
  prt = prtList

instance Print RhoBool where
  prt i e = case e of
    QTrue -> prPrec i 0 (concatD [doc (showString "true")])
    QFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print Quantity where
  prt i e = case e of
    QBool rhobool -> prPrec i 7 (concatD [prt 0 rhobool])
    QInt n -> prPrec i 7 (concatD [prt 0 n])
    QDouble d -> prPrec i 7 (concatD [prt 0 d])
    QString str -> prPrec i 7 (concatD [prt 0 str])
    QVar var -> prPrec i 7 (concatD [prt 0 var])
    QMap -> prPrec i 7 (concatD [doc (showString "Map()")])
    QDot quantity var quantitys -> prPrec i 6 (concatD [prt 7 quantity, doc (showString "."), prt 0 var, doc (showString "("), prt 0 quantitys, doc (showString ")")])
    QNeg quantity -> prPrec i 5 (concatD [doc (showString "-"), prt 6 quantity])
    QMult quantity1 quantity2 -> prPrec i 4 (concatD [prt 4 quantity1, doc (showString "*"), prt 5 quantity2])
    QDiv quantity1 quantity2 -> prPrec i 4 (concatD [prt 4 quantity1, doc (showString "/"), prt 5 quantity2])
    QAdd quantity1 quantity2 -> prPrec i 3 (concatD [prt 3 quantity1, doc (showString "+"), prt 4 quantity2])
    QMinus quantity1 quantity2 -> prPrec i 3 (concatD [prt 3 quantity1, doc (showString "-"), prt 4 quantity2])
    QLt quantity1 quantity2 -> prPrec i 2 (concatD [prt 2 quantity1, doc (showString "<"), prt 3 quantity2])
    QLte quantity1 quantity2 -> prPrec i 2 (concatD [prt 2 quantity1, doc (showString "<="), prt 3 quantity2])
    QGt quantity1 quantity2 -> prPrec i 2 (concatD [prt 2 quantity1, doc (showString ">"), prt 3 quantity2])
    QGte quantity1 quantity2 -> prPrec i 2 (concatD [prt 2 quantity1, doc (showString ">="), prt 3 quantity2])
    QEq quantity1 quantity2 -> prPrec i 1 (concatD [prt 1 quantity1, doc (showString "=="), prt 2 quantity2])
    QNeq quantity1 quantity2 -> prPrec i 1 (concatD [prt 1 quantity1, doc (showString "!="), prt 2 quantity2])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Quantity] where
  prt = prtList

instance Print Value where
  prt i e = case e of
    VQuant quantity -> prPrec i 0 (concatD [prt 0 quantity])
    EChar c -> prPrec i 0 (concatD [prt 0 c])
    ETuple procs -> prPrec i 0 (concatD [doc (showString "["), prt 0 procs, doc (showString "]")])

instance Print VarPattern where
  prt i e = case e of
    VarPtVar var -> prPrec i 0 (concatD [prt 0 var])
    VarPtWild -> prPrec i 0 (concatD [doc (showString "_")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [VarPattern] where
  prt = prtList

instance Print [PPattern] where
  prt = prtList

instance Print PPattern where
  prt i e = case e of
    PPtVal valpattern -> prPrec i 4 (concatD [prt 0 valpattern])
    PPtVar varpattern -> prPrec i 4 (concatD [prt 0 varpattern])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print CPattern where
  prt i e = case e of
    CPtVar varpattern -> prPrec i 0 (concatD [prt 0 varpattern])
    CValPtrn valpattern -> prPrec i 0 (concatD [prt 0 valpattern])
    CPtQuote ppattern -> prPrec i 0 (concatD [doc (showString "@"), prt 3 ppattern])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [CPattern] where
  prt = prtList

instance Print ValPattern where
  prt i e = case e of
    VPtTuple ppatterns -> prPrec i 0 (concatD [doc (showString "["), prt 0 ppatterns, doc (showString "]")])
    VPtTrue -> prPrec i 0 (concatD [doc (showString "true")])
    VPtFalse -> prPrec i 0 (concatD [doc (showString "false")])
    VPtInt n -> prPrec i 0 (concatD [prt 0 n])
    VPtDbl d -> prPrec i 0 (concatD [prt 0 d])
    VPtNegInt n -> prPrec i 0 (concatD [doc (showString "-"), prt 0 n])
    VPtNegDbl d -> prPrec i 0 (concatD [doc (showString "-"), prt 0 d])
    VPtStr str -> prPrec i 0 (concatD [prt 0 str])

instance Print [Var] where
  prt = prtList

