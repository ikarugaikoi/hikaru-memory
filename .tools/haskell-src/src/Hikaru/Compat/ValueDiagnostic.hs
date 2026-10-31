{-# LANGUAGE OverloadedStrings #-}
-- | Contextual diagnostics for Python operations on JSON-backed values.
-- Keep the receiver and operation at the failure site: a bare TypeError
-- cannot distinguish indexing, iteration, containment, and assignment.
module Hikaru.Compat.ValueDiagnostic
  ( typeName, missingAttribute, notIterable, notContained, badStringIndex
  , badItemAssignment, unhashable, missingKey, invalidInteger, unsupportedOrdering ) where
import qualified Data.Vector as V
import Hikaru.Compat.Diagnostic (Diagnostic(..))
import Hikaru.Compat.Json
import Hikaru.Compat.Render (pyRepr)

typeName :: Value -> PyString
typeName Null="NoneType"
typeName (Bool _)="bool"
typeName (Number (PyInteger _))="int"
typeName (Number _)="float"
typeName (String _)="str"
typeName (Array _)="list"
typeName (Object _)="dict"

missingAttribute :: PyString -> Value -> Diagnostic
missingAttribute name value=Diagnostic "AttributeError" (pyStringConcat
  ["'",typeName value,"' object has no attribute '",name,"'"])
notIterable :: Value -> Diagnostic
notIterable value=Diagnostic "TypeError" (pyStringConcat["'",typeName value,"' object is not iterable"])
notContained :: Value -> Diagnostic
notContained value=Diagnostic "TypeError" (pyStringConcat["argument of type '",typeName value,"' is not iterable"])
badStringIndex :: Value -> Diagnostic
badStringIndex (Array _)=Diagnostic "TypeError" "list indices must be integers or slices, not str"
badStringIndex (String _)=Diagnostic "TypeError" "string indices must be integers, not 'str'"
badStringIndex value=Diagnostic "TypeError" (pyStringConcat["'",typeName value,"' object is not subscriptable"])
badItemAssignment :: Value -> Diagnostic
badItemAssignment value@(Array _)=badStringIndex value
badItemAssignment value=Diagnostic "TypeError" (pyStringConcat["'",typeName value,"' object does not support item assignment"])
unhashable :: Value -> Diagnostic
unhashable value=Diagnostic "TypeError" (pyStringConcat["unhashable type: '",typeName value,"'"])
missingKey :: PyString -> Diagnostic
missingKey key=Diagnostic "KeyError" (pyRepr(String key))
invalidInteger :: PyString -> Diagnostic
invalidInteger value=Diagnostic "ValueError" (pyStringConcat["invalid literal for int() with base 10: ",
  pyStringFromChars(take 200(pyStringToChars(pyRepr(String value))))])

-- A list comparison reports the types of the first unequal elements whose
-- comparison fails, rather than the types of the two outer lists.
unsupportedOrdering :: PyString -> Value -> Value -> Diagnostic
unsupportedOrdering operation (Array xs) (Array ys)=go(V.toList xs)(V.toList ys)
  where
    go (x:restX) (y:restY)
      | pyEqual x y=go restX restY
      | otherwise=unsupportedOrdering operation x y
    go _ _=Diagnostic "TypeError" "unsupported ordering"
unsupportedOrdering operation left right=Diagnostic "TypeError" (pyStringConcat
  ["'",operation,"' not supported between instances of '",typeName left,"' and '",typeName right,"'"])
