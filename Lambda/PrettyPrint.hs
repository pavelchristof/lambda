{- |
Module      :  Lambda.PrettyPrint
Description :  Pretty printing of expressions, types and errors.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}
 
module Lambda.PrettyPrint where

import Data.Text (unpack)
import Data.Functor.Foldable (Fix(..))
import Text.PrettyPrint
import Text.Parsec.Pos
import Lambda.Name
import Lambda.Type
import Lambda.Syntax
import Lambda.Inference

class PrettyPrint t where
    format :: t -> Doc

instance PrettyPrint Name where
    format (Name s) = text . unpack $ s

instance PrettyPrint Type where
    format (TVar n) = format n
    format (TLit n) = format n
    format (TFun t1 t2) = parens (format t1 <+> "->" <+> format t2)
    format (TList t) = brackets (format t)

instance PrettyPrint Literal where
    format (LitChar c) = char c
    format (LitString s) = text . unpack $ s
    format (LitInteger i) = integer i
    format (LitDouble d) = double d

instance PrettyPrint TPExpr where
    format (Fix (EVar (_, t) n)) = parens (format n <+> colon <+> format t)
    format (Fix (ELit (_, t) lit)) = parens (format lit <+> colon <+> format t)
    format (Fix (EAbs (_, t) n e)) = parens ("λ" <> format n <> "." <> format e) <+> colon <+> format t
    format (Fix (EApp (_, t) e1 e2)) = parens (format e1 <+> format e2) <+> colon <+> format t
    format (Fix (ELet (_, t) n e1 e2)) =   parens (hang ("let" <+> format n <+> "=" <+> format e1 <+> "in") 1 (format e2))
                                          <+> colon <+> format t

instance PrettyPrint SourcePos where
    format pos =  "file" <+> doubleQuotes (text (sourceName pos)) 
               <> ", line" <+> int (sourceLine pos) 
               <> ", column" <+> int (sourceColumn pos)

errorAt :: SourcePos -> Doc
errorAt pos = "Error at" <+> format pos <> "."

instance PrettyPrint IError where
    format (TUnificationError pos t1 t2) =  errorAt pos 
                                         $$ "Cannot unify" 
                                         $$ nest 4 ("type:" <+> format t1) 
                                         $$ nest 4 ("with:" <+> format t2)
    format (TOccursCheckError pos n t) =  errorAt pos
                                       $$ "Occurs check failed."
    format (TUnboundVariable pos n) =  errorAt pos
                                    $$ "Unbound variable" <+> format n
