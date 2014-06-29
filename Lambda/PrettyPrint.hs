{- |
Module      :  Lambda.PrettyPrint
Description :  Pretty printing of types and typed expressions.
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

instance PrettyPrint TExpr where
    format (Fix (TExpr t (EVar n))) = parens (format n <+> colon <+> format t)
    format (Fix (TExpr t (ELit lit))) = parens (format lit <+> colon <+> format t)
    format (Fix (TExpr t (EAbs n e))) = parens ("λ" <> format n <> "." <> format e) <+> colon <+> format t
    format (Fix (TExpr t (EApp e1 e2))) = parens (format e1 <+> format e2) <+> colon <+> format t
    format (Fix (TExpr t (ELet n e1 e2))) =   parens (hang ("let" <+> format n <+> "=" <+> format e1 <+> "in") 1 (format e2))
                                          <+> colon <+> format t

instance PrettyPrint IError where
    format (TUnificationError t1 t2) = "Error: cannot unify" $$ nest 4 ("type:" <+> format t1) $$ nest 4 ("with:" <+> format t2)
    format (TOccursCheckError n t) = "Error: occurs check failed."
    format (TUnboundVariable n) = "Error: unbound variable" <+> format n
