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
    format LitUnit = "()"
    format (LitChar c) = char c
    format (LitString s) = doubleQuotes . text . unpack $ s
    format (LitInteger i) = integer i
    format (LitDouble d) = double d
    format LitEmptyList = "[]"

instance PrettyPrint PExpr where
    format (Fix (EVar _ n)) = format n
    format (Fix (ELit _ lit)) = format lit
    format (Fix (EAbs _ n e)) = parens ("λ" <> format n <> "." <> format e)
    format (Fix (EApp _ e1 e2)) = parens (format e1 <+> format e2)
    format (Fix (ELet _ n e1 e2)) = hang ("let" <+> format n <+> "=" <+> format e1 <+> "in") 1 (format e2)
    format (Fix (EFix _ n e)) = parens ("fix" <+> format n <+> format e)

dcolon :: Doc
dcolon = colon <> colon

instance PrettyPrint TPExpr where
    format (Fix (EVar (_, t) n)) = parens (format n <+> dcolon <+> format t)
    format (Fix (ELit (_, t) lit)) = parens (format lit <+> dcolon <+> format t)
    format (Fix (EAbs (_, t) n e)) = parens ("λ" <> format n <> "." <> format e) <+> dcolon <+> format t
    format (Fix (EApp (_, t) e1 e2)) = parens (format e1 <+> format e2) <+> dcolon <+> format t
    format (Fix (ELet (_, t) n e1 e2)) =   parens (hang ("let" <+> format n <+> "=" <+> format e1 <+> "in") 1 (format e2))
                                       <+> dcolon <+> format t
    format (Fix (EFix (_, t) n e)) =   parens ("fix" <+> format n <+> format e) 
                                     <+> dcolon <+> format t

instance PrettyPrint expr => PrettyPrint (Stmt expr) where
    format (SLet _ n e) = "let" <+> format n <+> "=" <+> format e
    format (SEval _ e) = format e

instance PrettyPrint (Stmt expr) => PrettyPrint [Stmt expr] where
    format = vcat . map ((<> semi) . format)

instance PrettyPrint SourcePos where
    format pos =  "file" <+> doubleQuotes (text (sourceName pos)) 
               <> ", line" <+> int (sourceLine pos) 
               <> ", column" <+> int (sourceColumn pos)

errorAt :: SourcePos -> Doc
errorAt pos = "Error in" <+> format pos <> "."

instance PrettyPrint IError where
    format (UnificationError pos t1 t2) =  errorAt pos 
                                        $$ "Cannot unify" 
                                        $$ nest 4 ("type:" <+> format t1) 
                                        $$ nest 4 ("with:" <+> format t2)
    format (OccursCheckError pos n t) =  errorAt pos
                                      $$ "Cannot unify"
                                      $$ nest 4 ("type:" <+> format n)
                                      $$ nest 4 ("with:" <+> format t)
    format (UnboundVariable pos n) =  errorAt pos
                                   $$ "Unbound variable" <+> doubleQuotes (format n) <> "."
    format (Redefinition pos n) =  errorAt pos
                                $$ "Redefined" <+> doubleQuotes (format n) <> "."
