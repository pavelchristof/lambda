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

import Data.IORef
import Data.Text.Lazy (unpack)
import Data.Functor.Foldable (Fix(..))
import Text.PrettyPrint
import qualified Data.Map as Map

import Lambda.Name
import Lambda.Type
import Lambda.Object
import Lambda.Syntax
import Lambda.Errors
import Lambda.SourceLoc

class PrettyPrint t where
    format :: t -> Doc

instance PrettyPrint Doc where
    format = id

instance PrettyPrint Name where
    format (Name s) = text . unpack $ s

instance PrettyPrint Type where
    format (TVar n) = format n
    format (TLit n) = format n
    format (TFun t1 t2) = parens (format t1 <+> "->" <+> format t2)
    format (TList t) = brackets (format t)

instance PrettyPrint Literal where
    format (LitChar c) = char c
    format (LitString s) = doubleQuotes . text . unpack $ s
    format (LitInteger i) = integer i
    format (LitReal d) = text (show d)
    format LitEmptyList = "[]"

instance PrettyPrint LExpr where
    format (Fix (EVar _ n)) = format n
    format (Fix (ELit _ lit)) = format lit
    format (Fix (EAbs _ n e)) = parens ("λ" <> format n <> "." <> format e)
    format (Fix (EApp _ e1 e2)) = parens (format e1 <+> format e2)
    format (Fix (ELet _ n e1 e2)) = hang ("let" <+> format n <+> "=" <+> format e1 <+> "in") 1 (format e2)
    format (Fix (ECase _ e cases)) = "case" <+> format e <+> "of" <+> braces (vcat $ punctuate semi (map format cases))

dcolon :: Doc
dcolon = colon <> colon

instance PrettyPrint TLExpr where
    format (Fix (EVar (_, t) n)) = parens (format n <+> dcolon <+> format t)
    format (Fix (ELit (_, t) lit)) = parens (format lit <+> dcolon <+> format t)
    format (Fix (EAbs (_, t) n e)) = parens ("λ" <> format n <> "." <> format e) <+> dcolon <+> format t
    format (Fix (EApp (_, t) e1 e2)) = parens (format e1 <+> format e2) <+> dcolon <+> format t
    format (Fix (ELet (_, t) n e1 e2)) =   parens (hang ("let" <+> format n <+> "=" <+> format e1 <+> "in") 1 (format e2))
                                       <+> dcolon <+> format t
    format (Fix (ECase (_, t) e cases)) = parens ( "case" <+> format e <+> "of" <+> braces (vcat $ punctuate semi (map format cases)))
                                       <+> dcolon <+> format t


instance PrettyPrint expr => PrettyPrint (Located Pattern, expr) where
    format (L _ p, e) = format p <+> "->" <+> format e

instance PrettyPrint Pattern where
    format (Decons n args) = format n <+> hsep (map (maybe "_" format . unLoc) args)
    format (Wildcard n) = maybe "_" format n

instance PrettyPrint expr => PrettyPrint (Decl expr) where
    format (DAssign n e) = format n <+> "=" <+> format e
    format (DData n consDefs) = "data" <+> format n <+> "=" <+> hsep (punctuate (char '|') (map (format . unLoc) consDefs))

instance PrettyPrint ConsDef where
    format (ConsDef n t) = format n <+> hsep (map format t)

instance PrettyPrint (Decl expr) => PrettyPrint [Decl expr] where
    format = vcat . map ((<> semi) . format)

instance PrettyPrint SourceRange where
    format (SourceRange (SourceLoc l c) (SourceLoc l' c')) =   "line" <+> int l <> ", column" <+> int c
                                                           <+> "to line" <+> int l' <> ", column" <+> int c'
    format (NoRange) = empty

errorAt :: SourceRange -> Doc
errorAt pos = "Error at" <+> format pos <> "."

instance PrettyPrint IError where
    format (UnificationError pos t1 t2) =  errorAt pos 
                                        $$ "Cannot unify" 
                                        $$ nest 4 ("type:" <+> format t1) 
                                        $$ nest 4 ("with:" <+> format t2)
                                        $$ ""
    format (OccursCheckError pos n t) =  errorAt pos
                                      $$ "Cannot unify"
                                      $$ nest 4 ("type:" <+> format n)
                                      $$ nest 4 ("with:" <+> format t)
                                      $$ ""
    format (UnboundVariable pos n) =  errorAt pos
                                   $$ "Unbound variable" <+> doubleQuotes (format n) <> "."
                                   $$ ""
    format (UnboundConstructor pos n) =  errorAt pos
                               $$ "Unbound constructor" <+> doubleQuotes (format n) <> "."
                               $$ ""
    format (UnboundType pos n) =  errorAt pos
                               $$ "Unbound type" <+> doubleQuotes (format n) <> "."
                               $$ ""
    format (Redefinition pos n) =  errorAt pos
                                $$ "Redefined" <+> doubleQuotes (format n) <> "."
                                $$ ""
    format (InvalidPattern pos p) =  errorAt pos
                                  $$ "Invalid pattern" <+> doubleQuotes (format p) <> "."
                                  $$ ""
    format (InvalidConstructor pos n) =  errorAt pos
                                      $$ "Invalid constructor" <+> doubleQuotes (format n) <> "."
                                      $$ ""

class PrettyPrintIO t where
    formatIO :: t -> IO Doc

instance PrettyPrintIO (Object e) where
    formatIO (OFun _) = return $ braces "fun"
    formatIO (OThunk l r) = do
        l' <- formatIO l
        r' <- formatIO r
        return $ braces ("thunk" <+> l' <+> r')
    formatIO (OSeq l r) = do
        l' <- formatIO l
        r' <- formatIO r
        return $ braces ("seq" <+> l' <+> r')
    formatIO (OChar c) = return $ quotes (char c)
    formatIO (OInt i) = return $ int i
    formatIO (ODouble d) = return $ double d
    formatIO (OList list) = do
        list' <- mapM formatIO list
        return $ brackets (hcat . punctuate "," $ list')
    formatIO (OCons n list) = do
        list' <- mapM formatIO list
        return $ brackets ( format n <+> (hcat . punctuate ",") list')
    formatIO (OCase e p) = do
        e' <- formatIO e
        return $ brackets ( "case" <+> e' )

instance PrettyPrintIO (LObject e) where
    formatIO ref = do
        obj <- readIORef ref
        case obj of
             Left _ -> return "_|_"
             Right obj' -> formatIO obj'

instance (PrettyPrint a, PrettyPrint k) => PrettyPrint (Map.Map a k) where
    format = Map.foldWithKey (\k a b -> b $$ format k <+> "=" <+> format a) empty
