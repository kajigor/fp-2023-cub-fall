module ProofTree where

import Syntax
import qualified Data.Map as M
import Control.Monad (guard, liftM2, join)
import Data.Functor (($>))
import Data.Traversable (sequence)
import Text.Printf (printf)
import Data.List (intercalate)

labelGuard :: Bool -> e -> Either e ()
labelGuard True _  = return ()
labelGuard False e = Left e

type Env = M.Map String Type
type Error = String

unionWithM :: (Monad m, Ord key) => (val -> val -> m val) -> M.Map key val -> M.Map key val -> m (M.Map key val)
unionWithM f m1 m2 = sequence $ M.unionWith (\xm ym -> join $ liftM2 f xm ym) (M.map return m1) (M.map return m2)

combineContexts' :: Env -> Env -> Maybe Env
combineContexts' = unionWithM (\a b -> guard (a == b) $> a)

combineContexts :: Env -> Env -> Either Error Env
combineContexts a b = case (combineContexts' a b) of
    Just env -> return env
    Nothing -> Left $ concat ["Incompatible contexts: ", show a, " and ", show b]

validateVariable :: String -> Type -> Env -> Either Error ()
validateVariable x t e = case M.lookup x e of
    Just t' -> labelGuard (t == t') $ concat ["Expected variable ", show x, " in enviromnent ", show e, " to have type ", show t]
    Nothing -> return ()

data ProofTree = PT { context :: Env, proofTerm :: Term String, resultType :: Type, ruleName :: String, subproofs :: [ProofTree] }

axiom :: String -> Type -> Either Error ProofTree
axiom v t = return $ PT { context = M.singleton v t, proofTerm = Var v, resultType = t, ruleName = "\\text{Ax}", subproofs = [] }

boolAxiom :: Bool -> Either Error ProofTree
boolAxiom b = return $ PT { context = M.empty, proofTerm = BoolLit b, resultType = Bool, ruleName = "\\text{Bool}", subproofs = [] }

abstraction :: String -> Type -> ProofTree -> Either Error ProofTree
abstraction x t b = do
    validateVariable x t (context b)
    return $ PT 
        { context = M.delete x (context b)
        , proofTerm = Abs x t (proofTerm b)
        , resultType = Arrow t (resultType b)
        , ruleName = "\\to\\text{I}"
        , subproofs = [b]
        }

application :: ProofTree -> ProofTree -> Either Error ProofTree
application x@(PT { proofTerm = m, resultType = t1 }) y@(PT { proofTerm = n, resultType = t2 }) = case t1 of
    Arrow t11 t12 | t2 == t11 -> do
                        env <- combineContexts (context x) (context y)
                        return $ PT 
                            { context = env
                            , proofTerm = App m n
                            , resultType = t12
                            , ruleName = "\\to\\text{E}"
                            , subproofs = [x, y]
                            }
                  | otherwise -> Left $ concat ["Invalid argument type: expected ", show t11, " as input type for ", show m, ", got ", show n, " : ", show t2]
    _ -> Left $ concat ["Expected ", show m, " to be an arrow type, got ", show t1]


tif :: ProofTree -> ProofTree -> ProofTree -> Either Error ProofTree
tif x@(PT {proofTerm = c, resultType = ct}) y@(PT {proofTerm = t, resultType = tt}) z@(PT {proofTerm = e, resultType = et}) = do
    labelGuard (ct == Bool) $ concat ["Expected condition to be Bool, got ", show c, " : ", show ct]
    labelGuard (tt == et) $ concat ["Expected branches to match, got ", show t, " : ", show tt, " and ", show e, " : ", show et]
    env' <- combineContexts (context x) (context y)
    env <- combineContexts env' (context z)
    return $ PT
        { context = env
        , proofTerm = If c t e
        , resultType = tt
        , ruleName = "\\text{T-If}"
        , subproofs = [x, y, z]
        }

tlet :: String -> ProofTree -> ProofTree -> Either Error ProofTree
tlet var x@(PT { proofTerm = v, resultType = t }) y@(PT {proofTerm = b, resultType = bt}) = do
    validateVariable var t (context y)
    env <- combineContexts (context x) (M.delete var (context y))
    return $ PT 
        { context = env
        , proofTerm = Let var v b
        , resultType = bt
        , ruleName = "\\text{let}"
        , subproofs = [x, y]
        }

displayType :: Type -> String
displayType (TyVar v) = v
displayType (Arrow t1@(Arrow _ _) t2) = printf  "(%s) \\to %s" (displayType t1) (displayType t2)
displayType (Arrow t1 t2) = printf  "%s \\to %s" (displayType t1) (displayType t2)
displayType (Bool) = "Bool"

displayTerm :: Term String -> String
displayTerm (Var x) = x
displayTerm (Abs x typ t) = printf "\\lambda %s^{\\color{blue}%s}.%s" x (displayType typ) (displayTerm t)
displayTerm (App t1 t2) = printf "(%s) (%s)" (displayTerm t1) (displayTerm t2)
displayTerm (BoolLit b) = printf "%s" (show b)
displayTerm (If c t e) = printf "\\text{if } %s \\text{ then } %s \\text{ else } %s" (displayTerm c) (displayTerm t) (displayTerm e)
displayTerm (Let x v t) = printf "\\text{let } %s = %s \\text{ in } %s" x (displayTerm v) (displayTerm t)

displayContext :: Env -> String
displayContext env = go (M.toList env)
    where
        go e = intercalate "," (map (\(x, t) -> printf "%s\\ :\\ %s" x (displayType t)) e)

displayProof :: ProofTree -> String
displayProof p = printf "\\infer[%s]{%s\\vdash %s\\ :\\ %s}{%s}" (ruleName p) (displayContext $ context p) (displayTerm $ proofTerm p) (displayType $ resultType p) (concat $ map displayProof (subproofs p))

printProof :: ProofTree -> String
printProof p = unlines 
    [ "\\documentclass[9pt]{article}"
    , "\\usepackage{amssymb,amsmath,pgfplots,proof}"
    , "\\usepackage[left=3cm, right=3cm]{geometry}"
    , "\\begin{document}"
    , "\\["
    , displayProof p
    , "\\]"
    , "\\end{document}"
    ]