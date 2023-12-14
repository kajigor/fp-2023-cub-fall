module Inference where

import Syntax
import qualified Data.Map as M
import qualified Data.Set as S

type Subst = String -> Mono
type Env = M.Map String Poly

type Mono = Type
data Poly = Mono Mono | Forall String Poly

inferenceEmpty :: Term String -> Maybe Type
inferenceEmpty term = do
    (res, _, _) <- inference term M.empty (varsGenerator (getLongestVarLength term + 1) "")
    return res

getLongestVarLength :: Term String -> Int
getLongestVarLength (Var x) = length x
getLongestVarLength (Abs x _ l) = max (length x) (getLongestVarLength l)
getLongestVarLength (App l r) = max (getLongestVarLength l) (getLongestVarLength r)
getLongestVarLength (BoolLit _) = 0
getLongestVarLength (If c t e) = max (getLongestVarLength c) (max (getLongestVarLength t) (getLongestVarLength e))
getLongestVarLength (Let x e1 e2) = max (length x) (max (getLongestVarLength e1) (getLongestVarLength e2))

varsGenerator :: Int -> String -> [String]
varsGenerator 0 str = str : varsGenerator 0 ('a' : str)
varsGenerator i str = varsGenerator (i - 1) ('a' : str)

inference :: Term String -> Env -> [String] -> Maybe (Type, Subst, [String])
inference (Var x) env newVars = case M.lookup x env of
    Just poly -> do
        let (t, newVars') = instantiate poly newVars
        return (t, TyVar, newVars')
    Nothing -> Nothing
inference (Abs x _ l) env (var : newVars) = do
    let env' = M.insert x (Mono $ TyVar var) env
    inference l env' newVars
inference (App l r) env (var : newVars) = do
    (tl, subst1, newVars') <- inference l env newVars
    let env' = substMap env subst1
    (tr, subst2, newVars'') <- inference r env' newVars'
    let t = TyVar var
    subst' <- unify (substitute tl subst2) (Arrow tr t)
    return (substitute t subst', \str -> (subst1 str `substitute` subst2)  `substitute` subst', newVars'')
inference (BoolLit _) _ newVars = return (Bool, TyVar, newVars)
inference (If c t e) env newVars = do
    (tc, subst, newVars') <- inference c env newVars
    substb <- unify tc Bool

    let substc str = subst str `substitute` substb
    let env' = substMap env substc
    (tt, substt, newVars'') <- inference t env' newVars'
    let env'' = substMap env' substt
    (te, subst, newVars''') <- inference e env'' newVars''
    substu <- unify (substitute tt subst) te

    let subste str = subst str `substitute` substu
    return (substitute tt subste, \str -> (substc str `substitute` substt)  `substitute` subste, newVars''')
inference (Let x e1 e2) env newVars = do
    (tl, subst1, newVars') <- inference e1 env newVars
    let env' = substMap env subst1
    let t' = generalize tl env'
    let env'' = M.insert x t' env'
    (tr, subst2, newVars'') <- inference e2 env'' newVars'
    return (tr, \str -> substitute (subst1 str) subst2, newVars'')
inference _ _ [] = undefined

substMap :: Env -> Subst -> Env
substMap env subst = M.map (substPoly subst) env

substPoly :: Subst -> Poly -> Poly
substPoly subst (Mono mono) = Mono (substitute mono subst)
substPoly subst (Forall x poly) = substPoly (\a -> if x == a then TyVar a else subst a) poly

instantiate :: Poly -> [String] -> (Mono, [String])
instantiate (Mono mono) newVars = (mono, newVars)
instantiate (Forall x poly) (var : newVars) = do
    let (mono, newVars') = instantiate poly newVars
    (substitute mono (\a -> TyVar (if a == x then var else a)), newVars')
instantiate _ [] = undefined

substitute :: Mono -> Subst -> Mono
substitute (TyVar x) f = f x
substitute (Arrow x y) f = Arrow (substitute x f) (substitute y f)
substitute Bool _ = Bool

unify :: Mono -> Mono -> Maybe Subst
unify (TyVar a) (TyVar b) | a == b = return TyVar
unify (TyVar x) b = do
    _ <- checkContains x b
    return (\a -> if a == x then b else TyVar a)
unify b (TyVar x) = do
    _ <- checkContains x b
    return (\a -> if a == x then b else TyVar a)
unify Bool Bool = return TyVar
unify (Arrow a a') (Arrow b b') = do
    subst <- unify a b
    subst' <- unify (substitute a' subst) (substitute b' subst)
    return $ \str -> substitute (subst str) subst'
unify _ _ = Nothing

checkContains :: String -> Mono -> Maybe ()
checkContains s (TyVar x)
    | s == x = Nothing
    | otherwise = Just ()
checkContains s (Arrow x y) = do
    _ <- checkContains s x
    _ <- checkContains s y
    Just ()
checkContains _ Bool = Just ()

generalize :: Mono -> Env -> Poly
generalize t env = foldl (flip Forall) (Mono t) (S.difference (freeTypeVars $ Mono t) (freeEnvVars env))

freeEnvVars :: Env -> S.Set String
freeEnvVars env = S.unions (map freeTypeVars $ M.elems env)

freeTypeVars :: Poly -> S.Set String
freeTypeVars (Forall s poly) = S.delete s (freeTypeVars poly)
freeTypeVars (Mono (TyVar x)) = S.singleton x
freeTypeVars (Mono (Arrow a b)) = S.union (freeTypeVars $ Mono a) (freeTypeVars $ Mono b)
freeTypeVars (Mono Bool) = S.empty