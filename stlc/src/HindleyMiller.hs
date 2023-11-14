{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module HindleyMiller where
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as Set
import Syntax
import Control.Applicative
import Data.Functor (($>))

labelGuard :: Bool -> e -> Either e ()
labelGuard True _  = return ()
labelGuard False e = Left e


instance Functor Type where

    fmap = liftM

instance Applicative Type where

    pure = TyVar

    (<*>) = ap

instance Monad Type where

    return = pure

    (TyVar v) >>= f = f v
    Bool >>= _ = Bool
    (Arrow a b) >>= f = Arrow (a >>= f) (b >>= f)

type Monotype v = Type (InstVar v)
data Polytype v = Monotype (Monotype v)
                | ForAll (Polytype (Maybe v))
                deriving (Eq, Functor)

toNormalType :: Monotype String -> Type String
toNormalType t = t >>= pure . normalize (minFresh t)
    where
        minFresh (TyVar (Fresh v)) = v
        minFresh (Arrow a b) = min (minFresh a) (minFresh b)
        minFresh _ = 0

        normalize n (Fresh i) = "x" ++ show (i - n + 1)
        normalize _ (Plain x) = x

instance Show (Type (InstVar String)) where

    show = show . toNormalType

liftSubst :: ((InstVar a) -> Monotype b) -> InstVar (Maybe a) -> Monotype (Maybe b)
liftSubst _ (Plain Nothing) = pure (Plain Nothing)
liftSubst f (Plain (Just v)) = (Just <$>) <$> (f (Plain v))
liftSubst _ (Fresh i) = pure (Fresh i)

subst t Nothing = pure t
subst _ (Just v) = pure v

applySubst :: ((InstVar a) -> Monotype b) -> Polytype a -> Polytype b
applySubst f (Monotype t) = Monotype $ t >>= f
applySubst f (ForAll t) = ForAll $ applySubst (liftSubst f) t

data InstVar v = Plain v
               | Fresh Int
               deriving (Show, Eq, Ord, Functor)

instSubst :: InstVar v -> InstVar (Maybe v) -> Monotype v
instSubst t (Plain Nothing) = pure t
instSubst _ (Plain (Just v)) = pure (Plain v)
instSubst _ (Fresh i) = pure $ Fresh i

newtype InferState = InferState { freshVar :: Int }

data TypeError w v
    = UnknownVariable w (Env w v)
    | UnificationFailed (Term w) (Monotype v) (Monotype v)
    | OccursFailure (Monotype v) (Monotype v)
    deriving (Eq)

type Error w v = Either (TypeError w v)
type Subst v = InstVar v -> Monotype v
type S w e = StateT InferState (Error w e)

type Env w v = M.Map w (Polytype v)

substEnv :: Subst v -> Env w v -> Env w v
substEnv s env = M.map (applySubst s) env

newvar :: S w e (InstVar v)
newvar = do
    i <- gets freshVar
    modify $ \s -> s { freshVar = i + 1 }
    return $ Fresh i


instantiate :: Polytype v -> S w e (Monotype v)
instantiate (Monotype t) = return t
instantiate (ForAll t) = do
    x <- newvar
    t' <- instantiate t
    return $ t' >>= (instSubst x)

generalize :: (Ord v) => Env w v -> Monotype v -> Polytype v
generalize env t = go toGeneralize (Monotype t)
    where

        go [] t = t
        go (v:vs) t = go vs $ ForAll (applySubst (doSubst v) t)

        doSubst v x | x == v = pure $ Plain Nothing
                    | otherwise = pure $ Just <$> x

        contextVars = Set.unions $ map getFreeVars $ M.elems env
        termVars = getFreeVars (Monotype t)
        toGeneralize = Set.toList $ Set.difference termVars contextVars

        getFreeVars :: (Ord v) => Polytype v -> Set.Set (InstVar v)
        getFreeVars = freeVars Set.singleton

        freeVars :: (Ord u) => (InstVar v -> Set.Set (InstVar u)) -> Polytype v -> Set.Set (InstVar u)
        freeVars provide (Monotype t) = freeVars' provide t
        freeVars provide (ForAll t) = freeVars (liftProvide provide) t

        freeVars' provide (TyVar v) = provide v
        freeVars' provide (Arrow a b) = Set.union (freeVars' provide a) (freeVars' provide b)
        freeVars' _ Bool = Set.empty

        liftProvide :: (InstVar v -> Set.Set (InstVar u)) -> InstVar (Maybe v) -> Set.Set (InstVar u)
        liftProvide _ (Plain Nothing) = Set.empty
        liftProvide provide (Plain (Just v)) = provide (Plain v)
        liftProvide provide (Fresh i) = provide (Fresh i)
        


makeSubst :: (Eq v) => InstVar v -> Monotype v -> Subst v
makeSubst v t x | x == v = t
                | otherwise = pure x

guardOccurs :: (Eq v) => (InstVar v) -> Monotype v -> Monotype v -> Error w v ()
guardOccurs x (TyVar v) t = labelGuard (x /= v) $ OccursFailure (TyVar x) t
guardOccurs x (Arrow a b) t = do
    guardOccurs x a t
    guardOccurs x b t
guardOccurs _ Bool _ = return ()

unify :: (Eq v) => Term w -> Monotype v -> Monotype v -> Error w v (Subst v)
unify _ (TyVar a) (TyVar b) | a == b = return pure
unify _ (TyVar a) b = guardOccurs a b b $> makeSubst a b
unify _ a (TyVar b) = guardOccurs b a a $> makeSubst b a
unify _ Bool Bool = return pure
unify t (Arrow a a') (Arrow b b') = do
    s <- unify t a b
    s' <- unify t (a' >>= s) (b' >>= s)
    return $ s >=> s'
unify t a b = Left $ UnificationFailed t a b

runTypeCheck :: Term String -> Either (TypeError String String) (Type String)
runTypeCheck term = do
    (t, _) <- evalStateT (typeCheck M.empty term) (InferState 0)
    return $ toNormalType t
typeCheck :: (Ord w, Ord v) => Env w v -> Term w -> S w v (Monotype v, Subst v)
typeCheck env (Var v) = case M.lookup v env of
    Just t -> do
        t' <- instantiate t
        return (t', pure)
    Nothing -> lift $ Left $ UnknownVariable v env
typeCheck env (App a b) = do
    (ta, s) <- typeCheck env a
    let env' = substEnv s env
    (tb, s') <- typeCheck env' b
    t <- pure <$> newvar
    s'' <- lift $ unify a (ta >>= s) (Arrow tb t)
    return (t >>= s'', s >=> s' >=> s'')
typeCheck env (Abs x _ b) = do
    t <- newvar
    (t', s) <- typeCheck (M.insert x (Monotype $ pure t) env) b
    return $ (Arrow (s t) t', s)
typeCheck _ (BoolLit _) = return (Bool, pure)
typeCheck env (If c t e) = do
    s <- typeFind env c Bool
    let env' = substEnv s env
    (tt, s') <- typeCheck env' t
    let env'' = substEnv s' env'
    s'' <- typeFind env'' e tt
    return (tt >>= s'', s >=> s' >=> s'')
typeCheck env (Let x v b) = do
    (t, s) <- typeCheck env v
    let env' = substEnv s env
    let tg = generalize env' t
    let env'' = M.insert x tg env'
    (t', s') <- typeCheck env'' b
    return (t', s >=> s')

typeFind :: (Ord w, Ord v) => Env w v -> Term w -> Monotype v -> S w v (Subst v)
typeFind env a t = do
    (t', s) <- typeCheck env a
    s' <- lift $ unify a (t >>= s) t'
    return $ s >=> s'