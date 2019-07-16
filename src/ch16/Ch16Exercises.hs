{-# LANGUAGE FlexibleInstances #-}

module Ch16Exercises where

data More a b = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L a (f b) a' 
    fmap f (R b a b') = R (f b) a (f b')

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk v) = Desk v
    fmap f (Bloor b) = Bloor (f b)

data K a b = K a

instance Functor (K x) where
    fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap g (Flip (K x)) = Flip $ K (g x)

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst x) where
    fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut fa) = LiftItOut $ fmap g fa

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa ga) = IgnoringSomething fa $ fmap f ga

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt

data List a = Nil | Cons a (List a) 

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a l) = Cons (f a) $ fmap f l


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap  f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats x y z) = MoreGoats (liftedF x) (liftedF y) (liftedF z)
        where liftedF = fmap f


data TalkToMe a = Halt | Print String a | Read (String -> a)
        
instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s $ f a
    fmap f (Read g) = Read $ f . g
