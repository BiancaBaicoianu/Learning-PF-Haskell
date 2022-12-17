{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
-- Scrieti instante ale clasei Functor pentru tipurile de date descrise mai jos.

newtype Identity a = Identity a deriving (Show)
instance Functor Identity where
    --fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity b) = Identity (f b)

data Pair a = Pair a a deriving (Show)
instance Functor Pair where
    --fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x)(f y)

data Constant a b = Constant b deriving (Show)
instance Functor (Constant c) where 
   -- fmap :: (x->y) -> Constant c x -> Constant c y
    fmap f (Constant x) = Constant (f x)

data Two a b = Two a b deriving (Show)
instance Functor (Two x) where
    --fmap ::(x->y)->Two c x ->Two c y
    fmap f (Two x y) = Two x (f y)
 

data Three a b c = Three a b c deriving (Show)
instance Functor (Three d e) where
    fmap f (Three d e g) = Three d e (f g)


data Three' a b = Three' a b b deriving (Show)
instance Functor (Three' x) where
    fmap f (Three' x c d) = Three' x (f c) (f d)


data Four a b c d = Four a b c d deriving (Show)
instance Functor (Four x y z) where
    fmap f (Four x y z t) = Four x y z (f t)


data Four'' a b = Four'' a a a b deriving (Show)
instance Functor (Four'' x) where   
    fmap f (Four'' x y z t) = Four'' x y z (f t)


data Quant a b = Finance | Desk a | Bloor b deriving (Show)
instance Functor (Quant a) where
    fmap f (Desk a) = Desk a
    fmap _ (Finance) = Finance
    fmap f (Bloor b) = Bloor (f b)


-- S-ar putea să fie nevoie să adăugati unele constrângeri la definirea instant,elor


data LiftItOut f a = LiftItOut (f a) deriving (Show)
instance Functor f => Functor (LiftItOut f) where
    fmap f' (LiftItOut g) = LiftItOut (fmap f' g)


data Parappa f g a = DaWrappa (f a) (g a) deriving (Show)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f' (DaWrappa g h) = DaWrappa (fmap f' g)(fmap f' h)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Show)
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f' (IgnoringSomething g h) = IgnoringSomething g (fmap f' h)


data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Show)
instance (Functor g) => Functor(Notorious g o a) where
    fmap f (Notorious h t z) = Notorious h t (fmap f z)


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Show)
instance Functor GoatLord where
    fmap _ (NoGoat) = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats b c d) = MoreGoats (fmap f b) (fmap f c) (fmap f d)

c= fmap (+1) (MoreGoats (MoreGoats (OneGoat 5) (OneGoat 6) (OneGoat 7)) (OneGoat 6) (OneGoat 7))             

data TalkToMe a = Halt | Print String a | Read (String -> a) 
instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) = Read (f.g)




