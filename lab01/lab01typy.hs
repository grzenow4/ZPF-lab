{-# LANGUAGE FlexibleInstances, FunctionalDependencies,
  GADTs, KindSignatures, UndecidableInstances #-}

data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

zero  = undefined :: Zero
one   = undefined :: One
two   = undefined :: Two
three = undefined :: Three
four  = undefined :: Four

class Add a b c | a b -> c where
  add :: a -> b -> c
  add = undefined
instance              Add  Zero    b  b
instance Add a b c => Add (Succ a) b (Succ c)

class Mul a b c | a b -> c where
  mul :: a -> b -> c
  mul = undefined
instance                           Mul  Zero    b Zero
instance (Mul a b c, Add b c d) => Mul (Succ a) b d

class Fac a b | a -> b where
  fac :: a -> b
  fac = undefined
instance                                Fac  Zero    One
instance (Fac a b, Mul (Succ a) b c) => Fac (Succ a) c

data Vec :: * -> * -> * where
  VNil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a

vhead :: Vec (Succ n) a -> a
vhead (x :> xs) = x

vtail :: Vec (Succ n) a -> Vec n a
vtail (x :> xs) = xs

vlast :: Vec (Succ n) a -> a
vlast (x :> VNil) = x
vlast (_ :> xs@(_ :> _)) = vlast xs

class Iso a b where
  iso :: a -> b
  osi :: b -> a

instance Iso a a where
  iso = id
  osi = id

instance Iso ((a,b)->c) (a->b->c) where
  iso = curry
  osi = uncurry

instance (Iso a b) => Iso [a] [b] where
  iso = map iso
  osi = map osi

instance Iso (a->b->c) (b->a->c) where
  iso = flip
  osi = flip

instance (Functor f, Iso a b) => Iso (f a) (f b) where
  iso = fmap iso
  osi = fmap osi

instance (Monad m, Iso a b) => Iso (m a) (m b) where
  iso = flip (>>=) (\x -> return (iso x))
  osi = flip (>>=) (\x -> return (osi x))
