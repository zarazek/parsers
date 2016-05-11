{-# LANGUAGE ExistentialQuantification #-}

module Data.Opaque (Opaque, toOpaque, fromOpaque) where
import Unsafe.Coerce (unsafeCoerce)

data Opaque = forall a. Opaque a

toOpaque :: a -> Opaque
toOpaque = Opaque

fromOpaque :: Opaque -> a
fromOpaque (Opaque x) = unsafeCoerce x
