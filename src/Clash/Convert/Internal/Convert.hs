{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module Clash.Convert.Internal.Convert where

import Clash.Prelude

{- $setup
>>> import Clash.Prelude
>>> import Clash.Convert
-}

{- | Conversions that are, based on their types, guaranteed to succeed.

== __Laws__
A conversion is safe and total if a round trip conversion is guaranteed to be
lossless. I.e.,

> Just x == maybeConvert (convert @a @b x)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an "Integral" instance, this
intuition is captured by:

> toInteger x == toInteger (convert @a @b x)

Instances should make sure their constraints are as \"tight\" as possible. I.e.,
if an instance exist, but the constraints cannot be satisfied, then
'Clash.Convert.convertMaybe' should return 'Nothing' for one or more values in
the domain of the source type @a@:

> L.any isNothing (L.map (maybeConvert @a @b) [minBound ..])

Additionally, any implementation should be translatable to synthesizable RTL.
-}
class Convert a b where
  {- | Convert a supplied value of type @a@ to a value of type @b@. The conversion
  is guaranteed to succeed.

  >>> convert (3 :: Index 8) :: Unsigned 8
  3

  The following will fail with a type error, as we cannot prove that all values
  of @Index 8@ can be represented by an @Unsigned 2@:

  >>> convert (3 :: Index 8) :: Unsigned 2
  ...
  -}
  convert :: a -> b

instance (KnownNat n, KnownNat m, n <= m) => Convert (Index n) (Index m) where
  convert = resize

instance (KnownNat n, KnownNat m, 1 <= n, n <= 2 ^ m) => Convert (Index n) (Unsigned m) where
  convert = resize . bitCoerce

{- | Note: Conversion from @Index 1@ to @Signed 0@ is total, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, 1 <= n, CLog 2 n + 1 <= m) => Convert (Index n) (Signed m) where
  convert = convert . bitCoerce @_ @(Unsigned (CLog 2 n))

instance (KnownNat n, KnownNat m, 1 <= n, n <= 2 ^ m) => Convert (Index n) (BitVector m) where
  convert = resize . pack

instance (KnownNat n, KnownNat m, 1 <= m, 2 ^ n <= m) => Convert (Unsigned n) (Index m) where
  convert = bitCoerce . resize

instance (KnownNat n, KnownNat m, n <= m) => Convert (Unsigned n) (Unsigned m) where
  convert = resize

{- | Note: Conversion from @Unsigned 0@ to @Signed 0@ is total, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, n + 1 <= m) => Convert (Unsigned n) (Signed m) where
  convert = bitCoerce . resize

instance (KnownNat n, KnownNat m, n <= m) => Convert (Unsigned n) (BitVector m) where
  convert = resize . pack

instance (KnownNat n, KnownNat m, n <= m) => Convert (Signed n) (Signed m) where
  convert = resize

instance (KnownNat n, KnownNat m, 1 <= m, 2 ^ n <= m) => Convert (BitVector n) (Index m) where
  convert = unpack . resize

instance (KnownNat n, KnownNat m, n <= m) => Convert (BitVector n) (Unsigned m) where
  convert = unpack . resize

{- | Note: Conversion from @BitVector 0@ to @Signed 0@ is total, but not within the
constraints of the instance.
-}
instance (KnownNat n, KnownNat m, n + 1 <= m) => Convert (BitVector n) (Signed m) where
  convert = unpack . resize

instance (KnownNat n, KnownNat m, n <= m) => Convert (BitVector n) (BitVector m) where
  convert = resize
