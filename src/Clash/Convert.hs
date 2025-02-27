{-# LANGUAGE CPP #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.Convert (Convert (..), MaybeConvert (..)) where

import Clash.Prelude

{- | Safe, total conversions between various Clash number types. A conversion is
safe and total if a round trip conversion is guaranteed to be lossless. I.e.,

> Just x == maybeConvert (convert @a @b x)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an "Integral" instance, this
intuition is captured by:

> toInteger x == toInteger (convert @a @b x)

Instances should make sure their constraints are as \"tight\" as possible. I.e.,
if an instance exist, but the constraints cannot be satisfied, then 'convertMaybe'
should return 'Nothing' for one or more values in the domain of the source type
@a@:

> L.any isNothing (L.map (maybeConvert @a @b) [minBound ..])

Additionally, any implementation should be translatable to synthesizable RTL.
-}
class Convert a b where
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

{- | Safe conversions between various Clash number types. A conversion is safe
if a round trip conversion does not produce errors (also see "Clash.XException").
I.e.,

> x == fromMaybe x (maybeConvert @a @b x >>= maybeConvert @b @a)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an "Integral" instance, this
intuition is captured by:

> toInteger x == fromMaybe (toInteger x) (toInteger (convert @a @b x))

If a conversion succeeds one way, it should also succeed the other way. I.e.,

> isJust (maybeConvert @a @b x) `implies` isJust (maybeConvert @b @a =<< maybeConvert @a @b x)

A conversion should succeed if and only if the value is representable in the
target type. For types that have a "Bounded" and "Integral" instance, this
intuition is captured by:

> isJust (maybeConvert @a @b x) == (i x >= i (minBound @b) && i x <= i (maxBound @b))

where @i = toInteger@.

Additionally, any implementation should be translatable to synthesizable RTL.
-}
class MaybeConvert a b where
  maybeConvert :: a -> Maybe b

instance (KnownNat n, KnownNat m) => MaybeConvert (Index n) (Index m) where
  maybeConvert = maybeResize

instance (KnownNat n, KnownNat m, 1 <= n) => MaybeConvert (Index n) (Unsigned m) where
  maybeConvert = maybeResize . bitCoerce @_ @(Unsigned (CLog 2 n))

instance (KnownNat n, KnownNat m, 1 <= n) => MaybeConvert (Index n) (Signed m) where
  maybeConvert = maybeConvert . bitCoerce @_ @(Unsigned (CLog 2 n))

instance (KnownNat n, KnownNat m, 1 <= n) => MaybeConvert (Index n) (BitVector m) where
  maybeConvert = maybeResize . pack

instance (KnownNat n, KnownNat m) => MaybeConvert (Unsigned n) (Index m) where
  maybeConvert = maybeResize . bitCoerce @_ @(Index (2 ^ n))

instance (KnownNat n, KnownNat m) => MaybeConvert (Unsigned n) (Unsigned m) where
  maybeConvert = maybeResize

instance (KnownNat n, KnownNat m) => MaybeConvert (Unsigned n) (Signed m) where
  maybeConvert = maybeResize . bitCoerce @(Unsigned (n + 1)) . extend

instance (KnownNat n, KnownNat m) => MaybeConvert (Unsigned n) (BitVector m) where
  maybeConvert = maybeResize . pack

instance (KnownNat n, KnownNat m) => MaybeConvert (Signed n) (Index m) where
  maybeConvert n
    | n < 0 = Nothing
    | otherwise = maybeResize (bitCoerce @_ @(Index (2 ^ (n + 1))) (extend n))

instance (KnownNat n, KnownNat m) => MaybeConvert (Signed n) (Unsigned m) where
  maybeConvert n
    | n < 0 = Nothing
    | otherwise = maybeResize (bitCoerce @(Signed (n + 1)) (extend n))

instance (KnownNat n, KnownNat m) => MaybeConvert (Signed n) (Signed m) where
  maybeConvert = maybeResize

instance (KnownNat n, KnownNat m) => MaybeConvert (Signed n) (BitVector m) where
  maybeConvert n
    | n < 0 = Nothing
    | otherwise = maybeResize (pack @(Signed (n + 1)) (extend n))

instance (KnownNat n, KnownNat m) => MaybeConvert (BitVector n) (Index m) where
  maybeConvert = maybeResize . unpack @(Index (2 ^ n))

instance (KnownNat n, KnownNat m) => MaybeConvert (BitVector n) (Unsigned m) where
  maybeConvert = maybeResize . unpack @(Unsigned n)

instance (KnownNat n, KnownNat m) => MaybeConvert (BitVector n) (Signed m) where
  maybeConvert = maybeResize . unpack @(Signed (n + 1)) . extend

instance (KnownNat n, KnownNat m) => MaybeConvert (BitVector n) (BitVector m) where
  maybeConvert = maybeResize

{- FOURMOLU_DISABLE -}
maybeResize ::
  forall a b f.
  ( Resize f
  , KnownNat a
  , KnownNat b
  , Bounded (f b)
  , Ord (f a)
  ) =>
  f a ->
  Maybe (f b)
maybeResize v =
#if MIN_VERSION_base(4,16,0)
  case SNat @a `cmpNat` SNat @b of
    GTI | v > resize (maxBound @(f b)) -> Nothing
    GTI | v < resize (minBound @(f b)) -> Nothing
    EQI -> Just v
    _ -> Just (resize v)
#else
  case natToNatural @a `compare` natToNatural @b of
    GT | v > resize (maxBound @(f b)) -> Nothing
    GT | v < resize (minBound @(f b)) -> Nothing
    _ -> Just (resize v)
#endif
{- FOURMOLU_ENABLE -}
