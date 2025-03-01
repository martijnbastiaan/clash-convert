{-# LANGUAGE CPP #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module Clash.Convert.Internal.MaybeConvert where

import Clash.Prelude

{- $setup
>>> import Clash.Prelude
>>> import Clash.Convert
-}

{- | Conversions that may fail for some values.

== __Laws__
A conversion is safe if a round trip conversion does not produce errors (also
see "Clash.XException"). I.e.,

> x == fromMaybe x (maybeConvert @a @b x >>= maybeConvert @b @a)

for all values @x@ of type @a@. It should also preserve the numerical value
interpretation of the bits. For types that have an "Integral" instance, this
intuition is captured by:

> toInteger x == fromMaybe (toInteger x) (toInteger (convert @a @b x))

If a conversion succeeds one way, it should also succeed the other way. I.e.,

> isJust (maybeConvert @a @b x) `implies` isJust (maybeConvert @a @b x >>= maybeConvert @b @a)

A conversion should succeed if and only if the value is representable in the
target type. For types that have a "Bounded" and "Integral" instance, this
intuition is captured by:

> isJust (maybeConvert @a @b x) == (i x >= i (minBound @b) && i x <= i (maxBound @b))

where @i = toInteger@.

Additionally, any implementation should be translatable to synthesizable RTL.
-}
class MaybeConvert a b where
  {- | Convert a supplied value of type @a@ to a value of type @b@. If the value
  cannot be represented in the target type, 'Nothing' is returned.

  >>> maybeConvert (1 :: Index 8) :: Maybe (Unsigned 2)
  Just 1
  >>> maybeConvert (7 :: Index 8) :: Maybe (Unsigned 2)
  Nothing
  -}
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
