{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Test generation of 'Convert' instances:

> constraints = {
>     ("Index",     "Index")     : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("Index",     "Unsigned")  : (False, "SNat @{n}              `compareSNat` SNat @(2 ^ {m})"),
>     ("Index",     "Signed")    : (True,  "SNat @(CLog 2 {n} + 1) `compareSNat` SNat @{m}"),
>     ("Index",     "BitVector") : (False, "SNat @{n}              `compareSNat` SNat @(2 ^ {m})"),
>     ("Unsigned",  "Index")     : (False, "SNat @(2^{n})          `compareSNat` SNat @{m}"),
>     ("Unsigned",  "Unsigned")  : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("Unsigned",  "Signed")    : (True,  "SNat @({n} + 1)        `compareSNat` SNat @{m}"),
>     ("Unsigned",  "BitVector") : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("Signed",    "Signed")    : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("BitVector", "Index")     : (False, "SNat @(2^{n})          `compareSNat` SNat @{m}"),
>     ("BitVector", "Unsigned")  : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
>     ("BitVector", "Signed")    : (True,  "SNat @({n} + 1)        `compareSNat` SNat @{m}"),
>     ("BitVector", "BitVector") : (False, "SNat @{n}              `compareSNat` SNat @{m}"),
> }
>
> for a in ["Index", "Unsigned", "Signed", "BitVector"]:
>     for b in ["Index", "Unsigned", "Signed", "BitVector"]:
>         ia_max = "indexMax" if a == "Index" else "otherMax"
>         ib_max = "indexMax" if b == "Index" else "otherMax"
>         n = "(n + 1)" if a == "Index" else "n"
>         m = "(m + 1)" if b == "Index" else "m"
>         if (a, b) not in constraints:
>             continue
>         print(f"""case_convert{a}{b} :: Assertion
> case_convert{a}{b} =
>   forM_ [0 .. {ia_max}] $ \\n ->
>     forM_ [0 .. {ib_max}] $ \\m ->
>       withSomeSNat n $ \\(SNat :: SNat n) ->
>         withSomeSNat m $ \\(SNat :: SNat m) ->
>           case {constraints[(a, b)][1].format(n=n, m=m)} of
>             SNatLE ->
>               forM_ [minBound .. maxBound] $ \\(i :: {a} {n}) -> do
>                 assertBool (show (n, m, i)) (convertLaw1 (Proxy @({b} {m})) i)
>                 assertBool (show (n, m, i)) (convertLaw2 (Proxy @({b} {m})) i)
>             _ -> do
>               let hasZeroZeroSpecialCase = {constraints[(a, b)][0]}
>               unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
>                 assertBool (show (n, m)) (convertLaw3 (Proxy @({a} {n})) (Proxy @({b} {m})))
>  """)
-}
module Tests.Clash.Convert where

import Clash.Convert (Convert (convert), MaybeConvert (maybeConvert))
import Clash.Prelude hiding (someNatVal, withSomeSNat)
import Control.Monad (forM_, unless)
import Data.Data (Proxy (..))
import Data.Maybe (isNothing)
import GHC.TypeNats (someNatVal)
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.TH (testGroupGenerator)

import qualified Data.List as L

-- > Just x == maybeConvert (convert @a @b x)
-- > toInteger x == toInteger (convert @a @b x)

convertLaw1 :: forall a b. (Convert a b, MaybeConvert b a, Eq a) => Proxy b -> a -> Bool
convertLaw1 _ x = Just x == maybeConvert (convert @a @b x)

convertLaw2 :: forall a b. (Convert a b, Eq a, Integral b, Integral a) => Proxy b -> a -> Bool
convertLaw2 _ x = toInteger x == toInteger (convert @a @b x)

{- | Tightness law: this law is tested for if there is _no_ instance of
'Convert'. If this is the case, 'MaybeConvert' should at least return a 'Nothing'
once when converting the domain of @a@ to @b@. If all conversions are possible,
the constraints of the instances should be relaxed.
-}
convertLaw3 :: forall a b. (MaybeConvert a b, Bounded a, Enum a) => Proxy a -> Proxy b -> Bool
convertLaw3 _ _ = L.any isNothing (L.map (maybeConvert @a @b) [minBound ..])

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = $(testGroupGenerator)

withSomeSNat :: Natural -> (forall (n :: Nat). SNat n -> r) -> r
withSomeSNat n f = case someNatVal n of
  SomeNat (_ :: Proxy n) -> f (SNat @n)

indexMax :: Natural
indexMax = 128

otherMax :: Natural
otherMax = 8

case_convertIndexIndex :: Assertion
case_convertIndexIndex =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @(m + 1) of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Index (m + 1))) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Index (m + 1))) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(Index (n + 1))) (Proxy @(Index (m + 1))))

case_convertIndexUnsigned :: Assertion
case_convertIndexUnsigned =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @(2 ^ m) of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Unsigned m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Unsigned m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(Index (n + 1))) (Proxy @(Unsigned m)))

case_convertIndexSigned :: Assertion
case_convertIndexSigned =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(CLog 2 (n + 1) + 1) `compareSNat` SNat @m of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Signed m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Signed m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = True
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(Index (n + 1))) (Proxy @(Signed m)))

case_convertIndexBitVector :: Assertion
case_convertIndexBitVector =
  forM_ [0 .. indexMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @(2 ^ m) of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: Index (n + 1)) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(BitVector m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(BitVector m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(Index (n + 1))) (Proxy @(BitVector m)))

case_convertUnsignedIndex :: Assertion
case_convertUnsignedIndex =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(2 ^ n) `compareSNat` SNat @(m + 1) of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Index (m + 1))) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Index (m + 1))) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(Unsigned n)) (Proxy @(Index (m + 1))))

case_convertUnsignedUnsigned :: Assertion
case_convertUnsignedUnsigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Unsigned m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Unsigned m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(Unsigned n)) (Proxy @(Unsigned m)))

case_convertUnsignedSigned :: Assertion
case_convertUnsignedSigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @m of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Signed m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Signed m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = True
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(Unsigned n)) (Proxy @(Signed m)))

case_convertUnsignedBitVector :: Assertion
case_convertUnsignedBitVector =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: Unsigned n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(BitVector m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(BitVector m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(Unsigned n)) (Proxy @(BitVector m)))

case_convertSignedSigned :: Assertion
case_convertSignedSigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: Signed n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Signed m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Signed m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(Signed n)) (Proxy @(Signed m)))

case_convertBitVectorIndex :: Assertion
case_convertBitVectorIndex =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. indexMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(2 ^ n) `compareSNat` SNat @(m + 1) of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Index (m + 1))) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Index (m + 1))) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(BitVector n)) (Proxy @(Index (m + 1))))

case_convertBitVectorUnsigned :: Assertion
case_convertBitVectorUnsigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Unsigned m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Unsigned m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(BitVector n)) (Proxy @(Unsigned m)))

case_convertBitVectorSigned :: Assertion
case_convertBitVectorSigned =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @(n + 1) `compareSNat` SNat @m of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(Signed m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(Signed m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = True
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(BitVector n)) (Proxy @(Signed m)))

case_convertBitVectorBitVector :: Assertion
case_convertBitVectorBitVector =
  forM_ [0 .. otherMax] $ \n ->
    forM_ [0 .. otherMax] $ \m ->
      withSomeSNat n $ \(SNat :: SNat n) ->
        withSomeSNat m $ \(SNat :: SNat m) ->
          case SNat @n `compareSNat` SNat @m of
            SNatLE ->
              forM_ [minBound .. maxBound] $ \(i :: BitVector n) -> do
                assertBool (show (n, m, i)) (convertLaw1 (Proxy @(BitVector m)) i)
                assertBool (show (n, m, i)) (convertLaw2 (Proxy @(BitVector m)) i)
            _ -> do
              let hasZeroZeroSpecialCase = False
              unless (hasZeroZeroSpecialCase && n == 0 && m == 0) $ do
                assertBool (show (n, m)) (convertLaw3 (Proxy @(BitVector n)) (Proxy @(BitVector m)))
