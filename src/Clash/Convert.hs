{-# LANGUAGE CPP #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Utilities for converting between Clash number types in a safe way. Its
existence is motivated by the observation that Clash users often need to convert
between different number types (e.g., 'Clash.Sized.Unsigned.Unsigned' to
'Clash.Sized.Signed.Signed') and that it is not always clear how to do so
properly. Two classes are exported:

* 'Convert': for conversions that, based on types, are guaranteed to succeed.
* 'MaybeConvert': for conversions that may fail for some values.

As opposed to 'Prelude.fromIntegral', all conversions are translatable to
synthesizable HDL.
-}
module Clash.Convert (Convert (..), MaybeConvert (..)) where

import Clash.Convert.Internal.Convert
import Clash.Convert.Internal.MaybeConvert
