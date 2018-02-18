{-# LANGUAGE LambdaCase, OverloadedStrings, InstanceSigs #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, DeriveGeneric, DeriveDataTypeable #-}
module Data.Digit where
import Prelude.Spiros

-- import Data.Word (Word8)
import GHC.Exts (IsString)

--------------------------------------------------------------------------------

{-| a single-digit decimal number, i.e. @[0..9]@ i.e. @Fin 10@.

the @Num@ instance provides numerical literals, and is partial like @Natural@. 

-}
newtype Digit = Digit Int -- Word8 Natural
 deriving stock   (Show,Read,Data)
 deriving newtype (Eq,Ord,Num,Ix,{-Bits,FiniteBits,-}NFData,Hashable) -- no Generic: it's abstract 
 -- TODO are the two bits classes correct ? since they use Int's instance 

-- TODO custom number instance with modular arithmetic or something

instance Bounded Digit where
  minBound = minimumDigit 
  maxBound = maximumDigit 

-- | 'toEnum' is partial 
instance Enum Digit where
  toEnum   :: Int -> Digit
  toEnum   = unsafeDigit :: Int -> Digit 
  fromEnum :: Digit -> Int
  fromEnum = fromDigit 

-- instance Enumerable Digit where
--   enumerated = [0..9]
--   cardinality _ = 10

-- instance Finite Digit where
--   type Cardinality Digit = 10

minimumDigit :: (Num a) => a
minimumDigit = 0
{-# SPECIALIZE minimumDigit :: Digit #-}
{-# SPECIALIZE minimumDigit :: Int   #-}

maximumDigit :: (Num a) => a
maximumDigit = 9
{-# SPECIALIZE maximumDigit :: Digit #-}
{-# SPECIALIZE maximumDigit :: Int   #-}
  
fromDigit :: Digit -> Int
fromDigit (Digit d) = d

toDigit :: (Integral a) => a -> Maybe Digit
toDigit i = if a <= i && i <= b
  then Just $ Digit (fromIntegral i)
  else Nothing
  where
  a = minimumDigit 
  b = maximumDigit 
{-# SPECIALIZE toDigit :: Int -> Maybe Digit #-}

unsafeDigit :: (Integral a) => a -> Digit
unsafeDigit i = i & (toDigit >>> maybe (__ERROR__ e) id)
 where
 e = ("[spiros:Digit.unsafeDigit] a Digit must be a single-digit number")
-- e = ("[spiros:Digit.unsafeDigit] " ++ show i ++ " is not a single-digit number") -- `show` forces Show constraint
{-# SPECIALIZE unsafeDigit :: Int -> Digit #-}

allDigits :: [Digit] -- Set Digit 
allDigits = [0..9]

--------------------------------------------------------------------------------

parseDigit' :: Char -> Maybe Digit
parseDigit' = \case
 '0' -> Just $ Digit 0
 '1' -> Just $ Digit 1
 '2' -> Just $ Digit 2
 '3' -> Just $ Digit 3
 '4' -> Just $ Digit 4
 '5' -> Just $ Digit 5
 '6' -> Just $ Digit 6
 '7' -> Just $ Digit 7
 '8' -> Just $ Digit 8
 '9' -> Just $ Digit 9
 _   -> Nothing

parseDigit :: (IsString s, Eq s) => s -> Maybe Digit
parseDigit = \case
 "0" -> Just $ Digit 0
 "1" -> Just $ Digit 1
 "2" -> Just $ Digit 2
 "3" -> Just $ Digit 3
 "4" -> Just $ Digit 4
 "5" -> Just $ Digit 5
 "6" -> Just $ Digit 6
 "7" -> Just $ Digit 7
 "8" -> Just $ Digit 8
 "9" -> Just $ Digit 9
 _   -> Nothing

--------------------------------------------------------------------------------

{-

newtype Digit = Digit Word8       -- TODO modular arithmetic: type Digit = Natural `Mod` 10
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

instance Bounded Digit where
 minBound = Digit 0
 maxBound = Digit 9

instance Enum Digit where
 toEnum i
  | minBound <= i && i <= maxBound = Digit (toEnum i)
  | otherwise                      = error ("Digit.toEnum: " ++ show i ++ " is not a single-digit integer")
 fromEnum (Digit i) = fromEnum i

parseDigit :: (IsString s, Eq s) => s -> Maybe Digit
parseDigit = \case
 "0" -> Just $ Digit 0
 "1" -> Just $ Digit 1
 "2" -> Just $ Digit 2
 "3" -> Just $ Digit 3
 "4" -> Just $ Digit 4
 "5" -> Just $ Digit 5
 "6" -> Just $ Digit 6
 "7" -> Just $ Digit 7
 "8" -> Just $ Digit 8
 "9" -> Just $ Digit 9
 _   -> Nothing

toDigit :: (Integral a) => a -> Maybe Digit
toDigit i = if 0 >= i && i <= 9 then Just (Digit (fromIntegral i)) else Nothing

-} 