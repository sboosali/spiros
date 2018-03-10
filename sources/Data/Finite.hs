{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Data.Finite where

-- ----------------------------------------

-- -- | kind
-- data Sign
--  = Positive
--  | Negative

-- -- | singleton 
-- data Sign_ (sign :: Sign) where
--   Positive_ :: Sign_ 'Positive
--   Negative_ :: Sign_ 'Negative

-- sign2int :: Sign_ sign -> Integer
-- sign2int = \case
--   Positive_ -> -1
--   Negative_ ->  1 

-- signVal :: proxy sign -> Integer
-- signVal sign = _

-- intVal :: proxy sign -> proxy nat -> Integer
-- intVal sign nat = signVal sign * natVal nat

-- -- reifyINTEGER
-- --   :: forall (i :: INTEGER).
-- --     ( KnownINTEGER i
-- --     )
-- --   => proxy i
-- --   -> Integer
-- -- reifyINTEGER _ 

-- ----------------------------------------

-- {-|

-- @
-- Between sin min sax max
-- @

-- -}
-- newtype Between
--  (sin :: Sign) (min :: Nat)
--  (sax :: Sign) (max :: Nat)
--  = Between Integer
--  deriving (Show,Eq,Ord,Generic,NFData)

-- minBetween
--   :: forall sin min sax max.
--      (KnownNat min)
--   => proxy (Between sin min sax max)
--   -> Integer
-- minBetween = _

-- maxBetween
--   :: forall sin min sax max.
--   (KnownNat min)
--   => proxy (Between sin min sax max)
--   -> Integer
-- maxBetween = _

-- -- | primary constructor. 
-- clipBetween
--   :: forall sin min sax max.
--      (Num a
--      )
--   => a
--   -> Between sin min sax max
-- clipBetween
--   = toInteger
--   > max (minBetween proxy)
--   > min (maxBetween proxy)
--   > Between
--   where
--   proxy = Proxy (Between sin min sax max)

-- ----------------------------------------

-- instance Enum (Between sin min sax max) where
--   fromEnum (Between i) = i
--   toEnum = clipBetween

-- instance Bounded (Between sin min sax max) where
--   minBound = minBetween
--   maxBound = maxBetween

-- instance Enumerable (Between sin min sax max) where
--   enumerated = [minBetween .. maxBetween]

-- --instance (Integral i, KnownNat n) => Num (i `Mod` n) where

-- instance Num (Between sin min sax max) where
--   fromInteger = fromInteger > small

--   Between i₁ + Between i₂ = small $ i₁ + i₂
--   Between i₁ * Between i₂ = small $ i₁ * i₂

--   abs    (Between i) = small $ abs i
--   signum (Between i) = small $ signum i
--   negate (Between i) = small $ negate i

-- ---------------------------------------


-- {-

-- ----------------------------------------

-- data INTEGER
--  = Negative Nat
--  | Positive Nat

-- type family KnownINTEGER (i :: INTEGER) :: Constraint where
--   KnownINTEGER (Negative n) = (KnownNat n)
--   KnownINTEGER (Positive n) = (KnownNat n)

-- reifyINTEGER
--   :: forall (i :: INTEGER).
--     ( KnownINTEGER i
--     )
--   => proxy i
--   -> Integer
-- reifyINTEGER _ 
  
-- ----------------------------------------

-- newtype Between (min :: INTEGER) (max :: INTEGER) = Between Integer
--  deriving (Show,Eq,Ord,Generic,NFData)

-- minBetween :: Between a => a -- Between
-- minBetween = -5

-- maxBetween :: Num a => a -- Between
-- maxBetween = 25

-- small :: Int -> Between
-- small = max minBetween > min maxBetween > Between

-- instance Enum Between where
--   fromEnum (Between i) = i
--   toEnum = small

-- instance Bounded Between where
--   minBound = minBetween
--   maxBound = maxBetween

-- instance Enumerable Between where
--   enumerated = [minBetween .. maxBetween]

-- --instance (Integral i, KnownNat n) => Num (i `Mod` n) where

-- instance Num Between where
--   fromInteger = fromInteger > small

--   Between i₁ + Between i₂ = small $ i₁ + i₂
--   Between i₁ * Between i₂ = small $ i₁ * i₂

--   abs    (Between i) = small $ abs i
--   signum (Between i) = small $ signum i
--   negate (Between i) = small $ negate i

-- ---------------------------------------

-- -}
