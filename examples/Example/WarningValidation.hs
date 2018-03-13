{-# LANGUAGE
    CPP,
    NoImplicitPrelude
  #-}

{-# LANGUAGE
    OverloadedStrings,
    OverloadedLists,
    PackageImports
  #-}

{-| 

@

-- successes
>>> example_validateNaturalRatio 1 2
WarningSuccess (Warnings []) (1 % 2)
>>> example_validateNaturalRatio 0 2
WarningSuccess (Warnings []) (0 % 1)

@

@

-- successes (with warnings)
>>> example_validateNaturalRatio 4 8
WarningSuccess (Warnings []) (1 % 2)
>>> example_validateNaturalRatio (-1) (-2)
WarningSuccess (Warnings ["the numerator and denominator SHOULD both be positive"]) (1 % 2)

@


@
-- errors
>>> example_validateNaturalRatio 1 0
WarningFailure (Warnings []) (Errors ("the denominator MUST be non-zero" :| []))
>>> example_validateNaturalRatio 1 (-2)
WarningFailure (Warnings ["the numerator and denominator SHOULD both be positive"]) (Errors ("the ratio MUST be non-negative" :| []))

@


@
-- errors (correctly accumulated)
>>> example_validateNaturalRatio (-1) 0
WarningFailure (Warnings ["the numerator and denominator SHOULD both be positive"]) (Errors ("the denominator MUST be non-zero" :| []))

@

(Please see the source too)

-}
module Example.WarningValidation where

import Prelude.Spiros
import Spiros.WarningValidation -- .Simple

--------------------

import "base" Data.Ratio

----------------------------------------

{-

@

[successes]

WarningSuccess (Warnings []) (1 % 2)
WarningSuccess (Warnings []) (0 % 1)

[warnings]

WarningSuccess (Warnings ["the numerator and denominator SHOULD both be positive"]) (1 % 2)

[errors (accumulated)]

WarningFailure (Warnings ["the numerator and denominator SHOULD both be positive"]) (Errors ("the denominator MUST be non-zero" :| []))

[errors]

WarningFailure (Warnings ["the numerator and denominator SHOULD both be positive"]) (Errors ("the ratio MUST be non-negative" :| []))
WarningFailure (Warnings []) (Errors ("the denominator MUST be non-zero" :| []))

@

-}

example_validateNaturalRatio
  :: Integer
  -> Integer
  -> WarningValidation
       Warnings
       Errors
       (Ratio Natural)

example_validateNaturalRatio n d =
  es *> ws *> success r

  where
  r  = n' % d' :: Ratio Natural
     -- assumes the laziness of @WarningValidation@ wrt the @a@
  n' = fromIntegral (abs n)
  d' = fromIntegral (abs d)

  es = sconcat
      [ hardAssertion "the denominator MUST be non-zero"
                      (d /= 0)
      , hardAssertion "the ratio MUST be non-negative"
                      (let sigratio = signum n * signum d in
                         sigratio == 1 || sigratio == 0)
      ]

  ws = mconcat -- sequenceA_
      [ softAssertion "the numerator and denominator SHOULD both be positive"
                      ((n >= 0) && (d >= 0))
      ]

----------------------------------------

validRatio :: Integer -> Integer -> WarningValidation Warnings Errors Rational
validRatio n d = e *> success r
  where
  r  = n % d
  e = hardAssertion "the denominator MUST be non-zero" (d /= 0)

----------------------------------------

{-

  -- success r <* checks 
  -- checks *> success r
  -- es *> ws *> success r

  -- checks = mconcat [ es, ws ]


  render s = (show > (++ " " ++ s))


 (Ratio Natural)

example_validateNaturalRatio n d = sequenceA_
  [ n      & failureUnless_ (/= 0) (render "the denominator must be non-zero")
  , (n, d) & failureUnless_ (\(n',d') -> signum n' == signum d') = failure "the ratio must be non-negative"
  | otherwise                  = 
     ( if   not ((n >= 0) && (d >= 0))
       then success r <* warning "the numerator and denominator were both negative"
       else success r
     )
  where
  r  = n' % d' :: Ratio Natural
  n' = fromIntegral (abs n)
  d' = fromIntegral (abs d)

  render s = (show > (++ " " ++ s))





example_validateNaturalRatio n d
  | not (d /= 0)               = failure "the denominator must be non-zero"
  | not (signum n == signum d) = failure "the ratio must be non-negative"
  | otherwise                  = 
     ( if   not ((n >= 0) && (d >= 0))
       then success r <* warning "the numerator and denominator were both negative"
       else success r
     )
  where
  r  = n' % d' :: Ratio Natural
  n' = fromIntegral (abs n)
  d' = fromIntegral (abs d)

validateNaturalNumber :: Integer -> WarningValidation w [String] Natural
validateNaturalNumber i =
  if (i>=0)
  then success n
  else failure0 "must be non-negative"
  where
  n = fromIntegral i

validatePositiveNumber :: Integer -> WarningValidation w [String] Natural
validatePositiveNumber i =
  if (i>=1)
  then success n
  else failure0 "must be positive"
  where
  n = fromIntegral i

validateNonZeroNumber :: Integer -> WarningValidation w [String] Integer
validateNonZeroNumber i =
  if (i/=0)
  then success n
  else failure0 "must be non-zero"
  where
  n = fromIntegral i

-- the numerator and denominator share the same sign, but both were negative
--  i.e. the numerator and denominator must both share the same sign"

-}

----------------------------------------
