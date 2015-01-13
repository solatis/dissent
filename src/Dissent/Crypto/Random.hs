-- | Random number crypto utility functions
--
--   High level wrapper around HsOpenSSL's random number generation
--   and generating a 'real' Haskell random number generator.
--
--   We need a function to shuffle a list, which this module
--   implements. We use the original Fisher-Yates algorithm, which is
--   secure, simple but is lacking in performance, which is O(n^2).
--
--   However, at the moment, I rather keep a simple algorithm which
--   works correctly than have a very-good-performing algorithm which
--   might have bugs.
--
--   See also: http://okmij.org/ftp/Haskell/perfect-shuffle.txt

module Dissent.Crypto.Random where

import           OpenSSL        (withOpenSSL)
import           OpenSSL.BN     (randIntegerZeroToNMinusOne)
import qualified OpenSSL.Random as OR (add, randBytes)

-- | Extracts an item from the list at a certain position, and
--   returns the remainders of the list
extract :: Int       -- ^ Offset, where 0 <= offset < length(list)
        -> [a]       -- ^ List we take item from
        -> (a, [a])  -- ^ Tuple with item and rest of the list
extract offset list =
  let (lhs, rhs) = splitAt offset list
  in (head rhs, lhs ++ tail rhs)

-- | Given a sequence (e1,...en), n > 0, to shuffle, and a sequence
--   (r1,...r[n-1]) of numbers such that r[i] is an independent sample
--   from a uniform random distribution [0..n-i], compute the
--   corresponding permutation of the input sequence.
shuffleWithDistribution :: [a]       -- ^ The sequence we want to shuffle
                        -> [Int]     -- ^ Random disitribution
                        -> [a]       -- ^ Shuffled list
shuffleWithDistribution l [] = l
shuffleWithDistribution input (x:xs) =
  let (cur, rest) = extract x input
  in  cur : shuffleWithDistribution rest xs


-- | Generates a sequence of random numbers, such that r[i] is a sample with
--   a dsitribution of [0..n-i].
generateDistribution :: Int
                     -> IO [Int]
generateDistribution 0 = return []
generateDistribution i = do
  cur  <- randIntegerZeroToNMinusOne (toInteger i)
  rest <- generateDistribution (i - 1)

  return (fromInteger cur : rest)

-- | Seeds the OpenSSL pseudo-random number generator
seedRng :: IO ()
seedRng =
  -- According to the OpenSSL documentation we can find at
  --
  --   https://www.openssl.org/docs/crypto/rand.html
  --
  -- OpenSSL's PRNG uses an internal state of 1023 bytes. Thus, for a perfect
  -- seed, we feed it 1023 bytes from a pure/strong random source.
  let l = 1023
  in do
    bytes <- OR.randBytes l
    OR.add bytes l

-- | Our actual shuffle implementation, which combines the pure shuffle function
--   with the distribution function to shuffle a list.
shuffle :: [a]
        -> IO [a]
shuffle input = withOpenSSL $ do
  -- This might be overkill, but every time we perform a shuffle, we'll seed the
  -- RNG again.
  _            <- seedRng
  distribution <- generateDistribution (length input)
  return (shuffleWithDistribution input distribution)
