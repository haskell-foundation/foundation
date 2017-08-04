module Foundation.Primitive.UTF8.Types
    (
    -- * Stepper
      Step(..)
    , StepBack(..)
    , StepASCII(..)
    , StepDigit(..)
    , isValidStepASCII
    , isValidStepDigit
    -- * Unicode Errors
    , ValidationFailure(..)
    ) where

import           Foundation.Primitive.Compat.Base
import           Foundation.Primitive.Types.OffsetSize

-- | Step when walking a String
--
-- this is a return value composed of :
-- * the unicode code point read (Char) which need to be
--   between 0 and 0x10ffff (inclusive)
-- * The next offset to start reading the next unicode code point (or end)
data Step = Step {-# UNPACK #-} !Char {-# UNPACK #-} !(Offset Word8)

-- | Similar to Step but used when processing the string from the end.
--
-- The stepper is thus the previous character, and the offset of
-- the beginning of the previous character
data StepBack = StepBack {-# UNPACK #-} !Char {-# UNPACK #-} !(Offset Word8)

-- | Step when processing digits. the value is between 0 and 9 to be valid
newtype StepDigit = StepDigit Word8

-- | Step when processing ASCII character
newtype StepASCII = StepASCII Word8

isValidStepASCII :: StepASCII -> Bool
isValidStepASCII (StepASCII w) = w < 0x80

isValidStepDigit :: StepDigit -> Bool
isValidStepDigit (StepDigit w) = w < 0xa

-- | Possible failure related to validating bytes of UTF8 sequences.
data ValidationFailure = InvalidHeader
                       | InvalidContinuation
                       | MissingByte
                       | BuildingFailure
                       deriving (Show,Eq,Typeable)

instance Exception ValidationFailure
