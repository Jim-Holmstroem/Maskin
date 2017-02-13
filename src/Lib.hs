module Lib
    ( render
    ) where

import           Prelude hiding (Word)

import           Data.Bits

import           Data.Word

import qualified Stack
import           Stack (Stack)

import qualified Zipper
import           Zipper (Zipper)


type WordUnit = Word32

data UnaryOperation w = Increment
                      | Decrement
                      | BitComplement
                      | BitsRotate Int
    deriving (Show, Eq)
unaryApply :: (Bits w, Num w) => UnaryOperation w -> w -> w
unaryApply Increment = (+1)
unaryApply Decrement = (+(-1))
unaryApply BitComplement = complement
unaryApply (BitsRotate n) = flip rotate n

data BinaryOperation w = Addition
                       | Subtraction
                       | Multiplication
                       | BitOr
                       | BitAnd
                       | BitXor
    deriving (Show, Eq)
binaryApply :: (Bits w, Num w) => BinaryOperation w -> w -> w -> w
binaryApply Addition = (+)
binaryApply Subtraction = (-)
binaryApply Multiplication = (*)
binaryApply BitOr = (.|.)
binaryApply BitAnd = (.&.)
binaryApply BitXor = xor


type Word = Word32
data Instruction = NoOp
    deriving (Show, Eq)


render :: IO ()
render = print $ unaryApply BitComplement (2 :: WordUnit)
