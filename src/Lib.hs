module Lib
    ( render
    ) where

import           Prelude hiding (Word)

import           Data.Maybe

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



data Instruction = NoOp
                | Store
                | Retrieve

                | Binary (BinaryOperation WordUnit)
                | Unary (UnaryOperation WordUnit)

                | Drop
                | Duplicate
                | Swap
                | Over

                | PushReturn
                | PopReturn

                | Call
                | Exit

                | If

                | Constant WordUnit
    deriving (Show, Eq)


type Program = [Instruction]
type PC = WordUnit
type Input = [WordUnit]
type Output = WordUnit
type Memory = [WordUnit]
type DataStack = Stack WordUnit
type ReturnStack = Stack WordUnit

data Machine = Machine { pc :: WordUnit  -- TODO pc :: PC (via type PC = WordUnit)
                       , program  :: Program
                       , input :: Input
                       , memory :: Memory
                       , dataStack :: DataStack
                       , returnStack :: ReturnStack
                       }

machineStart program input = Machine { pc = 0
                                     , program = program
                                     , input = input
                                     , memory = take (2^16) (repeat 0)  -- TODO hard to modify..
                                     , dataStack = Stack.empty
                                     , returnStack = Stack.empty
                                     }

step :: Machine -> (Machine, Maybe Output)
step machine = performInstruction (decodeInstruction (program machine) (pc machine)) machine
    where decodeInstruction program pc = program !! (fromIntegral pc)
          performInstruction NoOp machine = (machine, Nothing)

output :: Program -> Input -> [Output]
output program input = catMaybes $ output_ $ machineStart program input
    where output_ machine
              | isDone machine = []
              | otherwise = step_output : output_ machine'
                  where isDone (Machine { program = program , pc = pc}) = length program > fromIntegral pc
                        (machine', step_output) = step machine

run :: Program -> Input -> IO ()
run program input = do
    mapM_ print $ output program input
    putStrLn "exit"


render :: IO ()
--render = print $ unaryApply BitComplement (2 :: WordUnit)
render = run [ NoOp
             , NoOp
             ] [1..]
