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

import qualified Map
import           Map (Map)


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
                 | Constant WordUnit

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

                 | Read
                 | Write

                 | Call
                 | Exit

                 | If
    deriving (Show, Eq)


type Program = [Instruction]
type PC = WordUnit
type Input = [WordUnit]
type Output = WordUnit
type Memory = Map WordUnit WordUnit
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
                                     , memory = Map.empty
                                     , dataStack = Stack.empty
                                     , returnStack = Stack.empty
                                     }


pcPlus :: Machine -> Machine
pcPlus machine@(Machine {pc = pc}) = machine {pc = pc + 1}


step :: Machine -> (Machine, Maybe Output)
step machine = performInstruction (decodeInstruction (program machine) (pc machine)) machine
    where decodeInstruction program pc = program !! (fromIntegral pc)
          performInstruction NoOp machine = (pcPlus machine, Nothing)
          performInstruction (Constant c) machine@(Machine {dataStack=dataStack}) = (pcPlus machine{dataStack=Stack.push dataStack c}, Nothing)

          performInstruction Read machine@(Machine {input=input, dataStack=dataStack}) = (pcPlus machine{input=input', dataStack=Stack.push dataStack c}, Just c)
              where (c:input') = input
          performInstruction Write machine@(Machine {dataStack=dataStack}) = (pcPlus machine{dataStack=dataStack'}, Just c)
              where (Just (dataStack', c)) = Stack.pop dataStack

          performInstruction (Unary f) machine@(Machine {dataStack=dataStack}) = (pcPlus machine{dataStack=dataStack''}, Nothing)
              where (Just (dataStack', c)) = Stack.pop dataStack
                    dataStack'' = Stack.push dataStack' $ unaryApply f c
          performInstruction (Binary f) machine@(Machine {dataStack=dataStack}) = (pcPlus machine{dataStack=dataStack'''}, Nothing)
              where (Just ( dataStack', c )) = Stack.pop dataStack
                    (Just (dataStack'', c')) = Stack.pop dataStack'
                    dataStack''' = Stack.push dataStack' $ binaryApply f c c'


          performInstruction Drop machin@(Machine {dataStack=dataStack}) = (pcPlus machine{dataStack=dataStack'}, Nothing)
              where (Just (dataStack', _)) = Stack.pop dataStack
          performInstruction Duplicate machin@(Machine {dataStack=dataStack}) = (pcPlus machine{dataStack=dataStack'}, Nothing)
              where (Just (_, c)) = Stack.pop dataStack
                    dataStack' = Stack.push (Stack.push dataStack c) c
          performInstruction Swap machin@(Machine {dataStack=dataStack}) = (pcPlus machine{dataStack=dataStack'''}, Nothing)
              where (Just ( dataStack', c )) = Stack.pop dataStack
                    (Just (dataStack'', c')) = Stack.pop dataStack'
                    dataStack''' = Stack.push (Stack.push dataStack'' c) c'
          performInstruction Over machin@(Machine {dataStack=dataStack}) = (pcPlus machine{dataStack=dataStack'''}, Nothing)
              where (Just ( dataStack', c )) = Stack.pop dataStack
                    (Just (dataStack'', c')) = Stack.pop dataStack'
                    dataStack''' = Stack.push (Stack.push (Stack.push dataStack'' c) c') c

          performInstruction PopReturn machin@(Machine {returnStack=returnStack, dataStack=dataStack}) = (pcPlus machine{returnStack=returnStack',dataStack=dataStack'}, Nothing)
              where (Just (returnStack', r)) = Stack.pop returnStack
                    dataStack' = Stack.push dataStack r
          performInstruction PushReturn machin@(Machine {returnStack=returnStack, dataStack=dataStack}) = (pcPlus machine{returnStack=returnStack',dataStack=dataStack'}, Nothing)
              where (Just (dataStack', r)) = Stack.pop dataStack
                    returnStack' = Stack.push returnStack r

          performInstruction Store machine@(Machine {dataStack=dataStack, memory=memory}) = (pcPlus machine{dataStack=dataStack'', memory=memory'}, Nothing)
              where (Just ( dataStack', c )) = Stack.pop dataStack
                    (Just (dataStack'', c')) = Stack.pop dataStack'
                    memory' = Map.set (c, c') memory
          performInstruction Retrieve machine@(Machine {dataStack=dataStack, memory=memory}) = (pcPlus machine{dataStack=dataStack''}, Nothing)
              where (Just ( dataStack', c )) = Stack.pop dataStack
                    (Just c') = Map.get c memory
                    dataStack'' = Stack.push dataStack' c'

output :: Program -> Input -> [Output]
output program input = catMaybes $ output_ $ machineStart program input
    where output_ machine
              | isDone machine = []
              | otherwise = step_output : output_ machine'
                  where isDone (Machine { program = program , pc = pc}) = length program <= fromIntegral pc
                        (machine', step_output) = step machine

run :: Program -> Input -> IO ()
run program input = do
    mapM_ print $ output program input
    putStrLn "<done>"

render :: IO ()
--render = print $ unaryApply BitComplement (2 :: WordUnit)
render = run [
     Constant 13,
     Constant 37,
     Store,
     Constant 37,
     Retrieve,
     Write
        ] [1..]
