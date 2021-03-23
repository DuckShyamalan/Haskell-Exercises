data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructions :: [Int] -> Instruction -> [Int]

executeInstructions ns inst | inst == Add = sum(take 2 ns) : (drop 2 ns)
                            | inst == Multiply = product(take 2 ns) : (drop 2 ns)
                            | inst == Duplicate = (head ns) : ns
                            | inst == Pop = (drop 1 ns)

--addIns ns = sum(take 2 ns): (drop 2 ns)
--multIns ns = product(take 2 ns) : (drop 2 ns)
--duplIns ns = (head ns) : ns
--popIns ns = (drop 1 ns)
--executeInstruction ns [] = []
--executeInstructions ns (inst:ins) | inst == Add = addIns ns
--                                  | inst == Multiply = multIns ns
--                                  | inst == Duplicate = duplIns ns
--                                  | inst == Pop = popIns ns
executeInstructionSequence ns [] = []
executeInstructionSequence ns [x] = executeInstructions ns x
executeInstructionSequence ns (inst:ins) = executeInstructionSequence (executeInstructions ns inst) ins





findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers [] = []
findBusyBeavers xs | tryMultiply xs > tryAdd xs && tryMultiply xs > tryPop xs= [[Multiply]]
                   | tryAdd xs > tryMultiply xs && tryAdd xs > tryPop xs = [[Add]]
                   | tryPop xs > tryAdd xs && tryPop xs > tryMultiply xs = [[Pop]]
                   | tryMultiply xs > tryAdd xs && tryMultiply xs == tryPop xs = [[Pop],[Multiply]]
                   | tryAdd xs > tryMultiply xs && tryAdd xs == tryPop xs = [[Pop],[Add]]
                   | tryAdd xs == tryMultiply xs && tryAdd xs > tryPop xs = [[Multiply],[Add]]
                   | otherwise = [[Pop],[Multiply],[Add]]

tryMultiply :: [Int] -> Int
tryMultiply [] = 0
tryMultiply ns = head ns * head (tail ns)

tryAdd :: [Int] -> Int
tryAdd [] = 0
tryAdd ns = head ns + head (tail ns)

tryPop:: [Int] -> Int
tryPop [] = 0
tryPop ns = head (tail ns)