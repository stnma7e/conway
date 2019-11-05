module Patterns
( blinker
, glider
) where

import Lib (CellState(..))

n = 5

blinker _ = [[Dead | i <- [1..n]],[Dead | i <- [1..n]],[Dead | i <- [1..n]], [Dead, Dead, Alive, Alive, Alive, Dead, Dead], [Dead | i <- [1..n]],[Dead | i <- [1..n]],[Dead | i <- [1..n]]]

glider = [[Dead | i <- [1..n]],[Dead,Dead,Alive,Dead,Dead],[Dead, Dead, Dead, Alive, Dead], [Dead,Alive,Alive,Alive,Dead],[Dead | i <- [1..n]]]
