--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Flows   
    ( 

    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import qualified Numeric.LinearAlgebra as LA
import Configuration.Forex.Currencies
import qualified Data.Map as Map
import Data.List (sort, nub)
import Simulator

--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------
newtype IndexVector = IndexVector {getMap :: Map.Map Integer [Double]}
type IndexMatrix = Map.Map Integer  [[Double]]

--------------------------------------------------------------------------
------------------------------ Classes -----------------------------------
--------------------------------------------------------------------------
class Flow f where
    getFlowValue :: (Simulator s) => s -> f -> LA.Vector Double
    getPayDate   :: f -> Integer
    getFlowDates :: f -> [Integer]

class Observable o where
    getObsValue :: (Simulator s) => s -> o -> LA.Vector Double
    getObsDates :: o -> [Integer]

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------
data One = One {oneCurr :: String, oneDate :: Integer}
data Scale o f = Scale{scaleObs :: o, scaleFlow :: f}

--------------------------------------------------------------------------
------------------------------ Instances ---------------------------------
--------------------------------------------------------------------------
instance Flow One where
    getFlowValue s o = LA.fromList $ replicate 10 1
    getPayDate o = oneDate o
    getFlowDates o = [oneDate o]

instance (Observable o, Flow f) => Flow (Scale o f) where
    getFlowValue s scl = getObsValue s (scaleObs scl) * getFlowValue s (scaleFlow scl) 
    getPayDate scl = getPayDate $ scaleFlow scl 
    getFlowDates scl = sort . nub $ getObsDates (scaleObs scl) 
                                    ++ getFlowDates (scaleFlow scl) 
 











