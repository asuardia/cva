--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Flows2   
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
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------
data Flow = Flow {
                     flowValue :: LA.Vector Double,
                     flowPayDate   :: Integer,
                     flowDates :: [Integer]
                 }
data Observable = Observable {
                                 obsValue :: LA.Vector Double,
                                 obsDates :: [Integer]
                             }


--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
one :: String -> Integer -> Flow
one curr payDate = Flow {
                            flowValue = LA.fromList $ replicate 10 1,
                            flowPayDate = payDate,
                            flowDates = [payDate]                   
                        }    
scale :: Observable -> Flow -> Flow
scale obs fl = Flow {
                        flowValue = (obsValue obs) * (flowValue fl),
                        flowPayDate = flowPayDate fl,
                        flowDates = sort . nub $ (obsDates obs) 
                                                 ++ (flowDates fl)           
                    }













