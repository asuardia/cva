--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Flows3
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
data Flow = One {
                    flCurrency :: String,
                    flPayDate  :: Integer
                }
          | Scale {
                      flObs  :: Observable,
                      flFlow :: Flow
                  }

data Observable = Const {
                            obsValue :: Double
                        }
                | Bond {
                            obsFixDate :: Integer,
                            obsPayDate :: Integer,
                            obsCurve :: String
                       }


--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
one :: String -> Integer -> Flow
one curr payDate = One {
                           flCurrency = curr,
                           flPayDate  = payDate                   
                       }    
scale :: Observable -> Flow -> Flow
scale obs fl = Scale {
                         flObs = obs,
                         flFlow = fl                   
                     }    

valueFlow :: (Simulator s) => s -> Flow -> LA.Vector Double
valueFlow s One {} = LA.fromList $ replicate 10 1
valueFlow s Scale {flObs = o, flFlow = f} = (valueObs s o) * (valueFlow s f)


valueObs :: (Simulator s) => s -> Observable -> LA.Vector Double
valueObs s Const {obsValue = ov} = LA.fromList $ replicate 10 1
valueObs s Bond {
                    obsFixDate = fd,
                    obsPayDate = pd,
                    obsCurve = crv
                } = LA.fromList $ replicate 10 1












