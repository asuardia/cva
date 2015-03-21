{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XTypeSynonymInstances #-}
--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module CVA   
    ( 
     IndexMatrix, IndexVector, 
     Deal, EuropeanDeal, Simulator, Regressor
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import qualified Data.Map as Map
import qualified Data.Monoid as M
import qualified Numeric.LinearAlgebra as LA
import Simulator

--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------
newtype IndexVector = IndexVector {getMap :: Map.Map Integer [Double]}
type IndexMatrix = Map.Map Integer  [[Double]]

--------------------------------------------------------------------------
------------------------------ Classes -----------------------------------
--------------------------------------------------------------------------
class Deal d where
    calcMtM :: d -> IndexVector

class EuropeanDeal ed where
    getFlows :: ed -> [Flow]

class Regressor r where
    doRegression :: r -> IndexVector -> IndexVector


--------------------------------------------------------------------------
------------------------------ Instances ---------------------------------
--------------------------------------------------------------------------
instance M.Monoid IndexVector where
    mempty  = IndexVector (Map.fromList [(1, [])])
    mappend iv1 iv2 = IndexVector (Map.fromList result)
        where result = zipWith zList list1 list2
              list1 = Map.toList $ getMap iv1
              list2 = Map.toList $ getMap iv2  
              zList = (\(i1, ns1) (i2, ns2) -> (i1, zipWith (+) ns1 ns2))
--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------
data Flow

--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
calcMtMEuropean :: (Simulator sim, EuropeanDeal deal, Regressor reg) => 
                   sim -> [Integer] -> reg -> deal -> IndexVector
                    
calcMtMEuropean sim obsDts reg deal = (doRegression reg) rawGrid
    where flows = getFlows deal
          rawGrid = M.mconcat $ fmap (projectFlow sim obsDts) flows

projectFlow :: (Simulator sim) => sim -> [Integer] -> Flow -> IndexVector
projectFlow sim obsDts flow = IndexVector (Map.fromList [(1, [0.0])])





































