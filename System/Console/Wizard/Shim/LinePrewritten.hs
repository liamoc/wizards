{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, Trustworthy #-}
module System.Console.Wizard.Shim.LinePrewritten ( -- $module
                                                 ) where 
import System.Console.Wizard.Internal    
-- $module    
-- This module exports a shim instance of 'Run' to make all back-ends that support 'Line' support 'LinePrewritten'
-- simply by ignoring the default text.
-- Don't import this if you're using a back-end that already supports 'LinePrewritten'.
instance (Run m Line) => Run m LinePrewritten where runAlgebra (LinePrewritten p s1 s2 w) = runAlgebra (Line p w)