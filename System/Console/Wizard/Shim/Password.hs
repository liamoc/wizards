{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, Trustworthy #-}
module System.Console.Wizard.Shim.Password ( -- $module
                                           ) where 

import System.Console.Wizard.Internal    
-- $module    
-- This module exports a shim instance of 'Run' to make all back-ends that support 'Line' support 'Password'
-- simply by ignoring the mask character and reading input with 'Line'.
-- Don't import this if you're using a back-end that already supports 'Password'.
instance (Run m Line) => Run m Password where runAlgebra (Password p mc w) = runAlgebra (Line p w)  
