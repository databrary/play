-- | This exists to get the full library into the coverage report.
--
-- By including Main, the test exe inherits the full dependency graph,
-- pulling in a few dozen more top-level definitions. Naturally, none of the
-- missing modules are exercised by tests, so until this point, coverage numbers
-- were inflated. Happily, the number only changed by 1% when this fix was made,
-- from 17% to 16%.
module DatabraryMainTest where

import DatabraryMain

import Test.Tasty.HUnit

unit_flagConfig_example :: Assertion
unit_flagConfig_example = do
    flagConfig FlagEZID @?= Right FlagEZID
