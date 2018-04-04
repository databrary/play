module Main (main) where

import Test.Tasty
import qualified Databrary.HTTP.Form.DeformTest
import qualified Databrary.Controller.IngestTest
import qualified Databrary.Model.Audit.TypesTest
import qualified Databrary.Model.AuditTest
import qualified Databrary.Model.RecordTest
import qualified Databrary.Model.Record.TypesTest
import qualified Databrary.Model.Metric.TypesTest
import qualified Databrary.Model.MetricTest
import qualified Databrary.Model.IngestTest
import qualified Databrary.Model.Stats.TypesTest
import qualified Data.Csv.ContribTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "databrary"
    [ Databrary.HTTP.Form.DeformTest.tests
    , Databrary.Controller.IngestTest.tests
    , Databrary.Model.AuditTest.tests
    , Databrary.Model.IngestTest.tests
    , Databrary.Model.Record.TypesTest.tests
    , Databrary.Model.Metric.TypesTest.tests
    , Databrary.Model.MetricTest.tests
    , Databrary.Model.RecordTest.tests
    , Databrary.Model.Stats.TypesTest.tests
    , Data.Csv.ContribTest.tests
    ]
