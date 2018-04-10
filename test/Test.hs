module Main (main) where

import Test.Tasty

import qualified Data.Csv.ContribTest
import qualified Data.RangeSet.ParseTest
import qualified Databrary.Controller.IngestTest
import qualified Databrary.EZID.ANVLTest
import qualified Databrary.HTTP.Form.DeformTest
import qualified Databrary.HTTP.RequestTest
import qualified Databrary.Model.AuditTest
import qualified Databrary.Model.IngestTest
import qualified Databrary.Model.Metric.TypesTest
import qualified Databrary.Model.MetricTest
import qualified Databrary.Model.Party.TypesTest
import qualified Databrary.Model.Permission.TypesTest
import qualified Databrary.Model.Record.TypesTest
import qualified Databrary.Model.RecordTest
import qualified Databrary.Model.Stats.TypesTest
import qualified Databrary.Model.TimeTest
import qualified Databrary.OpsTest
import qualified Databrary.Service.MailTest
import qualified Databrary.Service.PasswdTest
import qualified Databrary.Store.ConfigTest
import qualified Databrary.StringUtilTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
    "databrary"
    [ Data.Csv.ContribTest.tests
    , Data.RangeSet.ParseTest.tests
    , Databrary.Controller.IngestTest.tests
    , Databrary.EZID.ANVLTest.tests
    , Databrary.StringUtilTest.tests
    , Databrary.HTTP.Form.DeformTest.tests
    , Databrary.HTTP.RequestTest.tests
    , Databrary.Model.AuditTest.tests
    , Databrary.Model.IngestTest.tests
    , Databrary.Model.Metric.TypesTest.tests
    , Databrary.Model.MetricTest.tests
    , Databrary.Model.Party.TypesTest.tests
    , Databrary.Model.Permission.TypesTest.tests
    , Databrary.Model.Record.TypesTest.tests
    , Databrary.Model.RecordTest.tests
    , Databrary.Model.Stats.TypesTest.tests
    , Databrary.Model.TimeTest.tests
    , Databrary.OpsTest.tests
    , Databrary.Service.MailTest.tests
    , Databrary.Service.PasswdTest.tests
    , Databrary.Store.ConfigTest.tests
    ]
