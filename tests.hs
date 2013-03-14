import Test.HUnit
import qualified Control.Monad as CM
import qualified Tests.Hakit.TestHakit as H
import qualified Tests.Hakit.Spice as S
import qualified Tests.Hakit.Validation as V
import qualified Tests.Hakit.Db.Mongo as M

-- Returns true if all tests pass.
testAll :: IO Bool
testAll = CM.liftM (all (== True)) . sequence $ map (\x -> runTestTT x >>= passed) [
        H.tests,
        S.tests,
        V.tests,
        M.tests
    ]

passed :: Counts -> IO Bool
passed c = return $ errors c == 0 && failures c == 0