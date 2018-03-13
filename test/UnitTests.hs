import qualified UnitTests.WarningValidation
-- the test is that the imported modules build correctly
main :: IO ()
main = do
  UnitTests.WarningValidation.main
