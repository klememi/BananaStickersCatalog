import Test.Hspec

import qualified SupportSpec
-- import qualified CatalogSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    -- describe "Main app" CatalogSpec.spec
    describe "Supporting functions" SupportSpec.spec
