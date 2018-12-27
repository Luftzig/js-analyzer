module AstSpec (spec) where

import           Data.Aeson
import           Test.Hspec

import           Analyze.WalkAst
import           Data



spec :: Spec
spec =
  describe "Walking the AST" $ do
    let getJson name = decodeFileStrict ("test/ast-snippets/" ++ name) :: IO (Maybe Value)
    let nodeCounts = visitNode (countPatterns)
    let sumCounts = foldl sumFileStats emptyStats
    let getCount count ast = fmap (count . sumCounts . nodeCounts) ast

    describe "for loop" $ do
      it "counts one for loop" $ do
        mj <- getJson "for.json"
        getCount forCount mj `shouldBe` Just 1

    describe "for-in loop" $ do
      it "counts 1 for-in loop" $ do
        mj <- getJson "for-in.json"
        getCount forInCount mj `shouldBe` Just 1

    describe "for-of loop" $ do
      it "counts 1 for-of loop" $ do
        mj <- getJson "for-of.json"
        getCount forOfCount mj `shouldBe` Just 1

    describe "while loop" $ do
      it "counts 1 while loop" $ do
        mj <- getJson "while.json"
        getCount whileCount mj `shouldBe` Just 1

      it "counts do while as while" $ do
        mj <- getJson "do-while.json"
        getCount whileCount mj `shouldBe` Just 1

    describe "map call" $ do
      it "counts map function call" $ do
        mj <- getJson "map-function.json"
        getCount mapCount mj `shouldBe` Just 1

      it "counts map method call `[].map(fn)`" $ do
        mj <- getJson "map-method.json"
        getCount mapCount mj `shouldBe` Just 1

    describe "forEach call" $ do
      it "counts forEach call" $ do
        mj <- getJson "for-each-method.json"
        getCount forEachCount mj `shouldBe` Just 1

    describe "complex examples" $ do
      describe "file with class, for each and for-in" $ do
        mj <- runIO $ getJson "complex-for-each-for-in.json"

        it "finds 1 forEach" $ do
          getCount forEachCount mj `shouldBe` Just 1

        it "finds 1 for-in" $ do
          getCount forInCount mj `shouldBe` Just 1
