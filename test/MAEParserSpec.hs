{-# LANGUAGE OverloadedStrings #-}

module MAEParserSpec where

import           Bio.MAE              (Block (..), Mae (..), Table (..),
                                       Value (..), fromFile)
import           Bio.MAE.Parser       (blockP, maeP, tableP, versionP)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Data.Attoparsec.Text (parseOnly)
import qualified Data.Map.Strict      as M (fromList)
import           Data.Text            (Text)
import           Test.Hspec

maeParserSpec :: Spec
maeParserSpec = describe "Mae format parser." $ do
    versionPSpec
    tablePSpec
    blockPSpec
    maePSpec

versionPSpec :: Spec
versionPSpec = describe "version parser" $
    it "one and only version" $ parseOnly versionP version `shouldBe` Right "2.0.0"
  where
    version :: Text
    version = "{\n  s_m_m2io_version\n ::: \n 2.0.0 \n } \n"

tablePSpec :: Spec
tablePSpec = describe "table parser" $ do
    it "simple table" $ parseOnly tableP simpleTableT `shouldBe` Right simpleTable
    it "missing values" $ parseOnly tableP tableWithMissingValuesT `shouldBe` Right tableWithMissingValues
    it "negative values" $ parseOnly tableP tableWithNegativeValuesT `shouldBe` Right tableWithNegativeValues
    it "quoted values" $ parseOnly tableP tableWithQuotedValuesT `shouldBe` Right tableWithQuotedValues
    it "with comments" $ parseOnly tableP tableWithCommentsT `shouldBe` Right tableWithComments

blockPSpec :: Spec
blockPSpec = describe "block parser" $ do
    it "simple block" $ parseOnly blockP simpleBlockT `shouldBe` Right simpleBlock
    it "with many tables" $ parseOnly blockP blockWithManyTablesT `shouldBe` Right blockWithManyTables

maePSpec :: Spec
maePSpec = describe "parses mae files up to the EOF" $ do
    it "Capri.mae" $ parseFileSpec "test/MAE/Capri.mae"
    it "h2o.mae" $ parseFileSpec "test/MAE/h2o.mae"
    it "docking_1.mae" $ parseFileSpec "test/MAE/docking_1.mae"
    it "docking_2.mae" $ parseFileSpec "test/MAE/docking_2.mae"

parseFileSpec :: FilePath -> Expectation
parseFileSpec path = do
    t <- fromFile path
    pure ()

simpleTableMap :: [(Text, [Value])]
simpleTableMap = [ ("i_val", fmap IntValue [1, 2, 3])
                 , ("r_val", fmap RealValue [1.0, 2.28, 3.22])
                 , ("s_val", fmap StringValue ["aaa", "bbb", "ccc"])
                 , ("b_val", fmap BoolValue [False, True, False])
                 ]

simpleTable :: Table
simpleTable = Table "simple_table" $ M.fromList simpleTableMap

simpleTableT :: Text
simpleTableT = "  simple_table[3] {\n  i_val \n  r_val   \n  s_val\n b_val\n ::: \n 1 1 1.0 aaa 0  \n2 2 2.28 bbb 1\n  3 3 3.22   ccc 0 \n   ::: \n  } \n"

tableWithMissingValuesMap :: [(Text, [Value])]
tableWithMissingValuesMap = [ ("i_can_be_missing", fmap IntValue [1, 2, 3] <> [Absent, Absent])
                            , ("r_can_be_missing", fmap RealValue [1.0, 2.28] <> [Absent, Absent, RealValue 3.22])
                            , ("s_can_be_missing", [Absent, Absent] <> fmap StringValue ["aaa", "bbb", "ccc"])
                            , ("r_can_be_missing_1", replicate 5 Absent)
                            , ("r_can_be_missing_2", [RealValue 1.0, Absent, Absent] <> fmap RealValue [2.28, 3.22])
                            , ("s_can_be_missing_1", fmap StringValue ["aaa", "bbb", "ccc", "ddd", "eee"])
                            ]

tableWithMissingValues :: Table
tableWithMissingValues = Table "table_with_missing_values" $ M.fromList tableWithMissingValuesMap

tableWithMissingValuesT :: Text
tableWithMissingValuesT = "table_with_missing_values[5]{  \n i_can_be_missing\n r_can_be_missing \n s_can_be_missing \n r_can_be_missing_1 \n r_can_be_missing_2 \n s_can_be_missing_1 \n :::\n 1 1 1.0 <> <> 1.0 aaa\n 2 2 2.28 <> <> <> bbb \n 3 3 <> aaa <> <> ccc \n 4 <> <> bbb <> 2.28 ddd \n 5 <> 3.22 ccc <> 3.22 eee \n ::: \n } \n"

tableWithNegativeValuesMap :: [(Text, [Value])]
tableWithNegativeValuesMap = [ ("i_neg", fmap IntValue [-1, -2, -3])
                             , ("r_neg", fmap RealValue [-1.0, -2.28, -3.22])
                             ]

tableWithNegativeValues :: Table
tableWithNegativeValues = Table "table_with_negative_values" $ M.fromList tableWithNegativeValuesMap

tableWithNegativeValuesT :: Text
tableWithNegativeValuesT = "table_with_negative_values[3]{  \n i_neg\n r_neg \n ::: \n 1 -1 -1.0 \n 2 -2 -2.28 \n 3 -3 -3.22\n ::: \n} \n"

tableWithQuotedValuesMap :: [(Text, [Value])]
tableWithQuotedValuesMap = [ ("s_quote0", fmap StringValue ["\"   ssss s \"", "\" more quotes \"", "\" this is long and dull comment \""])
                           , ("r_neg", fmap RealValue [-1.0, -2.28, -3.22])
                           , ("s_quote", fmap StringValue ["\" \"", "aaa", "\" this is long and dull comment \""])
                           , ("r_neg", fmap RealValue [-1.0, -2.28, -3.22])
                           ]

tableWithQuotedValues :: Table
tableWithQuotedValues = Table "table_with_quoted_values" $ M.fromList tableWithQuotedValuesMap

tableWithQuotedValuesT :: Text
tableWithQuotedValuesT = "table_with_quoted_values[3]{  \n s_quote0\n r_neg \n s_quote\n r_neg \n ::: \n 1 \"   ssss s \" -1.0 \" \" -1.0 \n 2 \" more quotes \" -2.28 aaa -2.28 \n 3 \" this is long and dull comment \" -3.22 \" this is long and dull comment \" -3.22\n ::: \n} \n"

tableWithCommentsMap :: [(Text, [Value])]
tableWithCommentsMap = [ ("i_val", replicate 7 Absent)
                       , ("r_val", replicate 7 Absent)
                       ]

tableWithComments :: Table
tableWithComments = Table "table_with_comments" $ M.fromList tableWithCommentsMap

tableWithCommentsT :: Text
tableWithCommentsT = "  table_with_comments[7] {\n  # comments here? \n # this is useful comment # \n i_val \n # i can write \" any \" thing  in these comment \' s \n r_val \n # comments even here? \n ::: \n 1 <> <> \n 2 <> <> \n 3 <> <> \n 4 <> <> \n 5 <> <> \n 6 <> <> \n 7 <> <> \n ::: \n  } \n"

simpleBlockMap :: [(Text, Value)]
simpleBlockMap = [ ("i_val", IntValue 3)
                 , ("r_val", RealValue 2.28)
                 , ("s_val", StringValue "\"aaa\"")
                 ]

simpleBlock :: Block
simpleBlock = Block "simple_block" (M.fromList simpleBlockMap) [simpleTable]

simpleBlockT :: Text
simpleBlockT = "simple_block {\n i_val \n r_val \n s_val \n ::: \n 3 \n 2.28 \n \"aaa\"\n  " <> simpleTableT <> "}\n"

blockWithManyTablesMap :: [(Text, Value)]
blockWithManyTablesMap = [ ("i_val", IntValue 3)
                         , ("s_val", StringValue "\" path/ to /my /favorite /dir\"")
                         , ("r_val", RealValue (-2.28))
                         , ("s_val1", StringValue "aaa")
                         ]

blockWithManyTables :: Block
blockWithManyTables = Block "block_with_many_tables" (M.fromList blockWithManyTablesMap) [simpleTable, tableWithComments, tableWithNegativeValues]

blockWithManyTablesT :: Text
blockWithManyTablesT = "block_with_many_tables {\n i_val \n s_val \n r_val \n s_val1 \n ::: \n 3 \n \" path/ to /my /favorite /dir\" \n -2.28 \n aaa\n  " <> simpleTableT <> "\n" <> tableWithCommentsT <> "\n" <> tableWithNegativeValuesT <> "}\n"
