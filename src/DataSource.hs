{-# LANGUAGE TemplateHaskell #-}

module DataSource (
    connect
  , convTypes
  , defineTable
  , Connection
) where

import           Data.Int                            (Int32)

import           Database.HDBC.MySQL
import           Database.HDBC.Query.TH              (defineTableFromDB')
import           Database.HDBC.Schema.Driver         (typeMap)
import           Database.HDBC.Schema.MySQL          (driverMySQL)
import           Database.Relational.Query.Component (defaultConfig,
                                                      normalizedTableName)
import           GHC.Generics                        (Generic)
import           Language.Haskell.TH                 (Dec, Q, TypeQ)

connect :: IO Connection
connect = connectMySQL defaultMySQLConnectInfo { mysqlDatabase = "INFORMATION_SCHEMA" }

convTypes :: [(String, TypeQ)]
convTypes = [("MEDIUMINT", [t|Int32|])]

defineTable :: String -> Q [Dec]
defineTable tableName =
  defineTableFromDB'
    connect
    (defaultConfig { normalizedTableName = False })
    (driverMySQL { typeMap = convTypes })
    "hagemai"
    tableName
    [''Show, ''Generic]


