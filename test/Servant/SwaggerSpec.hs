{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Servant.SwaggerSpec where

import           Control.Lens
import           Data.Aeson
    (ToJSON(..), Value, decodeFileStrict', genericToJSON)
import qualified Data.Aeson.Types as JSON
import           Data.Char (toLower)
import           Data.Int (Int64)
import           Data.Maybe (fromJust)
import           Data.Proxy
import           Data.Swagger
import           Data.Text (Text)
import           Data.Time
import           GHC.Generics
import           Servant.API
import           Servant.Swagger
import           Servant.Test.ComprehensiveAPI (comprehensiveAPI)
import           Test.Hspec hiding (example)

#if !MIN_VERSION_swagger2(2,4,0)
import           Data.Aeson.Lens (key, _Array)
import qualified Data.Vector as V
#endif

checkAPI :: HasSwagger api => Proxy api -> Value -> IO ()
checkAPI proxy = checkSwagger (toSwagger proxy)

checkSwagger :: Swagger -> Value -> IO ()
checkSwagger swag js = toJSON swag `shouldBe` js

spec :: Spec
spec = describe "HasSwagger" $ do
  it "Todo API" $ do
    todo <- todoAPI
    checkAPI (Proxy :: Proxy TodoAPI) todo
  it "Hackage API (with tags)" $ do
    hackage <- hackageAPI
    checkSwagger hackageSwaggerWithTags hackage
  it "GetPost API (test subOperations)" $ do
    getPost <- getPostAPI
    checkSwagger getPostSwagger getPost
  it "Comprehensive API" $ do
    let _x = toSwagger comprehensiveAPI
    True `shouldBe` True -- type-level test

main :: IO ()
main = hspec spec

-- =======================================================================
-- Todo API
-- =======================================================================

data Todo = Todo
  { created :: UTCTime
  , title   :: String
  , summary :: Maybe String
  } deriving (Generic)

instance ToJSON Todo
instance ToSchema Todo

newtype TodoId = TodoId String deriving (Generic)
instance ToParamSchema TodoId

type TodoAPI = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo

todoAPI :: IO Value
todoAPI = fromJust <$> decodeFileStrict' "test/todoAPI.json"

-- =======================================================================
-- Hackage API
-- =======================================================================

type HackageAPI
    = HackageUserAPI
 :<|> HackagePackagesAPI

type HackageUserAPI =
      "users" :> Get '[JSON] [UserSummary]
 :<|> "user"  :> Capture "username" Username :> Get '[JSON] UserDetailed

type HackagePackagesAPI
    = "packages" :> Get '[JSON] [Package]

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int64  -- Word64 would make sense too
  } deriving (Eq, Show, Generic)

lowerCutPrefix :: String -> String -> String
lowerCutPrefix s = map toLower . drop (length s)

instance ToJSON UserSummary where
  toJSON = genericToJSON JSON.defaultOptions { JSON.fieldLabelModifier = lowerCutPrefix "summary" }

instance ToSchema UserSummary where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = lowerCutPrefix "summary" } proxy
    & mapped.schema.example ?~ toJSON UserSummary
         { summaryUsername = "JohnDoe"
         , summaryUserid   = 123 }

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int64
  , groups   :: [Group]
  } deriving (Eq, Show, Generic)
instance ToSchema UserDetailed

newtype Package = Package { packageName :: Text }
  deriving (Eq, Show, Generic)
instance ToSchema Package

hackageSwaggerWithTags :: Swagger
hackageSwaggerWithTags = toSwagger (Proxy :: Proxy HackageAPI)
  & host ?~ Host "hackage.haskell.org" Nothing
  & applyTagsFor usersOps    ["users"    & description ?~ "Operations about user"]
  & applyTagsFor packagesOps ["packages" & description ?~ "Query packages"]
  where
    usersOps, packagesOps :: Traversal' Swagger Operation
    usersOps    = subOperations (Proxy :: Proxy HackageUserAPI)     (Proxy :: Proxy HackageAPI)
    packagesOps = subOperations (Proxy :: Proxy HackagePackagesAPI) (Proxy :: Proxy HackageAPI)

hackageAPI :: IO Value
hackageAPI = modifyValue . fromJust <$> decodeFileStrict' "test/hackageAPI.json"
  where
    modifyValue :: Value -> Value
#if MIN_VERSION_swagger2(2,4,0)
    modifyValue = id
#else
    -- swagger2-2.4 preserves order of tags
    -- swagger2-2.3 used Set, so they are ordered
    -- packages comes before users.
    -- We simply reverse, not properly sort here for simplicity: 2 elements.
    modifyValue = over (key "tags" . _Array) V.reverse
#endif


-- =======================================================================
-- Get/Post API (test for subOperations)
-- =======================================================================

type GetPostAPI = Get '[JSON] String :<|> Post '[JSON] String

getPostSwagger :: Swagger
getPostSwagger = toSwagger (Proxy :: Proxy GetPostAPI)
  & applyTagsFor getOps ["get" & description ?~ "GET operations"]
  where
    getOps :: Traversal' Swagger Operation
    getOps = subOperations (Proxy :: Proxy (Get '[JSON] String)) (Proxy :: Proxy GetPostAPI)

getPostAPI :: IO Value
getPostAPI = fromJust <$> decodeFileStrict' "test/getPostAPI.json"
