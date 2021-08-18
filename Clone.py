let

region = "us-west-1";

accessKeyId = "default";

in

{ machine = { config, pkgs, resources, ... }: {

deployment = {

targetEnv = "ec2";

ec2 = {

inherit accessKeyId region;

instanceType = "t2.nano";

keyPair = resources.ec2KeyPairs.my-key-pair;

securityGroups = [

resources.ec2SecurityGroups."http"

resources.ec2SecurityGroups."ssh"

];

};

};

networking.firewall.allowedTCPPorts = [ 80 ];

services.postgresql = {

enable = true;

authentication = ''

local all all ident map=mapping

'';

identMap = ''

mapping root postgres

mapping postgres postgres

'';

package = pkgs.postgresql_11;

initialScript = pkgs.writeText "initialScript.sql" ''

CREATE TABLE "user" (

name text NOT NULL,

PRIMARY KEY (name)

);

CREATE TABLE tweet (

id integer GENERATED ALWAYS AS IDENTITY,

contents text NOT NULL,

time TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,

author text NOT NULL,

PRIMARY KEY (id),

FOREIGN KEY ("author") REFERENCES "user" (name) ON DELETE CASCADE

);

CREATE TABLE follows (

follower text NOT NULL,

followed text NOT NULL,

PRIMARY KEY (follower, followed),

FOREIGN KEY (follower) REFERENCES "user" (name) ON DELETE CASCADE,

FOREIGN KEY (followed) REFERENCES "user" (name) ON DELETE CASCADE

);

'';

};

systemd.services.simple-twitter = {

wantedBy = [ "multi-user.target" ];

after = [ "postgresql.service" ];

script =

let

ghc =

pkgs.haskellPackages.ghcWithPackages (pkgs: [

pkgs.blaze-html

pkgs.blaze-markup

pkgs.exceptions

pkgs.http-api-data

pkgs.optparse-generic

pkgs.postgresql-simple

pkgs.servant

pkgs.servant-blaze

pkgs.servant-server

pkgs.text

pkgs.transformers

pkgs.warp

]

);

code = pkgs.writeText "Main.hs" ''

{-# LINE 90 "Main.hs" #-}

{-# LANGUAGE BlockArguments #-}

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Exception (SomeException)

import Control.Monad.IO.Class (MonadIO(..))

import Data.Foldable (traverse_)

import Data.Text (Text)

import Data.Proxy (Proxy(..))

import Data.Word (Word16)

import GHC.Generics (Generic)

import Options.Generic (ParseRecord)

import Prelude hiding (id)

import Database.PostgreSQL.Simple (FromRow, Only(..), Query, ToRow, (:.)(..))

import Database.PostgreSQL.Simple.SqlQQ (sql)

import Servant.HTML.Blaze (HTML)

import Servant.Server (Handler)

import Text.Blaze (Markup, (!))

import Text.Blaze.Html5 (AttributeValue)

import Web.HttpApiData (FromHttpApiData)

import Web.FormUrlEncoded (FromForm)

import Servant.API

( FormUrlEncoded

, Get

, Post

, QueryParam'

, ReqBody

, Required

, Strict

, (:>)

, (:<|>)(..)

)

import qualified Control.Exception as Exception

import qualified Control.Monad as Monad

import qualified Control.Monad.Catch as Catch

import qualified Database.PostgreSQL.Simple as PostgreSQL

import qualified Network.Wai.Handler.Warp as Warp

import qualified Options.Generic as Options

import qualified Servant.Server as Server

import qualified Text.Blaze.Html5 as Html

import qualified Text.Blaze.Html5.Attributes as Attr

newtype Options = Options { connectPort :: Word16 }

deriving stock (Generic)

deriving anyclass (ParseRecord)

newtype User = User { name :: Text }

deriving stock (Generic)

deriving anyclass (FromForm, FromRow, ToRow)

deriving newtype (FromHttpApiData)

data Follow = Follow { follower :: Text, followed :: Text }

deriving stock (Generic)

deriving anyclass (FromForm, ToRow)

data Tweet = Tweet { name :: Text, contents :: Text }

deriving stock (Generic)

deriving anyclass (FromForm, FromRow)

type API =

Get '[HTML] Markup

:<|> "user" :> ReqBody '[FormUrlEncoded] User :> Post '[HTML] Markup

:<|> "user" :> QueryParam' '[Required, Strict] "name" User :> Get '[HTML] Markup

:<|> "user" :> "delete" :> ReqBody '[FormUrlEncoded] User :> Post '[HTML] Markup

:<|> "users" :> Get '[HTML] Markup

:<|> "tweet" :> ReqBody '[FormUrlEncoded] Tweet :> Post '[HTML] Markup

:<|> "follow" :> ReqBody '[FormUrlEncoded] Follow :> Post '[HTML] Markup

main :: IO ()

main = do

Options {..} <- Options.getRecord "Simple Twitter"

let connectInfo =

PostgreSQL.defaultConnectInfo

{ PostgreSQL.connectPort = connectPort

, PostgreSQL.connectHost = ""

}

let open = PostgreSQL.connect connectInfo

let close = PostgreSQL.close

Exception.bracket open close \connection -> do

let execute :: (MonadIO io, ToRow input) => input -> Query -> io ()

execute input q = liftIO do

_ <- PostgreSQL.execute connection q input

return ()

let query

:: (MonadIO io, ToRow input, FromRow output)

=> input -> Query -> io [output]

query input q = liftIO (PostgreSQL.query connection q input)

let query_

:: (MonadIO io, FromRow output)

=> Query -> io [output]

query_ q = liftIO (PostgreSQL.query_ connection q)

let submit label =

Html.button

! Attr.type_ "submit"

! Attr.class_ "btn btn-primary btn-sm"

$ label

let field :: AttributeValue -> Markup

field name = do

Html.div ! Attr.class_ "form-group" $ do

Html.input

! Attr.type_ "text"

! Attr.class_ "form-control form-control-sm"

! Attr.name name

! Attr.placeholder name

let form action method html =

Html.div ! Attr.class_ "col-md-4" $ do

Html.form

! Attr.action action

! Attr.method method

! Attr.class_ "border m-3 p-2 bg-light"

$ html

let forms :: Markup

forms = do

Html.div ! Attr.class_ "row" $ do

form 

