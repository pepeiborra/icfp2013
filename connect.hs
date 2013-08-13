{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Connect where

import Control.Applicative
import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Maybe
import Data.Text (Text, toLower)
import qualified Data.Set as Set
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Types (statusCode, statusMessage)
import Text.Printf
import BasicPrelude
import System.IO.Unsafe
import qualified Prelude

import Language.BV.Parser
import Language.BV.Pretty
import Language.BV.Solve (Problem(..))
import Language.BV.Syntax

localMan = unsafePerformIO $ newManager def

mkURL path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=0153G7HQAEETJIyKol9T0vInmrzGJqX4SqzfpqamvpsH1H"

fetch :: Manager -> String -> Maybe L.ByteString -> ResourceT IO L.ByteString
fetch man path body = do
  req0 <- parseUrl (mkURL path)
  let req1 = req0 { method = "POST"}
  let req = case body of Nothing -> req1 ; Just b -> req1{requestBody = RequestBodyLBS b}
  let loop =
        httpLbs req man
        `catch` \case
                    StatusCodeException st _ _ | statusCode st == 429 -> do
                        liftIO $ printf "%s, waiting for 100 msec and retrying\n"
                                   (textToString $ show $ statusMessage st)
                        liftIO $ threadDelay 100000
                        loop
                    other -> fail (Prelude.show other)
  responseBody <$> loop

myProblems :: ResourceT IO (Maybe [Problem])
myProblems = fetch localMan "myproblems" Nothing >>= return.fromJust.decode
status :: Manager -> ResourceT IO (Maybe Status)
status man = fetch man "status" Nothing >>= return.decode

evalProblem man prob inputs = do
    let request = EvalRequest (Left $ probId prob) inputs
    -- liftIO $ printf "Sending eval request %s" (textToString $  show $ encode request)
    raw <- fetch man "eval" (Just $ encode request)
    let results = decode raw
    case results of
      Just(EvalResponse (Right xx)) ->
          return prob{values = zip inputs xx}
      Just(EvalResponse (Left err)) -> fail (textToString err)
      Nothing -> fail (textToString $ show raw)

train man size op = do
  let req = encode $ TrainRequest (Just size) op
  resp <- fetch man "train" (Just req)
  let (res :: Maybe TrainingProblem) = decode resp
  case res of
    Just x -> return x
    Nothing -> fail (textToString $ show resp)

trainBonus man ops = train man 42 TrainBonus

guess man prob (sol :: Prog String) = do
  let req = encode $ Guess (probId prob) (show sol)
  resp <- fetch man "guess" (Just req)
  liftIO $ evaluate resp
  let  (res :: Maybe GuessResponse) = decode resp
  case res of
    Just x -> return x
    Nothing -> fail (textToString $ show resp)

{-
  interface Problem {
    id: string;
    size: number;
    operators: string[];
    solved?: boolean;
    timeLeft?: number
  }
-}

instance FromJSON Problem where
    parseJSON (Object v) =
        Problem <$> v .: "id"
                <*> v .: "size"
                <*> ((Set.fromList . map readOp) <$> (v .: "operators"))
                <*> pure []

exProblem = "{\"id\":\"zyXZ9qDQVMfbhEecUf1KFcOC\",\"size\":22,\"operators\":[\"and\",\"if0\",\"not\",\"plus\",\"shl1\",\"shr16\",\"tfold\"]}"
{-
   interface EvalRequest {
    id?: string;
    program?: string;
    arguments: string[];
   }
-}
data EvalRequest = EvalRequest (Either Text Text) [Word64] 

instance ToJSON EvalRequest where
    toJSON (EvalRequest (Left id) args) =
        object [ "id" .= id, "arguments" .= map (printf "0x%x" :: Word64 -> String) args ]
    toJSON (EvalRequest (Right program) args) =
        object [ "program" .= program, "arguments" .= map (printf "0x%08x" :: Word64 -> String) args ]

test = toJSON (EvalRequest (Left "11" ) [11, 12])

{-
   interface EvalResponse {
     status: string;
     outputs?: string[];
     message?: string;
   }
-}
newtype EvalResponse = EvalResponse (Either Text [Word64]) deriving (Show)

instance FromJSON EvalResponse where
    parseJSON (Object v) = do
      status :: String <- v .: "status"
      case status of
        "ok" -> (EvalResponse . Right . map read) <$> v .: "outputs"
        "error" -> (EvalResponse . Left) <$> v .: "message"

{-

   interface Guess {
    id: string;
    program: string;
   }
-}
data Guess = Guess Text Text

instance ToJSON Guess where
    toJSON (Guess id program) = object [ "id" .= id, "program" .= program ]

{-
   interface GuessResponse {
     status: string;
     values?: string[];
     message?: string;
     lightning?: bool;
   }
-}
data GuessResponse = Win Bool
                   | Mismatch {input, expected, obtained :: Word64}
                   | GuessError Text

instance FromJSON GuessResponse where
    parseJSON (Object v) = do
      status :: String <- v .: "status"
      case status of
        "win"      -> (Win . fromMaybe False) <$> v .:? "lighning"
        "error"    -> GuessError <$> v .: "message"
        "mismatch" -> (\[i,e,o] -> Mismatch (read i) (read e) (read o)) <$> v .: "values"

instance Show GuessResponse where
    showsPrec _ (Win b) = ("Win" ++)
    showsPrec _ (GuessError t) = ("Error: " ++) . (textToString t ++)
    showsPrec _ (Mismatch i e o) =
        (printf "Mismatch! Input: 0x%x, Expected: 0x%x, Obtained: 0x%x" i e o ++)

{-
   interface TrainRequest {
    size?: number;
    operators?: string[];
   }
-}
data TrainOps = TrainBonus | TrainTFold | TrainFold | TrainSimple
data TrainRequest = TrainRequest (Maybe Int) TrainOps
instance ToJSON TrainRequest where
    toJSON (TrainRequest size TrainBonus) =
        object ( ("operators" .= ["bonus" :: Text]) : maybe [] (\size -> [ "size" .= size]) size)
    toJSON (TrainRequest size TrainTFold) =
        object ( ("operators" .= ["tfold" :: Text]) : maybe [] (\size -> [ "size" .= size]) size)
    toJSON (TrainRequest size TrainFold) =
        object ( ("operators" .= ["fold" :: Text]) : maybe [] (\size -> [ "size" .= size]) size)
    toJSON (TrainRequest size TrainSimple) =
        object ( ("operators" .= ([] :: [Text])) : maybe [] (\size -> [ "size" .= size]) size)

{-
   interface TrainingProblem {
     challenge: string;
     id: string;
     size: number;
     operators: string[];
   }
-}
data TrainingProblem = TrainingProblem (Prog String) Problem
                     | ParseFailure Text Text
                     deriving Show
instance FromJSON TrainingProblem where
    parseJSON (Object v) = do
      epgm <- parseProgram <$> v .: "challenge"
      case epgm of
        Left e -> ParseFailure (show e) <$> v .: "challenge"
        Right pgm -> do
          problem <- Problem
                        <$> v .: "id"
                        <*> v .: "size"
                        <*> ((Set.fromList . map readOp) <$> v .: "operators")
                        <*> pure []
          return (TrainingProblem pgm problem)

{-
 interface Status {
     easyChairId: string;
     contestScore: number;
     lightningScore: number;
     trainingScore: number;
     mismatches: number;
     numRequests: number;
     requestWindow: {
       resetsIn: number;
       amount: number;
       limit: number
     };
     cpuWindow: {
       resetsIn: number;
       amount: number;
       limit: number
     };
     cpuTotalTime:number;
   }
-}
data Status =
  Status { easyChairId :: Text,
           contestScore,
           lightningScore,
           trainingScore,
           mismatches,
           numRequests,
           cpuTotalTime :: Integer,
           requestWindow :: RequestWindow,
           cpuWindow :: RequestWindow
         } deriving (Show, Generic)

data RequestWindow =
  RequestWindow { resetsIn, amount, limit :: Integer }
                deriving (Show, Generic)

instance FromJSON RequestWindow
instance FromJSON Status
