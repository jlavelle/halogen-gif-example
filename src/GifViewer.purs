module GifViewer where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Argonaut (Json, decodeJson, jsonParser, (.?))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import DOM.Event.KeyboardEvent (key)

type State =
  { loading :: Boolean
  , gifUrl :: String
  , errorMessage :: String
  , topic :: String
  }

data Query a
  = LoadGif a
  | UpdateTopic String a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action LoadGif)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState = { loading: false, gifUrl: "", errorMessage: "", topic: "cats" }

  render :: State -> H.ComponentHTML Query
  render st = 
    HH.div_
      [ HH.h1_ 
          [HH.text "Halogen Gif Viewer!"]
      , HH.div_
        [ HH.p_ 
            [ HH.text $ "Current topic: " <> st.topic ]
        , HH.input
            [ HE.onValueInput $ HE.input UpdateTopic
            , HE.onKeyPress $ (\k -> if key k == "Enter" then (HE.input_ LoadGif unit) else Nothing)
            , HP.value st.topic
            ]
        , HH.button
            [ HE.onClick $ HE.input_ LoadGif]
            [ HH.text "Load Gif"]
        ]
      , HH.div_ [
          HH.img 
            [HP.src st.gifUrl]
      ]
      , if st.loading then HH.p_ [HH.text "Loading..."] else HH.p_ []
      , HH.p_ [HH.text st.errorMessage]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    LoadGif next -> do
      H.modify (_ { loading = true })
      topic <- H.gets _.topic 
      eUrl  <- H.liftAff $ randomGifUrl topic
      case eUrl of
        Right gifUrl -> H.modify (_ { gifUrl = gifUrl, loading = false, errorMessage = "" })
        Left _       -> H.modify (_ { errorMessage = "No gif found for topic " <> topic, loading = false})
      pure next
    UpdateTopic t next -> do
      H.modify (_ { topic = t })
      pure next


randomGifUrl :: forall eff. String -> Aff (ajax :: AX.AJAX | eff) (Either String String)
randomGifUrl topic = do
  r <- _.response <$> AX.get (baseUrl <> topic)
  let url = jsonParser r >>= decodeImageUrl
  pure url

  where baseUrl = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag="

decodeImageUrl :: Json -> Either String String
decodeImageUrl s = do
  obj <- decodeJson s
  dat <- obj .? "data"
  url <- dat .? "image_url"
  pure url