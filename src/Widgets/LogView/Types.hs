module Widgets.LogView.Types where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Data.Aeson (Value (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Exts (IsList (..))
import GHC.Generics
import Graphics.Vty qualified as V
import System.Info (os)
import System.Directory (findExecutable)
import Type.Log (Log (..))
import Type.Name
import Type.WidgetSize (WidgetSize (Auto))
import Widgets.Editor (emptyEditor)

data LogViewWidget = LogViewWidget
  { selectedLog :: Maybe Log
  , jsonPathEditor :: B.Editor Text Name
  , showJsonpath :: Bool
  , width :: WidgetSize
  , height :: WidgetSize
  , jsonpathFilteredValue :: Either Text Value
  , copyMethod :: CopyMethod
  , copyMethodToggler :: CopyMethod -> CopyMethod
  }
  deriving (Generic)

data CopyMethod = Native | Osc52

emptyLogWidget :: IO LogViewWidget
emptyLogWidget = do
  cmt <- mkCopyMethodToggler
  pure LogViewWidget
    { selectedLog = Nothing
    , jsonPathEditor = emptyEditor (mkName LogViewWidgetJsonpathEditor)
    , showJsonpath = False
    , jsonpathFilteredValue = Right (Array $ fromList [])
    , width = Auto
    , height = Auto
    , copyMethod = Osc52
    , copyMethodToggler = cmt
    }

data LogViewWidgetEvent
  = LogSelected Log
  | Scroll Int
  | Click LogViewWidgetName B.Location B.Location
  | Move LogViewWidgetName B.Location B.Location
  | Key V.Key [V.Modifier]

newtype LogViewWidgetCallbacks s = LogViewWidgetCallbacks
  {copied :: B.EventM Name s ()}

mkName :: LogViewWidgetName -> Name
mkName = WidgetName . LogViewWidgetName

jsonpathPrefix :: Text
jsonpathPrefix = "$"

mkCopyMethodToggler :: IO (CopyMethod -> CopyMethod)
mkCopyMethodToggler = case os of
    "linux" -> do
      hasXclip <- checkPresence "xclip"
      pure $ if hasXclip then toggle else const Osc52
    _ -> pure toggle -- pbcopy on MacOS and clip on windows should be preinstalled
    where
      toggle :: CopyMethod -> CopyMethod
      toggle Osc52 = Native
      toggle Native = Osc52

      checkPresence :: String -> IO Bool
      checkPresence command = isJust <$> findExecutable command
