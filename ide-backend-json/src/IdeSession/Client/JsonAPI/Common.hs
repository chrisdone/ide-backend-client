{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
module IdeSession.Client.JsonAPI.Common where

import Prelude hiding ((.), id)
import Control.Category
import Data.Aeson (Value)
import Data.Monoid
import Data.StackPrism
import Data.StackPrism.TH
import Data.Text (Text)
import Language.JsonGrammar
import qualified Data.Aeson.Types          as Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as BS (toString, fromString)
import qualified Data.ByteString.Lazy      as Lazy
import qualified Data.ByteString.Lazy.UTF8 as Lazy (toString, fromString)
import qualified Data.Text                 as Text

import IdeSession.Client.JsonAPI.Aux

{-------------------------------------------------------------------------------
  Stack prisms

  Stack prisms are just prisms with a special kind of type. See Data.StackPrism.
-------------------------------------------------------------------------------}

$(deriveStackPrismsWith prismNameForConstructor ''Maybe)

-- | Apply a prism to the top of the stack
top :: StackPrism a b -> StackPrism (a :- t) (b :- t)
top prism = stackPrism (\(a :- t) -> (forward prism a :- t))
                       (\(b :- t) -> (:- t) `fmap` backward prism b)

-- | Construct a stack prism from an isomorphism
iso :: (a -> b) -> (b -> a) -> StackPrism (a :- t) (b :- t)
iso f g = top (stackPrism f (Just . g))

{-------------------------------------------------------------------------------
  Auxiliary grammar definitions
-------------------------------------------------------------------------------}

instance Json String          where grammar = string
instance Json Bool            where grammar = bool
instance Json Lazy.ByteString where grammar = lazyByteString
instance Json BS.ByteString   where grammar = bytestring

-- | Define a grammar by enumerating the possible values
--
-- For example:
--
-- > bool :: Grammar Val (Value :- t) (Bool :- t)
-- > bool = enumeration [(True, "true"), (False, "false")]
enumeration :: Eq a => [(a, Text)] -> Grammar Val (Value :- t) (a :- t)
enumeration = mconcat . map aux
  where
    aux (a, txt) = defaultValue a . literal (Aeson.String txt)

-- | String literal
string :: Grammar Val (Value :- t) (String :- t)
string = fromPrism (iso Text.unpack Text.pack) . grammar

bool :: Grammar Val (Value :- t) (Bool :- t)
bool = enumeration [(True, "true"), (False, "false")]

-- | Lazy bytestring literal
lazyByteString :: Grammar Val (Value :- t) (Lazy.ByteString :- t)
lazyByteString = fromPrism (iso Lazy.fromString Lazy.toString) . grammar

-- | Strict bytestring literal
bytestring :: Grammar Val (Value :- t) (BS.ByteString :- t)
bytestring = fromPrism (iso BS.fromString BS.toString) . grammar

-- | Optional property
optProp :: Json a => Text -> Grammar Obj t (Maybe a :- t)
optProp propName = fromPrism just . prop propName <> fromPrism nothing

{-------------------------------------------------------------------------------
  Misc Utils

  Useful for ide-backend-client and its client.
-------------------------------------------------------------------------------}

sliceSpans :: Int -> Text -> [(Int, Int, a)] -> [(Text, Maybe a)]
sliceSpans _ txt _ | Text.null txt = []
sliceSpans _ txt [] = [(txt, Nothing)]
sliceSpans ix txt ((fr, to, x) : xs) =
    appendNonNull before Nothing $
    appendNonNull chunk (Just x) $
    sliceSpans to rest' xs
  where
    appendNonNull t mx = if Text.null t then id else ((t, mx) :)
    (chunk, rest') = Text.splitAt ((to - ix) - fr') rest
    (before, rest) = Text.splitAt fr' txt
    fr' = max 0 (fr - ix)
