{-# LANGUAGE OverloadedStrings #-}
module Renderer where

-- This renderer is just partial completed

import CMarkGFM
import Lucid
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as T

renderType :: Monad m => NodeType -> HtmlT m () -> HtmlT m ()
renderType (TEXT text) = \_ -> toHtml text
renderType (DOCUMENT) = div_ [class_ "markdown"]
renderType (PARAGRAPH) = p_ [class_ "paragraph"]
renderType (THEMATIC_BREAK) = \_ -> hr_ []
renderType (LINEBREAK) = \_ -> hr_ []
renderType (SOFTBREAK) = \_ -> br_ []
renderType (HTML_BLOCK html) = \_ -> toHtmlRaw html
renderType (HEADING level) = case level of
    1 -> h1_ []
    2 -> h2_ []
    3 -> h3_ []
    4 -> h4_ []
    5 -> h5_ []
    _ -> h6_ []
renderType _ = span_ [class_ "empty-placeholder"]

render :: Monad m => Node -> HtmlT m ()
render (Node _ ty nodes) = renderType ty $ do
    mconcat $ map render nodes 

renderMD :: T.Text -> T.Text
renderMD text = Lazy.toStrict $ renderText $ render $ commonmarkToNode [] [] text