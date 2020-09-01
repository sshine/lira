{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- MIT License
--
-- Copyright (c) 2019 eToroX Labs
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE OverloadedStrings #-}

module Lira.Contract.Pretty
  ( pp
  ) where

import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Lira.Contract

pp :: Contract -> Text
pp = renderStrict . layoutPretty defaultLayoutOptions . ppC

ppC :: Contract -> Doc ann
ppC (Transfer tok from to) = ppC' "transfer" (map pretty [pack tok, tshow from, tshow to])
ppC (Scale maxf scalef con) = ppC' "scale" [pretty maxf, ppExpr scalef, ppC con]
ppC (Both con1 con2) = ppC' "both" [ppC con1, ppC con2]
ppC (Translate delay con) = ppC' "translate" [ppT delay, ppC con]
ppC (IfWithin memExp con1 con2) = ppIfWithin memExp con1 con2
ppC Zero = pretty @Text "zero"

ppC' :: Text -> [Doc ann] -> Doc ann
ppC' combinator args = pretty combinator <> tupled args

ppExpr :: Expr -> Doc ann
ppExpr = undefined

ppT :: Time -> Doc ann
ppT = \case
  Now -> pretty @Text "now"
  Seconds i -> ppT' "seconds" i
  Minutes i -> ppT' "minutes" i
  Hours i -> ppT' "hours" i
  Days i -> ppT' "days" i
  Weeks i -> ppT' "weeks" i

ppT' :: Text -> Integer -> Doc ann
ppT' time duration =
  pretty time <> tupled [pretty duration]

ppIfWithin :: MemExp -> Contract -> Contract -> Doc ann
ppIfWithin (MemExp time expr) con1 con2 =
  pretty @Text "if" <+> ppExpr expr <+> pretty @Text "within" <+> ppT time
    <+> pretty @Text "then" <+> ppC con1
    <+> pretty @Text "else" <+> ppC con2

tshow :: Show a => a -> Text
tshow = pack . show
