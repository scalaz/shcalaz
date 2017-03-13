{-# LANGUAGE NoImplicitPrelude #-}

module Language.Scala where

import Papa
import Text.Parser.Char(CharParsing, anyChar)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse)

newtype Upper =
  Upper
    Char
  deriving (Eq, Ord, Show)

class AsUpper a where
  _Upper ::
    Prism' a Upper

instance AsUpper Char where
  _Upper =
    prism'
      (\(Upper c) -> c)
      (\c -> if isUpper c then Just (Upper c) else Nothing)

-- |
--
-- >>> parse upper "upper" "ABC"
-- Right (Upper 'A')
--
-- >>> isLeft (parse upper "upper" "abc")
-- True
upper ::
  (Monad m, CharParsing m) =>
  m Upper
upper =
  let p = anyChar >>= \c -> case c ^? _Upper of
                              Nothing ->
                                empty
                              Just d ->
                                return d
  in  p <?> "upper"
