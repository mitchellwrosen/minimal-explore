module Parser.Parser where

import Text.Parsec

position :: Parser (Int, Int, Int)
position = parens $ 
    (,,) <$> (token $ int <* char ',') 
         <*> (token $ int <* char ',') 
         <*> (token int)

color :: Parser (Int, Int, Int)
color = position
