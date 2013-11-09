module Text.Parsec where

import Data.Char (isDigit, strToInt)

infixl 3 <|>
infixl 4 <*>, <*, *>

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

parse :: String -> Parser a -> Either String (a, String)
parse s (Parser p) = p s

-- TODO: infix
(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> (Parser pa) = Parser $ \s ->
    case pa s of
        Left err      -> Left err
        Right (a, s') -> Right (f a, s')

-- TODO: infix
(<$) :: a -> Parser b -> Parser a
(<$) a pb = (const a) <$> pb

pure :: a -> Parser a
pure a = Parser $ \s -> Right (a, s)

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
(Parser pf) <*> (Parser pa) = Parser $ \s ->
    case pf s of
        Left err      -> Left err
        Right (f, s') ->
            case pa s' of
                Left err       -> Left err
                Right (a, s'') -> Right (f a, s'')

(*>) :: Parser a -> Parser b -> Parser b
(Parser pa) *> (Parser pb) = Parser $ \s ->
    case pa s of
        Left err      -> Left err
        Right (_, s') -> pb s'

(<*) :: Parser a -> Parser b -> Parser a
(Parser pa) <* (Parser pb) = Parser $ \s ->
    case pa s of
        Left err      -> Left err
        Right (a, s') ->
            case pb s' of
                Left err       -> Left err
                Right (_, s'') -> Right (a, s'')

(<|>) :: Parser a -> Parser a -> Parser a
(Parser p1) <|> (Parser p2) = Parser $ \s ->
    case p1 s of
        Left _        -> p2 s
        Right (a, s') -> Right (a, s')

many0 :: Parser a -> Parser [a]
many0 pa = Parser $ \s ->
    case parse s pa of
        Left _ -> Right ([], s)
        Right (a, s') -> parse s' $ (a:) <$> many0 pa

many1 :: Parser a -> Parser [a]
many1 pa = Parser $ \s ->
    case parse s pa of
        Left err -> Left err
        Right (a, s') -> parse s' $ (a:) <$> many0 pa

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s ->
    case s of
        []     -> Left "unsatisfied"
        (x:xs) -> if f x
                  then Right (x, xs)
                  else Left "unsatisfied"

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

int :: Parser Int
int = strToInt <$> many1 digit

between :: Parser a -> Parser b -> Parser c -> Parser c
between left right p = left *> p <* right

oneOf :: String -> Parser Char
oneOf (x:xs) = foldr (\a b -> char a <|> b) (char x) xs
oneOf _ = error "cannot call oneOf with empty string"

optional :: Parser a -> Parser ()
optional (Parser pa) = Parser $ \s ->
    case pa s of
        Left _        -> Right ((), s)
        Right (_, s') -> Right ((), s')

space :: Parser ()
space = () <$ oneOf " \n\t\r"

spaces :: Parser ()
spaces = () <$ many0 space

token :: Parser b -> Parser b
token = between spaces spaces

word :: Parser String
word = token alphaNum

alphaNum :: Parser String
alphaNum = many1 (digit <|> alpha)

alpha, lower, upper :: Parser Char
alpha = lower <|> upper
lower = oneOf ['a'..'z']
upper = oneOf ['A'..'Z']

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

