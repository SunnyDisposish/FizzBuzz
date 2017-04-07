import Data.Monoid
import Data.Maybe
import Control.Applicative

zzer :: Integral a => a -> String -> a -> Maybe String
zzer m s n = if n `mod` m == 0 then Just s else Nothing

fizzer :: Integral a => a -> Maybe String
fizzer = zzer 3 "fizz"

buzzer :: Integral a => a -> Maybe String
buzzer = zzer 5 "buzz"

fizzbuzz :: (Integral a, Show a) => a -> String
fizzbuzz n = fromJust $ (fizzer <> buzzer $ n) <|> (Just $ show n)

main :: IO ()
main = do
    let res = map fizzbuzz [1..100]
    print res