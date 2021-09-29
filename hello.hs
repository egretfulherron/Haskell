main :: IO()
main = putStrLn (greet "World")

greeting = "Hello "

greet who = greeting ++ "," ++ who