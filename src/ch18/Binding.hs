module Binding where

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "Name please:"
    name <- getLine
    putStrLn $ "Name: " ++ name

bindingAndSequencing' :: IO ()
bindingAndSequencing' = 
    putStrLn "Name please:" >> getLine >>= \name -> putStrLn $ "Name: " ++ name

twoBinds :: IO ()
twoBinds = do
    putStrLn "Name please: "
    name <- getLine
    putStrLn "Age please: "
    age <- getLine
    putStrLn $ "Name: " ++ name ++ " age: " ++ age

twoBinds' :: IO ()
twoBinds' = 
    putStrLn "Name please: " >>
    getLine >>= 
    \name -> 
    putStrLn "Age please: "  >>
    getLine >>=
    \age ->
    putStrLn $ "Name: " ++ name ++ " age: " ++ age


twoBinds'' :: IO ()
twoBinds'' = putStrLn "Name please: " >> getLine >>= \name -> putStrLn "Age please: " >> getLine >>= \age -> putStrLn $ "Name: " ++ name ++ " age: " ++ age