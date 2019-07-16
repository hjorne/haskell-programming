module MaybeApplicative where

import IdentityApplicative

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

validateLength :: Int
               -> String
               -> Maybe String
validateLength maxLen s 
    | length s > maxLen = Nothing
    | otherwise         = Just s
            

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address    
mkAddress s = Address <$> validateLength 100 s
-- mkAddress s = fmap Address $ validateLength 100 s

data Person = 
    Person Name Address 
    deriving (Eq, Show)

mkPerson :: String
         -> String
         -> Maybe Person 
-- mkPerson n a = 
--     case mkName n of
--         Nothing -> Nothing
--         Just n' -> case mkAddress a of
--             Nothing -> Nothing
--             Just a' -> Just $ Person n' a'
mkPerson n a = Person <$> mkName n <*> mkAddress a
-- Instead of doing pure Person <*> and using pure to lift Person to the Maybe
-- context, we just use fmap directly and save a function call


-- If an operator is left associative, it means it evalues its left arguments first


