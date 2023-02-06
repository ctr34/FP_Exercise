{-# LANGUAGE RecordWildCards #-}

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)  

data Action = Action
  { actMessage :: String,
    actEffect :: Maybe Int
  }
  deriving (Show)

act2pair :: Action -> (String, Maybe Int)
act2pair Action {..} = (actMessage, actEffect)

p :: Person
p = Person { firstName = "Mike"
            , lastName = "Jones"
            , age = 3  
            , height = 180  
            , phoneNumber = "911"
            , flavor = "Rap" 
            }               

