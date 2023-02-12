module Gen_examples where

import Test.QuickCheck

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq)

data Rank = Numeric Integer | Jack | Queen | King | Ace    
    deriving (Show, Eq)

data Card = Card Rank Suit
    deriving (Show, Eq)

data Hand = Empty | Add Card Hand    
    deriving (Show, Eq)

rSuit :: Gen Suit
rSuit = elements [Spades, Hearts, Diamonds, Clubs]

rRank :: Gen Rank
rRank = elements $ [ Numeric n | n <- [2..10]] ++ [ Jack, Queen, King, Ace ]

rCard :: Gen Card
rCard = do
    s <- rSuit
    r <- rRank
    return $ Card r s

rHand :: Gen Hand
rHand = oneof 
            [return Empty, 
            do c <- rCard
               h <- rHand
               return $ Add c h] 

instance Arbitrary Suit where arbitrary = rSuit
instance Arbitrary Rank where arbitrary = rRank
instance Arbitrary Card where arbitrary = rCard
instance Arbitrary Hand where arbitrary = rHand