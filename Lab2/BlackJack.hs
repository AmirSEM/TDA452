{- Lab 2
   Date: 
   Authors: Johan Larsson johx@student.chalmers.se
   Lab group: 77
 -}
module BlackJack where

import Cards
import RunGame
import Test.QuickCheck



hand0 = Empty
hand1 = Add (Card Jack Spades) hand0
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)


sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) hand0))
            , size (Add (Card (Numeric 2) Hearts) hand1)
            , size hand2
            ,2]



displayCard :: Card -> String
displayCard (Card r s) = show (r)  ++ " of " ++ show (s)

display :: Hand -> String
display Empty = ""
display (Add c h) = displayCard c ++ ", " ++ display h


value :: Hand -> Integer
value Empty = 0
value (Add c h)
   | initVal_ <= 21 = initVal_
   | initVal_ > 21 && numAces_ > 0 = initVal_ - 10*numAces_ 
   where
      initVal_ = initialValue (Add c h)
      numAces_ = numberOfAces (Add c h)

      initialValue :: Hand -> Integer
      initialValue Empty = 0
      initialValue (Add c' h') = (valueRank (rank c')) + initialValue h'

      numberOfAces :: Hand -> Integer
      numberOfAces Empty = 0
      numberOfAces (Add c' h') 
         | rank c' == Ace = 1 + (numberOfAces h')
         | otherwise = numberOfAces h'

      valueRank :: Rank -> Integer
      valueRank r
         | r == Jack = 10
         | r == Queen = 10
         | r == King = 10
         | r == Ace = 11
         | otherwise = valueNumeric r
         where
            valueNumeric :: Rank -> Integer
            valueNumeric (Numeric n)
               | n < 2 || n > 10 = error "Out of bounds numeric value"
               | otherwise = n



-- Trying to do option 2 as well but am unsure
--value' :: Hand -> Integer
--value Empty = 0
--value (Add c h) = undefined
--where
--   aceValue :: Hand -> Integer -> Integer
--   aceValue (Add c h) acePoints
--      | aceValue (Add c' h') = ()


q = Add (Card King Hearts) (Add (Card Ace Hearts) (Add (Card Ace Diamonds) Empty))
q' = Add (Card King Hearts) (Add (Card Ace Diamonds) Empty)