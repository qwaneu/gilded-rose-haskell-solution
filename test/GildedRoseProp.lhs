
\section{Legacy Code - an Answer}

Legacy Code is great, because it provides us with a test oracle 'for free' - the test oracle being the existing code. What we'll demonstrate here, is how to use existing, messy code and build a new version of that code that does everything the old code did, only in a more understandable way.

We're not going in to how to break dependencies to get to a point where one can test something in isolation. The legacy code we have here, from the GildedRose kata has one redeeming quality: it is already purely functional. 

Let's get started. We need to import QuickCheck, our existing code and it is probably best to make a new module for our new code. The extra constraint we give ourselves is to not touch the existing code. Normally, I'd be tempted to dive in and start refactoring, at least the bits in GildedRose that are type safe.

\begin{code}
module GildedRoseProp (runProps) where

import Test.QuickCheck
import GildedRose
import ImprovedGildedRose
import Debug.Trace (traceShow)
import Data.List (intercalate)
import Data.Monoid((<>)) 
import Control.Applicative ((<$>),(<*>))
\end{code}

The Item in GildedRose is stringly, not strongly typed. We could generate existing items, but that would have us write even more legacy code. Instead, we create a fromNewItem function that will convert NewItems to Items. That way we don't have to modify the existing code, and we can grow the NewItem in small steps.

We define a property that uses generated NewItems

\begin{code}

-- http://stackoverflow.com/questions/9977734/controlling-how-test-data-is-generated-in-quickcheck

newtype OneElementList = OneElementList [NewItem] deriving (Show, Eq)

instance Arbitrary OneElementList where
  arbitrary = sized $ \s -> do
                xs <- vectorOf 1 arbitrary
                return  (OneElementList xs)

propOldAndNewAreEqual :: OneElementList -> Bool
propOldAndNewAreEqual (OneElementList newItems) =
   debugShow $ new == nold
     where new = (updateQuality' newItems) 
           nold = toNewItems old
           old = (updateQuality $ fromNewItems newItems)
           debugShow = traceShow $ showAll newItems old new

showAll :: [NewItem] -> [Item] -> [NewItem] -> String
showAll inp old new = (show inp) <> "\n" <> (show old) <> show new

regularDaysGen :: Gen Days
regularDaysGen = Days <$> choose (0,40)

regularQualityGen :: Gen Quality
regularQualityGen = Quality <$> choose (0,50)

-- TODO generator for too much quality, and property to check quality does not change above 50
-- TODO generator for negative days

-- no quality above 50 or below zero, and no negative days
regularInputs :: Gen (Days, Quality)
regularInputs = (,) <$> regularDaysGen <*> regularQualityGen

-- running 100 tests does not always fail, so we need more
-- probably better distribute regular ones and irregular ones,
-- so each irregular one runs as offten as all the regular ones
runProps :: IO ()
runProps = do 
  quickCheckWith stdArgs { maxSuccess = 5000 } propOldAndNewAreEqual
  quickCheck $ qualityZeroOrMore regularQuality
  quickCheck (forAll regularInputs (uncurry (qualityLessThanFifty regularQuality)))
  quickCheck $ qualityDoesNotIncrease regularQuality

fromNewItems :: [NewItem] -> GildedRose
fromNewItems = map fromNewItem

fromNewItem :: NewItem -> Item
fromNewItem (NewItem l d q) = Item fs (days d) (quality q)
  where 
    fs = case l of
      AgedBrie ->  "Aged Brie"
      BackstagePassesToTafkal -> "Backstage passes to a TAFKAL80ETC concert"
      DexterityVest -> "+5 Dexterity Vest" 
      ElixirOfTheMongoose -> "Elixir of the Mongoose"
      SulfurasHandOfRagnoras -> "Sulfuras, Hand of Ragnaros"
      ConjuredManaCake -> "Conjured Mana Cake"
      UnsupportedItem -> "Unsupported Item"

toNewItems :: GildedRose -> [NewItem]
toNewItems = map toNewItem

-- copy all the items in initialInventory
toNewItem :: Item -> NewItem
toNewItem (Item l d q) = NewItem fs (Days d) (Quality q)
 where 
   fs = case l of
     "+5 Dexterity Vest" -> DexterityVest
     "Aged Brie" -> AgedBrie
     "Elixir of the Mongoose" -> ElixirOfTheMongoose
     "Sulfuras, Hand of Ragnaros" -> SulfurasHandOfRagnoras
     "Backstage passes to a TAFKAL80ETC concert" -> BackstagePassesToTafkal
     _ -> UnsupportedItem

\end{code}

The UnsupportedItem is interesting - Haskell was complaining about incomplete pattern matches. The problem with solving it like this is, that we will also generate unsupported items.
Maybe that is not a problem - how does the existing code respond when we pass in an unsupported item?



\subsection{Further Reading}



- Working effectively with legacy code
- The section on stateful testing / rest. This can help when dependencies can not yet be broken inside the program.
