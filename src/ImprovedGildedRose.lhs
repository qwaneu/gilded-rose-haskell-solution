ImprovedGildedRose implements an Abstract Data Type that exports just a handle for the NewItem and the updateQuality' function.

We start with an empty implementation, so we can see if we like the types, and we can run the tests quickly.


\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ImprovedGildedRose  where

import Test.QuickCheck 
import Control.Applicative ((<$>),(<*>))

newtype Days = Days { days :: Int} deriving (Eq, Show, Num, Ord)

instance Arbitrary Days where
  arbitrary = Days <$> choose(-5,40)

newtype Quality = Quality {quality :: Int} deriving (Eq, Show, Num, Ord)

instance Arbitrary Quality where
  arbitrary = Quality <$> choose(0,70)

data ForSale = 
  AgedBrie | 
  BackstagePassesToTafkal |
  DexterityVest |
  ElixirOfTheMongoose | 
  SulfurasHandOfRagnoras |
  ConjuredManaCake |
  UnsupportedItem 

    deriving (Eq, Show)

data NewItem = NewItem ForSale Days Quality  deriving (Eq, Show)

instance Arbitrary NewItem where
  arbitrary = NewItem 
                <$> elements [DexterityVest, ElixirOfTheMongoose, UnsupportedItem, SulfurasHandOfRagnoras, AgedBrie, BackstagePassesToTafkal ]
                <*> arbitrary
                <*> arbitrary


\end{code}

On top of the legacy code there is a map over the items. We separate the map into a separate function, so we can write code for on just one item.

\begin{code}
updateQuality' :: [NewItem] -> [NewItem]
updateQuality' = map matureItem
\end{code}

QuickCheck doesn't know that a list of things is not different from one thing, and we don't want to refactor the original code. What can we do to test just one item?.

We can generate a list of length 1. quickchecks vectorOf is just the thing.

How do we write the first implementation? We guess, and expect the test to give us an example and an expectation. Then we go from there, designing as best we can.

I was stuck on the solution here, thinking there are no units or baby steps to take, with arbitrary data you get all the testdata at once. However, we can play with the Arbitraries we generate and increase their scope as we go.

I first started looking at just the code, because [the specification](https://github.com/NotMyself/GildedRose) is probably lying. The code with all its branches confused me, but one thing jumps out: some of the item types are special, but others are not. Less than a handful of item types are mentioned in the code, so maybe we should start with the regular item types. If we are lucky, what the spefication says about regular items might just be correct, and QuickCheck will tell us if we are wrong.

The regular item types are DexterityVest and ElixirOfTheMongoose. so we write 'elements [DexterityVest, ElixirOfTheMongoose]' in our Arbitrary, see the tests fail and implement the check for regular item. Then we add the special items one by one, get QuickCheck to pass, and possibly factor out some commonalities.

In our case UnsupportedItem is also a regular item. Anything the legacy code does not mention as a special item is a regular item, therefore an UnsuportedItem is also a regular item.

added properties: quality always > 0, days always decrease by 1. for regular items: quality' <= quality

\begin{code}

isRegularItem :: ForSale -> Bool
isRegularItem i = elem i [DexterityVest, ElixirOfTheMongoose, UnsupportedItem]

matureRegularItem rk sellin quality = rk sellin' quality'
  where sellin' = sellin - 1
        quality' = regularQuality sellin quality

matureAgedBrie rk sellin quality = rk sellin' q'
  where sellin' = sellin - 1
        q'            | (quality > 50)                             = quality -- this is a defect, spec says quality can not be more than 50
                      | otherwise                                  = boundedQuality withMaxQ
        withMaxQ      | sellin > expiryDate                        = quality + 1
                      | otherwise                                  = quality + 2
        expiryDate = 0


matureTafkal rk sellin quality = rk sellin' q'
  where sellin' = sellin - 1
        q'            | (quality > 50) && sellin > expiryDate       = quality -- this is a defect, spec says quality can not be more than 50
                      | otherwise                                   = boundedQuality withMaxQ
        withMaxQ      | sellin >  (expiryDate + 10)                 = quality + 1
                      | sellin >  (expiryDate + 5)                  = quality + 2
                      | sellin > expiryDate                         = quality + 3
                      | otherwise                                   = 0
        expiryDate = 0

boundedQuality q 
         | q >= 50 = 50
         | q <= 0  = 0
         | otherwise = q
    
matureItem :: NewItem -> NewItem
matureItem item@(NewItem kind sellIn quality) 
  | (isRegularItem kind) = matureRegularItem (NewItem kind) sellIn quality
  | kind == AgedBrie     = matureAgedBrie (NewItem kind) sellIn quality
  | kind == BackstagePassesToTafkal = matureTafkal (NewItem kind) sellIn quality
  | otherwise = item

\end{code}

At this point we could add the new conjured items, but we would have to leave our legacy code as reference behind. Or maybe we don't. We can use a separate generator and property for the new items, and run the legacy test only on the items it knows.

The implementation passes the test, but has quite a lot of duplication. I find it more understandable than the nested-if legacy code, so for me it is an improvement, but it is not pretty or close to minimal.

Before we refactor or add new items, we should check the coverage, to make sure we have all the crazy corner cases from the legacy code. In a real project this would also be a good moment to have some tough conversations about those cases. Do we really want to accept items that have quality over 50 as inputs? Do we want to keep changing the quality of items after the sellBy date? We probably won't sell them, so is the quality still relevant?


Code coverage

We add the -fhpc flag to our .cabal file, in the library and test section. When we run cabal test it produces a test coverage report:

dist/hpc/html/prop/hpc_index.html

It also provides a report for the sample program in Main

dist/hpc/html/gilded-rose-0.1.0.0/hpc_index.html

100% of Alternatives and expressions have been covered by our tests, both in the new and the old code. For the legacy code this is an improvement - the Main with sample data suplied covers 93% of Alternatives and 96% of expressions. This is reasonable coverage, but with this error-prone logic like this I prefer 100%.

The top level has not been 100% covered. It looks like all of the hand-written code is covered, but some of the typeclass implementations we let generate (Eq, Show) have not beeen used, even though quickCheck should use Eq for legacy Items and NewItems alike.

Conjured Items

So now we can finally create a conjured item. The old Main suggests "Conjured Mana Cake". The informal specification only tells us that "Conjured items degrade twice as fast as regular items". We assume it shares other properties with regular items. This would be a good time to convert those properties to code. I'm also wondering how best to express the "twice as fast" property. We have a number of regular items we can generate, and only one conjured item. We also have the aforementioned duplication in the code for maturing Items.

let's see if we can safely factor out the part of matureItem that calculates the change in quality, and write some properties for it.

\begin{code}

regularQuality :: Days -> Quality -> Quality
regularQuality sellin quality = quality'
  where
        quality' | quality == 0                               = 0
                 | otherwise = quality + delta
        delta    | sellin > expiryDate                        = - 1
                 | sellin <= expiryDate && quality > 1        = - 2
                 | otherwise                                  = - 1
        expiryDate = 0
\end{code}

I mixed this in matureQuality and it still works. We now have the delta separate. Since we defined sellin and quality as Days and Quality newtypes, defining new properties for this function is relatively straightforward.

We write the properties against this function, since we covered the end-to-end behaviour with the legacy test. Since we want to test several quality functions against the same constraints, we define a property that takes a day, a quality and a function to generate a new quality from those parameters.

The function to test is the first paramater. We will supply this when defining the final property, and let QuickCheck generate the Days and Quality

\begin{code}

qualityZeroOrMore :: (Days -> Quality -> Quality) -> Days -> Quality -> Bool
qualityZeroOrMore f s q = (f s q) >= 0

qualityLessThanFifty :: (Days -> Quality -> Quality) -> Days -> Quality -> Bool
qualityLessThanFifty f s q  = (f s q) <= 50

qualityDoesNotIncrease :: (Days -> Quality -> Quality) -> Days -> Quality -> Bool
qualityDoesNotIncrease f s q = (f s q) <= q

\end{code}


