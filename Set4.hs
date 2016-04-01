{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}


module Set4 where

import MCPrelude
import Set2 
import Data.List (concatMap)


{-
generalA :: (a -> b) -> Gen a -> Gen b --set1
transMaybe :: (a -> b) -> Maybe a -> Maybe b --set2
liftM 



generalB :: (a -> b -> c) ->  Gen a -> Gen b -> Gen c  --set1
ylink ::  (a -> b -> c) ->  Maybe a -> Maybe b -> Maybe c --set2
allComb :: (a->b->c) -> [a] -> [b] -> [c] --set3
liftM2 --Set4

genTwo :: Gen a -> (a -> Gen b) -> Gen b --set1
link :: Maybe a -> (a -> Maybe b) -> Maybe b --set2
bind --set4



mkGen :: a -> Gen a --set1
mkMaybe :: a -> Maybe a --set2
return --Set4
-}



{-
generalA :: (a -> b) -> m a -> m b 

ylink ::  (a -> b -> c) ->  m a -> m b -> mc 

link :: m a -> (a -> m b) -> m b -

mk :: a -> m a 
-}



newtype Gen a = Gen {getGen :: (Seed -> (a,Seed))}

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f =Gen (\s ->  let  (a,s') = (getGen ga) s
                         in  (getGen  (f a)) s')

evalGen :: Gen a -> Seed -> a
evalGen g s = fst $ getGen g $ s

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a



instance Monad Maybe  where
  bind Nothing f = Nothing
  bind (Just a) f = f a 
  return a = Just a
  

instance Monad [] where
  bind x f = concatMap f x
  return a = [a]


instance Monad Gen where
  bind g f = genTwo g f
  return a = Gen (\s-> (a,s))




sequence ::(Monad m) =>  [m a] -> m [a]
sequence [] = return []
sequence (x:xs) = bind x (\a -> liftM (a:)  (sequence xs))
                          
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f ma = bind ma (\a -> return $ f a)

liftM2 ::(Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = bind ma (\a -> bind mb
                                (\b -> return (f a b)))


liftM3 ::(Monad m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = bind ma (\a -> bind mb
                                   (\b -> bind mc
                                          (\c -> return (f a b c))))


(<<=) ::(Monad m) =>  (a-> m b) -> m a -> m b
f <<= ma = bind ma f


join ::(Monad m) => m (m a) -> m a
join ma = bind ma id


ap ::(Monad m) => m (a->b) -> m a -> m b
ap mf ma = bind mf (\f -> liftM f ma)




--Redoing Set1 with monads

randG = Gen rand
randLetter = Gen (\s -> let (i,s') = rand s
                        in (toLetter i,s'))
fiverands :: [Integer]
fiverands =   fst $(getGen (sequence  ((replicate 5 randG)))  (mkSeed 1))

randString3 :: String
randString3 =  fst $(getGen (sequence  ((replicate 3 randLetter)))  (mkSeed 1))

randEven :: Gen Integer
randEven = liftM (+2) randG

randOdd :: Gen Integer
randOdd = liftM (+2) randEven


randPair :: Gen (Char,Integer)
randPair = liftM2 (,) randLetter randG


generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair ga gb = liftM2 (,) ga gb



--redoing set2 with monads

--queryGreek :: GreekData -> String -> Maybe Double
--queryGreek d s = 
