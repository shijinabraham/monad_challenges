{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MonadComprehensions #-}

module Set1 where

import MCPrelude


--rand :: Seed -> (Integer,Seed)
--mkseed :: Integer -> Seed
--toLetter :: Integer -> Char



{-
fiverands :: [Integer]
fiverands = take 5 $ recur (mkSeed 1)
            where recur s = let (i,s') = rand s
                                in i:(recur s')



randString3 :: [Char]
randString3 = take 3 $ recurChar (mkSeed 1)
  where recurChar s = let (c,s') = randLetter s
                      in c:(recurChar s')
-}


{-
recur :: (Seed-> (t,Seed)) -> Seed  -> [t]
recur f s = let (k,s') = f s
                in k:(recur f s')


fiverands :: [Integer]
fiverands = take 5 $ recur rand (mkSeed 1)
            

randLetter :: Seed -> (Char,Seed)
randLetter s = let (i,s') = rand s
                   in (toLetter i,s')

randString3 :: [Char]
randString3 = take 3 $ recur randLetter (mkSeed 1)
-}

type Gen t = Seed -> (t,Seed)

--rand :: Gen Integer

recur :: Gen t -> Seed  -> [t]
recur f s = let (k,s') = f s
                in k:(recur f s')

generalA :: (a->b) -> Gen a -> Gen b
generalA f ga = (\s -> let (i,s') = ga s
                         in (f i,s'))


fiverands :: [Integer]
fiverands = take 5 $ recur rand (mkSeed 1)
            

randLetter :: Gen Char
randLetter s = let (i,s') = rand s
                   in (toLetter i,s')

randString3 :: [Char]
randString3 = take 3 $ recur randLetter (mkSeed 1)

{-

randEven :: Gen Integer
randEven s = let (i,s') = rand s
             in (i*2,s')

randOdd :: Gen Integer
randOdd s = let (i,s') = rand s
             in (i*2+1,s')
-}
randEven :: Gen Integer
randEven = generalA (*2) rand


{-randOdd :: Gen Integer
randOdd = generalA (\i->i*2+1) rand
-}

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

{-randTen :: Gen Integer
randTen = generalA (*10) rand
  -}

randTen :: Gen Integer
randTen = generalA (*5) randEven

--- map (\f-> fst $ f(mkSeed 1)) [randEven,randOdd,randTen]


{-randPair :: Gen (Char,Integer)
randPair s = let (c,s') = randLetter s
                 (i,s'') = rand s'
                 in ((c,i),s'')
-}

randPair :: Gen (Char,Integer)
--randPair = generalPair randLetter rand
randPair = generalPair2 randLetter rand 

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair ga gb s =   let (a,s') = ga s
                            (b,s'') = gb s'
                        in ((a,b),s'')

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

generalB :: (a->b->c) ->  Gen a -> Gen b -> Gen c
generalB fab ga gb s =   let (a,s') = ga s
                             (b,s'') = gb s'
                         in (fab a b,s'')

{-
repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([],s)
repRandom (g:gs) s = let (a,s') = g s
                         (as,s'') = repRandom gs s'
                     in (a:as,s'')

-}
--repRandom [rand,randEven,randOdd,randTen] (mkSeed 1)


genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga fab s = let (a,s') = ga s
                      in (fab a) s'


generalB2 :: (a->b->c) -> Gen a -> Gen b -> Gen c
generalB2 f a b = genTwo a (\a' ->
                             genTwo b (\b' -> mkGen (f a' b')))



                         

mkGen :: a -> Gen a
mkGen a = (\s -> (a,s))


repRandom :: [Gen a] -> Gen [a]
{-repRandom [] s = mkGen [] $  s
repRandom (g:gs) s = let (a,s') = g s
                         (as,s'') = repRandom gs s'
                     in (a:as,s'')

-}

repRandom []  = mkGen [] 
repRandom (g:gs)  = genTwo g (\a-> generalA (a:) (repRandom gs))


--repRandom (replicate 3 randLetter) (mkSeed 1)
