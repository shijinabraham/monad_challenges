{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MonadComprehensions #-}

module Set2 where

import MCPrelude



data Maybe a = Just a | Nothing

instance (Show a) =>  Show (Maybe a)  where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ (show a)


instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  (Just a) == (Just b) = a == b
  _ == _ = False

maximum [a] = a
maximum (a:as) = let mas = maximum as
                 in if a > mas then a else mas

minimum [a] = a
minimum (a:as) = let mas = minimum as
                 in if a < mas then a else mas 
                     
                        

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a:as) = Just a


tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (a:as) = Just as




lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay k [] = Nothing
lookupMay k ((a,b):xs) = if a == k
                           then Just b
                           else lookupMay k xs 


divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay dd dv = if dv == 0
               then Nothing
               else Just (dd/dv)


maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay a = Just (maximum a)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay a = Just (minimum a) 


--greekdata :: [(String,[Integer])]

queryGreek :: GreekData -> String -> Maybe Double
queryGreek l k = case lookupMay k l of Nothing -> Nothing
                                       Just v -> let h = headMay v
                                                     t = tailMay v
                                                 in case h of Nothing -> Nothing
                                                              Just h' -> case t of Nothing -> Nothing
                                                                                   Just t' -> case ( maximumMay t') of  Nothing -> Nothing
                                                                                                                        Just m' -> divMay (fromIntegral m')  (fromIntegral h')




chain :: (a -> Maybe b) ->  Maybe a -> Maybe b
chain f a = case a of Nothing -> Nothing
                      Just a' -> f a'



link :: Maybe a -> (a -> Maybe b) -> Maybe b
link a f = chain f a 


queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 l k = link (lookupMay k l) (\v -> link (headMay v)
                                                (\h -> link (tailMay v)
                                                       (\t -> link (maximumMay t)
                                                              (\m -> divMay (fromIntegral m)  (fromIntegral h)))))




ylink ::  (a-> b -> c) ->  Maybe a -> Maybe b -> Maybe c
{-ylink f a b  = case a of Nothing -> Nothing
                         Just a' -> case b of Nothing -> Nothing
                                              Just b' -> mkMaybe (f a' b')
-}
ylink f a b = link a (\a' ->
                       link b (\b' -> mkMaybe (f a' b')))
                             
                             
                       
addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
{-addSalaries s n1 n2 =  case lookupMay n1 s of Nothing -> Nothing
                                              Just s1  -> case lookupMay n2 s of Nothing -> Nothing
                                                                                 Just s2 -> Just (s1 + s2)
-}

addSalaries s n1 n2 = ylink (+) (lookupMay n1 s) (lookupMay n2 s) 




{-
generalA :: (a->b) -> Maybe a -> Maybe b
generalA f a = case a of Nothing -> Nothing
                         Just a' -> Just (f a')
-}

mkMaybe :: a -> Maybe a
mkMaybe a = Just a


{-
tailProd :: (Num a) => [a] -> Maybe a
tailProd l = case tailMay l of Nothing -> Nothing
                               Just t -> mkMaybe (product t)


tailSum :: (Num a) => [a] -> Maybe a
tailSum l = case tailMay l of Nothing -> Nothing
                              Just t -> mkMaybe (sum t)
-}

transMaybe :: (a->b) -> Maybe a -> Maybe b
transMaybe f a = case a of Nothing -> Nothing
                           Just a' -> mkMaybe (f a')



tailProd :: (Num a) => [a] -> Maybe a
tailProd l = transMaybe product (tailMay l)
                        


tailSum :: (Num a) => [a] -> Maybe a
tailSum l = transMaybe product (tailMay l)


tailMin :: (Ord a) => [a] -> Maybe (Maybe a)
tailMin l = transMaybe minimumMay (tailMay l)

tailMax :: (Ord a) => [a] -> Maybe (Maybe a)
tailMax l = transMaybe maximumMay (tailMay l)



combine :: (Maybe (Maybe a)) -> Maybe a
combine Nothing = Nothing
combine (Just a) = a


