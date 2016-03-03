{-# LANGUAGE RankNTypes #-}

module Lists where

class Listy l where
  values :: l -> [String]
  find :: l -> String -> Maybe String
  delete :: l -> String -> l
  add :: l -> String -> l

data SingleList = SNil | SCons String SingleList

instance Listy SingleList where
  values SNil = []
  values (SCons s rest) = s : values rest

  find SNil _ = Nothing
  find (SCons s rest) match | s == match = Just match
                            | otherwise = find rest s

  delete SNil _ = SNil
  delete (SCons s rest) match | s == match = rest
                              | otherwise = SCons s (delete rest match)

  add SNil snew = SCons snew SNil
  add (SCons s rest) snew = SCons s (add rest snew)



data DoubleList = DNil | DCons String DoubleList | DSnoc DoubleList String

instance Listy DoubleList where
  values DNil = []
  values (DCons s rest) = s : values rest
  values (DSnoc rest s) = values rest ++ [s]

  find DNil _ = Nothing
  find (DCons s rest) match | s == match = Just match  
                            | otherwise = find rest s
  find (DSnoc rest s) match | s == match = Just match
                            | otherwise = find rest s
                              
  delete DNil _ = DNil
  delete (DCons s rest) match | s == match = rest
                              | otherwise = DCons s (delete rest match)
  delete (DSnoc rest s) match | s == match = rest
                              | otherwise = DSnoc (delete rest match) s
                                
  add DNil snew = DCons snew DNil
  add (DCons s rest) snew = DCons s (add rest snew)  
  add (DSnoc rest s) snew = DSnoc (add rest snew) s


newtype ChurchList = ChurchList { runChurch :: forall r. (String -> r -> r) -> r -> r }

chNil = ChurchList (\_ nil -> nil)

chCons s (ChurchList ss) = ChurchList cns
  where cns = (\cons nil -> cons s (ss cons nil))

fromList :: [String] -> ChurchList
fromList = foldr chCons chNil

instance Show ChurchList where
  show = show . values 

instance Listy ChurchList where
  values l = runChurch l (:) []

  find l match = runChurch l fold Nothing
    where fold s Nothing | s == match = Just match
                         | otherwise = Nothing
          fold _ r = r

  delete l match = runChurch l fold chNil
    where fold s r | s == match = r
                   | otherwise = chCons s r

  add l snew = runChurch l chCons (chCons snew chNil)
