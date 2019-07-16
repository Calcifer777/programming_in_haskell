import Data.List


-- 4.1

ifEven myFunction x = if even x
                      then myFunction x
                      else x

ifEvenCube x = ifEven (\x -> x^3) x 

-- custom sort
names = [("Ian", "Curtis"),
         ("Bernard", "Summer"),
         ("Peter", "Hook"),
         ("Stephen", "Morris")]

compareLastNames name1 name2 = if lastName1 > lastName2
    then GT
    else if lastName1 < lastName2
        then LT
        else EQ
    where lastName1 = snd name1
          lastName2 = snd name2

compareLastNamesV2 name1 name2
      | lastName1 > lastName2 =  GT
      | lastName1 < lastName2 = LT
      | otherwise = EQ
    where lastName1 = snd name1 
          lastName2 = snd name2

-- sortBy compareLastNamesV2 names


-- 4.2

sfOffice name = if lastName < "L"
                then nameText ++ ": PO Box 1234 - SF"
                else nameText ++ ": PO Box 1010 - SF"
    where lastName = snd name
          nameText = fst name ++ " " ++ snd name

nyOffice name = nameText ++ ": PO Box 789 - NY"
        where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ ": PO Box 456 - Reno"
        where nameText = snd name


getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    _ -> \name -> fst name ++ " " ++ snd name

addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location

-- addressLetter ("Bob", "Smith") "ny"

-- q41
compareLastNamesV3 name1 name2 = compare (snd name1) (snd name2)
