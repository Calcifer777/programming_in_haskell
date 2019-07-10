-- constructor of cup object. Store flOz value
cup flOz = \method -> method flOz

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
    where ozDiff = flOz - ozDrank
          flOz = getOz aCup

isEmpty aCup = getOz aCup == 0
