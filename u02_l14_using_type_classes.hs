data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"

{-
instance Enum SixSidedDie where
    toEnum 1 = S1
    toEnum 2 = S2
    toEnum 3 = S3
    toEnum 4 = S4
    toEnum 5 = S5
    toEnum 6 = S6
    toEnum _ = error "No such value"

    fromEnum S1 = 1
    fromEnum S2 = 2
    fromEnum S3 = 3
    fromEnum S4 = 4
    fromEnum S5 = 5
    fromEnum S6 = 6
-}

newtype Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where 
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [Name ("Emil", "Cioran"),
         Name ("Eugene", "Thacker"),
         Name ("Friedrich", "Nietsche")]

