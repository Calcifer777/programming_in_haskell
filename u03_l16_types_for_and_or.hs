data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving Show
data BreakfastMeat = Sausage | Bacon | Ham deriving Show
data BreakfastMain = Egg | Pancake | Waffle deriving Show

-- Product types

data AuthorP = AuthorP {
    firstName :: String,
    secondName :: String
}

-- Sum types

type FirstName = String 
type MiddleName = String 
type LastName = String 

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

data Author = Author Name deriving Show

data Creator = AuthorCreator Author | ArtistCreator Artist

data Artist = Person Name | Band String

data Book = Book {
    author :: Creator,
    isbn :: Creator,
    bookTitle :: String,
    bookYear :: Int,
    bookPrice :: Double
}

data VynilRecord = VynilRecord {
    artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name :: String,
    description :: String,
    toyPrice :: Double
}

data StoreItem = BookItem Book 
                | RecordItem VynilRecord
                | ToyItem CollectibleToy

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"

instance Show Creator where
    show (AuthorCreator author) = show author
    show (ArtistCreator artist) = show artist