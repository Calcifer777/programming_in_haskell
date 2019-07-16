type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type Weight = Int

type PatientName = (String, String)

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

patientInfo :: PatientName -> Age -> Height -> String
patientInfo patient age height = name ++ ageHeight
    where name = lastName patient ++ ", " ++ firstName patient
          ageHeight = "( " ++ show age ++ " yrs. " ++ show height ++ " in.)"

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RHType = Pos | Neg

showRH :: RHType -> String
showRH Pos = "+"
showRH Neg = "-"

data ABOType = A | B | AB | O

showABOType :: ABOType -> String
showABOType A = "A"
showABOType B = "B"
showABOType AB = "AB"
showABOType O = "O"

data BloodType = BloodType ABOType RHType

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABOType abo ++ showRH rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False -- otherwise

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l 
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l 
        
data Patient = Patient {
    name :: Name,
    sex :: Sex,
    age :: Age,
    height :: Height,
    weight :: Weight,
    bloodType :: BloodType
    }
