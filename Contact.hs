module Contact where

data Contact = Contact { ident :: String
	                      , firstname :: String
	                      , surname :: String
	                      , company :: String
	                      , phoneNumber :: String
	                      , email :: String
	                      , birthdate :: String
	                      } deriving (Show,Read,Eq)


instance Ord Contact where
  (Contact id1 _ _ _ _ _ _) `compare` (Contact id2 _ _ _ _ _ _) = id1 `compare` id2

printContact (Contact ident name surname company number email  birthdate) = do
	let string = ident ++ ". Name: " ++ name ++ " " ++ surname ++ " Company: " ++ company ++ " Phone: " ++ number ++ 
		" Email: " ++ email ++ " "++ " Birthday: " ++ birthdate 
	putStrLn string

fullName::Contact -> String
fullName (Contact  ident  firstname surname _ _ _ _ ) = ident++". "++firstname++" "++surname
changeFirstname :: Contact -> String -> Contact
changeFirstname (Contact  atr1 old atr3 atr4 atr5 atr6 atr7) new =  Contact atr1 new atr3 atr4 atr5 atr6 atr7
changeSurname :: Contact -> String -> Contact
changeSurname (Contact atr1 atr2 old atr4 atr5 atr6 atr7) new =  Contact atr1 atr2 new atr4 atr5 atr6 atr7
changeCompany :: Contact -> String -> Contact
changeCompany (Contact atr1 atr2 atr3 old atr5 atr6 atr7) new =  Contact atr1 atr2 atr3 new atr5 atr6 atr7
changePhoneNumber:: Contact -> String -> Contact
changePhoneNumber(Contact atr1 atr2 atr3 atr4 old atr6 atr7) new =  Contact atr1 atr2 atr3 atr4 new atr6 atr7
changeEmail :: Contact -> String -> Contact
changeEmail (Contact atr1 atr2 atr3 atr4 atr5 old atr7) new =  Contact atr1 atr2 atr3 atr4 atr5 new atr7
changeBirthday :: Contact -> String -> Contact
changeBirthday (Contact atr1 atr2 atr3 atr4 atr5 atr6 old) new =  Contact atr1 atr2 atr3 atr4 atr5 atr6 new
