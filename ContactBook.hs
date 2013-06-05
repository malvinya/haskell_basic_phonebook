module ContactBook where
import Contact as Contact
import Group as Group

data ContactBook = ContactBook { contacts :: [Contact]
                                                    ,groups :: [Group]
                                                    } deriving (Show)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						CONTACT LIST
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addContact :: ContactBook -> Contact -> ContactBook
addContact (ContactBook contactList g) new = ContactBook (new:contactList) g

removeContact :: ContactBook ->  Contact -> ContactBook
removeContact (ContactBook contactList g) contact =ContactBook (filter p contactList) g
	where p x = x /= contact

editContact::ContactBook->String->String->String->ContactBook
editContact book@(ContactBook contacts groups) ident attr value = 
	let 
		contact = getContactById (book) ident
	in case attr of
		"0"	-> addContact (removeContact book contact) (Contact.changeFirstname contact value)
		"1"	-> addContact (removeContact book contact) (Contact.changeSurname contact value)
		"2"	-> addContact (removeContact book contact) (Contact.changeCompany contact value )
		"3"	-> addContact (removeContact book contact) (Contact.changePhoneNumber contact value)
		"4"	-> addContact (removeContact book contact) (Contact.changeEmail contact value)
		"5"       -> addContact (removeContact book contact) (Contact.changeBirthday contact value)
		"6"       -> let 
				originalGroup = getGroupByName  book value
				updatedGroup = Group.addMember originalGroup contact
				groupName = Group.name originalGroup
			in addGroup(removeGroup book groupName) updatedGroup
		"7" 	-> let 
				originalGroup =  getGroupByName  book value
				updatedGroup = Group.removeMember originalGroup contact
				groupName = Group.name originalGroup
			in addGroup(removeGroup book groupName) updatedGroup

isAMemberById :: ContactBook -> String -> Bool
isAMemberById (ContactBook c g) ident = any(\x -> Contact.ident  x== ident) c 

getContactById :: ContactBook -> String -> Contact
getContactById (ContactBook [] g) ident = error "No person with id"
getContactById book@(ContactBook (first:contactList) g) ident
						| Contact.ident first == ident 			= first
						| otherwise  					= getContactById (ContactBook contactList g) ident

getContactListBySurname :: ContactBook -> String -> [Contact]
getContactListBySurname (ContactBook [] g) surname = []
getContactListBySurname  book@(ContactBook (first:contactList) g) surname
						|Contact.surname first == surname 		= first:getContactListBySurname (ContactBook contactList g) surname
						| otherwise					= getContactListBySurname  (ContactBook contactList g) surname

getContactListByEmail :: ContactBook -> String -> [Contact]
getContactListByEmail (ContactBook [] g) email = []
getContactListByEmail book@(ContactBook (first:contactList) g) email
						| Contact.email first == email 			= first:getContactListByEmail (ContactBook contactList g) email
						| otherwise					= getContactListByEmail (ContactBook contactList g) email

getContactListByPhoneNumber :: ContactBook -> String -> [Contact]
getContactListByPhoneNumber (ContactBook [] g) phoneNumber = []
getContactListByPhoneNumber book@(ContactBook (first:contactList) g) phoneNumber
						| Contact.phoneNumber first == phoneNumber 	= first:getContactListByPhoneNumber (ContactBook contactList g) phoneNumber
						| otherwise 					= getContactListByPhoneNumber (ContactBook contactList g) phoneNumber																																					

getContactListByCompany :: ContactBook -> String -> [Contact]
getContactListByCompany (ContactBook [] g) company = []
getContactListByCompany book@(ContactBook (first:contactList) g) company 
						| Contact.company first == company 		= first:getContactListByCompany (ContactBook contactList g) company
						| otherwise 					= getContactListByCompany (ContactBook contactList g) company

getContactListByBirthdate :: ContactBook -> String -> [Contact]
getContactListByBirthdate (ContactBook [] g) birthdate = []
getContactListByBirthdate book@(ContactBook (first:contactList) g) birthdate 
						| Contact.birthdate first == birthdate 		= first:getContactListByBirthdate (ContactBook contactList g) birthdate
						| otherwise 					= getContactListByBirthdate (ContactBook contactList g) birthdate

getContactListByGroupName :: ContactBook -> String -> [Contact]
getContactListByGroupName  book@(ContactBook contactList g) groupName =getContactListByIdList (ContactBook contactList g) (Group.members group)
																																						where group = getGroupByName  (ContactBook contactList g) groupName																																
getContactListByIdList :: ContactBook -> [String] -> [Contact]
getContactListByIdList (ContactBook [] g) ids = []
getContactListByIdList (ContactBook c g) [] = []
getContactListByIdList book@(ContactBook contactList g) (first:ids) = (getContactById book first):getContactListByIdList book ids

--getTodayBirthdays :: ContactBook -> [Contact]

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						GROUP LIST
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addEmptyGroup :: ContactBook -> String -> ContactBook
addEmptyGroup (ContactBook c groups) groupName = ContactBook c (new:groups)
	where new = Group.Group groupName []

addGroup :: ContactBook -> Group -> ContactBook
addGroup (ContactBook c groups) group = ContactBook c (group:groups)

removeGroup :: ContactBook ->  String -> ContactBook
removeGroup (ContactBook c groups) groupName =ContactBook c (filter p groups)
	where p x = Group.name x /= groupName

renameGroup::ContactBook->String ->String ->ContactBook
renameGroup book@(ContactBook contacts groups) name newName = 
	let 
		oldGroup = getGroupByName  book name
		newGroup = Group.rename oldGroup newName
	in removeGroup (addGroup book newGroup) name

joinGroups::ContactBook->String->String->ContactBook
joinGroups book@(ContactBook contacts groups) groupName1 groupName2 =
	let 
		group1 = getGroupByName book groupName1
		group2 = getGroupByName book groupName2
		newGroup = Group.join group1 group2 (groupName1++groupName2)
	in addGroup(removeGroup (removeGroup book groupName1) groupName2) newGroup

getGroupByName :: ContactBook -> String -> Group
getGroupByName (ContactBook c [] ) name  = error "No grup with the given name"
getGroupByName (ContactBook c (first:groups)) name 
						| Group.name first == name 	= first
						| otherwise			= getGroupByName (ContactBook c groups) name

