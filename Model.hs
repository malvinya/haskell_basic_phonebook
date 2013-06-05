module ContactBook where
import Person as Person
import Group as Group

data ContactBook = ContactBook { contacts :: [Person]
																		,groups :: [Group]
																		} deriving (Show)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						CONTACT LIST
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addContact :: ContactBook -> Person -> ContactBook
addContact (ContactBook contactList g) new = ContactBook (new:contactList) g

removeContact :: ContactBook ->  Person -> ContactBook
removeContact (ContactBook contactList g) contact =ContactBook (filter p contactList) g
	where p x = x /= contact

editContact::ContactBook->String->String->String->ContactBook
editContact book@(ContactBook contacts groups) ident attr value = 
	let 
		person = getContactById (ContactBook contacts groups) ident
		newContacts = removeContact (ContactBook contacts groups) person
	in case attr of
		"0"	-> addContact (newContacts) (Person.changeFirstname person value)
		"1"	-> addContact (newContacts) (Person.changeSurname person value)
		"2"	-> addContact (newContacts) (Person.changeCompany person value )
		"3"	-> addContact (newContacts) (Person.changePhoneNumber person value)
		"4"	-> addContact (newContacts) (Person.changeEmail person value)
		"5" -> addContact (newContacts) (Person.changeBirthday person value)
		"6" -> let 
						oldGroup = getGroupByName  (ContactBook contacts groups) value
						newGroup = Group.addMember oldGroup person
					in addGroup(removeGroup (ContactBook contacts groups) (Group.name oldGroup)) newGroup
		"7" -> let 
						oldGroup =  getGroupByName  (ContactBook contacts groups) value
						newGroup = Group.removeMember oldGroup person
					in addGroup(removeGroup (ContactBook contacts groups) (Group.name oldGroup)) newGroup


getContactById :: ContactBook -> String -> Person
getContactById (ContactBook [] g) ident = error "No person with id"
getContactById (ContactBook (first:contactList) g) ident = if Person.ident first == ident then first
																													else getContactById (ContactBook contactList g) ident
-- SURNAME --
getContactListBySurname :: ContactBook -> String -> [Person]
getContactListBySurname (ContactBook [singleContact] g) surname = if Person.surname singleContact== surname then [singleContact] else []
getContactListBySurname (ContactBook (first:contactList) g) surname = if Person.surname first == surname then first:getContactListBySurname(ContactBook contactList g) surname
																																					else getContactListBySurname(ContactBook contactList g) surname
-- EMAIL --
getContactListByEmail :: ContactBook -> String -> [Person]
getContactListByEmail (ContactBook [singleContact] g) email 		= if Person.email singleContact== email then [singleContact] else []
getContactListByEmail (ContactBook (first:contactList) g) email 	= if Person.email first == email then first:getContactListByEmail(ContactBook contactList g) email
																																	else getContactListByEmail(ContactBook contactList g) email
-- PHONE NUMBER --
getContactListByPhoneNumber :: ContactBook -> String -> [Person]
getContactListByPhoneNumber (ContactBook [singleContact] g) phoneNumber = if Person.phoneNumber singleContact== phoneNumber then [singleContact] else []
getContactListByPhoneNumber (ContactBook (first:contactList) g) phoneNumber 	= if Person.phoneNumber first == phoneNumber then first:getContactListByPhoneNumber(ContactBook contactList g) phoneNumber
																																											else getContactListByPhoneNumber(ContactBook contactList g) phoneNumber																																					
-- COMPANY --
getContactListByCompany :: ContactBook -> String -> [Person]
getContactListByCompany (ContactBook [singleContact] g) company = if Person.company singleContact== company then [singleContact] else []
getContactListByCompany (ContactBook (first:contactList) g) company = if Person.company first == company then first:getContactListByCompany(ContactBook contactList g) company
																																					else getContactListByCompany(ContactBook contactList g) company
-- BIRTHDAY --
getContactListByBirthdate :: ContactBook -> String -> [Person]
getContactListByBirthdate (ContactBook [singleContact] g) birthdate = if Person.birthdate singleContact== birthdate then [singleContact] else []
getContactListByBirthdate (ContactBook (first:contactList) g) birthdate = if Person.birthdate first == birthdate then first:getContactListByBirthdate(ContactBook contactList g) birthdate
																																					else getContactListByBirthdate(ContactBook contactList g) birthdate

getContactListByGroupName :: ContactBook -> String -> [Person]
getContactListByGroupName  (ContactBook contactList g) groupName =getContactListByIdList (ContactBook contactList g) (Group.members group)
																																						where group = getGroupByName  (ContactBook contactList g) groupName																																
-- no grup , no user, users list, one user only
-- ID LIST --
getContactListByIdList :: ContactBook -> [String] -> [Person]
getContactListByIdList (ContactBook [] g) ids = []
getContactListByIdList (ContactBook c g) [] = []
getContactListByIdList (ContactBook contactList g) (first:ids) = (getContactById (ContactBook contactList g) first):getContactListByIdList(ContactBook contactList g) ids
-- ADD GROUP --
addEmptyGroup :: ContactBook -> String -> ContactBook
addEmptyGroup (ContactBook c groups) groupName = 
	let new = Group.Group groupName []
	in ContactBook c (new:groups)

addGroup :: ContactBook -> Group -> ContactBook
addGroup (ContactBook c groups) group = 
	ContactBook c (group:groups)

-- REMOVE GROUP --
removeGroup :: ContactBook ->  String -> ContactBook
removeGroup (ContactBook c groups) groupName =ContactBook c (filter p groups)
	where p x = Group.name x /= groupName

renameGroup::ContactBook->String ->String ->ContactBook
renameGroup (ContactBook contacts groups) name newName = 
	let 
		oldGroup = getGroupByName  (ContactBook contacts groups) name
		newGroup = Group.rename oldGroup newName
	in removeGroup (addGroup (ContactBook contacts groups) newGroup) name

joinGroups::ContactBook->String->String->ContactBook
joinGroups (ContactBook contacts groups) groupName1 groupName2 =
	let 
		group1 = getGroupByName (ContactBook contacts groups) groupName1
		group2 = getGroupByName (ContactBook contacts groups) groupName2
		newGroup = Group.join group1 group2 (groupName1++groupName2)
	in addGroup(removeGroup (removeGroup (ContactBook contacts groups) groupName1) groupName2) newGroup

-- FILTERS --
-- ID --

-- GROUP --
getGroupByName :: ContactBook -> String -> Group
getGroupByName (ContactBook c [] ) name  = error "No grup with the given name"
getGroupByName (ContactBook c (first:groups)) name = if Group.name first == name then first
																													else getGroupByName (ContactBook c groups) name

-- CONTTACT BY GROUP --


--getTodayBirthdays :: ContactBook -> [Person]

