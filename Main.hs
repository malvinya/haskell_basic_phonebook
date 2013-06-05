module Main where
import ContactBook as ContactBook
import System.IO
import Data.Char
import Contact
import Utils


main = do
	putStrLn "--------------------------------------------------------CONTACT BOOK--------------------------------------------------------"  
	putStrLn " ";  
	putStrLn "Projekt SPOP 2013 : Książka adresowa"  
	putStrLn "Malwina Kowalczyk"  
	putStrLn " ";  
            mainMenu (ContactBook [] [])


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						MAIN MENU
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

mainMenu book@(ContactBook contacts groups) = do
	putStrLn "--------------------------------------------------------CONTACT BOOK--------------------------------------------------------"  
	putStrLn "1. Contacts"  
	putStrLn "2. Groups" 
	putStrLn "3. Search contacts"
	putStrLn "4. Today's birthday"
	putStrLn "5. Exit"
	putStrLn "--------------------------------------------------------CONTACT BOOK--------------------------------------------------------"
	putStrLn "Press 'B' to return to the previous menu."  
	putStr "Press '1'...'5' to choose option:"

	input <- getLine

	case input of
		"1"	-> contactsMenu book 
		"2"	-> groupsMenu book 
		"3"	-> searchMenu book 
		"4"	-> birthdayMenu book 
		"5"	-> do
				--wirte file
				return()
		otherwise -> do 
				putStr "Wrong input. Press '1'...'5' to choose option:"
				mainMenu(ContactBook contacts groups)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						CONTACTS MENU
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

contactsMenu book@(ContactBook contacts groups) = do
	putStrLn ""
	putStrLn "-----------------------------------------------------------CONTACTS-----------------------------------------------------------"  


	putStrLn "1. Show All Contacts"  
	putStrLn "2. Add Contact"  
	putStrLn "3. Delete Contact" 
	putStrLn "4. Edit Contact"
	putStrLn "-----------------------------------------------------------CONTACTS-----------------------------------------------------------"  
	putStrLn "Press 'B' to return to the previous menu."  
	putStr "Press '1'...'4' to choose option:"

	input <- getLine

	case input of
		"1"	-> showAllContacts book
		"2"	-> addContactForm book  
		"3"	-> deleteContactForm book  
		"4"	-> editContactForm book
		"B" -> mainMenu book
		"b"	-> mainMenu book
		otherwise -> do
			putStr "Wrong input. Press '1'...'3' to choose option:"
			contactsMenu(ContactBook contacts groups)




------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						GROUPS MENU
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

groupsMenu (ContactBook contacts groups) = do
	putStrLn ""
	putStrLn "-----------------------------------------------------------GROUPS-----------------------------------------------------------"  
	putStrLn "1. List all groups"  
	putStrLn "2.  Show group"  
	putStrLn "3. Add Group"  
	putStrLn "4. Delete Group" 
	putStrLn "5. Rename Group"
	putStrLn "6. Join Groups"
	putStrLn "-----------------------------------------------------------GROUPS-----------------------------------------------------------"  
	putStrLn "Press 'Q' to quit and return to the previous menu."  
	putStr "Press '1'...'3' to choose option:"
	
	input <- getLine

	case input of
		"1"	-> showAllGroups(ContactBook contacts groups)  
		"2"	-> showGroupForm(ContactBook contacts groups)  
		"3"	-> addGroupForm(ContactBook contacts groups)  
		"4"	-> deleteGroupForm(ContactBook contacts groups)  
		"5"	-> editGroupForm(ContactBook contacts groups)
		"6"	-> joinGroupsForm(ContactBook contacts groups)
		"B"  	-> mainMenu(ContactBook contacts groups)
		"b"	-> mainMenu(ContactBook contacts groups)
		otherwise -> do
			putStr "Wrong input. Press '1'...'3' to choose option:"
			groupsMenu(ContactBook contacts groups)




------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						SEARCH MENU
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

searchMenu (ContactBook contacts groups) = do
	putStrLn ""
	putStrLn "-----------------------------------------------------------SEARCH CONTACTS-----------------------------------------------------------"  
	putStrLn "1. By Id"  
	putStrLn "2. By Surname" 
	putStrLn "3. By Company" 
	putStrLn "4. By Email"
	putStrLn "5. By Phone Number"
	putStrLn "6. By Group"
	putStrLn "-----------------------------------------------------------SEARCH CONTACTS-----------------------------------------------------------"  
	putStrLn "Press 'Q' to quit and return to the previous menu."  
	putStrLn "Press '1'...'5' to choose option."

	input <- getLine
	putStr "Value: "
	value <-getLine
	putStrLn "-----------------------------------------------------------SEARCH RESULTS-----------------------------------------------------------"
	case input of
		"1"	-> print (getContactById (ContactBook contacts groups)  input)
		"2"	-> mapM_ print (getContactListBySurname (ContactBook contacts groups)  input)
		"3"	-> mapM_ print (getContactListByCompany (ContactBook contacts groups)  input)
		"4"	-> mapM_ print (getContactListByEmail (ContactBook contacts groups)  input)
		"5"	-> mapM_ print (getContactListByPhoneNumber (ContactBook contacts groups)  input)
		"6"	-> mapM_  print (getContactListByGroupName (ContactBook contacts groups)  input)
		"B"  -> mainMenu(ContactBook contacts groups) 
		"b"	-> mainMenu(ContactBook contacts groups)
		otherwise -> do
			putStr "Wrong input. Press '1'...'4' to choose option:"
	searchMenu(ContactBook contacts groups)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						BIRTHDAY MENU
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
birthdayMenu(ContactBook contacts groups)  = do
	putStrLn ""
	putStrLn "-----------------------------------------------------------TODAY'S BIRTHDAY-----------------------------------------------------------"  





------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						CONTACT ACTIONS
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
showAllContacts(ContactBook contacts groups) = do
	putStrLn "------------------------------------------------------------ALL CONTACTS------------------------------------------------------------"
	mapM_ print contacts
	putStrLn "------------------------------------------------------------ALL CONTACTS------------------------------------------------------------"
	contactsMenu(ContactBook contacts groups)

addContactForm book@(ContactBook contacts groups) = do
	putStrLn "New contact:"
	putStrLn "ID:"
	ident <- getLine
	if  not (Utils.validNumber ident) then do
		putStrLn "ID should be a number. Try again"
		addContactForm book
	else do
		if isAMemberById book ident then do
			putStrLn "ID is already used"
			addContactForm book
		else do
			putStrLn "First name:"
			name <- getLine
			if not (Utils.validString name) then do addContactInputError book "First name should contain letters only."
			else do
				putStrLn "Surname:"
				surname <- getLine
				if not (Utils.validString surname) then do addContactInputError book "Surname should contain letters only."
				else do 
					putStrLn "Company:"
					company <- getLine
					if not (Utils.validString company) then do addContactInputError book "Company name should contain letters only."
					else do
						putStrLn "Email:"
						email <- getLine
						if not (Utils.validString email) then do addContactInputError book "Incorrect email address."
						else do
							putStrLn "Phone Number:"
							phoneNumber <- getLine
							if not (Utils.validNumber phoneNumber) then do addContactInputError book "Phone number should contain digits only."
							else do
								putStrLn "Birthdate (dd mm yyyy):"
								birthday <- getLine
								if not (Utils.validString birthday) then do addContactInputError book "Date format incorrect (dd mm yyyy)."
								else do
									let newContact = Contact.Contact ident name surname company email phoneNumber birthday
									putStrLn "New contact added:"
									print newContact
									contactsMenu(addContact book newContact)

addContactInputError book@(ContactBook contacts groups) message = do
	putStr "Couldn't add contact: "
	putStrLn message
	addContactForm book

deleteContactForm book@(ContactBook contacts groups)  = do
	putStr "Delete contact with ID: "
	ident <- getLine
	if isAMemberById book ident then do
		let contactToDelete = getContactById book ident
		print (removeContact book (getContactById book ident))
		contactsMenu(removeContact book contactToDelete)
	else do
		putStrLn "Contact with the given ID doesn't exist. Try again."
		deleteContactForm book

editContactForm book@(ContactBook contacts groups) = do
	putStr "Edit contact with ID: "
	ident <- getLine
	if isAMemberById book ident then do
		putStrLn "Choose Attribute:"
		putStrLn "0. Name"  
		putStrLn "1. Surname"  
		putStrLn "2. Company"  
		putStrLn "3. Phone Number" 
		putStrLn "4. Email" 
		putStrLn "5. Birthdate" 
		putStrLn "6. Add group membership"
		putStrLn "6. Remove group membership"

		attr <-getLine
		putStrLn "New value: "
		value <- getLine
		contactsMenu(editContact book ident attr value)
	else do
		putStrLn "Contact with the given ID doesn't exist. Try again."
		editContactForm book

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						GROUP ACTIONS
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
showAllGroups(ContactBook contacts groups)  = do
	putStrLn "------------------------------------------------------------ALL------------------------------------------------------------"
	mapM_ print groups
	putStrLn "------------------------------------------------------------ALL------------------------------------------------------------"
	groupsMenu(ContactBook contacts groups)

showGroupForm(ContactBook contacts groups) = do
	putStrLn "Show group"
	putStr "Group name:"
	value <- getLine
	print (getGroupByName (ContactBook contacts groups) value)
	groupsMenu(ContactBook contacts groups)


addGroupForm(ContactBook contacts groups) = do
	putStrLn "Create new group"
	putStr "Group name:"
	value <- getLine
	groupsMenu(addEmptyGroup (ContactBook contacts groups) value)

deleteGroupForm(ContactBook contacts groups)  = do
	putStrLn "Delete group"
	putStr "Group name: "
	value <- getLine
	groupsMenu(removeGroup (ContactBook contacts groups) value)

editGroupForm(ContactBook contacts groups) = do
	putStrLn "Rename group"
	putStr "Group name: "
	name <- getLine
	putStr "New name: "
	newName<- getLine
	groupsMenu(renameGroup(ContactBook contacts groups) name newName)

joinGroupsForm(ContactBook contacts groups) = do
	putStrLn "Join groups"
	putStr "First group: "
	group1 <- getLine
	putStr "Second group: "
	group2<- getLine
	groupsMenu(joinGroups(ContactBook contacts groups) group1 group2)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						GROUP ACTIONS
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------








