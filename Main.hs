module Main where
import ContactBook as ContactBook
import System.IO
import Data.Char
import Contact
import Group
import Utils


main = do
	putStrLn "-----------------------------------------------------------CONTACT BOOK-----------------------------------------------------------"  
	putStrLn " ";  
	putStrLn "Projekt SPOP 2013 : Książka adresowa"  
	putStrLn "Malwina Kowalczyk"  
	putStrLn " ";  
            mainMenu (ContactBook [] [])


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						MAIN MENU
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

mainMenu book@(ContactBook contacts groups) = do
	putStrLn "-----------------------------------------------------------CONTACT BOOK-----------------------------------------------------------"  
	putStrLn "1. Contacts"  
	putStrLn "2. Groups" 
	putStrLn "3. Search contacts"
	putStrLn "4. Today's birthday"
	putStrLn "5. Exit"
	putStrLn "-----------------------------------------------------------CONTACT BOOK-----------------------------------------------------------"
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
	putStrLn "-------------------------------------------------------------CONTACTS-------------------------------------------------------------"  
	putStrLn "1. List All Contacts"  
	putStrLn "2. Add Contact"  
	putStrLn "3. Delete Contact" 
	putStrLn "4. Edit Contact"
	putStrLn "-------------------------------------------------------------CONTACTS-------------------------------------------------------------"  
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
			putStr "Wrong input. Press '1'...'4' to choose option:"
			contactsMenu(ContactBook contacts groups)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						GROUPS MENU
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

groupsMenu book@(ContactBook contacts groups) = do
	putStrLn ""
	putStrLn "--------------------------------------------------------------GROUPS--------------------------------------------------------------"  
	putStrLn "1. List all groups"  
	putStrLn "2. Show group members"  
	putStrLn "3. Add Group"  
	putStrLn "4. Delete Group" 
	putStrLn "5. Rename Group"
	putStrLn "6. Join Groups"
	putStrLn "--------------------------------------------------------------GROUPS--------------------------------------------------------------"  
	putStrLn "Press 'B' to quit and return to the previous menu."  
	putStr "Press '1'...'6' to choose option:"
	
	input <- getLine

	case input of
		"1"	-> showAllGroups book  
		"2"	-> showGroupForm book  
		"3"	-> addGroupForm book  
		"4"	-> deleteGroupForm book  
		"5"	-> editGroupForm book
		"6"	-> joinGroupsForm book
		"B" -> mainMenu book
		"b"	-> mainMenu book
		otherwise -> do
			putStr "Wrong input. Press '1'...'6' to choose option:"
			groupsMenu(ContactBook contacts groups)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						SEARCH MENU
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

searchMenu book@(ContactBook contacts groups) = do
	putStrLn ""
	putStrLn "---------------------------------------------------------SEARCH CONTACTS----------------------------------------------------------"  
	putStrLn "1. By Id"  
	putStrLn "2. By Surname" 
	putStrLn "3. By Company" 
	putStrLn "4. By Email"
	putStrLn "5. By Phone Number"
	putStrLn "6. By Group"
	putStrLn "---------------------------------------------------------SEARCH CONTACTS----------------------------------------------------------"  
	putStrLn "Press 'B' to quit and return to the previous menu."  
	putStr "Press '1'...'6' to choose option:"

	input <- getLine
	if input `elem` ["b", "B"] then do
		mainMenu book
	else do
		putStr "Value: "
		value <-getLine
		if value `elem` ["b", "B"] then do
			mainMenu book	
		else do
			putStrLn "----------------------------------------------------------SEARCH RESULTS----------------------------------------------------------"
			case input of
				"1"	-> print (getContactById book  value)
				"2"	-> mapM_ print (getContactListBySurname book  value)
				"3"	-> mapM_ print (getContactListByCompany book  value)
				"4"	-> mapM_ print (getContactListByEmail  book  value)
				"5"	-> mapM_ print (getContactListByPhoneNumber  book  value)
				"6"	-> mapM_  print (getContactListByGroupName  book  value)
				otherwise -> do
					putStr "Wrong input. Press '1'...'6' to choose option:"
			searchMenu(ContactBook contacts groups)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						BIRTHDAY MENU
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
birthdayMenu book@(ContactBook contacts groups)  = do
	putStrLn ""
	putStrLn "---------------------------------------------------------TODAY'S BIRTHDAY---------------------------------------------------------" 
	dateString <-Utils.date
	let birthdayList = getContactListByBirthdate book dateString
	if null (birthdayList) then do 
		putStrLn "Such a sad day. Nobody's celebrating!"
	else do
		mapM_ print birthdayList 
		putStrLn "Remember to send them best regards from me!"
	mainMenu book

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						CONTACT ACTIONS
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
showAllContacts(ContactBook contacts groups) = do
	putStrLn "-----------------------------------------------------------ALL CONTACTS-----------------------------------------------------------"
	mapM_ print contacts
	putStrLn "-----------------------------------------------------------ALL CONTACTS-----------------------------------------------------------"
	contactsMenu(ContactBook contacts groups)

addContactForm book@(ContactBook contacts groups) = do
	putStrLn "ADD CONTACT:"
	putStrLn "ID:"
	ident <- getLine
	if ident `elem` ["b", "B"] then do
		contactsMenu book	
	else do
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
							putStrLn "Phone Number:"
							phoneNumber <- getLine
							if not (Utils.validNumber phoneNumber) then do addContactInputError book "Phone number should contain digits only."
							else do
								putStrLn "Email:"
								email <- getLine
								if not (Utils.validString email) then do addContactInputError book "Incorrect email address."
								else do
									putStrLn "Birthdate (yyyy-mm-dd):"
									birthday <- getLine
									if not (Utils.validDate birthday) then do addContactInputError book "Date format incorrect (yyyy-mm-dd)."
									else do
										let newContact = Contact.Contact ident name surname company phoneNumber email birthday
										putStrLn "Success."
										putStrLn "New contact added:"
										print newContact
										contactsMenu(addContact book newContact)

addContactInputError book@(ContactBook contacts groups) message = do
	putStr "Couldn't add contact: "
	putStrLn message
	addContactForm book

deleteContactForm book@(ContactBook contacts groups)  = do
	putStr "DELETE CONTACT with ID: "
	ident <- getLine
	if ident `elem` ["b", "B"] then do
		contactsMenu book	
	else do
		if isAMemberById book ident then do
			let contactToDelete = getContactById book ident
			putStrLn "Success."
			putStrLn "Contact deleted:"
			print contactToDelete
			contactsMenu(removeContact book contactToDelete)
		else do
			putStrLn "Contact with the given ID doesn't exist. Try again."
			deleteContactForm book

editContactForm book@(ContactBook contacts groups) = do

	putStr "EDIT CONTACT with ID: "
	ident <- getLine
	if ident `elem` ["b", "B"] then do
			contactsMenu book	
	else do
		if isAMemberById book ident then do
			putStrLn "Choose Attribute:"
			putStrLn "1. Name"  
			putStrLn "2. Surname"  
			putStrLn "3. Company"  
			putStrLn "4. Phone Number" 
			putStrLn "5. Email" 
			putStrLn "6. Birthdate" 
			putStrLn "7. Add group membership"
			putStrLn "8. Remove group membership"

			putStrLn "Press 'B' to quit and return to the previous menu."  
			putStr "Press '1'...'8' to choose option:"

			attr <-getLine
			if attr `elem` ["b", "B"] then do
				contactsMenu book	
			else do
				putStrLn "New value: "
				value <- getLine
				if (attr `elem` ["1","2","3","5"] &&  not (Utils.validString value)) then do
					putStrLn "Attribute should contain letters only. Try again."
					editContactForm book
				else do
					if (attr `elem` ["4"]  && not (Utils.validNumber value)) then do
						putStrLn "Phone number should contain digits only. Try Again."
						editContactForm book
					else do
						if (attr `elem` ["6"]  && not (Utils.validDate value)) then do
							putStrLn "Wrong date format. It should be: yyyy-mm-dd Try again."
							editContactForm book
						else do
							if attr `elem` ["7", "8"]  && not(groupExistsByName book value) then do
								putStrLn "Group with the given name doesn't exist. Try again."
								editContactForm book
							else do
								putStrLn "Success."
								putStrLn "Contact modified:"
								let result = editContact book ident attr value
								print (getContactById result ident)
								contactsMenu(result)
		else do
			putStrLn "Contact with the given ID doesn't exist. Try again."
			editContactForm book

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--						GROUP ACTIONS
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
showAllGroups book@(ContactBook contacts groups)  = do
	printGroups book
	groupsMenu book

printGroups(ContactBook contacts groups) = do
	putStrLn "------------------------------------------------------------ALL GROUPS------------------------------------------------------------"
	mapM_ (print.Group.name) groups
	putStrLn "------------------------------------------------------------ALL GROUPS------------------------------------------------------------"


showGroupForm book@(ContactBook contacts groups) = do
	printGroups book
	putStr "SHOW GROUP with NAME:"
	name <- getLine
	if groupExistsByName book name then do
		putStrLn name
		putStrLn "Members:"
		print (showGroupMembers book name)
		--print (getGroupByName book name)
		groupsMenu book
	else do
		putStrLn "The group with the given name doesn't exist. Try again."
		showGroupForm book

addGroupForm book@(ContactBook contacts groups) = do
	printGroups book
	putStr "ADD GROUP with NAME:"
	name <- getLine
	if not (groupExistsByName book name) then do
		groupsMenu(addEmptyGroup book name)
	else do
		putStrLn "The group with the given name already exists. Try again with different name."
		addGroupForm book

deleteGroupForm book@(ContactBook contacts groups)  = do
	printGroups book
	putStr "DELETE GROUP with NAME:"
	name <- getLine
	if groupExistsByName book name then do
		groupsMenu(removeGroup book name)
	else do
		putStrLn "The group with the given name doesn't exist. Try again."
		deleteGroupForm book


editGroupForm book@(ContactBook contacts groups) = do
	printGroups book
	putStr "RENAME GROUP with NAME:"
	name <- getLine
	if groupExistsByName book name then do
		putStr "New name: "
		newName<- getLine
		groupsMenu(renameGroup book name newName)
	else do
		putStrLn "The group with the given name doesn't exist. Try again."
		editGroupForm book

joinGroupsForm book@(ContactBook contacts groups) = do
	printGroups book
	putStrLn "JOIN GROUPS"
	putStr "First group: "
	group1 <- getLine
	if groupExistsByName book group1 then do
		putStr "Second group: "
		group2<- getLine
		groupsMenu(joinGroups book group1 group2)
	else do
		putStrLn "The group with the given name doesn't exist. Try again."
		joinGroupsForm book		







