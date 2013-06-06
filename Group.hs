module Group where
import Contact as Contact
  
data Group = Group { name :: String
	                      , members :: [String]
	                      } deriving (Show)

addMember:: Group -> Contact -> Group
addMember (Group name members) person= Group name ((Contact.ident person):members)

removeMember:: Group -> Contact -> Group
removeMember (Group name []) person = Group name []
removeMember group@(Group name members) person
					| isAMember  group person = Group name (filter p members)
					| otherwise = error "Not a member"
					where p id = id /= (Contact.ident person)

isAMember::Group ->Contact ->Bool
isAMember (Group name members) person = any (==Contact.ident person) members

join::Group ->Group -> String -> Group
join (Group name1 members1) (Group name2 members2) newName =  Group newName  (members2++(filter p members1))
	where p id = not (id `elem` members2)

rename::Group->String->Group
rename (Group name members) newName = Group newName members