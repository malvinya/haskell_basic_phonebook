import ContactBook
import System.Environment
import System.Directory
import MainMenu

main :: IO ()
main = do
	fileAlreadyCreated <- doesFileExist "contactbook.txt"
	if fileAlreadyCreated then do
		fileContent <- readFile "contactbook.txt"
		mainMenu(read fileContent::ContactBook)
	else do
		writeFile "contactbook.txt" (show (ContactBook [] []))
		mainMenu (ContactBook [] [])
