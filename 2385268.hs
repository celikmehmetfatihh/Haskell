-- Mehmet Fatih Çelik


--I used Maybe Int because the person could be not dead. So while adding in familytree function, I used Just deathYear which can be
--Nothing if the person is still alive or the death year of the person is unknown. [FamilyTree] representing the person's children
--if the person has no child, the list will be just empty.
data FamilyTree = EmptyTree | Node String Int (Maybe Int) [FamilyTree] deriving (Show, Eq, Ord)

--parsing the input we got and calling familytreeList
familytree [] [] [] = EmptyTree
familytree (name:names) (numChildren:children) ((birthYear, deathYear): years) = 
    let childs = familytreeList (take numChildren names) (take numChildren children) (take numChildren years)
    in Node name birthYear (Just deathYear) childs
    

--input: list of names, list of number of children per node, a list of pairs of birth and death year per node
--extracts the elemnts from input list and calling familytree to make the tree for each child. 
familytreeList [] [] [] = []
familytreeList (name:names) (numChildren:children) (year:years) = 
    let birthYear = fst year
        deathYear = snd year
    in familytree name birthYear (Just deathYear) : familytreeList (take numChildren names) (take numChildren children) (take numChildren years)
	
--base case, tree emopty-> no search -> false	
search EmptyTree _ = False
--if current node's name is equal to searchKey -> True if not, we have a helper function called childrenSearch which takes the
--list of the children and the searchKey we are searching for. If this list empty return False, if not continues to search
--recursively in the function.
search (Node name _ _ children) searchKey = if name == searchKey then True
                                            else childrenSearch children searchKey where
        childrenSearch [] _ = False
        childrenSearch (x:xs) searchKey = if search x searchKey then True
                                          else childrenSearch xs searchKey

--base case The person is not in the tree.
parent EmptyTree _ = "Not Found"
--When input is not empty helper function which is findParent is called. 
parent (Node _ _ _ children) searchKey = findParent searchKey children where
	--Base case: List of children is empty(The person is the root) -> No parent will be returned
    findParent _ [] = "No Parent"
	--list is not empty, checks x is equal to the name, if it is equal, returns current child's name which is the parent of the input person.
	--If it is not equal recursively calling itself till the list becomes empty.
    findParent searchValue (x:xs) = if name == searchValue then name
                               else findParent searchValue xs where
                                   Node name _ _ _ = x
		
-- if no sibling then empty tree as a base case	
siblings EmptyTree _ = []
--If the input tree is not empty, the helper function named siblingNames is called. Checks whether siblingList is empty or not from siblingNames function. If it is empty
--returns [] else, returns siblingList
siblings (Node _ _ _ children) searchKey = let siblingsList = siblingNames searchKey children in 
                                           if null siblingsList then []
                                           else siblingsList where
        siblingNames _ [] = []
		--recursively searches node of the list of children whichs name = searchValue. If found, another helper function childNames will be called.
		--If it doesnot find any node, recursively continues.
        siblingNames searchValue (x:xs) = 
            if search x searchValue then childNames xs 
            else siblingNames searchValue xs
        childNames [] = []
		--Childnames gets list of the remaining children and returns names of all children except the input node. 
        childNames (x:xs) = getName x : childNames xs
        search (Node name _ _ _) searchValue = name == searchValue
        getName (Node name _ _ _) = name
		

--avglifetime function returns average lifetime of all people in the tree unless the input is EmptyTree which is base case.
--The total life time is calculated by helper function named lifeTimeTotal
--The total number of people is calculated by helper function named numPeople.
--Used fromIntegral in order to make all the variables as a Num type which is more general to not get any error in division and adition
avglifetime EmptyTree = 0
avglifetime (Node _ _ death children) = fromIntegral (lifeTimeTotal death children) / fromIntegral (numPeople children)

--lifeTimeTotal takes Maybe Int(death year of the parent node) and list of its children nodes. The lifetime
--for each people is calculated in the calculateLifeTime helper function. We use this lifeTimeTotal function as a recurisve function, to add all the people's 
--lifetime in the tree.
lifeTimeTotal _ [] = 0
lifeTimeTotal death (x:xs) = calculateLifeTime x death + lifeTimeTotal death xs
  where 
  --calculateLifeTime takes a node of its parent and death year of its parent. If the node has death year as Nothing since it is defined as Maybe Int,
  --returns 0. If node has a death year, and its parent node has a death year too, owndeathyear - parentDeathYear and adding this result to
  --the lifetime of its parent node recursively. 
  --If node doesnot have any death year, but its parent has a death year, lifetime will be equal to parent's death year.
  --If it has a death year but its parent does not have any death year, the lifetime will be equal to 0 simply.
    calculateLifeTime (Node _ _ Nothing _) Nothing = 0
    calculateLifeTime (Node _ _ (Just deathYear) _) (Just parentDeathYear) = deathYear - parentDeathYear + calculateLifeTime (Node "" "" (Just parentDeathYear) []) (Just parentDeathYear)
    calculateLifeTime (Node _ _ Nothing _) (Just parentDeathYear) = parentDeathYear
    calculateLifeTime (Node _ _ _ _) _ = 0

--Utilizes a helper function called countPeople, which takes a node and returns the total number of people related to that node who are either its children, grandchildren, or great-grandchildren.
--So, it computes the subtree.

numPeople [] = 0
numPeople (x:xs) = countPeople x + numPeople xs
  where 
--Returns how many individuals are present in the subtree at that node. If the node is EmptyTree, which is the base case returns 0 
--If the node has any children, it recursively calculates 1 + the total number of individuals in each child subtree.
    countPeople EmptyTree = 0
    countPeople (Node _ _ _ children) = 1 + sum (map countPeople children)
