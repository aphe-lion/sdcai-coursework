module GAT where
import Data.List

data Person = A | B | C
    deriving (Show, Eq, Enum)

data Colour = Green | Yellow | Red
    deriving (Show, Eq, Enum)

data House = House {owner :: Person, colour :: Colour}
    deriving (Show, Eq)

data Triplet = Triplet {one :: House, two :: House, three :: House}
    deriving (Show, Eq)

-- All colours in a list
allColours :: [Colour]
allColours = [Green ..]

-- All people in a list
allPeople :: [Person]
allPeople = [A ..]

-- All combinations of people and houses
-- Task 1
allHouses :: [House]
allHouses = [House o c | o <- allPeople, c <- allColours]

-- All "fake" house triplets
--allFakeTriplets :: [[(Person, Colour)]]
--allFakeTriplets = [zip p c | p <- permutations allPeople, c <- permutations allColours]

-- Convert a "fake house" into a house
--fakeHouseToHouse :: (Person, Colour) -> House
--fakeHouseToHouse (p, c) = House p c

-- Convert a fake triplet into a list of houses
--fakeTripletToHouseList :: [(Person, Colour)] -> [House]
--fakeTripletToHouseList xs = map fakeHouseToHouse xs

-- All triplets in list form
--allHouseLists :: [[House]]
--allHouseLists = map fakeTripletToHouseList allFakeTriplets

-- Convert a fake triplet to a triplet
--fakeTripletToTriplet:: [House] -> Triplet
--fakeTriplettoTriplet xs = Triplet (xs !! 0) (xs !! 1) (xs !! 2)

-- All triplets (janky)
-- Task 2
allTriplets :: [Triplet]
allTriplets = map fakeTripletToTriplet allHouseLists
    where
    fakeTripletToTriplet xs = Triplet (xs !! 0) (xs !! 1) (xs !! 2)
    allHouseLists = map fakeTripletToHouseList allFakeTriplets
    fakeTripletToHouseList xs = map fakeHouseToHouse xs
    fakeHouseToHouse (p, c) = House p c
    allFakeTriplets = [zip p c | p <- permutations allPeople, c <- permutations allColours]

{-
*GAT> permutations [1, 2, 3]
[[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
-}

trueTestTrip = Triplet {one = House {owner = C, colour = Yellow}, 
                          two = House {owner = A, colour = Green}, 
                          three = House {owner = B, colour = Red}}

falseTestTrip = Triplet {one = House {owner = A, colour = Red}, 
                        two = House {owner = C, colour = Yellow}, 
                        three = House {owner = B, colour = Green}}

-- Task 3
position :: Eq a => (House -> a) -> Triplet -> a -> (Triplet -> House)
position attrFunc trip match
    | (attrFunc $ one trip) == match   = one
    | (attrFunc $ two trip) == match   = two
    | (attrFunc $ three trip) == match = three

colourToPerson :: Triplet -> Colour -> Person
colourToPerson trip col = owner $ position colour trip col $ trip

personToColour :: Triplet -> Person -> Colour
personToColour trip pers = colour $ position owner trip pers $ trip

-- Task 4
houseInfoOne :: Triplet -> Bool
houseInfoOne trip
    | (firstHouseYellow && secondHouseA) = True
    | (firstHouseYellow && thirdHouseA)  = True
    | (secondHouseYellow && thirdHouseA) = True
    | otherwise                          = False
    where
    firstHouseYellow = (colour $ one $ trip) == Yellow
    secondHouseYellow = (colour $ two $ trip) == Yellow
    secondHouseA = (owner $ two $ trip) == A
    thirdHouseA = (owner $ three $ trip) == A

-- Task 5
houseInfoTwo :: Triplet -> Bool
houseInfoTwo trip = personToColour trip B == Red

-- Task 6
houseInfoThree :: Triplet -> Bool
houseInfoThree trip = (colour . three) trip /= Green

-- Task 7
houseInfo :: Triplet -> Bool
houseInfo trip = all ($ trip) [houseInfoOne, houseInfoTwo, houseInfoThree]

-- Task 8
answer :: [Triplet]
answer = filter houseInfo allTriplets