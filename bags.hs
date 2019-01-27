{- 
COM2108 Assignment 1: Bags
Done by: Samuel Fung
Student ID: 170162982
-}

module Bags where
    -- datatype for Item: Use tuple of polymorphic element to represent the item and Int to represent the quantity of the item
    type Item a = (a, Int)
    -- datatype for Bag: Stores the items into a list
    type Bag a = [Item a]

    -- listToBag    : Returns a bag of items (with number of items)
    -- Trivial Case : The bag is empty
    listToBag :: Eq a => [a] -> Bag a
    listToBag lis = listToBagA lis []
    -- listToBagA   : Auxiliary function to help listToBag
    -- Trivial Case : The list is empty
    -- Otherwise    : Calls itself recursively until the list is empty 
    listToBagA :: Eq a => [a] -> Bag a -> Bag a
    listToBagA [] _ = []
    listToBagA lis bag
        | null t    = bagInsert h bag
        | otherwise = listToBagA t (bagInsert h bag)
      where
        h:t = lis
    
    -- bagEqual     : Returns true if two bags have the exact same items and same number of items
    -- Trivial Case : Both bags are empty
    -- Returns      : True if both bags contain the exact same number of items
    bagEqual :: Eq a => Bag a -> Bag a -> Bool
    bagEqual [] []     = True                       -- Base Case: True if the both bags are emptyu
    bagEqual bagX bagY = totalX == totalXY          -- True if the sum of 2 BagX's is the same as the same of bagX and bagY's 
      where
        totalX  = bagSum bagX bagX                  -- Sum of two BagX
        totalXY = bagSum bagX bagY                  -- Sum of two BagY
            
    -- bagSum        : Returns the sum of the two bags
    -- Trivial Case  : Both bags are empty
    -- Otherwise     : Calls itself recursively until one bag 
    bagSum :: Eq a => Bag a -> Bag a -> Bag a
    bagSum [] [] = []
    bagSum ((itemX, quanX):tailX) bagY
        | null quanYLis  = (itemX, quanX):(bagSum tailX bagY)
        | otherwise = (itemX, quanX + quanY):(bagSum tailX nBagY)
      where
        quanYLis = [b | (a, b) <- bagY, a == itemX]      -- get the quantity of the type same as itemX, the result is returned as list  
        quanY:_  = quanYLis                              -- if lis is not an empty list, then get the amount of Y that is same as itemX
        nBagY    = [(a, b) | (a, b) <- bagY, a /= itemX] -- remove the old itemY from bagY
    
    -- bagInsert      : Inserts an item into a Bag type
    -- Trivial Case   : Adding an item to an empty bag
    -- Otherwise      : Calls itself recurisvely until the second bag becomes empty
    bagInsert :: Eq a => a -> Bag a -> Bag a
    bagInsert item [] = [(item, 1)]
    bagInsert item ((currItem, currItemQuantity):remBag)
        | item == currItem  = (currItem, currItemQuantity + 1):remBag              -- adding an item to a bag which already has the same item
        | otherwise         = (currItem, currItemQuantity):(bagInsert item remBag) -- adding an item to a bag that is empty/has other items

    -- bagIntersection: Returns a bag of items that are present in two bags
    -- Trivial Case   : One of the bags are empty
    -- Otherwise      : Calls itself recursively until one of the bags are empty
    bagIntersection :: Eq a => Bag a -> Bag a -> Bag a
    bagIntersection [] _ = []
    bagIntersection _ [] = []
    bagIntersection ((firstElementX, firstQuanX):tailX) bagY
        | firstElementX `elem` bagYList = (firstElementX, lowQuanXY):bagIntersection tailX bagY               -- Checks if the head of bagX is in bagY
        | otherwise                     = bagIntersection tailX bagY
      where
        bagYList                                  = [a | (a, _) <- bagY]                                      -- Converts elements in bagY into a list of elements
        lowQuanXY:_                               = [b | (a, b) <- bagY, a == firstElementX, b <= firstQuanX] -- Gets the lowest number of occurrences of an item between the two bags