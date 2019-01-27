module Bags18 where
  import Data.List

{- Bags for assignment 1, 2018
   at least 2 versions for each fn:
    * direct recursion 
    * using mapping fns  etc (commented out)
  either version is OK. Only direct recursion has been covered in class but students may read on
  
  NAMING CONVENTIONS
  
  bg,bg1,bg2.. Bags
  x,y.. Items in a Bag
  n,m.. number of occurences of an item
-}
  ------------------------------------------------------------------------------

  -- type definition
  -- pairs could be the other way round
  -- polymorphic, in most fns type must support ==
  
  type Bag a = [(Int,a)]
  
  emptyBag = []

  ------------------------------------------------------------------------------

  -- 1. listToBag - use bagInsert
  -- given a List of a's, return the equivalent Bag 

  listToBag :: Ord a =>[a]->Bag a

 
  listToBag [] = []
  listToBag (h:t) = bagInsert h (listToBag t) --insert all in tail then insert h

  
{- as a fold
   listToBag lis = foldr bagInsert emptyBag lis
 
   using group .. students may have read this far ..

   listToBag lis = map ( \ p@(x:_)->(length p,x)) (group (sort lis))
-}
  

  ------------------------------------------------------------------------------

  -- 2. bagEqual
  -- Do 2 Bags have the same items and the same number of each item
  -- Can't assume Bags are in same order
  -- Do the Bags have the same number of items and is one a sub-Bag of the other?

  bagEqual :: Eq a => Bag a->Bag a->Bool

  -- trivial cases

  bagEqual [] [] = True
  bagEqual [] _ = False
  bagEqual _ [] = False
 
  bagEqual bg1 bg2
   |(length bg1) /= (length bg2) = False -- length constraints
   | otherwise = subBagP bg1 bg2 -- otherwise, is bg1 a subBag of bg2?
   
  -- subBagP
  -- is bg1 a subBag of bg2
  -- i.e. do all items in bg1 occur in bg2 with same numbers?
  
  subBagP :: Eq a=> Bag a -> Bag a -> Bool
  
  subBagP [] _ = True -- trivially True
  
  subBagP ((n,x):tbg1) bg2
   |(bagOccur x bg2 /= n) = False
   |otherwise = subBagP tbg1 bg2
  
 
{- subBagP as a filter
  subBagP bg1 bg2 = null (filter (\ (n,x) -> n /= bagOccur x bg2) bg1)
-}
  
   
     
    
  ------------------------------------------------------------------------------
  -- 3. bagInsert
  -- Insert an a into a Bag of a & return resulting Bag
  -- uses bagOccur .. how many times does an item appear in a Bag?

  bagInsert :: Eq a => a->Bag a->Bag a

  bagInsert x [] = [(1,x)] --insert into empty bag

  bagInsert x ((n,y):t)
   |x==y = ((n+1,y):t) -- insert into head item
   |otherwise = (n,y):(bagInsert x t) -- insert into tail & cons back head
 
 {- as a map
  bagInsert x bg
   |(bagOccur x bg)==0 = (1,x):bg -- new item
   |otherwise = map (\ (n,y) -> if (x==y) then (n+1,y) else (n,y)) bg
 -}
 
  ------------------------------------------------------------------------------

  -- 4. bagSum
  -- add up contens of 2 Bags
  -- divide into items in b1 or both and items in b2 only

  bagSum :: Eq a => Bag a -> Bag a -> Bag a

  bagSum bg1 bg2 = (b1OrBoth bg1 bg2)++(b2Only bg1 bg2)-- items in bg1 or both bg1&bg2 ++ items in bg2 only

  b2Only _ [] = []

  b2Only bg1 ((n,y):tbg2)
    |(bagOccur y bg1)== 0 = (n,y):(b2Only bg1 tbg2)
    |otherwise = (b2Only bg1 tbg2)
    
{- as a filter
 -- items in bg2 only
  b2Only :: Eq a => Bag a -> Bag a -> Bag a
  b2Only bg1 bg2 = (filter (\ (n,x)->(bagOccur x bg1)==0) bg2)
-}  

{-
 --  with a comprehension & @ notation
  b2Only bg1 bg2 = [d|d@(n,x)->bg2, (bagOccur x bg1)==0)]
-}
 
 -- items in bg1 or both
  b1OrBoth :: Eq a => Bag a -> Bag a -> Bag a
 
  b1OrBoth [] _ = []

  b1OrBoth ((n,y):tbg1) bg2 = ((n+(bagOccur y bg2)),y):b1OrBoth tbg1 bg2

 {- as a map
 -- b1OrBoth bg1 bg2 = map (\ (n,x) -> (n+ (bagOccur x bg2),x)) bg1
 -}
 
 {- 
 -- using partial fn for bagOccur
 
  b1OrBoth bg1 bg2 = [(n+m,x)|((n,x),m)<-zip bg1 (map (bagOccurR bg2) (map snd bg1))]
 -}                                                
  ------------------------------------------------------------------------------


  -- 5. bagIntersection
  -- For each item appearing in 2 Bags, what's min number?

  bagIntersection :: Eq a => Bag a->Bag a ->Bag a

  bagIntersection [] _ = [] --trivial failure

  -- Version using where
  
  bagIntersection ((n,y):t) bg2
   |res==0 = bagIntersection t bg2
   |res<n  = (res,y): bagIntersection t bg2
   |otherwise = (n,y): bagIntersection t bg2
   where
     res=bagOccur y bg2 --occurences of first bg1 item in bg2
  
  {-- version without let - wasteful repeated calls of bagOccur 
              
  bagIntersection ((n,y):t) bg2
   |(bagOccur y bg2)==0 = bagIntersection t bg2
   |otherwise = ((min n (bagOccur y bg2)),y): (bagIntersection t bg2)
  -} 

 {- as a filter of a map                                                                   
  --bagIntersection bg1 bg2 = filter (\ (n,_) -> n>0) (map (\ (n,x) -> ((minimum [n,bagOccur x bg2]),x)) bg1)                                                                                 
  ------------------------------------------------------------------------------
  -- with a comprehension
  bagIntersection bg1 bg2 = 
    [ (minimum [n, m],x)| ((n,x),m)<-(zip bg1 [bagOccur x bg2 | (_,x)<-bg1]), m>0]

  -- other fns
  -- bagOccur - how many times does an item appear in a bag?
 -}
  bagOccur :: Eq a => a->Bag a-> Int


  bagOccur _ [] = 0 --empty bag - 0 occurences

  bagOccur x ((n,y):t)
   |x==y = n -- first item gives answer
   |otherwise = bagOccur x t -- else look in rest

 {-  
  bagOccur x bg  
   |null r = 0
   |otherwise = m
   where
    r = filter (\ (n,y) -> x==y) bg
    [(m,_)] = r -- lazy eval - won't be evaluated if null r triggers
 -}  
    
  ------------------------------------------------------------------------------
  -- with args the other way round for partial fns
 {- 
  bagOccurR :: Eq a => (Bag a) -> a-> Int
   
  bagOccurR bg x
   |null r = 0
   |otherwise = m
   where
    r = filter (\ (n,y) -> x==y) bg
    [(m,_)] = r -- lazy eval - won't be evaluated if null r triggers
 -}