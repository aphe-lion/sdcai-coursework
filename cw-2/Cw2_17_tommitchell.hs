module Cw2_17 where
import Char
import Hugs.Prelude

{-
Symbolic and Declarative Computing & AI
CW2 - Reasoning about Programs in Haskell
 
NAME:   Tom Mitchell
UB NUM: 16010488
-}

{-========= Function definitions ========-}
toLast :: [a] -> [a]
toLast []       = []                -- tL.1
toLast [x]      = [x]               -- tL.2
toLast (x:y:xs) = y:(toLast (x:xs)) -- tL.3

(|++|) :: [a] -> [a] -> [a]
[]     |++| ys = ys             -- c.1
(x:xs) |++| ys = x:(xs |++| ys) -- c.2
{-=======================================-}

{-
TASK 1

============================================================
GOAL: Prove that the statement (1) is correct. 
(1) "For all lists xs of type [a] and all x of type a, it is 
     toLast (x:xs) = xs |++| [x]"
============================================================

To prove (1), we will apply structual induction.

============================================================
SUB-GOAL: Prove that the statement (2) is correct. This will be considered the BASE CASE.
(2) "For the particular list elements x of type a, and the empty list ([]), it is 
     toLast [x] = [] |++| [x]"
============================================================

The proof of the statement (2) is as follows:
     
L.H.S.:
toLast [x]
= [x]           (by tL.2)

R.H.S.:
[] |++| [x]
= [x]           (by c.1)

Because the L.H.S and R.H.S are equivalent, (2) is proven.

============================================================
We now assume the statement (3) is correct. This will be considered the INDUCTION HYPOTHESIS.
(3) "For particular list elements x and y of type a, it is 
     toLast [x, y] = [y] |++| [x]"
============================================================
     
We now perform the INDUCTIVE STEP - proving (1) via the use of (3).

L.H.S.:
toLast (x:xs)
= xs |++| [x]   (by ind. hyp.) 

R.H.S.:
xs |++| [x]

Because the L.H.S and R.H.S are equivalent, (1) is proven.
-}

{-
TASK 2

toLast [7,3,9,1,8]               (by definition)
= toLast (7:3:[9,1,8])           (by tL.3 L.H.S.)
= 3:(toLast (7:[9, 1, 8]))       (by tL.3 R.H.S.)
= 3:(toLast [7, 9, 1, 8])        (by cons)
= 3:(toLast (7:9:[1, 8]))        (by tL.3 L.H.S.)
= 3:(9:(toLast 7:[1, 8]))        (by tL.3 R.H.S.)
= 3:(9:(toLast [7, 1, 8]))       (by cons)
= 3:(9:(toLast (7:1:[8])))       (by tL.3 L.H.S.)
= 3:(9:(1:(toLast (7:[8]))))     (by tL.3 R.H.S.)
= 3:(9:(1:(toLast [7, 8])))      (by cons)
= 3:(9:(1:(toLast (7:8:[]))))    (by tL.3 L.H.S.)
= 3:(9:(1:(8:(toLast (7:[])))))  (by tL.3 R.H.S.)
= 3:(9:(1:(8:(toLast [7]))))     (by cons)
= 3:(9:(1:(8:[7])))              (by tL.2 R.H.S.)
= 3:(9:(1:([8, 7])))             (by cons)
= 3:(9:([1, 8, 7]))              (by cons)
= 3:([9, 1, 8, 7])               (by cons)
= [3, 9, 1, 8, 7]                (by cons)

As shown above:
The statement "toLast [7,3,9,1,8]" is equivalent to "[3, 9, 1, 8, 7]"
-}