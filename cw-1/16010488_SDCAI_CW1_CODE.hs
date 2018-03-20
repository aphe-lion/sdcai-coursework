module Bigtext where
import Char
import Hugs.Prelude

----------------------------------------------------------
-- Haskell Coursework 1 (2017-2018): text magnification --
----------------------------------------------------------

-- Author:            Tom Mitchell
-- UB Number:         16010488

----------------------------------------------------------

----------------------------------------------------------
{- Task 1 - capital -}

{-
a. Take an input Char, c.
b. If isLower evaluates to True, return capitalise c.
c. In any other case, return c.
d. Returns True if c is in the range of 'a' and 'z', inclusive.
e. Returns the Unicode character 32 positions before c.
-}
----------------------------------------------------------

capital :: Char -> Char                       
capital c                               -- a     
  | isLower   = capitalise c            -- b       
  | otherwise = c                       -- c      
  where 
  isLower    = ('a' <= c) && (c <= 'z') -- d
  capitalise = (\c -> chr $ ord c - 32) -- e

----------------------------------------------------------
{- Task 2 - letter, appendNewlineChar, letter_as_text -}

{-
a. Capital letters.
b. Numbers.
c. Punctuation and other characters.
d. Accounts for lowercase letters via a recursive call.
-}

----------------------------------------------------------

letter :: Char -> [String]
letter 'A' = [" AAA ",     -- a
              "A   A",
              "A   A",
              "A   A",
              "AAAAA",
              "A   A",
              "A   A"]
letter 'B' = ["BBBB ",
              "B   B",
              "B   B",
              "BBBB ",
              "B   B",
              "B   B",
              "BBBB "]
letter 'C' = [" CCCC",
              "C    ",
              "C    ",
              "C    ",
              "C    ",
              "C    ",
              " CCCC"]
letter 'D' = ["DDDD ",
              "D   D",
              "D   D",
              "D   D",
              "D   D",
              "D   D",
              "DDDD "]
letter 'E' = ["EEEEE",
              "E    ",
              "E    ",
              "EEEEE",
              "E    ",
              "E    ",
              "EEEEE"]              
letter 'F' = ["FFFFF",
              "F    ",
              "F    ",
              "FFFFF",
              "F    ",
              "F    ",
              "F    "]
letter 'G' = [" GGGG",
              "G    ",
              "G    ",
              "G  GG",
              "G   G",
              "G   G",
              " GGGG"]
letter 'H' = ["H   H",
              "H   H",
              "H   H",
              "HHHHH",
              "H   H",
              "H   H",
              "H   H"]
letter 'I' = ["IIIII",
              "  I  ",
              "  I  ",
              "  I  ",
              "  I  ",
              "  I  ",
              "IIIII"]               
letter 'J' = [" JJJJ",
              "    J",
              "    J",
              "    J",
              "    J",
              "J   J",
              " JJJ "]               
letter 'K' = ["K   K",
              "K  K ",
              "K K  ",
              "KK   ",
              "K K  ",
              "K  K ",
              "K   K"]               
letter 'L' = ["L    ",
              "L    ",
              "L    ",
              "L    ",
              "L    ",
              "L    ",
              "LLLLL"]               
letter 'M' = ["M   M",
              "MM MM",
              "M M M",
              "M M M",
              "M   M",
              "M   M",
              "M   M"]               
letter 'N' = ["N   N",
              "NN  N",
              "NN  N",
              "N N N",
              "N  NN",
              "N  NN",
              "N   N"]               
letter 'O' = [" OOO ",
              "O   O",
              "O   O",
              "O   O",
              "O   O",
              "O   O",
              " OOO "]               
letter 'P' = ["PPPP ",
              "P   P",
              "P   P",
              "PPPP ",
              "P    ",
              "P    ",
              "P    "]               
letter 'Q' = [" QQQ ",
              "Q   Q",
              "Q   Q",
              "Q   Q",
              "Q   Q",
              " QQQ ",
              "    Q"]               
letter 'R' = ["RRRR ",
              "R   R",
              "R   R",
              "RRRR ",
              "R   R",
              "R   R",
              "R   R"]
letter 'S' = [" SSS ",
              "S   S",
              "S    ",
              " SSS ",
              "    S",
              "S   S",
              " SSS "]                            
letter 'T' = ["TTTTT",
              "  T  ",
              "  T  ",
              "  T  ",
              "  T  ",
              "  T  ",
              "  T  "]               
letter 'U' = ["U   U",
              "U   U",
              "U   U",
              "U   U",
              "U   U",
              "U   U",
              " UUU "]               
letter 'V' = ["V   V",
              "V   V",
              "V   V",
              "V   V",
              " V V ",
              " V V ",
              "  V  "]               
letter 'W' = ["W   W",
              "W   W",
              "W   W",
              "W   W",
              "W W W",
              "W W W",
              " W W "]
letter 'X' = ["X   X",
              "X   X",
              " X X ",
              "  X  ",
              " X X ",
              "X   X",
              "X   X"]               
letter 'Y' = ["Y   Y",
              "Y   Y",
              " Y Y ",
              "  Y  ",
              "  Y  ",
              "  Y  ",
              "  Y  "]               
letter 'Z' = ["ZZZZZ",
              "    Z",
              "   Z ",
              "  Z  ",
              " Z   ",
              "Z    ",
              "ZZZZZ"]
letter '0' = [" 000 ", -- b
              "0   0",
              "0   0",
              "0 0 0",
              "0   0",
              "0   0",
              " 000 "]
letter '1' = ["111  ",
              "  1  ",
              "  1  ",
              "  1  ",
              "  1  ",
              "  1  ",
              "11111"]
letter '2' = [" 222 ",
              "2   2",
              "    2",
              "   2 ",
              "  2  ",
              " 2  ",
              "22222"]
letter '3' = ["3333 ",
              "    3",
              "    3",
              "3333 ",
              "    3",
              "    3",
              "3333 "]
letter '4' = ["4  4 ",
              "4  4 ",
              "4  4 ",
              "44444",
              "   4 ",
              "   4 ",
              "   4 "]
letter '5' = ["55555",
              "5    ",
              "5    ",
              "5555 ",
              "    5",
              "    5",
              "5555 "]
letter '6' = [" 666 ",
              "6    ",
              "6    ",
              "6666 ",
              "6   6",
              "6   6",
              " 666 "]
letter '7' = ["77777",
              "    7",
              "   7 ",
              "   7 ",
              "  7  ",
              " 7   ",
              " 7   "]
letter '8' = [" 888 ",
              "8   8",
              "8   8",
              " 888 ",
              "8   8",
              "8   8",
              " 888 "]
letter '9' = [" 999 ",
              "9   9",
              "9   9",
              " 9999",
              "    9",
              "    9",
              " 999 "]                                               
letter '!' = [" !!  ", -- c
              " !!  ",
              " !!  ",
              " !!  ",
              " !!  ",
              "     ",
              " !!  "]
letter '"' = ["\"\" \"\"",
              "\"\" \"\"",
              "     ",
              "     ",
              "     ",
              "     ",
              "     "]
letter '#' = ["  # #",
              " # # ",
              "#####",
              " # # ",
              "#####",
              " # # ",
              "# #  "]              
letter '$' = ["  $  ",
              " $$$$",
              "$ $  ",
              " $$$ ",
              "  $ $",
              "$$$$ ",
              "  $  "]              
letter '%' = ["%%   ",
              "%%  %",
              "   % ",
              "  %  ",
              " %   ",
              "%  %%",
              "   %%"]              
letter '&' = ["   &&",
              " &&  ",
              " &   ",
              "& & &",
              "&  & ",
              "&  & ",
              " && &"]              
letter '\'' = ["  '' ",
              "  '' ",
              "     ",
              "     ",
              "     ",
              "     ",
              "     "]
letter '(' = ["  (( ",
              " (   ",
              "(    ",
              "(    ",
              "(    ",
              " (   ",
              "  (( "]
letter ')' = [" ))  ",
              "   ) ",
              "    )",
              "    )",
              "    )",
              "   ) ",
              " ))  "]
letter '*' = ["  *  ",
              "* * *",
              " *** ",
              "* * *",
              "  *  ",
              "     ",
              "     "]
letter '+' = ["     ",
              "  +  ",
              "  +  ",
              "+++++",
              "  +  ",
              "  +  ",
              "     "]
letter ',' = ["     ",
              "     ",
              "     ",
              "     ",
              "     ",
              "  ,, ",
              " ,,  "]
letter '-' = ["     ",
              "     ",
              "     ",
              " --- ",
              "     ",
              "     ",
              "     "]
letter '.' = ["     ",
              "     ",
              "     ",
              "     ",
              "     ",
              " ..  ",
              " ..  "]
letter '/' = ["    /",
              "   / ",
              "   / ",
              "  /  ",
              " /   ",
              " /   ",
              "/    "]
letter ':' = ["     ",
              " ::  ",
              " ::  ",
              "     ",
              " ::  ",
              " ::  ",
              "     "]
letter ';' = [" ;;  ",
              " ;;  ",
              "     ",
              " ;;  ",
              " ;;  ",
              "  ;  ",
              " ;   "]              
letter '<' = ["     ",
              "   <<",
              " <<  ",
              "<    ",
              " <<  ",
              "   <<",
              "     "]
letter '=' = ["     ",
              "     ",
              "=====",
              "     ",
              "=====",
              "     ",
              "     "]
letter '>' = ["     ",
              ">>   ",
              "  >> ",
              "    >",
              "  >> ",
              ">>   ",
              "     "]
letter '?' = [" ??? ",
              "?   ?",
              "    ?",
              "   ? ",
              "  ?  ",
              "     ",
              "  ?  "]
letter '@' = [" @@@ ",
              "@   @",
              "@  @@",
              "@ @ @",
              "@  @ ",
              "@    ",
              " @@@@"]
letter '[' = ["[[[  ",
              "[    ",
              "[    ",
              "[    ",
              "[    ",
              "[    ",
              "[[[  "]
letter '\\' = ["\\    ",
              "\\    ",
              " \\   ",
              "  \\  ",
              "   \\ ",
              "    \\",
              "    \\"]              
letter ']' = ["  ]]]",
              "    ]",
              "    ]",
              "    ]",
              "    ]",
              "    ]",
              "  ]]]"]              
letter '^' = ["  ^  ",
              " ^ ^ ",
              "^   ^",
              "     ",
              "     ",
              "     ",
              "     "]
letter ' ' = ["     ",
              "     ",
              "     ",
              "     ",
              "     ",
              "     ",
              "     "]
letter '{' = ["   {{",
              "  {  ",
              "  {  ",
              "{{   ",
              "  {  ",
              "  {  ",
              "   {{"]
letter '|' = [" ||  ",
              " ||  ",
              " ||  ",
              " ||  ",
              " ||  ",
              " ||  ",
              " ||  "]
letter '}' = ["}}   ",
              "  }  ",
              "  }  ",
              "   }}",
              "  }  ",
              "  }  ",
              "}}   "]
letter '~' = ["     ",
              "     ",
              " ~   ",
              "~ ~ ~",
              "   ~ ",
              "     ",
              "     "]
letter '¬' = ["     ",
              "     ",
              "     ",
              "¬¬¬¬¬",
              "    ¬",
              "     ",
              "     "]
letter '`' = [" ``  ",
              "  `` ",
              "     ",
              "     ",
              "     ",
              "     ",
              "     "]
letter '_' = ["     ",
              "     ",
              "     ",
              "     ",
              "     ",
              "     ",
              "_____"]              
letter c = letter $ capital c -- d

----------------------------------------------------------

{-
a. Use list ([Char]) concatenation to append a string containing a newline character ("\n")
-}

----------------------------------------------------------

appendNewlineChar :: String -> String
appendNewlineChar s = s ++ "\n"       -- a

----------------------------------------------------------

{-
a. Magnify character c, append newline chars to all strings in the resultant list
   via mapping and concatenate the resultant list into a single string.
-}

----------------------------------------------------------

letter_as_text :: Char -> String
letter_as_text c = concat $ map appendNewlineChar $ letter c -- a

----------------------------------------------------------
{- Task 3 - (|++|) -}

{-
a. Remove and concatenate the heads (x and y) of two lists xs and ys, 
   append them to the list generated by the recursive call of (|++|) xs ys
b. Return an empty list in the case that either list does not match the 
   x:xs pattern
-}

(|++|) :: [String] -> [String] -> [String]
(|++|) (x:xs) (y:ys) = (x ++ y) : ((|++|) xs ys) -- a
(|++|) _ _           = []                        -- b

----------------------------------------------------------
{- Task 4 - insertSpace, big -}

{- Type aliases -}
type MyWord = String
type Text = String

----------------------------------------------------------

{- 
a. It makes no sense to "space out" an empty string, return the same empty list.
b. Prepend each character in xs to a space, concatenate the result list of strings,
   and drop the last character (a space)
-}

insertSpace :: MyWord -> MyWord
insertSpace [] = []                                    -- a
insertSpace xs = init $ concat $ map (\c -> c:" ") xs  -- b

----------------------------------------------------------

{-
a. Empty strings cannot be magnified
b. Generate all the "rows of pixels" required to represent the magnified string,
   fold (|++|) into that list, append newlines to all the results,
   and concatenate the list into a single string.
-}

big :: MyWord -> String
big [] = []                                                                           -- a
big xs = concat $ map appendNewlineChar $ foldr1 (|++|) $ map letter $ insertSpace xs -- b

----------------------------------------------------------
{- Task 5 - magnify, getNextWord, dropFirstWord -}

{-
a. Take letters from xs one by one until a space is taken,
   then concatenate all letters prior to the space and return them.
   More abstractly, this returns the first word of a sentence
   contained in xs.
-}

getNextWord = takeWhile (/=' ')

----------------------------------------------------------

{-
a. If dropFirstWord xs is an empty list, eg, we dropped the
   last word in the string, return an empty list.
b. If we drop any word that is not the last, 
   return the remainder of xs after having dropped the first word, 
   and also removed the leading space.
c. Evaluate elements from a list one by one - 
   if the predicate "element is a not space" is not met, 
   return the space and the rest of the string. 
   If the character is not a space, disregard it.
d. Return True if we drop the last word in the sentence - 
   (the result of dropping the first word is an empty list).

-}

dropFirstWord xs
  | noTail    = []                         -- a
  | otherwise = tail $ dropFirstWord' xs   -- b
  where 
  dropFirstWord' = dropWhile (/= ' ')   -- c
  noTail         = dropFirstWord' xs == [] -- d

----------------------------------------------------------

{-
a. An empty string cannot be magnified
b. Generate a string representing a magnified version of the first
   word, append a newline char, and prepend this string to one generated
   by a recursive call of magnify with xs without its first word as an argument
-}

magnify :: Text -> String
magnify [] = []                                                         -- a
magnify xs = big (getNextWord xs) ++ "\n" ++ magnify (dropFirstWord xs) -- b

----------------------------------------------------------
{- Task 6a, 6b, 6c - hugeLetters, bigger, hugeMagnify -}

{- 
  Note that comments for bigger and hugemagnify are the same as 
  big and magnify because the functions have almost identical definitions.
-}

{-
a. Map replicateN onto every character in every string in the result of letter c, 
   to "stretch" the characters width. Map replicateN onto the result of this to
   "stretch" the characters height. Concatenate the result into a single string.
b. Define replicateN by partial application.
-}

hugeLetters :: Int -> Char -> [String]
hugeLetters n c = concat $ map replicateN $ map concat $ map (map replicateN) $ letter c -- a
  where                                                                                  
  replicateN = replicate n                                                               -- b

----------------------------------------------------------

{-
a. Empty strings cannot be magnified.
b. Generate all lines of "pixels" required to represent the mapnified string,
   fold (|++|) into the list, append newlines to all the results and concatenate
   the list into a single string.
-}

bigger :: Int -> MyWord -> String
bigger _ [] = []                                                                                    -- a
bigger n xs = concat $ map appendNewlineChar $ foldr1 (|++|) $ map (hugeLetters n) $ insertSpace xs -- b

----------------------------------------------------------

{-
a. An empty string cannot be magnified.
b. Generate a string representing a "huge magnified" version of the first word, 
   append a newline char, and prepend this string to one generated by a recursive
   call of hugeMagnify with xs without its first word as an argument.
-}

hugeMagnify :: Int -> Text -> String
hugeMagnify _ [] = []                                                                         -- a
hugeMagnify n xs = (bigger n $ getNextWord xs) ++ "\n" ++ (hugeMagnify n $ dropFirstWord xs)  -- b