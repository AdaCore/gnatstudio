--  SDC comprises several sub-components:

--    o An INPUT-OUTPUT sub-system (packages Input & Output). These 2 packages
--      group the low-level routines used to read characters typed by the user
--      (package Input) as well as messages emitted by SDC (package Output).

--    o A LEXICAL ANALYZER (package Tokens). Its goal is to read the characters
--      typed by the user and convert them into a series of tokens.
--      There are 3 types of tokens:
--
--        . Values
--        . Operations that operate on these values
--        . Instructions (eg print, clear, quit)
--
--      The routines to scan each token (ie recognize a sequence of characters
--      as being a specific token) as well as process each token are stored
--      in the packages for Values, Operations and Instructions.

--    o A STACK (package Stack) used to store the values input to SDC as well
--      as all temporary computations.

--    o The main SDC routine (this procedure). This is the main loop of SDC.

procedure Sdc;

