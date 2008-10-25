--  Low level routines to Read character from the input

package Input is

   function Next_Word return String;
   --  Reads and returns the next word from the input. Alphabetical
   --  characters are converted to upper case.  A word is a contiguous
   --  sequence of printable symbols (letters, digits, +, *, etc.) not
   --  interrupted by a space or TAB.

   type Number_Kind is (No_Number, Int_Number, Real_Number);
   procedure Read_Number
     (S : in  String;
      I : out Integer;
      R : out Float;
      K : out Number_Kind);
   --  If string S contains an integer or real number this routine reads the
   --  number into either I or R and sets K to either Int_Number or Real_Number
   --  If no number is found or the number is ill-formed K is set to No_Number.
   --  The number must have the same format as Ada integers or floats.

   procedure Skip_Line;
   --  Skips to the next input line.

   function Current_Line return String;
   --  Returns the current input Line;

   function Column_Number return Natural;
   --  Returns the column number of the last character read.

   function Line_Number return Natural;
   --  Returns the current line number.

end Input;
