with Ada.Characters.Handling;
with Except;
with Ada.Text_IO; use Ada.Text_IO;

package body Input is

   ----------------
   -- Local Data --
   ----------------

   subtype Printable_Character is Character range '!' .. '~';
   --  In ASCII the first printable non blank character is '!', the last one
   --  is '~'.

   Line_Size  : constant := 1024;
   Line       : String (1 .. Line_Size);
   --  Used to save the current input line.

   First_Char : Positive := 1;
   --  Indicates the position of the first character in Line that has not yet
   --  been read by the routine Get_Char below.

   Last_Char  : Natural := 0;
   --  Gives the position of the last valid character in Line;

   Line_Num   : Natural := 0;
   --  Keeps track of the number of lines read for the input.

   --------------------
   -- Local Routines --
   --------------------

   function Get_Char return Character;
   --  Reads and returns the next character from the input.

   function End_Line (N : Positive := 1) return Boolean;
   --  Returns True if at least N characters can be read from Line, before
   --  reaching its end. If End_Line (1) returns True then all the available
   --  characters in Line have been read by the Get_Char.

   procedure Read_New_Line;
   --  Reads in Line a new input line.

   procedure Skip_Spaces;
   --  Skip all spaces, tabs and carriage returns from the input and advance
   --  the current character on the first non blank character.

   procedure Unread_Char (N : Positive := 1);
   --  If the last N characters read from the input did not contain a
   --  carriage return, unreads these N characters, ie it puts them back in
   --  the stream of input characters to read. Otherwise unread the last k <
   --  N characters that followed the last carriage return.

   -------------------
   -- Column_Number --
   -------------------

   function Column_Number return Natural is
      Col : Natural := 0;

   begin
      --  The column computation is complicated by the presence of TAB (HT)
      --  characters. In fact when you hit a TAB your next column is the
      --  closest multiple of 8 (so if you are in column 14 and you hit a
      --  TAB the column number becomes 16).

      for I in 1 .. First_Char - 1 loop
         if Line (I) /= ASCII.HT then
            Col := Col + 1;
         else
            Col := Col + 8 - (Col mod 8);
         end if;
      end loop;

      return Col;
   end Column_Number;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line return String is
   begin
      return Line (Line'First .. Last_Char);
   end Current_Line;

   --------------
   -- End_Line --
   --------------

   function End_Line (N : Positive := 1) return Boolean is
   begin
      return Last_Char < First_Char + (N - 1);
   end End_Line;

   --------------
   -- Get_Char --
   --------------

   function Get_Char return Character is
      C : Character;

   begin
      --  First check if the line is empty or has been all read.

      if End_Line then
         Read_New_Line;
      end if;

      C := Line (First_Char);
      First_Char := First_Char + 1;

      return C;
   end Get_Char;

   -----------------
   -- Line_Number --
   -----------------

   function Line_Number return Natural is
   begin
      return Line_Num;
   end Line_Number;

   -----------------
   -- Next_Number --
   -----------------

   procedure Read_Number
     (S : in  String;
      I : out Integer;
      R : out Float;
      K : out Number_Kind)
   is
      --  GNAT may complain that I and R are not always set by this
      --  procedure, so disable corresponding warnings.
      pragma Warnings (Off, I);
      pragma Warnings (Off, R);

      package Int_Io  is new Ada.Text_IO.Integer_IO (Integer);
      package Real_Io is new Ada.Text_IO.Float_IO   (Float);

      Last : Positive;

   begin
      K := No_Number;

      Try_Integer : declare
      begin
         Int_Io.Get (From => S, Item => I, Last => Last);

         if Last = S'Last then
            K := Int_Number;
            return;
         end if;

      exception
         when Ada.Text_IO.Data_Error =>
            null;
      end Try_Integer;

      Try_Float : declare
      begin
         Real_Io.Get (From => S, Item => R, Last => Last);

         if Last = S'Last then
            K := Real_Number;
            return;
         end if;

      exception
         when Ada.Text_IO.Data_Error =>
            null;
      end Try_Float;

   end Read_Number;

   ---------------
   -- Next_Word --
   ---------------

   function Next_Word return String is
      Start : Natural;

   begin
      Input.Skip_Spaces;

      Start := First_Char;
      while Line (First_Char) in Printable_Character loop
         First_Char := First_Char + 1;
      end loop;

      --  Now convert the string to an upper case string of characters

      declare
         S : String := Line (Start .. First_Char - 1);

      begin
         for I in S'Range loop
            S (I) := Ada.Characters.Handling.To_Upper (S (I));
         end loop;

         return S;
      end;
   end Next_Word;

   -------------------
   -- Read_New_Line --
   -------------------

   procedure Read_New_Line is
      use type Ada.Text_IO.File_Access;
   begin
      First_Char := Line'First;

      if Ada.Text_IO.End_Of_File then
         raise Except.Exit_SDC;
      end if;

      --  Read a line from the standard input. Routine Text_Io.Get_Line
      --  reads all the input character up to (but not including) the next
      --  carriage return into Line. After this call Last_Char contains the
      --  number of characters read into Line.

      Ada.Text_IO.Get_Line (Line, Last_Char);

      if Ada.Text_IO.Current_Input /= Ada.Text_IO.Standard_Input then
         Ada.Text_IO.Put_Line (Line (Line'First .. Last_Char));
      end if;

      --  Save a carriage return at the end of the Line and update the line
      --  count.

      Last_Char := Last_Char + 1;
      Line (Last_Char) := ASCII.CR;

      Line_Num := Line_Num + 1;
   end Read_New_Line;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line is
   begin
      First_Char := Last_Char + 1;
   end Skip_Line;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces is
      Current_Char : Character;

   begin
      loop
         Current_Char := Input.Get_Char;
         exit when Current_Char in Printable_Character;
      end loop;

      --  We must unread the non blank character just read.

      Input.Unread_Char;
   end Skip_Spaces;

   -----------------
   -- Unread_Char --
   -----------------

   procedure Unread_Char (N : Positive := 1) is
   begin
      if First_Char - N >= Line'First then
         First_Char := First_Char - N;
      else
         First_Char := Line'First;
      end if;
   end Unread_Char;

end Input;
