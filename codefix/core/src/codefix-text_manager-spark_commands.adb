------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNAT.Regpat;                      use GNAT.Regpat;

package body Codefix.Text_Manager.Spark_Commands is

   --  Move_Tilde_Or_Percent_Cmd

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Move_Tilde_Or_Percent_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) is
   begin
      This.Cursor := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Cursor));
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Move_Tilde_Or_Percent_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : File_Cursor :=
        File_Cursor (Get_Current_Cursor (Current_Text, This.Cursor.all));

      --  The following matcher should work for any number of array and field
      --  accesses and for multi-dimentional arrays. The only possible case
      --  of failing to match is when an index expression contains a closing
      --  parenthesis.
      Matcher : constant Pattern_Matcher :=
        Compile ("(\w+)((\([^)]*\)|\.\w+)+)(~|%)");
      Matches : Match_Array (0 .. 4);

      --  Since we cannot match the regular expression backward from Cursor,
      --  we match instead forward from the beginning of the line.
      --  This should be equivalent in this case: even if we correct this way
      --  a similar error on the same line, the other application of the auto-
      --  fix will correct the current problem.
      Str : constant String := Current_Text.Get_Line (Cursor, 1);
   begin
      Match (Matcher, Str, Matches);

      if Matches (0) /= No_Match then
         declare
            Cur_Word : constant String :=
              Str (Matches (0).First .. Matches (0).Last);
            New_Word : constant String :=
              Str (Matches (1).First .. Matches (1).Last)
              & Str (Matches (4).First .. Matches (4).Last)
              & Str (Matches (2).First .. Matches (2).Last);
         begin
            Cursor.Set_Column (Visible_Column_Type (Matches (0).First));
            Current_Text.Replace (Cursor, Cur_Word'Length, New_Word);
         end;
      end if;

      Free (Cursor);
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Move_Tilde_Or_Percent_Cmd) is
   begin
      Free (This.Cursor);
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Move_Tilde_Or_Percent_Cmd) return Boolean is
   begin
      return This.Cursor.Get_File.Is_Writable;
   end Is_Writable;

end Codefix.Text_Manager.Spark_Commands;
