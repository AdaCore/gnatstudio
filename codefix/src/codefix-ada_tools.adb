-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with String_Utils; use String_Utils;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;

with Language;     use Language;

package body Codefix.Ada_Tools is

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Use) is
      procedure Free_Pool is new
        Ada.Unchecked_Deallocation (Use_Type, Ptr_Use);
   begin
      Free (This.Position);
      Free (This.Name);
      Free_Pool (This);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_With) is
      procedure Free_Pool is new
        Ada.Unchecked_Deallocation (With_Type, Ptr_With);
   begin
      Free (This.Name_Str);

      for J in This.Name'Range loop
         Free (This.Name (J));
      end loop;

      Free_Pool (This);
   end Free;

   ---------------------
   -- Get_Use_Clauses --
   ---------------------

   --  ??? WARNING ! This function is not yet terminated, it doesn't work on
   --  instantiated packages, but only if the with appears !

   function Get_Use_Clauses
     (Clause_Name  : String;
      File_Name    : String;
      Current_Text : Text_Navigator_Abstr'Class;
      Exclusive    : Boolean := False) return Words_Lists.List
   is
      List_Of_With : With_Lists.List := List_All_With
        (Current_Text, File_Name);
      List_Of_Use  : Use_Lists.List := List_All_Use
        (Current_Text, File_Name);
      Seek_Node    : With_Lists.List_Node;
      Result       : Words_Lists.List;

   begin
      Link_All_Clauses (List_Of_With, List_Of_Use);
      Seek_Node := First (List_Of_With);

      while Seek_Node /= With_Lists.Null_Node loop
         if Data (Seek_Node).Name_Str.all = Clause_Name then
            for J in Data (Seek_Node).Clauses'Range loop
               if Data (Seek_Node).Clauses (J) /= null
                 and then (not Exclusive
                           or else Data (Seek_Node).Clauses (J).Nb_Ref = 1)
               then
                  declare
                     Word_Used : Word_Cursor;
                  begin
                     Assign (Word_Used.File_Name, File_Name);
                     Word_Used.Col := Data
                       (Seek_Node).Clauses (J).Position.Col;
                     Word_Used.Line := Data
                       (Seek_Node).Clauses (J).Position.Line;
                     Assign
                       (Word_Used.String_Match,
                        Data (Seek_Node).Clauses (J).Name.all);
                     Append (Result, Word_Used);
                  end;
               end if;
            end loop;

            Free (List_Of_With);
            Free (List_Of_Use);

            return Result;
         end if;

         Seek_Node := Next (Seek_Node);
      end loop;

      raise Codefix_Panic;
   end Get_Use_Clauses;


   ----------------------
   -- Get_Parts_Number --
   ----------------------

   function Get_Parts_Number (Str : String) return Positive is
      Total : Positive := 1;
   begin
      for J in Str'Range loop
         if Str (J) = '.' then
            Total := Total + 1;
         end if;
      end loop;

      return Total;
   end Get_Parts_Number;

   -----------------
   -- Get_Arr_Str --
   -----------------

   function Get_Arr_Str (Str : String) return Arr_Str is
      Result             : Arr_Str (1 .. Get_Parts_Number (Str));
      Start_Ind, End_Ind : Positive := Str'First;
   begin
      for J in Result'Range loop
         Skip_To_Char (Str, End_Ind, '.');

         Result (J) := new String'(Str (Start_Ind .. End_Ind - 1));
         End_Ind := End_Ind + 1;
         Start_Ind := End_Ind;
      end loop;

      return Result;
   end Get_Arr_Str;

   ----------------------
   -- Try_Link_Clauses --
   ----------------------

   procedure Try_Link_Clauses
     (With_Clause : Ptr_With; Use_Clause : Ptr_Use)
   is
      Use_Parsed : Arr_Str := Get_Arr_Str (Use_Clause.Name.all);
      With_Index : Positive := 1;
      Success    : Boolean;
   begin
      while With_Index <= With_Clause.Nb_Elems loop
         Success := True;

         for Use_Index in 1 .. Use_Parsed'Last loop

            if With_Index + Use_Index - 1 > With_Clause.Name'Last
              or else Use_Parsed (Use_Index).all /=
                With_Clause.Name (With_Index + Use_Index - 1).all
            then
               Success := False;
            end if;

         end loop;

         if Success then
            Use_Clause.Nb_Ref := Use_Clause.Nb_Ref + 1;
            With_Clause.Clauses (With_Index + Use_Parsed'Length - 1) :=
              Use_Clause;

            for J in Use_Parsed'Range loop
               Free (Use_Parsed (J));
            end loop;

            return;
         end if;

         With_Index := With_Index + 1;

         exit when With_Index > With_Clause.Nb_Elems
           or else With_Clause.Clauses (With_Index) = null;
      end loop;

      for J in Use_Parsed'Range loop
         Free (Use_Parsed (J));
      end loop;
   end Try_Link_Clauses;

   -------------------
   -- List_All_With --
   -------------------

   function List_All_With
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : String) return With_Lists.List
   is
      Structure  : constant Construct_List_Access :=
        Get_Structure (Current_Text, File_Name);
      Iterator   : Construct_Access := Structure.First;
      New_Clause : Ptr_With;
      Result     : With_Lists.List;

   begin
      while Iterator /= null loop
         if Iterator.Category = Cat_With then
            New_Clause := new With_Type
              (Get_Parts_Number (Iterator.Name.all));
            New_Clause.Name := Get_Arr_Str
              (Iterator.Name.all);
            Assign (New_Clause.Name_Str, Iterator.Name.all);
            Append (Result, New_Clause);
         end if;

         Iterator := Iterator.Next;
      end loop;

      return Result;
   end List_All_With;

   ------------------
   -- List_All_Use --
   ------------------

   function List_All_Use
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : String) return Use_Lists.List
   is
      Structure  : constant Construct_List_Access :=
        Get_Structure (Current_Text, File_Name);
      Iterator   : Construct_Access := Structure.First;
      New_Clause : Ptr_Use;
      Result     : Use_Lists.List;

   begin
      while Iterator /= null loop
         if Iterator.Category = Cat_Use then
            New_Clause := new Use_Type;
            New_Clause.Name := new String'(Iterator.Name.all);
            New_Clause.Position :=
              (Col       => Iterator.Sloc_Start.Column,
               Line      => Iterator.Sloc_Start.Line,
               File_Name => new String'(File_Name));
            Append (Result, New_Clause);
         end if;

         Iterator := Iterator.Next;
      end loop;

      return Result;
   end List_All_Use;

   ----------------------
   -- Link_All_Clauses --
   ----------------------

   procedure Link_All_Clauses
     (List_Of_With : in out With_Lists.List;
      List_Of_Use  : in out Use_Lists.List)
   is
      With_Node : With_Lists.List_Node;
      Use_Node  : Use_Lists.List_Node;
   begin
      Use_Node := First (List_Of_Use);

      while Use_Node /= Use_Lists.Null_Node loop
         With_Node := First (List_Of_With);

         while With_Node /= With_Lists.Null_Node loop
            Try_Link_Clauses (Data (With_Node), Data (Use_Node));
            With_Node := Next (With_Node);
         end loop;

         Use_Node := Next (Use_Node);
      end loop;
   end Link_All_Clauses;

   -----------------------
   -- Is_In_Escape_Part --
   -----------------------

   --  ??? Add checks for simple quotes: '
   function Is_In_Escape_Part
     (This     : Ada_Escape_Str;
      Text     : String;
      Position : Natural) return Boolean
   is
      pragma Unreferenced (This);

      Is_In_String : Boolean := False;
      J            : Natural;
   begin
      J := Text'First;

      while J <= Position loop
         case Text (J) is
            when '"' =>
               if Is_In_String
                 and then J < Position
                 and then Text (J + 1) /= '"'
               then
                  Is_In_String := False;
               else
                  Is_In_String := True;
               end if;
            when '-' =>
               if not Is_In_String
                 and then J < Position
                 and then Text (J + 1) = '-'
               then
                  return True;
               end if;
            when others =>
               null;
         end case;

         J := J + 1;
      end loop;

      if Is_In_String then
         return True;
      else
         return False;
      end if;

   end Is_In_Escape_Part;

   ----------------------------
   -- Get_Next_With_Position --
   ----------------------------

   function Get_Next_With_Position
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : String) return File_Cursor'Class
   is
      Current_Cursor : File_Cursor;
      Current_Info   : Construct_Information;
      Found_With     : Boolean := False;
   begin
      Current_Cursor.File_Name := new String'(File_Name);
      Current_Cursor.Line := 1;
      Current_Cursor.Col := 1;

      loop
         Current_Info := Get_Unit
           (Current_Text, Current_Cursor, After);

         exit when Current_Info.Category /= Cat_With
           and then Current_Info.Category /= Cat_Use;

         Found_With := True;
         Current_Cursor.Col := Current_Info.Sloc_End.Column + 1;
         Current_Cursor.Line := Current_Info.Sloc_End.Line;
      end loop;

      if not Found_With then
         Current_Cursor.Col := 1;
         Current_Cursor.Line := Current_Info.Sloc_Start.Line - 1;
      end if;

      return Current_Cursor;
   end Get_Next_With_Position;

   -----------------
   -- Search_With --
   -----------------

   function Search_With
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : String;
      Pkg_Name     : String) return File_Cursor'Class
   is
      Iterator   : Construct_Access :=
        Get_Structure (Current_Text, File_Name).First;
      Result     : File_Cursor;
   begin
      while Iterator /= null loop
         if Iterator.Category = Cat_With
           and then Iterator.Name.all = Pkg_Name then
            Assign (Result.File_Name, File_Name);
            Result.Col := Iterator.Sloc_Start.Column;
            Result.Line := Iterator.Sloc_Start.Line;

            return Result;
         end if;

         Iterator := Iterator.Next;
      end loop;

      return Null_File_Cursor;
   end Search_With;

end Codefix.Ada_Tools;
