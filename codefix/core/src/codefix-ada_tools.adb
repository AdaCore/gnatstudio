------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

with GNATCOLL.Symbols;       use GNATCOLL.Symbols;
with Language;               use Language;
with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;
with String_Utils;           use String_Utils;

package body Codefix.Ada_Tools is

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Use) is
      procedure Free_Pool is new
        Ada.Unchecked_Deallocation (Use_Type, Ptr_Use);
   begin
      Free (This.Position);
      Free_Pool (This);
   end Free;

   ---------------------
   -- Get_Use_Clauses --
   ---------------------

   --  ??? WARNING ! This function is not yet terminated, it doesn't work on
   --  instantiated packages, but only if the with appears !

   procedure Get_Use_Clauses
     (Clause_Name  : String;
      File_Name    : GNATCOLL.VFS.Virtual_File;
      Current_Text : Text_Navigator_Abstr'Class;
      Exclusive    : Boolean := False;
      Result       : out Words_Lists.Vector)
   is
      Dummy : constant Update_Lock := Lock_Updates
        (Current_Text.Get_Structured_File (File_Name));

      List_Of_With : With_Lists.Vector;
      List_Of_Use  : Use_Lists.Vector;

   begin
      List_All_With (Current_Text, File_Name, List_Of_With);
      List_All_Use  (Current_Text, File_Name, List_Of_Use);

      Link_All_Clauses (List_Of_With, List_Of_Use);

      for Item of List_Of_With loop
         if Item.Name_Str = Clause_Name then
            for J in Item.Clauses'Range loop
               if Item.Clauses (J) /= null
                 and then (not Exclusive
                           or else Item.Clauses (J).Nb_Ref = 1)
               then
                  declare
                     Word_Used : Word_Cursor;
                  begin
                     Set_File (Word_Used, File_Name);
                     Set_Location
                       (Word_Used,
                        Line   => Get_Line (Item.Clauses (J).Position),
                        Column => Get_Column (Item.Clauses (J).Position));
                     Set_Word (Word_Used, Item.Clauses (J).Name);
                     Append (Result, Word_Used);
                  end;
               end if;
            end loop;

            List_Of_With.Clear;
            List_Of_Use.Clear;

            return;
         end if;
      end loop;
   end Get_Use_Clauses;

   --------------------------------
   -- Get_Use_Duplicates_On_Line --
   --------------------------------

   procedure Get_Use_Duplicates_On_Line
     (Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor;
      Result       : out Words_Lists.Vector)
   is
      use type Basic_Types.Visible_Column_Type;

      List_Of_Use : Use_Lists.Vector;
      First       : Boolean := True;

   begin
      List_All_Use (Current_Text, Get_File (Word), List_Of_Use);

      for Item of List_Of_Use loop
         exit when Word.Get_Line < Get_Line (Item.Position);

         if Item.Name = Get_Word (Word) then
            if Word.Get_Line = Get_Line (Item.Position)
              and then Word.Get_Column < Get_Column (Item.Position)
              and then not First
            then
               declare
                  Word_Used : Word_Cursor;
               begin
                  Set_File (Word_Used, Get_File (Word));
                  Set_Location
                    (Word_Used,
                     Line   => Get_Line (Item.Position),
                     Column => Get_Column (Item.Position));
                  Set_Word (Word_Used, Item.Name);
                  Append (Result, Word_Used);
               end;

            else
               First := False;
            end if;
         end if;
      end loop;
   end Get_Use_Duplicates_On_Line;

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

         Result (J) := To_Unbounded_String (Str (Start_Ind .. End_Ind - 1));
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
      Use_Parsed : constant Arr_Str :=
        Get_Arr_Str (To_String (Use_Clause.Name));
      With_Index : Positive := 1;
      Success    : Boolean;
   begin
      while With_Index <= With_Clause.Nb_Elems loop
         Success := True;

         for Use_Index in 1 .. Use_Parsed'Last loop

            if With_Index + Use_Index - 1 > With_Clause.Name'Last
              or else Use_Parsed (Use_Index) /=
                With_Clause.Name (With_Index + Use_Index - 1)
            then
               Success := False;
            end if;

         end loop;

         if Success then
            Use_Clause.Nb_Ref := Use_Clause.Nb_Ref + 1;
            With_Clause.Clauses (With_Index + Use_Parsed'Length - 1) :=
              Use_Clause;

            return;
         end if;

         With_Index := With_Index + 1;

         exit when With_Index > With_Clause.Nb_Elems
           or else With_Clause.Clauses (With_Index) = null;
      end loop;
   end Try_Link_Clauses;

   -------------------
   -- List_All_With --
   -------------------

   procedure List_All_With
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : GNATCOLL.VFS.Virtual_File;
      Result       : out With_Lists.Vector)
   is
      Dummy : constant Update_Lock := Lock_Updates
        (Current_Text.Get_Structured_File (File_Name));

      Tree  : constant Construct_Tree :=
        Get_Tree (Current_Text.Get_Structured_File (File_Name));
      Iterator   : Construct_Tree_Iterator := First (Tree);
      New_Clause : Ptr_With;
   begin
      while Iterator /= Null_Construct_Tree_Iterator loop
         if Get_Construct (Iterator).Category = Cat_With then
            New_Clause := new With_Type
              (Get_Parts_Number (Get (Get_Construct (Iterator).Name).all));
            New_Clause.Name := Get_Arr_Str
              (Get (Get_Construct (Iterator).Name).all);
            New_Clause.Name_Str :=
              To_Unbounded_String (Get (Get_Construct (Iterator).Name).all);
            Append (Result, New_Clause);
         end if;

         Iterator := Next (Tree, Iterator, Jump_Over);
      end loop;
   end List_All_With;

   ------------------
   -- List_All_Use --
   ------------------

   procedure List_All_Use
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : GNATCOLL.VFS.Virtual_File;
      Result       : out Use_Lists.Vector)
   is
      Dummy : Update_Lock := Lock_Updates
        (Current_Text.Get_Structured_File (File_Name));

      Tree  : constant Construct_Tree :=
        Get_Tree (Current_Text.Get_Structured_File (File_Name));
      Iterator   : Construct_Tree_Iterator := First (Tree);
      New_Clause : Ptr_Use;

   begin
      while Iterator /= Null_Construct_Tree_Iterator loop
         if Get_Construct (Iterator).Category = Cat_Use then
            New_Clause := new Use_Type;
            New_Clause.Name :=
              To_Unbounded_String (Get (Get_Construct (Iterator).Name).all);
            Set_File (New_Clause.Position, File_Name);
            Set_Location
              (New_Clause.Position,
               Line      => Get_Construct (Iterator).Sloc_Start.Line,
               Column    => 1);

            declare
               Line : constant String :=
                 Get_Line (Current_Text, New_Clause.Position);
            begin
               Set_Location
                 (New_Clause.Position,
                  Line      => Get_Construct (Iterator).Sloc_Start.Line,
                  Column    => To_Column_Index
                    (String_Index_Type
                       (Get_Construct (Iterator).Sloc_Start.Column), Line));
            end;

            Append (Result, New_Clause);
         end if;

         Iterator := Next (Tree, Iterator, Jump_Into);
      end loop;
   end List_All_Use;

   ----------------------
   -- Link_All_Clauses --
   ----------------------

   procedure Link_All_Clauses
     (List_Of_With : in out With_Lists.Vector;
      List_Of_Use  : in out Use_Lists.Vector) is
   begin
      for Use_Item of List_Of_Use loop
         for With_Item of List_Of_With loop
            Try_Link_Clauses (With_Item, Use_Item);
         end loop;
      end loop;
   end Link_All_Clauses;

   ----------------------------
   -- Get_Next_With_Position --
   ----------------------------

   function Get_Next_With_Position
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : GNATCOLL.VFS.Virtual_File) return File_Cursor'Class
   is
      Lock : Update_Lock := Lock_Updates
        (Current_Text.Get_Structured_File (File_Name));

      Current_Cursor : File_Cursor;
      Current_Info   : Construct_Tree_Iterator;
      Last_Info      : Construct_Tree_Iterator := Null_Construct_Tree_Iterator;
      Tree           : Construct_Tree;
   begin
      Set_File (Current_Cursor, File_Name);
      Set_Location (Current_Cursor, 1, 1);
      Tree := Get_Tree
        (Current_Text.Get_Structured_File (Get_File (Current_Cursor)));

      Current_Info := Get_Iterator_At
        (Current_Text, Current_Cursor, Position => After);

      --  Skip the with, use clauses and pragmas.

      while Get_Construct (Current_Info).Category = Cat_With
        or else Get_Construct (Current_Info).Category = Cat_Use
        or else Get_Construct (Current_Info).Category = Cat_Pragma
      loop
         Last_Info := Current_Info;

         Current_Info := Next (Tree, Current_Info, Jump_Over);
      end loop;

      if Last_Info /= Null_Construct_Tree_Iterator then
         Set_Line (Current_Cursor, Get_Construct (Last_Info).Sloc_End.Line);

         declare
            Line : constant String := Get_Line (Current_Text, Current_Cursor);
         begin
            Set_Column
              (Current_Cursor,
               To_Column_Index
                 (String_Index_Type
                    (Get_Construct (Last_Info).Sloc_End.Column) + 1, Line));
         end;
      else
         Set_Location (Current_Cursor, 0, 1);
      end if;

      Lock.Unlock;

      return Current_Cursor;
   end Get_Next_With_Position;

   -----------------
   -- Search_With --
   -----------------

   function Search_With
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : GNATCOLL.VFS.Virtual_File;
      Pkg_Name     : String) return File_Cursor'Class
   is
      Lock : Update_Lock := Lock_Updates
        (Current_Text.Get_Structured_File (File_Name));

      Tree  : constant Construct_Tree :=
        Get_Tree (Current_Text.Get_Structured_File (File_Name));
      Iterator   : Construct_Tree_Iterator := First (Tree);
      Result     : File_Cursor;
   begin
      while Iterator /= Null_Construct_Tree_Iterator loop
         if Get_Construct (Iterator).Category = Cat_With
           and then Get (Get_Construct (Iterator).Name).all = Pkg_Name
         then
            Set_File (Result, File_Name);

            Set_Location
              (Result, Get_Construct (Iterator).Sloc_Start.Line,
               1);

            declare
               Line : constant String := Get_Line (Current_Text, Result);
            begin
               Set_Location
                 (Result, Get_Construct (Iterator).Sloc_Start.Line,
                  To_Column_Index
                    (String_Index_Type
                       (Get_Construct (Iterator).Sloc_Start.Column), Line));
            end;

            Lock.Unlock;

            return Result;
         end if;

         Iterator := Next (Tree, Iterator, Jump_Over);
      end loop;

      Lock.Unlock;

      return Null_File_Cursor;
   end Search_With;

end Codefix.Ada_Tools;
