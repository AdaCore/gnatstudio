-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2003                    --
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

with String_Utils;            use String_Utils;
with Ada.Text_IO;             use Ada.Text_IO;
with Projects;                use Projects;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with String_Utils;            use String_Utils;
with Language.Ada;            use Language.Ada;
with Basic_Types;             use Basic_Types;
with Language;                use Language;

package body Aunit_Filters is
   subtype String_Access is Basic_Types.String_Access;

   ----------------
   -- Suite_Name --
   ----------------

   procedure Get_Suite_Name
     (File_Name    : in String;
      Package_Name : out GNAT.OS_Lib.String_Access;
      Suite_Name   : out GNAT.OS_Lib.String_Access)
   is
      Index             : Integer;
      F                 : File_Descriptor;
      Length            : Integer;
      File_Buffer       : String_Access;
      Constructs        : aliased Construct_List;
      Current_Construct : Construct_Access;
      Found             : Boolean := False;

   begin
      if not Is_Regular_File (File_Name)
        or else File_Name'Length <= 4
        or else
          (File_Name (File_Name'Last - 3 .. File_Name'Last) /= ".ads"
           and then File_Name (File_Name'Last - 3 .. File_Name'Last) /= ".adb")
      then
         return;
      end if;

      F           := Open_Read (File_Name, Binary);
      File_Buffer := new String (1 .. Integer (File_Length (F)));
      Length      := Read (F, File_Buffer.all'Address, File_Buffer'Length);
      Close (F);

      Parse_Constructs (Ada_Lang, File_Buffer (1 .. Length), Constructs);
      Current_Construct := Constructs.First;

      --  Find the name of the suite or test case.

      if File_Name (File_Name'Last - 3 .. File_Name'Last) = ".ads" then
         while not Found
           and then Current_Construct /= null
         loop
            if Current_Construct.Category = Cat_Class
              and then Current_Construct.Name /= null
            then
               Index := Current_Construct.Sloc_Start.Index;

               while Index + 2 <= Current_Construct.Sloc_End.Index
                 and then To_Lower (File_Buffer (Index .. Index + 2))
                 /= "new"
               loop
                  Index := Index + 1;
               end loop;

               while Index + 8 <= Current_Construct.Sloc_End.Index
                 and then To_Lower (File_Buffer (Index .. Index + 8))
                 /= "test_case"
               loop
                  Index := Index + 1;
               end loop;

               if Index + 8 <= Current_Construct.Sloc_End.Index then
                  Found := True;
                  Suite_Name := GNAT.OS_Lib.String_Access
                    (Current_Construct.Name);
               end if;
            end if;

            Current_Construct := Current_Construct.Next;
         end loop;

      elsif File_Name (File_Name'Last - 3 .. File_Name'Last) = ".adb" then
         while not Found
           and then Current_Construct /= null
         loop
            if Current_Construct.Category = Cat_Function
              and then Current_Construct.Name /= null
            then
               Index := Current_Construct.Sloc_Start.Index;

               while Index + 16 <= Current_Construct.Sloc_End.Index
                 and then To_Lower (File_Buffer (Index .. Index + 16))
                 /= "access_test_suite"
               loop
                  Index := Index + 1;
               end loop;

               if Index + 16 <= Current_Construct.Sloc_End.Index then
                  Found := True;
                  Suite_Name := GNAT.OS_Lib.String_Access
                    (Current_Construct.Name);
               end if;
            end if;

            Current_Construct := Current_Construct.Next;
         end loop;
      end if;

      --  Find the name of the main unit.

      if Found then
         Package_Name := new String'(Constructs.Last.Name.all);
      end if;

      Free (Constructs);

   exception
      when Use_Error =>
         null;
      when GNAT.OS_Lib.Device_Error =>
         Close (F);
   end Get_Suite_Name;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Suites;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixbuf    : out Gdk_Pixbuf;
      Text      : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : GNAT.OS_Lib.String_Access;
      Package_Name : GNAT.OS_Lib.String_Access;

   begin
      --  To find suites, look for tests and suites in body files.

      if File'Length > 4
        and then File (File'Last - 3 .. File'Last) = ".adb"
      then
         Get_Suite_Name (Dir & File, Package_Name, Suite_Name);

         if Suite_Name /= null then
            State  := Normal;
            Text   := Suite_Name;
            Pixbuf := Filter.Pixbuf;
            return;
         end if;
      end if;

      State  := Invisible;
      Text   := new String'("");
      Pixbuf := Null_Pixbuf;
   end Use_File_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Tests;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixbuf    : out Gdk_Pixbuf;
      Text      : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : GNAT.OS_Lib.String_Access;
      Package_Name : GNAT.OS_Lib.String_Access;
   begin
      Get_Suite_Name (Dir & File, Package_Name, Suite_Name);

      if Suite_Name /= null then
         State  := Normal;
         Text   := Suite_Name;
         Pixbuf := Filter.Pixbuf;
      else
         State  := Invisible;
         Text   := new String'("");
         Pixbuf := Null_Pixbuf;
      end if;
   end Use_File_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Ada;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixbuf    : out Gdk_Pixbuf;
      Text      : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
   begin
      --  ??? In fact, we should gather all the possible extensions in the
      --  projects, as well as the naming scheme exceptions.

      if File'Length >= 4
        and then (File (File'Last - 3 .. File'Last) = ".ads"
                  or else File (File'Last - 3 .. File'Last) = ".adb")
      then
         State := Normal;

         declare
            Found     : Boolean := False;
            File_T    : File_Type;
            Index_End : Integer;
            Line      : String (1 .. 1024);
            Line_Last : Integer;

         begin
            Ada.Text_IO.Open (File_T, In_File, Dir & File);

            while not Found loop
               Get_Line (File_T, Line, Line_Last);
               Index_End := 1;
               Skip_To_String (Line, Index_End, "--");

               if Index_End > Line_Last - 2 then
                  Index_End := 1;
                  Skip_To_String (Line, Index_End, " is");

                  if Index_End < Line_Last - 1 then
                     Text :=
                       new String'(Line (1 .. Index_End - 1));
                     Found := True;
                  end if;
               end if;
            end loop;

            Close (File_T);
         exception
            when Name_Error =>
               null;
            when Use_Error =>
               null;
            when End_Error =>
               Close (File_T);
         end;

      elsif File'Length >= 4
        and then File (File'Last - 3 .. File'Last) = Project_File_Extension
      then
         State := Highlighted;
         Text := new String'("Project");
      else
         State := Insensitive;
         Text := new String'("");
      end if;

      Pixbuf := Null_Pixbuf;

      if File'Length >= 4 then
         if File (File'Last - 3 .. File'Last) = ".adb" then
            Pixbuf := Filter.Body_Pixbuf;

         elsif File (File'Last - 3 .. File'Last) = ".ads" then
            Pixbuf := Filter.Spec_Pixbuf;
         end if;
      end if;
   end Use_File_Filter;

end Aunit_Filters;
