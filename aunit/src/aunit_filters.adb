-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2006                    --
--                              AdaCore                              --
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

with Ada.Text_IO;             use Ada.Text_IO;
with Projects;                use Projects;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with OS_Utils;                use OS_Utils;
with String_Utils;            use String_Utils;
with Language.Ada;            use Language.Ada;
with Basic_Types;             use Basic_Types;
with Language;                use Language;

package body Aunit_Filters is

   --------------------
   -- Get_Suite_Name --
   --------------------

   procedure Get_Suite_Name
     (File_Name    : String;
      Package_Name : out GNAT.OS_Lib.String_Access;
      Suite_Name   : out GNAT.OS_Lib.String_Access;
      F_Type       : out Test_Type)
   is
      Index             : Integer;
      File_Buffer       : GNAT.OS_Lib.String_Access;
      Constructs        : aliased Construct_List;
      Current_Construct : Construct_Access;
      Found             : Boolean := False;

   begin
      Package_Name := null;
      Suite_Name   := null;
      F_Type       := Unknown;

      if not Is_Regular_File (File_Name)
        or else File_Name'Length <= 4
        or else
          (File_Name (File_Name'Last - 3 .. File_Name'Last) /= ".ads"
           and then File_Name (File_Name'Last - 3 .. File_Name'Last) /= ".adb")
      then
         return;
      end if;

      File_Buffer := Read_File (File_Name);
      Parse_Constructs (Ada_Lang, File_Buffer.all, Constructs);
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
                  F_Type := Test_Case;
                  Suite_Name := GNAT.OS_Lib.String_Access
                    (Current_Construct.Name);
               end if;
            end if;

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
                  F_Type := Test_Suite;
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
         Suite_Name   := new String'(Suite_Name.all);
      end if;

      Free (Constructs);
      Free (File_Buffer);
   end Get_Suite_Name;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter : access Filter_Show_Suites;
      Win    : access File_Selector_Window_Record'Class;
      File   : VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : GNAT.OS_Lib.String_Access;
      Package_Name : GNAT.OS_Lib.String_Access;
      F_Type       : Test_Type;

   begin
      Get_Suite_Name (Full_Name (File).all, Package_Name, Suite_Name,
                      F_Type);

      --  Don't need package name
      Free (Package_Name);

      if F_Type = Test_Suite then
         State  := Normal;
         Text   := Suite_Name;
         Pixbuf := Filter.Pixbuf;
         return;
      end if;

      Free (Suite_Name);
      State  := Invisible;
      Text   := new String'("");
      Pixbuf := Null_Pixbuf;
   end Use_File_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter : access Filter_Show_Tests;
      Win    : access File_Selector_Window_Record'Class;
      File   : VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : GNAT.OS_Lib.String_Access;
      Package_Name : GNAT.OS_Lib.String_Access;
      F_Type       : Test_Type;

   begin
      Get_Suite_Name (Full_Name (File).all, Package_Name, Suite_Name,
                      F_Type);

      --  Don't need package name
      Free (Package_Name);

      if F_Type = Test_Suite or else F_Type = Test_Case then
         State  := Normal;
         Text   := Suite_Name;
         Pixbuf := Filter.Pixbuf;
         return;
      end if;

      Free (Suite_Name);
      State  := Invisible;
      Text   := new String'("");
      Pixbuf := Null_Pixbuf;
   end Use_File_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter : access Filter_Show_Ada;
      Win    : access File_Selector_Window_Record'Class;
      File   : VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
      Base : constant String := Base_Name (File);
   begin
      --  ??? In fact, we should gather all the possible extensions in the
      --  projects, as well as the naming scheme exceptions.

      if Base'Length >= 4
        and then (Base (Base'Last - 3 .. Base'Last) = ".ads"
                  or else Base (Base'Last - 3 .. Base'Last) = ".adb")
      then
         State := Normal;

         declare
            Found     : Boolean := False;
            File_T    : File_Type;
            Index_End : Integer;
            Line      : String (1 .. 1024);
            Line_Last : Integer;

         begin
            --  ??? We should not use Ada.Text_IO files here...
            Ada.Text_IO.Open (File_T, In_File, Full_Name (File).all);

            while not Found loop
               Get_Line (File_T, Line, Line_Last);
               Index_End := 1;
               Skip_To_String (Line, Index_End, "--");

               if Index_End > Line_Last - 2 then
                  Index_End := 1;
                  Skip_To_String (Line, Index_End, " is");

                  if Index_End < Line_Last - 1 then
                     Text := new String'(Line (1 .. Index_End - 1));
                     Found := True;
                  end if;
               end if;
            end loop;

            Close (File_T);
         exception
            when Name_Error | Use_Error | End_Error =>
               Close (File_T);
         end;

      elsif Base'Length >= 4
        and then Base (Base'Last - 3 .. Base'Last) = Project_File_Extension
      then
         State := Highlighted;
         Text := new String'("Project");
      else
         State := Invisible;
         Text := new String'("");
      end if;

      Pixbuf := Null_Pixbuf;

      if Base'Length >= 4 then
         if Base (Base'Last - 3 .. Base'Last) = ".adb" then
            Pixbuf := Filter.Body_Pixbuf;

         elsif Base (Base'Last - 3 .. Base'Last) = ".ads" then
            Pixbuf := Filter.Spec_Pixbuf;
         end if;
      end if;
   end Use_File_Filter;

end Aunit_Filters;
