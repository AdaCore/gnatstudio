-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2007, AdaCore           --
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

with GNAT.OS_Lib;
with GNAT.Mmap;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Glib.Convert;            use Glib.Convert;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with Language;                use Language;
with Language.Ada;            use Language.Ada;
with Projects;                use Projects;
with Namet;                   use Namet;

package body Aunit_Filters is

   --------------------
   -- Get_Suite_Name --
   --------------------

   procedure Get_Suite_Name
     (Kernel       : GPS.Kernel.Kernel_Handle;
      File_Name    : String;
      Package_Name : out GNAT.Strings.String_Access;
      Suite_Name   : out GNAT.Strings.String_Access;
      F_Type       : out Test_Type)
   is
      Index             : Integer;
      File_Buffer       : GNAT.Strings.String_Access;
      Constructs        : aliased Construct_List;
      Current_Construct : Construct_Access;
      Found             : Boolean := False;
      Part              : Unit_Part;
      Unit_Name         : Name_Id;
      Lang              : Name_Id;

   begin
      Package_Name := null;
      Suite_Name   := null;
      F_Type       := Unknown;

      if not GNAT.OS_Lib.Is_Regular_File (File_Name) then
         return;
      end if;

      Projects.Get_Unit_Part_And_Name_From_Filename
        (Filename  => File_Name,
         Project   => Get_Project (Kernel),
         Part      => Part,
         Unit_Name => Unit_Name,
         Lang      => Lang);

      if Get_String (Lang) /= Ada_String then
         return;
      end if;

      File_Buffer := GNAT.Mmap.Read_Whole_File
        (Locale_From_UTF8 (File_Name),
         Empty_If_Not_Found => True);
      Parse_Constructs
        (Ada_Lang, Locale_To_UTF8 (File_Buffer.all), Constructs);
      Current_Construct := Constructs.First;

      --  Find the name of the suite or test case.

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
               Suite_Name := GNAT.Strings.String_Access
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
               Suite_Name := GNAT.Strings.String_Access
                 (Current_Construct.Name);
            end if;
         end if;

         Current_Construct := Current_Construct.Next;
      end loop;

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
      Text   : out GNAT.Strings.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : GNAT.Strings.String_Access;
      Package_Name : GNAT.Strings.String_Access;
      F_Type       : Test_Type;

   begin
      Get_Suite_Name
        (Filter.Kernel, Full_Name (File).all, Package_Name, Suite_Name,
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
      Text   : out GNAT.Strings.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : GNAT.Strings.String_Access;
      Package_Name : GNAT.Strings.String_Access;
      F_Type       : Test_Type;

   begin
      Get_Suite_Name
        (Filter.Kernel, Full_Name (File).all, Package_Name, Suite_Name,
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
      Text   : out GNAT.Strings.String_Access)
   is
      pragma Unreferenced (Win);
      Part      : Unit_Part;
      Unit_Name : Namet.Name_Id;
      Lang      : Namet.Name_Id;
   begin

      Projects.Get_Unit_Part_And_Name_From_Filename
        (Filename  => File.Base_Name,
         Project   => Get_Project (Filter.Kernel),
         Part      => Part,
         Unit_Name => Unit_Name,
         Lang      => Lang);

      if Get_String (Lang) = "ada" then
         State := Normal;
         Text  := new String'(Get_String (Unit_Name));

         if Part = Unit_Spec then
            Pixbuf := Filter.Spec_Pixbuf;
         else
            Pixbuf := Filter.Body_Pixbuf;
         end if;
         return;
      end if;

      State  := Invisible;
      Text   := new String'("");
      Pixbuf := Null_Pixbuf;
   end Use_File_Filter;

end Aunit_Filters;
