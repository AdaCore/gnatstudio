-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2010, AdaCore                  --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with Glib.Convert;            use Glib.Convert;

with GPS.Kernel.Project;      use GPS.Kernel.Project;
with Language;                use Language;
with Language.Ada;            use Language.Ada;
with Projects;                use Projects;

package body Aunit_Filters is

   --------------------
   -- Get_Suite_Name --
   --------------------

   procedure Get_Suite_Name
     (Kernel       : GPS.Kernel.Kernel_Handle;
      File_Name    : Virtual_File;
      Package_Name : out GNATCOLL.Symbols.Symbol;
      Suite_Name   : out GNATCOLL.Symbols.Symbol;
      F_Type       : out Test_Type)
   is
      Index             : Integer;
      File_Buffer       : GNAT.Strings.String_Access;
      Constructs        : aliased Construct_List;
      Current_Construct : Construct_Access;
      Found             : Boolean := False;
      Info              : File_Info;

   begin
      Package_Name := No_Symbol;
      Suite_Name   := No_Symbol;
      F_Type       := Unknown;

      if not Is_Regular_File (File_Name) then
         return;
      end if;

      Info := Get_Registry (Kernel).Tree.Info (File_Name);

      if Info.Language /= "ada" then
         return;
      end if;

      File_Buffer := File_Name.Read_File;
      if File_Buffer /= null then
         Parse_Constructs
           (Ada_Lang, Locale_To_UTF8 (File_Buffer.all), Constructs);
         Current_Construct := Constructs.First;
      end if;

      --  Find the name of the suite or test case

      while not Found
        and then Current_Construct /= null
      loop
         if Current_Construct.Category = Cat_Class
           and then Current_Construct.Name /= No_Symbol
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
               Suite_Name := Current_Construct.Name;
            end if;
         end if;

         if Current_Construct.Category = Cat_Function
           and then Current_Construct.Name /= No_Symbol
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
               Suite_Name := Current_Construct.Name;
            end if;
         end if;

         Current_Construct := Current_Construct.Next;
      end loop;

      Free (Constructs);
      Free (File_Buffer);
   end Get_Suite_Name;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   overriding procedure Use_File_Filter
     (Filter : access Filter_Show_Suites;
      Win    : access File_Selector_Window_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.Strings.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : Symbol;
      Package_Name : Symbol;
      F_Type       : Test_Type;

   begin
      Get_Suite_Name (Filter.Kernel, File, Package_Name, Suite_Name, F_Type);

      if F_Type = Test_Suite then
         State  := Normal;
         Text   := new String'(Get (Suite_Name).all);
         Pixbuf := Filter.Pixbuf;
         return;
      end if;

      State  := Invisible;
      Text   := new String'("");
      Pixbuf := Null_Pixbuf;
   end Use_File_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   overriding procedure Use_File_Filter
     (Filter : access Filter_Show_Tests;
      Win    : access File_Selector_Window_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.Strings.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : Symbol;
      Package_Name : Symbol;
      F_Type       : Test_Type;

   begin
      Get_Suite_Name (Filter.Kernel, File, Package_Name, Suite_Name, F_Type);

      if F_Type = Test_Suite or else F_Type = Test_Case then
         State  := Normal;
         Text   := new String'(Get (Suite_Name).all);
         Pixbuf := Filter.Pixbuf;
         return;
      end if;

      State  := Invisible;
      Text   := new String'("");
      Pixbuf := Null_Pixbuf;
   end Use_File_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   overriding procedure Use_File_Filter
     (Filter : access Filter_Show_Ada;
      Win    : access File_Selector_Window_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.Strings.String_Access)
   is
      pragma Unreferenced (Win);
      Info : constant File_Info :=
        Get_Registry (Filter.Kernel).Tree.Info (File);
   begin
      if Info.Language = "ada" then
         State := Normal;
         Text  := new String'(Info.Unit_Name);

         if Info.Unit_Part = Unit_Spec then
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
