-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001-2002                    --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with String_Utils;              use String_Utils;
with Ada.Text_IO;               use Ada.Text_IO;
with Prj;                       use Prj;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with String_Utils; use String_Utils;
with Ada_Analyzer; use Ada_Analyzer;
with Basic_Types;  use Basic_Types;
with Language;         use Language;

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
      Index       : Integer;
      F           : File_Descriptor;
      Length      : Integer;
      Indent      : Natural;
      Next_Indent : Natural;
      File_Buffer : String_Access;
      New_Buffer  : Extended_Line_Buffer;
      Constructs  : aliased Construct_List;
      Current_Construct : Construct_Access;

      Found       : Boolean := False;

   begin
      if not Is_Regular_File (File_Name)
        or else File_Name'Length <= 4
        or else File_Name (File_Name'Last - 3 .. File_Name'Last - 1) /= ".ad"
      then
         return;
      end if;

      F           := Open_Read (File_Name, Binary);
      File_Buffer := new String (1 .. Integer (File_Length (F)));
      Length      := Read (F, File_Buffer.all'Address, File_Buffer'Length);
      Close (F);

      Analyze_Ada_Source
        (To_Unchecked_String (File_Buffer.all'Address),
         Length,
         New_Buffer,
         Default_Indent_Parameters,
         Reserved_Casing  => Unchanged,
         Ident_Casing     => Unchanged,
         Format_Operators => False,
         Indent           => False,
         Constructs       => Constructs'Unchecked_Access,
         Current_Indent   => Next_Indent,
         Prev_Indent      => Indent);

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
         Package_Name := GNAT.OS_Lib.String_Access
           (Constructs.Last.Name);
      end if;

   exception
      when Use_Error =>
         null;
      when Device_Error =>
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
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : GNAT.OS_Lib.String_Access := null;
      Package_Name : GNAT.OS_Lib.String_Access;
   begin
      --  To find suites, look for tests and suites in body files.

      if File'Length > 4
        and then File (File'Last - 3 .. File'Last) = ".adb"
      then
         Get_Suite_Name (Dir & File, Package_Name, Suite_Name);

         if Suite_Name /= null then
            State := Normal;
            Text := Suite_Name;
            Pixmap := Filter.Suite_Pixmap;
            Mask := Filter.Suite_Bitmap;
         end if;
      else
         State := Invisible;
         Text := new String'("");
         Pixmap := Gdk.Pixmap.Null_Pixmap;
         Mask   := Gdk.Bitmap.Null_Bitmap;
      end if;
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
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
      Suite_Name   : GNAT.OS_Lib.String_Access;
      Package_Name : GNAT.OS_Lib.String_Access;
   begin
      Get_Suite_Name (Dir & File, Package_Name, Suite_Name);

      if Suite_Name /= null then
         State := Normal;
         Text := Suite_Name;
         Pixmap := Filter.Suite_Pixmap;
         Mask := Filter.Suite_Bitmap;
      else
         State := Invisible;
         Text := new String'("");
         Pixmap := Gdk.Pixmap.Null_Pixmap;
         Mask   := Gdk.Bitmap.Null_Bitmap;
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
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
   begin
      --  ??? In fact, we should gather all the possible extensions in the
      --  ??? projects, as well as the naming scheme exceptions.
      if File'Length >= 4
        and then (File (File'Last - 3 .. File'Last) = ".ads"
                  or else File (File'Last - 3 .. File'Last) = ".adb")
      then
         State := Normal;

         declare
            Found     : Boolean := False;
            File_T    : File_Type;
            Index_End : Integer;
            Line      : String (1 .. 256);
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
                       new String' (Line (1 .. Index_End - 1));
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
         Text := new String'("PROJECT");
      else
         State := Insensitive;
         Text := new String'("");
      end if;

      Pixmap := Gdk.Pixmap.Null_Pixmap;
      Mask   := Gdk.Bitmap.Null_Bitmap;

      if File'Length >= 4 then
         if File (File'Last - 3 .. File'Last) = ".adb" then
            Pixmap := Filter.Body_Pixmap;
            Mask := Filter.Body_Bitmap;

         elsif File (File'Last - 3 .. File'Last) = ".ads" then
            Pixmap := Filter.Spec_Pixmap;
            Mask := Filter.Spec_Bitmap;
         end if;
      end if;
   end Use_File_Filter;

end Aunit_Filters;
