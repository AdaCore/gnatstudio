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

with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Glib;                    use Glib;
with Gdk.Pixbuf;              use Gdk.Pixbuf;

with Gtk.Enums;               use Gtk.Enums;

with Gtkada.File_Selector;    use Gtkada.File_Selector;
with Gtkada.Types;            use Gtkada.Types;

with Aunit_Filters;           use Aunit_Filters;
with Case_Handling;           use Case_Handling;
with GPS.Intl;                use GPS.Intl;
with Pixmaps_IDE;             use Pixmaps_IDE;
with Row_Data;                use Row_Data;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;

with Templates_Parser;        use Templates_Parser;
with AUnit_Templates;         use AUnit_Templates;

package body Make_Suite_Window_Pkg.Callbacks is

   --------------------
   -- Check_Validity --
   --------------------

   procedure Check_Validity
     (Object : access Gtk_Widget_Record'Class)
   is
      Win   : constant Make_Suite_Window_Access :=
                Make_Suite_Window_Access (Get_Toplevel (Object));
      Valid : Boolean;
   begin
      Valid := True;

      if not Is_Directory (Get_Text (Win.Directory_Entry)) then
         Valid := False;
         Set_Text (Win.Label, -"Invalid or non existing directory selected");
      end if;

      if Valid and then Get_Text (Win.Name_Entry) = "" then
         Valid := False;
         Set_Text (Win.Label, -"Missing test suite name");
      end if;

      if Valid and then Get_Rows (Win.Test_List) = 0 then
         Valid := False;
         Set_Text (Win.Label, -"Missing test");
      end if;

      if Valid then
         Set_Text
           (Win.Label,
            -"Ready to create new test suite " & Get_Text (Win.Name_Entry));
      end if;

      Set_Response_Sensitive (Win, Gtk_Response_OK, Valid);
   end Check_Validity;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      --  Open explorer window to select components of the suite

      Suite_Window : constant Make_Suite_Window_Access :=
                       Make_Suite_Window_Access (Get_Toplevel (Object));

      Filter_A     : Filter_Show_All_Access;
      Filter_B     : Filter_Show_Ada_Access;
      Filter_C     : Filter_Show_Tests_Access;
      Response     : GNATCOLL.VFS.Virtual_File;
      Suite_Name   : String_Access;
      Package_Name : String_Access;
      F_Type       : Test_Type;
      Row_Num      : Gint;
      Explorer     : File_Selector_Window_Access;

   begin
      Filter_A := new Filter_Show_All;
      Filter_B := new Filter_Show_Ada;
      Filter_C := new Filter_Show_Tests;

      Filter_B.Kernel := Suite_Window.Kernel;
      Filter_C.Kernel := Suite_Window.Kernel;

      Filter_A.Label := new String'(-"All files");
      Filter_B.Label := new String'(-"Ada files");
      Filter_C.Label := new String'(-"Suite and test files");

      if Get_Text (Suite_Window.Directory_Entry) = "" then
         Gtk_New
           (Explorer,
            Local_Root_Dir,
            Get_Current_Dir,
            "Select test suite",
            History => null);
      else
         Gtk_New
           (Explorer,
            Local_Root_Dir,
            Create (Get_Text (Suite_Window.Directory_Entry)),
            "Select test suite",
            History => null);
      end if;

      Filter_C.Pixbuf := Gdk_New_From_Xpm_Data (box_xpm);
      Filter_B.Spec_Pixbuf := Gdk_New_From_Xpm_Data (box_xpm);
      Filter_B.Body_Pixbuf := Gdk_New_From_Xpm_Data (package_xpm);

      Register_Filter (Explorer, Filter_C);
      Register_Filter (Explorer, Filter_B);
      Register_Filter (Explorer, Filter_A);

      Response := Select_File (Explorer);

      if Response = GNATCOLL.VFS.No_File then
         return;
      end if;

      Get_Suite_Name (Suite_Window.Kernel, Response.Full_Name.all,
                      Package_Name, Suite_Name, F_Type);

      if Suite_Name /= null and then Package_Name /= null then
         if F_Type = Test_Case then
            Row_Num := Append
              (Suite_Window.Test_List,
               Null_Array
               + Response.Full_Name.all + ("(test) " & Suite_Name.all));
            Set (Suite_Window.Test_List, Row_Num, Package_Name.all);

         elsif F_Type = Test_Suite then
            Row_Num := Append
              (Suite_Window.Test_List,
               Null_Array
               + Response.Full_Name.all + ("(suite) " & Suite_Name.all));
            Set (Suite_Window.Test_List,
                 Row_Num,
                 Package_Name.all);
         end if;
      end if;

      Check_Validity (Suite_Window);
   end On_Add_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked (Object : access Gtk_Button_Record'Class) is
      --  Remove selected files from the suite component list

      Suite_Window : constant Make_Suite_Window_Access :=
        Make_Suite_Window_Access (Get_Toplevel (Object));

      use Gtk.Enums.Gint_List;

      List : constant Gtk_Clist := Suite_Window.Test_List;
      I    : Gint;

   begin
      Freeze (List);

      loop
         exit when Length (Get_Selection (List)) = 0;
         I := Get_Data (First (Get_Selection (List)));
         Remove (List, I);
      end loop;

      Thaw (List);

      Check_Validity (Suite_Window);
   end On_Remove_Clicked;

   ---------------------------------
   -- On_Browse_Directory_Clicked --
   ---------------------------------

   procedure On_Browse_Directory_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      --  Open explorer window to select suite
      Suite_Window : constant Make_Suite_Window_Access :=
        Make_Suite_Window_Access (Get_Toplevel (Object));
   begin
      Browse_Location (Suite_Window.Directory_Entry);
      Check_Validity (Suite_Window);
   end On_Browse_Directory_Clicked;

   -------------------
   -- On_Ok_Clicked --
   -------------------

   procedure On_Ok_Clicked (Window : Make_Suite_Window_Access) is
      --  Generate suite source file.  Exit program if successful

      Directory_Name : constant String := Get_Text (Window.Directory_Entry);
      Name           : String := Get_Text (Window.Name_Entry);
      use Row_List;
      List : Row_List.Glist := Get_Row_List (Window.Test_List);
      Translation    : Translate_Set;
      Packages_Tag   : Tag;
      Test_Tag       : Tag;
      Test_Kind_Tag  : Tag;
      Success        : Boolean;

   begin
      --  Correct the case for Name, if needed.
      Mixed_Case (Name);
      Insert (Translation,
              Assoc ("TEST_SUITE_PACKAGE", Name));
      --  ??? customizable test suite function name
      Insert
        (Translation, Assoc ("TEST_SUITE_NAME", "Suite"));

      while List /= Null_List loop
         declare
            Package_Name : String :=
                             Get (Window.Test_List, Get_Data (List));
            S            : String :=
                             Get_Text (Window.Test_List, Get_Data (List), 1);
         begin
            if Package_Name /= "" then
               Mixed_Case (S);
               Mixed_Case (Package_Name);
               Packages_Tag := Packages_Tag & Package_Name;

               if S (S'First .. S'First + 2) = "(te" then
                  Test_Tag := Test_Tag & S (S'First + 7 .. S'Last);
                  Test_Kind_Tag := Test_Kind_Tag & "TEST_CASE";

               elsif S (S'First .. S'First + 2) = "(su" then
                  Test_Tag := Test_Tag & S (S'First + 8 .. S'Last);
                  Test_Kind_Tag := Test_Kind_Tag & "TEST_SUITE";

               else
                  Test_Tag := Test_Tag & "";
                  Test_Kind_Tag := Test_Kind_Tag & "UNKNOWN";
               end if;
            end if;
         end;

         List := Next (List);
      end loop;
      Insert (Translation, Assoc ("TEST_SUITE_PACKAGES", Packages_Tag));
      Insert (Translation, Assoc ("TEST_SUITE_TESTS", Test_Tag));
      Insert (Translation, Assoc ("TEST_SUITE_TESTS_KIND", Test_Kind_Tag));

      if Get_Active (Window.Aunit1_Button) then
         Insert
           (Translation,
            Assoc ("AUNIT", "AUNIT1.X"));
      end if;

      Create_Files
        (Window.Kernel,
         "test_suite",
         Translation,
         Directory_Name,
         Name,
         Success);

      if Success then
         Hide (Window);
      end if;
   end On_Ok_Clicked;

end Make_Suite_Window_Pkg.Callbacks;
