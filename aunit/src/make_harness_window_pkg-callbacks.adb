-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007, AdaCore             --
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

with Case_Handling;   use Case_Handling;

with Pixmaps_IDE;     use Pixmaps_IDE;

with Aunit_Filters;   use Aunit_Filters;

with Gdk.Pixbuf;      use Gdk.Pixbuf;

with GPS.Intl;                  use GPS.Intl;
with VFS;                       use VFS;

with AUnit_Templates;         use AUnit_Templates;
with Templates_Parser;        use Templates_Parser;

with Gtkada.File_Selector;    use Gtkada.File_Selector;

package body Make_Harness_Window_Pkg.Callbacks is
   --  Callbacks for main "AUnit_Make_Harness" window.

   --------------------
   -- Check_Validity --
   --------------------

   procedure Check_Validity
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant Make_Harness_Window_Access :=
        Make_Harness_Window_Access (Get_Toplevel (Object));
      Valid          : Boolean;

   begin
      Valid := True;

      if not Is_Directory (Get_Text (Win.Directory_Entry)) then
         Valid := False;
         Set_Text (Win.Label, -"Invalid or non existing directory selected");
      end if;

      if Valid and then Get_Text (Win.Procedure_Entry) = "" then
         Valid := False;
         Set_Text (Win.Label, -"Missing the harness name");
      end if;

      if Valid and then
        (Win.Suite_Name = null
         or else Win.Package_Name = null)
      then
         Valid := False;
         Set_Text
           (Win.Label,
            (-"Please select a test suite file"));
      end if;

      if Valid then
         Set_Text
           (Win.Label,
            (-"Ready to create '") &
            Get_Text (Win.Procedure_Entry) &
            (-"' with test suite '") &
            Win.Suite_Name.all &
            (-"' from package '") &
            Win.Package_Name.all &
            "'");
      end if;

      Set_Response_Sensitive (Win, Gtk_Response_OK, Valid);
   end Check_Validity;

   -----------------------
   -- On_Browse_Clicked --
   -----------------------

   procedure On_Browse_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      --  Open explorer window to select suite
      Win          : constant Make_Harness_Window_Access :=
                       Make_Harness_Window_Access (Get_Toplevel (Object));
      Filter_A     : Filter_Show_All_Access;
      Filter_B     : Filter_Show_Ada_Access;
      Filter_C     : Filter_Show_Suites_Access;
      Response     : VFS.Virtual_File;
      Suite_Name   : String_Access;
      Package_Name : String_Access;
      F_Type       : Test_Type;
      Explorer     : File_Selector_Window_Access;

   begin
      Filter_A := new Filter_Show_All;
      Filter_B := new Filter_Show_Ada;
      Filter_C := new Filter_Show_Suites;

      Filter_B.Kernel := Win.Kernel;
      Filter_C.Kernel := Win.Kernel;

      Filter_A.Label := new String'(-"All files");
      Filter_B.Label := new String'(-"Ada files");
      Filter_C.Label := new String'(-"Suite files");

      Filter_C.Pixbuf := Gdk_New_From_Xpm_Data (box_xpm);
      Filter_B.Spec_Pixbuf := Gdk_New_From_Xpm_Data (box_xpm);
      Filter_B.Body_Pixbuf := Gdk_New_From_Xpm_Data (package_xpm);

      if Get_Text (Win.Directory_Entry) = "" then
         Gtk_New
           (Explorer,
            VFS.Local_Root_Dir,
            VFS.Get_Current_Dir,
            -"Select test harness",
            History => null); --  ??? No history
      else
         Gtk_New
           (Explorer,
            VFS.Local_Root_Dir,
            VFS.Create (Get_Text (Win.Directory_Entry)),
            -"Select test harness",
            History => null); --  ??? No history
      end if;

      Register_Filter (Explorer, Filter_C);
      Register_Filter (Explorer, Filter_B);
      Register_Filter (Explorer, Filter_A);

      Response := Select_File (Explorer);

      if Response = VFS.No_File then
         return;
      end if;

      Get_Suite_Name (Win.Kernel, Response.Full_Name.all,
                      Package_Name, Suite_Name, F_Type);

      if Suite_Name /= null
        and then Package_Name /= null
        and then F_Type /= Unknown
      then
         Win.Suite_Name := Suite_Name;
         Win.Package_Name := Package_Name;
      end if;

      Free (Win.Suite_Name);
      Free (Win.Package_Name);

      Get_Suite_Name (Win.Kernel, Response.Full_Name.all,
                      Package_Name, Suite_Name, F_Type);

      if Suite_Name = null
        or else Package_Name = null
        or else F_Type /= Test_Suite
      then
         Set_Text
           (Win.Label,
            (-"The file ") &
            Response.Display_Base_Name &
            (-" does not contain a test suite."));
         Free (Suite_Name);
         Free (Package_Name);

         return;
      end if;

      Win.Suite_Name := Suite_Name;
      Win.Package_Name := Package_Name;
      Set_Text (Win.File_Name_Entry, Response.Full_Name.all);

      Check_Validity (Win);
   end On_Browse_Clicked;

   ---------------------------------
   -- On_Browse_Directory_Clicked --
   ---------------------------------

   procedure On_Browse_Directory_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      --  Open explorer window to select suite
      Harness_Window : constant Make_Harness_Window_Access :=
        Make_Harness_Window_Access (Get_Toplevel (Object));
   begin
      Browse_Location (Harness_Window.Directory_Entry);

      Check_Validity (Harness_Window);
   end On_Browse_Directory_Clicked;

   -------------------
   -- On_Ok_Clicked --
   -------------------

   procedure On_Ok_Clicked (Win : Make_Harness_Window_Access) is
      --  Generate harness body source file. Close window and main loop if
      --  successful

      Directory_Name : constant String := Get_Text (Win.Directory_Entry);
      Procedure_Name : String := Get_Text (Win.Procedure_Entry);
      Translation    : Translate_Set;
      Success        : Boolean;
      pragma Unreferenced (Success);

   begin
      if Win.Suite_Name = null then
         Win.Suite_Name := new String'("");
         Win.Package_Name := new String'("");
      end if;

      Mixed_Case (Procedure_Name);
      Mixed_Case (Procedure_Name);

      Insert (Translation,
              Assoc ("HARNESS_NAME", Procedure_Name));
      Insert (Translation,
              Assoc ("HARNESS_TEST_SUITE_PACKAGE", Win.Package_Name.all));
      Insert (Translation,
              Assoc ("HARNESS_TEST_SUITE", Win.Suite_Name.all));

      Create_Files
        (Win.Kernel,
         "harness",
         Translation,
         Directory_Name,
         Procedure_Name,
         Success);
   end On_Ok_Clicked;

end Make_Harness_Window_Pkg.Callbacks;
