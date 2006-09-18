-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004                            --
--                             AdaCore                               --
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

with Adp_Converter;             use Adp_Converter;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Projects;                  use Projects;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtk.Label;                 use Gtk.Label;
with GPS.Intl;                  use GPS.Intl;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;
with VFS;                       use VFS;
with Wizards;                   use Wizards;

package body Creation_Wizard.Adp is

   type Adp_Selection_Page is new Project_Wizard_Page_Record with record
      Kernel        : Kernel_Handle;
      Adp_File_Name : Gtk.GEntry.Gtk_Entry;
   end record;
   type Adp_Selection_Page_Access is access all Adp_Selection_Page'Class;
   procedure Generate_Project
     (Page               : access Adp_Selection_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean);
   function Create_Content
     (Page : access Adp_Selection_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   function Is_Complete (Page : access Adp_Selection_Page) return String;
   --  See inherited documentation

   procedure On_Browse
     (Widget : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page);
   --  Called when the browse button is pressed.

   --------------------------
   -- Add_Adp_Wizard_Pages --
   --------------------------

   procedure Add_Adp_Wizard_Pages
     (Wiz : access Project_Wizard_Record'Class)
   is
      Adp_Page : constant Adp_Selection_Page_Access := new Adp_Selection_Page;
   begin
      Adp_Page.Kernel := Get_Kernel (Wiz);
      Add_Page (Wiz, Adp_Page,
                Toc         => -".adp file selection",
                Description => -"Select .adp file name");
   end Add_Adp_Wizard_Pages;

   -----------------
   -- Is_Complete --
   -----------------

   function Is_Complete
     (Page : access Adp_Selection_Page) return String is
   begin
      if Page.Adp_File_Name /= null
        and then Get_Text (Page.Adp_File_Name) = ""
      then
         Grab_Focus (Page.Adp_File_Name);
         return -"Specify a .adp project to convert";
      end if;
      return "";
   end Is_Complete;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page               : access Adp_Selection_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean)
   is
      pragma Unreferenced (Scenario_Variables);
      Adp_File : constant String := Get_Text (Page.Adp_File_Name);
   begin
      if Adp_File /= "" then
         Convert_Adp_File
           (Adp_Filename   => Adp_File,
            Registry       => Get_Registry (Kernel).all,
            Project        => Project,
            Spec_Extension => ".ads",
            Body_Extension => ".adb");
         Changed := True;
      end if;
   end Generate_Project;

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
     (Page : access Adp_Selection_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Box2   : Gtk_Box;
      Button : Gtk_Button;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);

      Gtk_New (Label, -"Enter the name of the .adp file:");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New_Hbox (Box2, Homogeneous => False);
      Pack_Start (Box, Box2, Expand => False);

      Gtk_New (Page.Adp_File_Name);
      Pack_Start (Box2, Page.Adp_File_Name, Expand => True);

      Gtk_New (Button, -"Browse");
      Pack_Start (Box2, Button, Expand => False);
      Page_Handlers.Connect
        (Button, "clicked", On_Browse'Access,
         User_Data => Project_Wizard_Page (Page));

      Widget_Callback.Object_Connect
        (Page.Adp_File_Name, "changed", Update_Buttons_Sensitivity'Access,
         Wiz);

      Grab_Focus (Page.Adp_File_Name);

      return Gtk_Widget (Box);
   end Create_Content;

   ---------------
   -- On_Browse --
   ---------------

   procedure On_Browse
     (Widget : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page)
   is
      P    : constant Adp_Selection_Page_Access :=
        Adp_Selection_Page_Access (Page);
      Name : constant VFS.Virtual_File := Select_File
        (Use_Native_Dialog  => Get_Pref (Use_Native_Dialogs),
         File_Pattern       => "*.adp",
         Pattern_Name       => -"Glide project files",
         Parent             => Gtk_Window (Get_Toplevel (Widget)),
         Kind               => Open_File);
   begin
      if Name /= VFS.No_File then
         Set_Text (P.Adp_File_Name, Full_Name (Name).all);
      end if;
   end On_Browse;

end Creation_Wizard.Adp;
