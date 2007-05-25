-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004-2007                       --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Object;               use Glib.Object;
with Glib;                      use Glib;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Widget;                use Gtk.Widget;

with Creation_Wizard.Adp;       use Creation_Wizard.Adp;
with Creation_Wizard.Extending; use Creation_Wizard.Extending;
with Creation_Wizard.Full;      use Creation_Wizard.Full;
with Creation_Wizard.Simple;    use Creation_Wizard.Simple;
with Creation_Wizard;           use Creation_Wizard;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel;                use GPS.Kernel;
with Traces;                    use Traces;
with VFS;                       use VFS;
with Wizards;                   use Wizards;

package body Creation_Wizard.Selector is

   type Wizard_Selector_Page is new Project_Wizard_Page_Record with record
      Last_Selected : Integer := -1;
      Name_And_Loc  : Name_And_Location_Page_Access;
      From_Scratch  : Gtk_Radio_Button;
      From_Existing : Gtk_Radio_Button;
      From_Library  : Gtk_Radio_Button;
      From_Adp      : Gtk_Radio_Button;
      Extending     : Gtk_Radio_Button;
   end record;
   type Wizard_Selector_Page_Access is access all Wizard_Selector_Page'Class;
   function Create_Content
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   procedure Generate_Project
     (Page               : access Wizard_Selector_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean);
   function Next_Page
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Wizard_Page;
   --  See inherited documentation

   ---------------
   -- Next_Page --
   ---------------

   function Next_Page
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Wizard_Page
   is
      Selected : Integer := 1;
   begin
      if Get_Active (Page.From_Existing) then
         Selected := 1;
      elsif Get_Active (Page.From_Adp) then
         Selected := 2;
      elsif Get_Active (Page.From_Scratch) then
         Selected := 3;
      elsif Get_Active (Page.From_Library) then
         Selected := 4;
      elsif Get_Active (Page.Extending) then
         Selected := 5;
      end if;

      if Page.Last_Selected /= Selected then
         Page.Last_Selected := Selected;
         Remove_Pages (Wiz, After => Page.Name_And_Loc);
         case Selected is
            when 1 => Add_Simple_Wizard_Pages (Project_Wizard (Wiz));
            when 2 => Add_Adp_Wizard_Pages (Project_Wizard (Wiz));
            when 3 => Add_Full_Wizard_Pages
                 (Project_Wizard (Wiz), Page.Name_And_Loc, "wizard");
            when 4 => Add_Full_Wizard_Pages
                 (Project_Wizard (Wiz), Page.Name_And_Loc, "library_wizard");
            when 5 => Add_Extending_Wizard_Pages (Project_Wizard (Wiz));
            when others =>
               null;
         end case;
      end if;

      return null;
   end Next_Page;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page               : access Wizard_Selector_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean)
   is
      pragma Unreferenced (Page, Kernel, Scenario_Variables, Project, Changed);
   begin
      null;
   end Generate_Project;

   ------------------------
   -- Create_New_Project --
   ------------------------

   function Create_New_Project
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Boolean
   is
      Wiz  : Project_Wizard;
      P    : Wizard_Selector_Page_Access;
      Name : Virtual_File;
   begin
      Gtk_New (Wiz, Kernel, -"Create New Project");
      P := new Wizard_Selector_Page;
      Add_Page
        (Wiz,
         Page        => P,
         Description => -"Select the type of project(s) to create",
         Toc         => -"Project type");

      P.Name_And_Loc := Add_Name_And_Location_Page (Wiz);

      Name := Run (Wiz);
      if Name /= VFS.No_File then
         Load_Project (Kernel, Name);
         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Create_New_Project;

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Wiz);
      Button    : Gtk_Widget;
      Box       : Gtk_Box;
      Separator : Gtk_Separator;
      Label     : Gtk_Label;
      pragma Unreferenced (Button);

   begin
      Gtk_New_Vbox (Box, Homogeneous => False);

      Gtk_New (Page.From_Scratch, Label => -"Single Project");
      Pack_Start (Box, Page.From_Scratch, Expand => False);
      Gtk_New
        (Label,
         -("Create a new project file, with full control of the properties"));
      Set_Padding (Label, 20, 5);
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New_Hseparator (Separator);
      Pack_Start (Box, Separator, Expand => False);

      Gtk_New
        (Page.From_Existing, Get_Group (Page.From_Scratch), -"Project Tree");
      Pack_Start (Box, Page.From_Existing, Expand => False);
      Gtk_New
        (Label,
         -("Create a new set of projects given an existing build environment."
           & ASCII.LF
           & "GPS will try to preserve the build structure you already have"));
      Set_Padding (Label, 20, 5);
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New_Hseparator (Separator);
      Pack_Start (Box, Separator, Expand => False);

      Gtk_New
        (Page.From_Adp,
         Get_Group (Page.From_Scratch),
         -"Convert GLIDE Project (.adp)");
      Pack_Start (Box, Page.From_Adp, Expand => False);
      Gtk_New
        (Label,
         -("Converts a .adp file into a project file. adp files are simple"
           & ASCII.LF
           & "project files used in the Emacs based GLIDE environment"));
      Set_Padding (Label, 20, 5);
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New_Hseparator (Separator);
      Pack_Start (Box, Separator, Expand => False);

      Gtk_New (Page.From_Library, Get_Group (Page.From_Scratch),
               Label => -"Library Project");
      Pack_Start (Box, Page.From_Library, Expand => False);
      Gtk_New
        (Label,
         -("Create a new project file, defining a library rather than an"
           & ASCII.LF
           & "executable"));
      Set_Padding (Label, 20, 5);
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New_Hseparator (Separator);
      Pack_Start (Box, Separator, Expand => False);

      Gtk_New
        (Page.Extending, Get_Group (Page.From_Scratch), -"Extending Project");
      Pack_Start (Box, Page.Extending, Expand => False);
      Gtk_New
        (Label,
         -("Create an extending project that allows you to work on a copy of"
           & ASCII.LF
           & "some sources and recompile them locally without affecting the"
           & ASCII.LF
           & "project's build"));
      Set_Padding (Label, 20, 5);
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New_Hseparator (Separator);
      Pack_Start (Box, Separator, Expand => False);

      Set_Active (Page.From_Scratch, True);
      return Gtk_Widget (Box);
   end Create_Content;

   --------------------
   -- On_New_Project --
   --------------------

   procedure On_New_Project
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      Tmp : Boolean;
      pragma Unreferenced (Widget, Tmp);
   begin
      Tmp := Create_New_Project (Kernel);
   end On_New_Project;

end Creation_Wizard.Selector;
