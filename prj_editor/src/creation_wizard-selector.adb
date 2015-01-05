------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2015, AdaCore                     --
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

with Glib.Object;               use Glib.Object;
with Glib;                      use Glib;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Widget;                use Gtk.Widget;

with Creation_Wizard.Extending; use Creation_Wizard.Extending;
with Creation_Wizard.Full;      use Creation_Wizard.Full;
with Creation_Wizard.Simple;    use Creation_Wizard.Simple;
with Creation_Wizard.GNATname;  use Creation_Wizard.GNATname;
with Creation_Wizard;           use Creation_Wizard;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel;                use GPS.Kernel;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with Wizards;                   use Wizards;

package body Creation_Wizard.Selector is
   Me : constant Trace_Handle := Create ("WIZARD");

   type Wizard_Kinds is
     (Unknown,
      From_Scratch,
      Gnatname,
      From_Existing,
      From_Library,
      Extending);

   subtype Valid_Wizard_Kinds is Wizard_Kinds range From_Scratch .. Extending;
   type Wizard_Radio_Button_Array is
     array (Valid_Wizard_Kinds) of Gtk_Radio_Button;

   type Wizard_Selector_Page is new Project_Wizard_Page_Record with record
      Last_Selected : Wizard_Kinds := Unknown;
      Name_And_Loc  : Name_And_Location_Page_Access;
      Buttons       : Wizard_Radio_Button_Array;
   end record;
   type Wizard_Selector_Page_Access is access all Wizard_Selector_Page'Class;
   overriding function Create_Content
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   overriding procedure Generate_Project
     (Page               : access Wizard_Selector_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Project            : in out Project_Type;
      Changed            : in out Boolean);
   overriding function Next_Page
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Wizard_Page;
   --  See inherited documentation

   ---------------
   -- Next_Page --
   ---------------

   overriding function Next_Page
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Wizard_Page
   is
      Selected : Wizard_Kinds := Unknown;
   begin
      for J in Page.Buttons'Range loop
         if Get_Active (Page.Buttons (J)) then
            Selected := J;
            exit;
         end if;
      end loop;

      if Page.Last_Selected /= Selected then
         Page.Last_Selected := Selected;
         Remove_Pages (Wiz, After => Page.Name_And_Loc);

         case Selected is
            when Unknown =>
               --  Should never happen
               pragma Assert (False);
            when From_Scratch =>
               Add_Full_Wizard_Pages
                 (Project_Wizard (Wiz), Page.Name_And_Loc, "wizard");
            when From_Existing =>
               Add_Simple_Wizard_Pages (Project_Wizard (Wiz));
            when From_Library =>
               Add_Full_Wizard_Pages
                 (Project_Wizard (Wiz), Page.Name_And_Loc, "library_wizard");
            when Extending =>
               Add_Extending_Wizard_Pages (Project_Wizard (Wiz));
            when Gnatname =>
               Add_GNATname_Wizard_Pages
                 (Project_Wizard (Wiz), Page.Name_And_Loc, "gnatname_wizard");
         end case;
      end if;

      return null;
   end Next_Page;

   ----------------------
   -- Generate_Project --
   ----------------------

   overriding procedure Generate_Project
     (Page               : access Wizard_Selector_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Project            : in out Project_Type;
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
      if Name /= GNATCOLL.VFS.No_File then
         Load_Project (Kernel, Name);
         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Create_New_Project;

   --------------------
   -- Create_Content --
   --------------------

   overriding function Create_Content
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Wiz);

      procedure Add
        (Kind       : Valid_Wizard_Kinds;
         Short_Text : String;
         Full_Text  : String);
      --  Add choice wizard with given Kind and texts

      Box       : Gtk_Box;

      ---------
      -- Add --
      ---------

      procedure Add
        (Kind       : Valid_Wizard_Kinds;
         Short_Text : String;
         Full_Text  : String)
      is
         Separator : Gtk_Separator;
         Label     : Gtk_Label;
      begin
         if Kind = Valid_Wizard_Kinds'First then
            Gtk_New (Page.Buttons (Kind), Label => -Short_Text);
         else
            Gtk_New (Page.Buttons (Kind),
                     Get_Group (Page.Buttons (Valid_Wizard_Kinds'First)),
                     Label => -Short_Text);
         end if;

         Pack_Start (Box, Page.Buttons (Kind), Expand => False);
         Gtk_New (Label, -Full_Text);
         Set_Padding (Label, 20, 5);
         Set_Alignment (Label, 0.0, 0.5);
         Pack_Start (Box, Label, Expand => False);
         Gtk_New_Hseparator (Separator);
         Pack_Start (Box, Separator, Expand => False);
      end Add;

   begin
      Gtk_New_Vbox (Box, Homogeneous => False);

      Add (From_Scratch,
           "Single Project",
           "Create a new project file, with full control of the properties");

      Add (Gnatname,
           "Single Project with complex naming scheme",
           "Create a new project for existing source code stored with"
           & ASCII.LF
           & "arbitrary file naming conventions using gnatname tool");

      Add (From_Existing,
           "Project Tree",
           "Create a new set of projects given an existing build environment."
           & ASCII.LF
           & "GPS will try to preserve the build structure you already have");

      Add (From_Library,
           "Library Project",
           "Create a new project file, defining a library rather than an"
           & ASCII.LF
           & "executable");

      Add (Extending,
           "Extending Project",
           "Create an extending project that allows you to work on a copy of"
           & ASCII.LF
           & "some sources and recompile them locally without affecting the"
           & ASCII.LF
           & "project's build");

      Set_Active (Page.Buttons (From_Scratch), True);
      return Gtk_Widget (Box);
   end Create_Content;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access New_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Tmp : Boolean;
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      pragma Unreferenced (Command, Tmp);
   begin
      Tmp := Create_New_Project (Kernel);
      return Commands.Success;
   end Execute;

end Creation_Wizard.Selector;
