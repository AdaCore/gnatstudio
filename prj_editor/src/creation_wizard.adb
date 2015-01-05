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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Glib;                      use Glib;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Editable;              use Gtk.Editable;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;

with File_Utils;                use File_Utils;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Intl;                  use GPS.Intl;
with Projects;                  use Projects;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with Wizards;                   use Wizards;

package body Creation_Wizard is
   Me : constant Trace_Handle := Create ("WIZARD");

   procedure Advanced_Prj_Location
     (Widget : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page);
   --  Open up a dialog to select the project location

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Wiz               : out Project_Wizard;
      Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title             : String;
      Show_Toc          : Boolean := True;
      Auto_Save_On_Exit : Boolean := True;
      Project           : Project_Type := No_Project) is
   begin
      Wiz := new Project_Wizard_Record;
      Initialize (Wiz, Kernel, Title, Show_Toc, Auto_Save_On_Exit, Project);
   end Gtk_New;

   --------------------------------
   -- Add_Name_And_Location_Page --
   --------------------------------

   function Add_Name_And_Location_Page
     (Wiz                 : access Project_Wizard_Record'Class;
      Force_Relative_Dirs : Boolean := False)
      return Name_And_Location_Page_Access
   is
      Page : Name_And_Location_Page_Access;
   begin
      Page := new Name_And_Location_Page;
      Page.Kernel := Get_Kernel (Wiz);
      Page.Force_Relative_Dirs := Force_Relative_Dirs;
      Add_Page
        (Wiz,
         Page         => Page,
         Description => -"Enter the project name and location",
         Toc         => -"Naming the project");
      return Page;
   end Add_Name_And_Location_Page;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Wiz               : access Project_Wizard_Record'Class;
      Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title             : String;
      Show_Toc          : Boolean := True;
      Auto_Save_On_Exit : Boolean := True;
      Project           : Project_Type := No_Project) is
   begin
      Wizards.Initialize
        (Wiz,
         Kernel   => Kernel,
         Title    => Title,
         Show_Toc => Show_Toc);
      Wiz.Project := Project;
      Wiz.Auto_Save_On_Exit := Auto_Save_On_Exit;
   end Initialize;

   -----------------
   -- Is_Complete --
   -----------------

   overriding function Is_Complete
     (Page : access Name_And_Location_Page) return String
   is
      Ignored : Message_Dialog_Buttons;
      pragma Unreferenced (Ignored);
   begin
      if Page.Project_Name = null then
         return -"Specify a project name";
      end if;

      declare
         Project  : constant String := Get_Text (Page.Project_Name);
      begin
         if Project = "" then
            if not Page.Project_Location.Has_Focus then
               Grab_Focus (Page.Project_Name);
            end if;

            return -"Specify a project name";
         end if;

         if not Is_Valid_Project_Name (Project) then
            if not Page.Project_Location.Has_Focus then
               Grab_Focus (Page.Project_Name);
            end if;

            return -("Invalid name for the project "
                     & "(only letters, digits and underscores "
                     & "and cannot be an Ada reserved word)");
         end if;
      end;

      if Get_Text (Page.Project_Location) = "" then
         if not Page.Project_Name.Has_Focus then
            Grab_Focus (Page.Project_Location);
         end if;

         return -"Specify a directory to store the project in";
      end if;

      return "";
   end Is_Complete;

   --------------------
   -- Create_Content --
   --------------------

   overriding function Create_Content
     (Page : access Name_And_Location_Page;
      Wiz  : access Wizards.Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      Table    : Gtk_Table;
      Label    : Gtk_Label;
      Button   : Gtk_Button;
      Main_Box : Gtk_Vbox;
      Box      : Gtk_Vbox;
      Frame    : Gtk_Frame;
   begin
      Gtk_New_Vbox (Main_Box);
      Set_Border_Width (Main_Box, 5);

      Gtk_New (Frame, -"Name & Location");
      Set_Border_Width (Frame, 5);
      Pack_Start (Main_Box, Frame, Expand => False);

      Gtk_New (Table, Rows => 4, Columns => 2, Homogeneous => False);
      Add (Frame, Table);

      Gtk_New (Label, -"Enter the name of the project to create:");
      Attach (Table, Label, 0, 2, 0, 1);

      Gtk_New (Page.Project_Name);
      Page.Project_Name.Set_Max_Length (255);
      Attach (Table, Page.Project_Name, 0, 1, 1, 2);
      Set_Activates_Default (Page.Project_Name, True);
      Grab_Focus (Page.Project_Name);

      Widget_Callback.Object_Connect
        (Page.Project_Name,
         Signal_Changed, Update_Buttons_Sensitivity'Access, Wiz);

      Set_Row_Spacing (Table, 1, 20);

      Gtk_New
        (Label,
         -"Enter the directory where the project file will be created:");
      Attach (Table, Label, 0, 2, 2, 3);

      Gtk_New (Page.Project_Location);
      Page.Project_Location.Set_Max_Length (255);
      Set_Text (Page.Project_Location, Get_Current_Dir);
      Attach (Table, Page.Project_Location, 0, 1, 3, 4);
      Set_Activates_Default (Page.Project_Location, True);
      Widget_Callback.Object_Connect
        (Page.Project_Location, Signal_Changed,
         Update_Buttons_Sensitivity'Access, Wiz);

      Gtk_New (Button, -"Browse");
      Attach (Table, Button, 1, 2, 3, 4, Xoptions => 0);
      Page_Handlers.Connect
        (Button, Signal_Clicked, Advanced_Prj_Location'Access,
         User_Data => Project_Wizard_Page (Page));

      if not Page.Force_Relative_Dirs then
         Gtk_New (Frame, -"General");
         Set_Border_Width (Frame, 5);
         Pack_Start (Main_Box, Frame, Expand => False);

         Gtk_New_Vbox (Box);
         Set_Border_Width (Box, 5);
         Add (Frame, Box);

         Gtk_New (Page.Relative_Paths, -"Use relative paths in the projects");
         Set_Active (Page.Relative_Paths,
                     Generate_Relative_Paths.Get_Pref);
         Pack_Start (Box, Page.Relative_Paths, Expand => False);
      else
         Page.Relative_Paths := null;
      end if;

      return Gtk_Widget (Main_Box);
   end Create_Content;

   ---------------------------
   -- Advanced_Prj_Location --
   ---------------------------

   procedure Advanced_Prj_Location
     (Widget : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page)
   is
      P : constant Name_And_Location_Page_Access :=
            Name_And_Location_Page_Access (Page);
      Name : constant GNATCOLL.VFS.Virtual_File := Select_Directory
        (Title             => -"Select project file location",
         Parent            => Gtk_Window (Get_Toplevel (Widget)),
         Base_Directory    => Create (+Get_Text (P.Project_Location)),
         --  ??? What if the filesystem path is non-UTF8?
         Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
         History           => Get_History (P.Kernel));

   begin
      if Name /= GNATCOLL.VFS.No_File then
         GNATCOLL.VFS.Ensure_Directory (Name);
         Set_Text (P.Project_Location, +GNATCOLL.VFS.Full_Name (Name));
         --  ??? What if the filesystem path is non-UTF8?
      end if;
   end Advanced_Prj_Location;

   ----------------------
   -- Generate_Project --
   ----------------------

   overriding procedure Generate_Project
     (Page               : access Name_And_Location_Page;
      Kernel             : access Kernel_Handle_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Project            : in out Project_Type;
      Changed            : in out Boolean)
   is
      Dir            : constant Virtual_File :=
                         Create_From_UTF8 (Get_Text (Page.Project_Location));
      Name           : constant String := Get_Text (Page.Project_Name);
      Relative_Paths : constant Boolean :=
        Page.Relative_Paths = null or else Get_Active (Page.Relative_Paths);
      Tmp            : Boolean;
      Error          : Import_Project_Error;
      Result         : Message_Dialog_Buttons;
      Parent         : Project_Type;
      pragma Unreferenced (Tmp, Scenario_Variables, Result);

      Project_Name   : constant String := Get_Text (Page.Project_Name);
      Prj_Base_File  : constant Filesystem_String :=
                         To_File_Name (+Project_Name);
      Location       : constant Virtual_File :=
                         Create_From_UTF8 (Get_Text (Page.Project_Location));
      Prj_File       : Virtual_File;
      Parent_Window  : Gtk_Window;

   begin
      Parent_Window := Gtk_Window (Get_Toplevel (Page.Get_Content));
      Prj_File := Create_From_Dir
        (Location, Prj_Base_File & Project_File_Extension);
      if Is_Regular_File (Prj_File) then
         if Message_Dialog
           (Msg         => Prj_File.Display_Full_Name
               & (-" already exists. Do you want to overwrite ?"),
            Title       => -"File exists",
            Dialog_Type => Gtkada.Dialogs.Error,
            Buttons     => Button_Yes or Button_No,
            Parent      => Parent_Window) = Button_No
         then
            raise Invalid_Project_Page;
         end if;
      end if;

      if not Is_Directory (Location) then
         if Message_Dialog
           (Msg         => Location.Display_Full_Name
            & (-" is not a directory, would you like to create it ?"),
            Title       => -"Directory not found",
            Dialog_Type => Information,
            Buttons     => Button_Yes or Button_No,
            Parent      => Parent_Window) = Button_Yes
         then
            begin
               Make_Dir (Location);
            exception
               when Directory_Error =>
                  null;
            end;
         end if;
      end if;

      if Name'Length > 4
        and then Name (Name'Last - 3 .. Name'Last) = ".gpr"
      then
         Project := Get_Registry (Kernel).Tree.Create_Project
           (Name => Name (Name'First .. Name'Last - 4), Path => Dir);
      else
         Project := Get_Registry (Kernel).Tree.Create_Project
           (Name => Name, Path => Dir);
      end if;

      if Relative_Paths then
         Set_Paths_Type (Project, Relative);
      else
         Set_Paths_Type (Project, Absolute);
      end if;

      for J in reverse Name'First .. Name'Last - 4 loop
         if Name (J) = '.' then
            Parent := Get_Registry (Kernel).Tree.Project_From_Name
              (Name     => Name (Name'First .. J - 1));

            if Parent /= No_Project then
               Error := Project.Add_Imported_Project
                 (Imported_Project   => Parent,
                  Use_Relative_Path  => True);
            else
               Error := Get_Registry (Kernel).Tree.Add_Imported_Project
                 (Project           => Project,
                  Imported_Project_Location =>
                    Create (+Name (Name'First .. J - 1)),
                  Use_Relative_Path => True);
            end if;

            if Error /= Success then
               Result := Message_Dialog
                 (Msg => -("The parent project couldn't be found,"
                           & ASCII.LF
                           & "and a dependency to it couldn't be added."
                           & ASCII.LF
                           & "Please fix the project manually"),
                  Buttons => Button_OK,
                  Parent  => Parent_Window);
            end if;
            exit;
         end if;
      end loop;

      Changed := True;
   end Generate_Project;

   --------------------
   -- Perform_Finish --
   --------------------

   overriding procedure Perform_Finish (Wiz : access Project_Wizard_Record) is
      Pages   : constant Wizard_Pages_Array_Access := Get_Pages (Wiz);
      Changed : Boolean := False;
      Tmp     : Boolean;
      pragma Unreferenced (Tmp);
   begin
      for P in Pages'Range loop
         --  The first page will create the project (Name_And_Location_Page)
         Generate_Project
           (Project_Wizard_Page (Pages (P)),
            Get_Kernel (Wiz),
            Scenario_Variables => All_Scenarios,
            Project            => Wiz.Project,
            Changed            => Changed);
      end loop;

      if Changed then
         Recompute_View (Get_Kernel (Wiz));
      end if;

      if Wiz.Auto_Save_On_Exit then
         Tmp := Save_Single_Project (Get_Kernel (Wiz), Wiz.Project);

         for P in Pages'Range loop
            Project_Saved
              (Project_Wizard_Page (Pages (P)),
               Get_Kernel (Wiz),
               Wiz.Project);
         end loop;
      end if;

   exception
      when Invalid_Project_Page =>
         null;
      when E : others =>
         Trace (Me, E);
   end Perform_Finish;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Wiz : access Project_Wizard_Record'Class) return Project_Type is
   begin
      return Wiz.Project;
   end Get_Project;

   ---------
   -- Run --
   ---------

   function Run
     (Wiz : access Project_Wizard_Record) return GNATCOLL.VFS.Virtual_File
   is
      Name : GNATCOLL.VFS.Virtual_File;
   begin
      Show_All (Wiz);
      Present (Wiz);

      if Run (Wiz) = Gtk_Response_Apply then
         if Wiz.Project = No_Project then
            Destroy (Wiz);
            return GNATCOLL.VFS.No_File;
         else
            Name := Project_Path (Wiz.Project);
            Destroy (Wiz);
            return Name;
         end if;
      else
         Destroy (Wiz);
         return GNATCOLL.VFS.No_File;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Destroy (Wiz);
         return GNATCOLL.VFS.No_File;
   end Run;

   ---------------------
   -- Get_Path_Widget --
   ---------------------

   function Get_Path_Widget
     (Page : access Name_And_Location_Page) return Gtk.GEntry.Gtk_Entry is
   begin
      return Page.Project_Location;
   end Get_Path_Widget;

   ---------------------
   -- Get_Name_Widget --
   ---------------------

   function Get_Name_Widget
     (Page : access Name_And_Location_Page) return Gtk.GEntry.Gtk_Entry is
   begin
      return Page.Project_Name;
   end Get_Name_Widget;

end Creation_Wizard;
