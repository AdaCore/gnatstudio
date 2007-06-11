-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004-2007                       --
--                            AdaCore                                --
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Projects.Editor;           use Projects, Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with Traces;                    use Traces;

with Wizards;                   use Wizards;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Intl;                  use GPS.Intl;
with File_Utils;                use File_Utils;
with VFS;                       use VFS;

package body Creation_Wizard is

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
      Project           : Projects.Project_Type := Projects.No_Project) is
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
      Project           : Projects.Project_Type := Projects.No_Project) is
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

   function Is_Complete
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
            if not Has_Focus_Is_Set (Page.Project_Location) then
               Grab_Focus (Page.Project_Name);
            end if;

            return -"Specify a project name";
         end if;

         if not Is_Valid_Project_Name (Project) then
            if not Has_Focus_Is_Set (Page.Project_Location) then
               Grab_Focus (Page.Project_Name);
            end if;

            return -("Invalid name for the project "
                     & "(only letters, digits and underscores)");
         end if;
      end;

      if Get_Text (Page.Project_Location) = "" then
         if not Has_Focus_Is_Set (Page.Project_Name) then
            Grab_Focus (Page.Project_Location);
         end if;

         return -"Specify a directory to store the project in";
      end if;

      return "";
   end Is_Complete;

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
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

      Gtk_New (Page.Project_Name, 255);
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

      Gtk_New (Page.Project_Location, 255);
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
                     Get_Pref (Generate_Relative_Paths));
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
      Name : constant VFS.Virtual_File := Select_Directory
        (Title             => -"Select project file location",
         Parent            => Gtk_Window (Get_Toplevel (Widget)),
         Base_Directory    => VFS.Create (Get_Text (P.Project_Location)),
         Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
         History           => Get_History (P.Kernel));

   begin
      if Name /= VFS.No_File then
         VFS.Ensure_Directory (Name);
         Set_Text (P.Project_Location, VFS.Full_Name (Name).all);
      end if;
   end Advanced_Prj_Location;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page               : access Name_And_Location_Page;
      Kernel             : access Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean)
   is
      Dir            : constant String := Name_As_Directory
        (Get_Text (Page.Project_Location));
      Name           : constant String := Get_Text (Page.Project_Name);
      Relative_Paths : constant Boolean :=
        Page.Relative_Paths = null or else Get_Active (Page.Relative_Paths);
      Tmp            : Boolean;
      Error          : Import_Project_Error;
      Result         : Message_Dialog_Buttons;
      Parent         : Project_Type;
      pragma Unreferenced (Tmp, Scenario_Variables, Result);

      Project_Name : constant String := Get_Text (Page.Project_Name);
      Prj_File     : constant String := To_File_Name (Project_Name);
      Location     : constant String := Get_Text (Page.Project_Location);
   begin
      if Is_Regular_File (Location & Prj_File & Project_File_Extension) then
         if Message_Dialog
           (Msg         => Location & Prj_File & Project_File_Extension
               & (-" already exists. Do you want to overwrite ?"),
            Title       => -"File exists",
            Dialog_Type => Gtkada.Dialogs.Error,
            Buttons     => Button_Yes or Button_No) = Button_No
         then
            raise Invalid_Project_Page;
         end if;
      end if;

      if not Is_Directory (Location) then
         if Message_Dialog
           (Msg         => Location
            & (-" is not a directory, would you like to create it ?"),
            Title       => -"Directory not found",
            Dialog_Type => Information,
            Buttons     => Button_Yes or Button_No) = Button_Yes
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
         Project := Create_Project
           (Get_Registry (Kernel).all,
            Name => Name (Name'First .. Name'Last - 4), Path => Create (Dir));
      else
         Project := Create_Project
           (Get_Registry (Kernel).all, Name => Name, Path => Create (Dir));
      end if;

      if Relative_Paths then
         Set_Paths_Type (Project, Relative);
      else
         Set_Paths_Type (Project, Absolute);
      end if;

      for J in reverse Name'First .. Name'Last - 4 loop
         if Name (J) = '.' then
            Parent := Get_Project_From_Name
              (Registry => Get_Registry (Kernel).all,
               Name     => Get_String (Name (Name'First .. J - 1)));

            if Parent /= No_Project then
               Error := Add_Imported_Project
                 (Root_Project       => Get_Project (Kernel),
                  Project            => Project,
                  Imported_Project   => Parent,
                  Use_Relative_Path  => True);
            else
               Error := Add_Imported_Project
                 (Root_Project => Get_Project (Kernel),
                  Project      => Project,
                  Imported_Project_Location =>
                    Create (Name (Name'First .. J - 1)),
                  Use_Relative_Path => True);
            end if;

            if Error /= Success then
               Result := Message_Dialog
                 (Msg => -("The parent project couldn't be found,"
                           & ASCII.LF
                           & "and a dependency to it couldn't be added."
                           & ASCII.LF
                           & "Please fix the project manually"),
                  Buttons => Button_OK);
            end if;
            exit;
         end if;
      end loop;

      Changed := True;
   end Generate_Project;

   --------------------
   -- Perform_Finish --
   --------------------

   procedure Perform_Finish (Wiz : access Project_Wizard_Record) is
      Pages   : constant Wizard_Pages_Array_Access := Get_Pages (Wiz);
      Changed : Boolean := False;
      Tmp     : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Push_State (Get_Kernel (Wiz), Processing);

      for P in Pages'Range loop
         --  The first page will create the project (Name_And_Location_Page)
         Generate_Project
           (Project_Wizard_Page (Pages (P)),
            Get_Kernel (Wiz),
            Scenario_Variables => Projects.No_Scenario,
            Project            => Wiz.Project,
            Changed            => Changed);
      end loop;

      if Changed then
         Recompute_View (Get_Kernel (Wiz));
      end if;

      if Wiz.Auto_Save_On_Exit then
         Tmp := Save_Single_Project (Get_Kernel (Wiz), Wiz.Project);
      end if;

      Pop_State (Get_Kernel (Wiz));

   exception
      when Invalid_Project_Page =>
         Pop_State (Get_Kernel (Wiz));

      when E : others => Trace (Exception_Handle, E);
   end Perform_Finish;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Wiz : access Project_Wizard_Record'Class) return Projects.Project_Type is
   begin
      return Wiz.Project;
   end Get_Project;

   ---------
   -- Run --
   ---------

   function Run
     (Wiz : access Project_Wizard_Record) return VFS.Virtual_File
   is
      Name : VFS.Virtual_File;
   begin
      Show_All (Wiz);

      if Run (Wiz) = Gtk_Response_Apply then
         if Wiz.Project = No_Project then
            Destroy (Wiz);
            return VFS.No_File;
         else
            Name := Project_Path (Wiz.Project);
            Destroy (Wiz);
            return Name;
         end if;
      else
         Destroy (Wiz);
         return VFS.No_File;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Destroy (Wiz);
         return VFS.No_File;
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
