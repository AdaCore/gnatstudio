-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
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

with Glib;                  use Glib;
with Gtk.Arguments;         use Gtk.Arguments;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Label;             use Gtk.Label;
with Gtk.Table;             use Gtk.Table;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;
with Gtkada.Dialogs;        use Gtkada.Dialogs;
with Gtkada.File_Selector;  use Gtkada.File_Selector;
with Gtkada.Handlers;       use Gtkada.Handlers;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Projects.Editor; use Projects, Projects.Editor;
with Projects.Registry; use Projects.Registry;

with Basic_Types;      use Basic_Types;
with Wizards;          use Wizards;
with Glide_Kernel;     use Glide_Kernel;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Intl;       use Glide_Intl;
with File_Utils;       use File_Utils;
with Project_Viewers;  use Project_Viewers;
with Project_Properties; use Project_Properties;

package body Creation_Wizard is

   function First_Page
     (Wiz : access Prj_Wizard_Record'Class) return Gtk_Box;
   --  Return the widget to use for the "General" page in the wizard

   procedure Change_Forward_State (Wiz : access Gtk_Widget_Record'Class);
   --  Checks whether the contents of the first page has been fully answered,
   --  and activate (or not) the next button.

   procedure Page_Checker (Wiz : access Gtk_Widget_Record'Class);
   --  Check that the contents of the current page is valid. If not, prevent
   --  the user from changing the page

   procedure Advanced_Prj_Location (W : access Gtk_Widget_Record'Class);
   --  Open up a dialog to select the project location.

   function Generate_Prj (W : access Gtk_Widget_Record'Class) return String;
   --  Generate the project files from the contents of the wizard W.
   --  Return the directory/name of the project that was just created.

   procedure Switch_Page
     (Wiz : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when a new page is selected in the wizard. We dynamically create
   --  the page if needed.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Wiz    : out Prj_Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Wiz := new Prj_Wizard_Record;
      Creation_Wizard.Initialize (Wiz, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Wiz    : access Prj_Wizard_Record'Class;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Page  : Project_Editor_Page;
      Attr_Count : constant Natural := Attribute_Editors_Page_Count;
      Count : constant Natural := Project_Editor_Pages_Count (Kernel);
      Main_Page_Box : Gtk_Box;
      Box           : Gtk_Box;

   begin
      Wiz.Kernel := Kernel_Handle (Kernel);
      Wiz.XML_Pages_Count := 0;
      Wizards.Initialize
        (Wiz, Kernel, -"Project setup", Num_Pages => 1);

      Set_Toc (Wiz, 1, -"Naming the project", -"Creating a new project");
      Main_Page_Box := First_Page (Wiz);
      Set_Page (Wiz, 1, Main_Page_Box);

      for E in 1 .. Attr_Count loop
         Box := Attribute_Editors_Page_Box
           (Kernel           => Kernel,
            Project          => No_Project,
            General_Page_Box => Main_Page_Box,
            Path_Widget      => Wiz.Project_Location,
            Nth_Page         => E,
            Context          => "wizard");
         if Box /= null then
            Wiz.XML_Pages_Count := Wiz.XML_Pages_Count + 1;
            Add_Page (Wiz,
                      Page         => Box,
                      Title        => Attribute_Editors_Page_Name (E),
                      Toc_Contents => Attribute_Editors_Page_Name (E));
         end if;
      end loop;

      for E in 1 .. Count loop
         Page := Get_Nth_Project_Editor_Page (Kernel, E);
         Add_Page (Wiz,
                   Page         => Widget_Factory
                     (Page, No_Project,
                      Name_As_Directory (Get_Text (Wiz.Project_Location))
                      & Get_Text (Wiz.Project_Name),
                      Kernel),
                   Title        => Get_Title (Page),
                   Toc_Contents => Get_Toc (Page));
      end loop;

      Widget_Callback.Connect (Wiz, "switch_page", Switch_Page'Access);
   end Initialize;

   -----------------
   -- Switch_Page --
   -----------------

   procedure Switch_Page
     (Wiz : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      W        : constant Prj_Wizard := Prj_Wizard (Wiz);
      Page_Num : constant Guint := To_Guint (Args, 1);
   begin
      if Integer (Page_Num - 1) <= W.XML_Pages_Count then
         null;

      elsif Integer (Page_Num - 1) - W.XML_Pages_Count <=
        Project_Editor_Pages_Count (W.Kernel)
      then
         declare
            Languages : GNAT.OS_Lib.String_List := Get_Current_Value
              (Kernel  => W.Kernel,
               Pkg     => "",
               Name    => "languages",
               Index   => "");
         begin
            Refresh
              (Page         => Get_Nth_Project_Editor_Page
                 (W.Kernel, Integer (Page_Num - 1) - W.XML_Pages_Count),
               Widget       => Get_Nth_Page (W, Integer (Page_Num)),
               Project      => No_Project,
               Languages    => Languages);
            Free (Languages);
         end;
      end if;
   end Switch_Page;

   --------------------------
   -- Change_Forward_State --
   --------------------------

   procedure Change_Forward_State (Wiz : access Gtk_Widget_Record'Class) is
      W : constant Prj_Wizard := Prj_Wizard (Wiz);
   begin
      Set_Sensitive (Next_Button (W), Get_Text (W.Project_Name)'Length /= 0);
   end Change_Forward_State;

   ------------------
   -- Page_Checker --
   ------------------

   procedure Page_Checker (Wiz : access Gtk_Widget_Record'Class) is
      W       : constant Prj_Wizard := Prj_Wizard (Wiz);
      Ignored : Message_Dialog_Buttons;
      pragma Unreferenced (Ignored);

   begin
      if Get_Current_Page (W) = 1 then
         declare
            Project  : constant String := Get_Text (W.Project_Name);
            Prj_File : constant String := To_File_Name (Project);
            Location : constant String := Get_Text (W.Project_Location);

         begin
            if not Is_Valid_Project_Name (Project) then
               Ignored := Message_Dialog
                 (Msg =>
                    (-"Invalid name for the project ") &
                    (-"(only letters, digits and underscores)"),
                  Title => -"Invalid name",
                  Dialog_Type => Error,
                  Buttons => Button_OK);

               Gtk.Handlers.Emit_Stop_By_Name (Next_Button (W), "clicked");
               return;
            end if;

            if Is_Regular_File
              (Location & Prj_File & Project_File_Extension)
            then
               if Message_Dialog
                 (Msg => Location
                  & Prj_File & Project_File_Extension
                  & (-" already exists. Do you want to overwrite ?"),
                  Title => -"File exists",
                  Dialog_Type => Error,
                  Buttons => Button_Yes or Button_No) = Button_No
               then
                  Gtk.Handlers.Emit_Stop_By_Name (Next_Button (W), "clicked");
               end if;
            end if;

            if not Is_Directory (Location) then
               if Message_Dialog
                 (Msg => Location
                  & (-" is not a directory, would you like to create it ?"),
                  Title => -"Directory not found",
                  Dialog_Type => Information,
                  Buttons => Button_Yes or Button_No) = Button_Yes
               then
                  begin
                     Make_Dir (Location);
                  exception
                     when Directory_Error =>
                        null;
                  end;
               end if;
            end if;
         end;
      end if;
   end Page_Checker;

   ----------------
   -- First_Page --
   ----------------

   function First_Page
     (Wiz : access Prj_Wizard_Record'Class) return Gtk_Box
   is
      Table  : Gtk_Table;
      Label  : Gtk_Label;
      Button : Gtk_Button;
      Page   : Gtk_Vbox;
      Box    : Gtk_Vbox;
      Frame  : Gtk_Frame;
   begin
      Gtk_New_Vbox (Page);
      Set_Border_Width (Page, 5);

      Gtk_New (Frame, -"Name & Location");
      Set_Border_Width (Frame, 5);
      Pack_Start (Page, Frame, Expand => False);

      Gtk_New (Table, Rows => 4, Columns => 2, Homogeneous => False);
      Add (Frame, Table);

      Gtk_New (Label, -"Enter the name of the project to create:");
      Attach (Table, Label, 0, 2, 0, 1);

      Gtk_New (Wiz.Project_Name, 255);
      Attach (Table, Wiz.Project_Name, 0, 1, 1, 2);
      Set_Activates_Default (Wiz.Project_Name, True);

      --  We can't move to the next page until the name of the project has been
      --  specified

      Set_Sensitive (Next_Button (Wiz), False);

      Widget_Callback.Object_Connect
        (Wiz.Project_Name, "changed",
         Widget_Callback.To_Marshaller (Change_Forward_State'Access), Wiz);

      Set_Row_Spacing (Table, 1, 20);

      Gtk_New
        (Label,
         -"Enter the directory where the project file will be created:");
      Attach (Table, Label, 0, 2, 2, 3);

      Gtk_New (Wiz.Project_Location, 255);
      Set_Text (Wiz.Project_Location, Get_Current_Dir);
      Attach (Table, Wiz.Project_Location, 0, 1, 3, 4);
      Set_Activates_Default (Wiz.Project_Location, True);

      Gtk_New (Button, -"Browse");
      Attach (Table, Button, 1, 2, 3, 4, Xoptions => 0);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Advanced_Prj_Location'Access), Wiz);

      Gtk_New (Frame, -"General");
      Set_Border_Width (Frame, 5);
      Pack_Start (Page, Frame, Expand => False);

      Gtk_New_Vbox (Box);
      Set_Border_Width (Box, 5);
      Add (Frame, Box);

      Gtk_New (Wiz.Relative_Paths, -"Use relative paths in the projects");
      Set_Active (Wiz.Relative_Paths,
                  Get_Pref (Wiz.Kernel, Generate_Relative_Paths));
      Pack_Start (Box, Wiz.Relative_Paths, Expand => False);

      Widget_Callback.Object_Connect
        (Next_Button (Wiz), "clicked",
         Widget_Callback.To_Marshaller (Page_Checker'Access), Wiz);

      Show_All (Page);

      return Page;
   end First_Page;

   ---------------------------
   -- Advanced_Prj_Location --
   ---------------------------

   procedure Advanced_Prj_Location (W : access Gtk_Widget_Record'Class) is
      Name : constant String := Select_Directory
        (Title          => -"Select project file location",
         Parent         => Gtk_Window (Get_Toplevel (W)),
         Base_Directory => Name_As_Directory
           (Get_Text (Prj_Wizard (W).Project_Location)),
         History        => Get_History (Prj_Wizard (W).Kernel));
   begin
      if Name /= "" then
         Set_Text (Prj_Wizard (W).Project_Location, Name);
      end if;
   end Advanced_Prj_Location;

   ------------------
   -- Generate_Prj --
   ------------------

   function Generate_Prj (W : access Gtk_Widget_Record'Class) return String is
      Wiz        : constant Prj_Wizard := Prj_Wizard (W);
      Count      : constant Natural := Project_Editor_Pages_Count (Wiz.Kernel);
      Dir        : constant String := Name_As_Directory
        (Get_Text (Wiz.Project_Location));
      Name           : constant String := Get_Text (Wiz.Project_Name);
      Relative_Paths : constant Boolean := Get_Active (Wiz.Relative_Paths);
      Languages      : GNAT.OS_Lib.String_List := Get_Current_Value
        (Kernel  => Wiz.Kernel,
         Pkg     => "",
         Name    => "languages",
         Index   => "");
      Project        : Project_Type;
      Changed        : Boolean;
      pragma Unreferenced (Changed);


   begin
      Push_State (Wiz.Kernel, Processing);

      if Name'Length > 4
        and then Name (Name'Last - 3 .. Name'Last) = ".gpr"
      then
         Project := Create_Project
           (Get_Registry (Wiz.Kernel).all,
            Name => Name (Name'First .. Name'Last - 4), Path => Dir);
      else
         Project := Create_Project
           (Get_Registry (Wiz.Kernel).all, Name => Name, Path => Dir);
      end if;

      if Relative_Paths then
         Set_Paths_Type (Project, Relative);
      else
         Set_Paths_Type (Project, Absolute);
      end if;

      Changed := Update_Project_Attributes
        (Project            => Project,
         Scenario_Variables => (1 .. 0 => No_Variable));

      for P in 1 .. Count loop
         --  We are only interested in the side effect of Project_Editor, since
         --  we know for sure that the project will be modified
         Changed := Project_Editor
           (Get_Nth_Project_Editor_Page (Wiz.Kernel, P),
            Project,
            Wiz.Kernel,
            Get_Nth_Page (Wiz, P + Wiz.XML_Pages_Count + 1),
            Languages          => Languages,
            Scenario_Variables => (1 .. 0 => No_Variable),
            Ref_Project => Project);
      end loop;

      Save_Single_Project (Wiz.Kernel, Project);
      Free (Languages);
      Pop_State (Wiz.Kernel);

      return Project_Path (Project);
   end Generate_Prj;

   ---------
   -- Run --
   ---------

   function Run (Wiz : access Prj_Wizard_Record) return String is
   begin
      Show_All (Wiz);
      Set_Current_Page (Wiz, 1);
      Grab_Focus (Wiz.Project_Name);

      if Run (Wiz) = Gtk_Response_Apply then
         return Generate_Prj (Wiz);
      else
         return "";
      end if;
   end Run;

end Creation_Wizard;
