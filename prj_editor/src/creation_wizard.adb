-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
with Gtkada.Dialogs;        use Gtkada.Dialogs;
with Gtkada.File_Selector;  use Gtkada.File_Selector;
with Gtkada.Handlers;       use Gtkada.Handlers;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Prj.Tree; use Prj.Tree;
with Prj;      use Prj;
with Snames;   use Snames;

with Basic_Types;      use Basic_Types;
with Wizards;          use Wizards;
with Prj_API;          use Prj_API;
with Glide_Kernel;     use Glide_Kernel;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Intl;       use Glide_Intl;
with String_Utils;     use String_Utils;

package body Creation_Wizard is

   function First_Page
     (Wiz : access Prj_Wizard_Record'Class) return Gtk_Widget;
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

   function Get_Languages (Wiz : access Prj_Wizard_Record'Class)
      return Argument_List;
   --  Return the list of languages selected by the user. The returned array
   --  must be freed by the caller.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Wiz : out Prj_Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Wiz := new Prj_Wizard_Record;
      Creation_Wizard.Initialize (Wiz, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Wiz : out Prj_Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Page : Project_Editor_Page;
      Count : constant Natural := Project_Editor_Pages_Count (Kernel);
   begin
      Wiz.Kernel := Kernel_Handle (Kernel);
      Wizards.Initialize
        (Wiz, Kernel, -"Project setup", Num_Pages => 1 + Count);

      Set_Toc (Wiz, 1, -"Naming the project", -"Creating a new project");

      for E in 1 .. Count loop
         Page := Get_Nth_Project_Editor_Page (Kernel, E);
         Set_Toc (Wiz, 1 + E, Get_Toc (Page), Get_Title (Page));
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
      if Page_Num = 1 then
         W.Language_Changed := True;

         if Get_Nth_Page (W, 1) = null then
            Set_Page (W, 1, First_Page (W));
         end if;

      elsif Integer (Page_Num - 1) <=
        Project_Editor_Pages_Count (W.Kernel)
      then
         if Get_Nth_Page (W, Integer (Page_Num)) = null then
            Set_Page
              (W, Integer (Page_Num),
               Widget_Factory
               (Get_Nth_Project_Editor_Page (W.Kernel, Integer (Page_Num - 1)),
                No_Project,
                Name_As_Directory (Get_Text (W.Project_Location)) &
                Get_Text (W.Project_Name),
                W.Kernel));
         end if;

         declare
            Languages : Argument_List := Get_Languages (W);
         begin
            Refresh
              (Page         => Get_Nth_Project_Editor_Page
               (W.Kernel, Integer (Page_Num - 1)),
               Widget       => Get_Nth_Page (W, Integer (Page_Num)),
               Project_View => No_Project,
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
      W      : constant Prj_Wizard := Prj_Wizard (Wiz);
      Result : Message_Dialog_Buttons;
   begin
      if Get_Current_Page (W) = 1 then
         declare
            Project  : constant String := Get_Text (W.Project_Name);
            Prj_File : constant String := To_File_Name (Project);
            Location : constant String :=
              Dir_Name (Get_Text (W.Project_Location));

         begin
            if not Is_Valid_Project_Name (Project) then
               Result := Message_Dialog
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
              (Location & Prj_File & Prj.Project_File_Extension)
            then
               Result := Message_Dialog
                 (Msg => Location
                  & Prj_File & Prj.Project_File_Extension
                  & (-" already exists. Do you want to overwrite ?"),
                  Title => -"File exists",
                  Dialog_Type => Error,
                  Buttons => Button_Yes or Button_No);

               if Result = Button_No then
                  Gtk.Handlers.Emit_Stop_By_Name (Next_Button (W), "clicked");
               end if;
            end if;
         end;
      end if;
   end Page_Checker;

   ----------------
   -- First_Page --
   ----------------

   function First_Page
     (Wiz : access Prj_Wizard_Record'Class) return Gtk_Widget
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

      Gtk_New (Label, -"Enter the directory where to copy the file to:");
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

      Gtk_New (Frame, -"Programming Languages");
      Set_Border_Width (Frame, 5);
      Pack_Start (Page, Frame, Expand => False);

      Gtk_New_Vbox (Box);
      Set_Border_Width (Box, 5);
      Add (Frame, Box);

      Gtk_New (Wiz.Ada_Support, "Ada");
      Set_Active (Wiz.Ada_Support, True);
      Pack_Start (Box, Wiz.Ada_Support, Expand => False);

      Gtk_New (Wiz.C_Support, "C");
      Pack_Start (Box, Wiz.C_Support, Expand => False);

      Gtk_New (Wiz.Cpp_Support, "C++");
      Pack_Start (Box, Wiz.Cpp_Support, Expand => False);

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

      return Gtk_Widget (Page);
   end First_Page;

   ---------------------------
   -- Advanced_Prj_Location --
   ---------------------------

   procedure Advanced_Prj_Location (W : access Gtk_Widget_Record'Class) is
      Name : constant String := Select_Directory
        (Title          => -"Select project file location",
         Base_Directory => Name_As_Directory
         (Get_Text (Prj_Wizard (W).Project_Location)),
         History        => Get_History (Prj_Wizard (W).Kernel));
   begin
      if Name /= "" then
         Set_Text (Prj_Wizard (W).Project_Location, Name);
      end if;
   end Advanced_Prj_Location;

   -------------------
   -- Get_Languages --
   -------------------

   function Get_Languages (Wiz : access Prj_Wizard_Record'Class)
      return Argument_List
   is
      Languages : Argument_List (1 .. 3);
      Current : Natural := Languages'First;
   begin
      if Get_Active (Wiz.C_Support) then
         Languages (Current) := new String'(C_String);
         Current := Current + 1;
      end if;

      if Get_Active (Wiz.Cpp_Support) then
         Languages (Current) := new String'(Cpp_String);
         Current := Current + 1;
      end if;

      if Get_Active (Wiz.Ada_Support) then
         Languages (Current) := new String'(Ada_String);
         Current := Current + 1;
      end if;

      return Languages (Languages'First .. Current - 1);
   end Get_Languages;

   ------------------
   -- Generate_Prj --
   ------------------

   function Generate_Prj (W : access Gtk_Widget_Record'Class) return String is
      Wiz            : constant Prj_Wizard := Prj_Wizard (W);
      Dir            : constant String := Name_As_Directory
        (Get_Text (Wiz.Project_Location));
      Name           : constant String := Get_Text (Wiz.Project_Name);
      Relative_Paths : constant Boolean := Get_Active (Wiz.Relative_Paths);
      Languages      : Argument_List := Get_Languages (Wiz);
      Project        : Project_Node_Id;

   begin
      Push_State (Wiz.Kernel, Processing);

      Project := Create_Project (Name => Name, Path => Dir);
      Set_Project_Uses_Relative_Paths (Wiz.Kernel, Project, Relative_Paths);

      Update_Attribute_Value_In_Scenario
        (Project            => Project,
         Scenario_Variables => (1 .. 0 => Empty_Node),
         Attribute_Name     => Get_String (Name_Languages),
         Values             => Languages);

      --  Mark the project as modified, otherwise it won't actually be saved
      --  in case we are overwritting an existing file.

      Set_Project_Modified (Wiz.Kernel, Project, True);
      Save_Single_Project (Wiz.Kernel, Project, Langs => Languages);
      Free (Languages);
      Pop_State (Wiz.Kernel);

      return Dir & To_File_Name (Name) & Project_File_Extension;
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
