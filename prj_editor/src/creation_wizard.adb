-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
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

with Gdk.Bitmap;            use Gdk.Bitmap;
with Gdk.Color;             use Gdk.Color;
with Gdk.Pixmap;            use Gdk.Pixmap;
with Glib;                  use Glib;
with Gtk.Arguments;         use Gtk.Arguments;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Label;             use Gtk.Label;
with Gtk.Menu;              use Gtk.Menu;
with Gtk.Table;             use Gtk.Table;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with Gtkada.Handlers;       use Gtkada.Handlers;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Text_IO;               use Ada.Text_IO;

with Prj.PP;   use Prj.PP;
with Prj.Tree; use Prj.Tree;
with Prj;      use Prj;
with Snames;   use Snames;
with Namet;    use Namet;

with Wizards;          use Wizards;
with Directory_Tree;   use Directory_Tree;
with Switches_Editors; use Switches_Editors;
with Naming_Editors;   use Naming_Editors;
with Prj_API;          use Prj_API;
with Pixmaps_Prj;      use Pixmaps_Prj;
with Glide_Kernel;     use Glide_Kernel;
with Glide_Intl;       use Glide_Intl;

package body Creation_Wizard is

   function First_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget;
   function Second_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget;
   function Third_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget;
   function Fourth_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget;
   function Fifth_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget;
   --  function Sixth_Page (Wiz : access Prj_Wizard_Record'Class)
   --     return Gtk_Widget;
   --  Return the widget to use for any of the pages in the wizard

   procedure First_Page_Checker (Wiz : access Gtk_Widget_Record'Class);
   --  Checks whether the contents of the first page has been fully answered,
   --  and activate (or not) the next button.

   procedure Advanced_Prj_Location (W : access Gtk_Widget_Record'Class);
   --  Open up a dialog to select the project location.

   function Directory_Name (File_Name : String) return String;
   --  Return the directory name for File_Name (always ends with a directory
   --  separator).

   function Generate_Prj (W : access Gtk_Widget_Record'Class) return String;
   --  Generate the project files from the contents of the wizard W.
   --  Return the directory/name of the project that was just created.

   procedure Emit_Switches
     (Wiz : access Prj_Wizard_Record'Class;
      Project : Project_Node_Id;
      Name : String;
      Tool : Tool_Names);
   --  Create a new variable in a package called Name to represent the default
   --  switches to use for this tool

   procedure Switch_Page
     (Wiz : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when a new page is selected in the wizard. We dynamically create
   --  the page if needed.

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
      Pix  : Gdk_Pixmap;
      Mask : Gdk_Bitmap;
   begin
      Wiz.Kernel := Kernel_Handle (Kernel);
      Wizards.Initialize
        (Wiz, Kernel, -"Project setup", "#0e79bd", Num_Pages => 5);

      Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, logo_xpm);
      Add_Logo (Wiz, Pix, Mask);

      Set_Toc (Wiz, 1, -"Naming the project");
      Set_Toc (Wiz, 2, -"Selecting sources");
      Set_Toc (Wiz, 3, -"Build directory");
      Set_Toc (Wiz, 4, -"Switches");
      Set_Toc (Wiz, 5, -"Naming scheme");

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
      case Page_Num is
         when 1 =>
            Set_Wizard_Title (W, -"Creating a new project");
            W.Language_Changed := True;

            if Get_Nth_Page (W, 1) = null then
               Set_Page (W, 1, First_Page (W));
            end if;

         when 2 =>
            Set_Wizard_Title
              (W, -"Please select the source directories for this project");

            if Get_Nth_Page (W, 2) = null then
               Set_Page (W, 2, Second_Page (W));
            end if;

         when 3 =>
            Set_Wizard_Title
              (W, -"Please select the build directory for this project");

            if Get_Nth_Page (W, 3) = null then
               Set_Page (W, 3, Third_Page (W));
            end if;

         when 4 =>
            Set_Wizard_Title
              (W, -"Please select the switches to build the project");

            if W.Language_Changed or else Get_Nth_Page (W, 4) = null then
               Set_Page (W, 4, Fourth_Page (W));
            end if;

         when 5 =>
            Set_Wizard_Title (W, -"Please select the naming scheme to use");

            if W.Language_Changed or else Get_Nth_Page (W, 5) = null then
               Set_Page (W, 5, Fifth_Page (W));
               W.Language_Changed := False;
            end if;

         when others =>
            null;
      end case;
   end Switch_Page;

   ------------------------
   -- First_Page_Checker --
   ------------------------

   procedure First_Page_Checker (Wiz : access Gtk_Widget_Record'Class) is
      W : constant Prj_Wizard := Prj_Wizard (Wiz);
   begin
      Set_Sensitive (Next_Button (W), Get_Text (W.Project_Name)'Length /= 0);
   end First_Page_Checker;

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

      Gtk_New (Frame, -"Name and Location");
      Set_Border_Width (Frame, 5);
      Pack_Start (Page, Frame, Expand => False);

      Gtk_New (Table, Rows => 4, Columns => 2, Homogeneous => False);
      Add (Frame, Table);

      Gtk_New (Label, -"Enter the name of the project to create:");
      Attach (Table, Label, 0, 2, 0, 1);

      Gtk_New (Wiz.Project_Name, 255);
      Attach (Table, Wiz.Project_Name, 0, 1, 1, 2);

      --  We can't move to the next page until the name of the project has been
      --  specified

      Set_Sensitive (Next_Button (Wiz), False);

      Widget_Callback.Object_Connect
        (Wiz.Project_Name, "changed",
         Widget_Callback.To_Marshaller (First_Page_Checker'Access), Wiz);

      Set_Row_Spacing (Table, 1, 20);

      Gtk_New (Label, -"Enter the directory where to copy the file to:");
      Attach (Table, Label, 0, 2, 2, 3);

      Gtk_New (Wiz.Project_Location, 255);
      Set_Text (Wiz.Project_Location, Get_Current_Dir);
      Attach (Table, Wiz.Project_Location, 0, 1, 3, 4);

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

      return Gtk_Widget (Page);
   end First_Page;

   -----------------
   -- Second_Page --
   -----------------

   function Second_Page
     (Wiz : access Prj_Wizard_Record'Class) return Gtk_Widget is
   begin
      Gtk_New (Wiz.Src_Dir_Selection,
               Initial_Directory => Get_Current_Dir,
               Multiple_Directories => True,
               Busy_Cursor_On => Get_Window (Wiz));
      return Gtk_Widget (Wiz.Src_Dir_Selection);
   end Second_Page;

   ----------------
   -- Third_Page --
   ----------------

   function Third_Page
     (Wiz : access Prj_Wizard_Record'Class) return Gtk_Widget is
   begin
      Gtk_New (Wiz.Obj_Dir_Selection,
               Initial_Directory => Get_Current_Dir,
               Multiple_Directories => False,
               Busy_Cursor_On => Get_Window (Wiz));
      return Gtk_Widget (Wiz.Obj_Dir_Selection);
   end Third_Page;

   -----------------
   -- Fourth_Page --
   -----------------

   function Fourth_Page
     (Wiz : access Prj_Wizard_Record'Class) return Gtk_Widget is
   begin
      Gtk_New (Wiz.Switches);

      if not Get_Active (Wiz.Ada_Support) then
         Destroy_Pages (Wiz.Switches, Ada_Page or Binder_Page);
      end if;

      if not Get_Active (Wiz.C_Support) then
         Destroy_Pages (Wiz.Switches, C_Page);
      end if;

      if not Get_Active (Wiz.Cpp_Support) then
         Destroy_Pages (Wiz.Switches, Cpp_Page);
      end if;

      return Get_Window (Wiz.Switches);
   end Fourth_Page;

   ----------------
   -- Fifth_Page --
   ----------------

   function Fifth_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget is
   begin
      Gtk_New (Wiz.Naming);
      return Get_Window (Wiz.Naming);
   end Fifth_Page;

   --------------------
   -- Directory_Name --
   --------------------

   function Directory_Name (File_Name : String) return String is
   begin
      for J in reverse File_Name'Range loop
         if File_Name (J) = GNAT.OS_Lib.Directory_Separator
           or else File_Name (J) = '/'
         then
            return File_Name (File_Name'First .. J);
         end if;
      end loop;
      return "";
   end Directory_Name;

   ---------------------------
   -- Advanced_Prj_Location --
   ---------------------------

   procedure Advanced_Prj_Location (W : access Gtk_Widget_Record'Class) is
      Name : constant String := File_Selection_Dialog
         (-"Select project file location", Dir_Only => True);
   begin
      if Name /= "" then
         Set_Text (Prj_Wizard (W).Project_Location, Name);
      end if;
   end Advanced_Prj_Location;

   -------------------
   -- Emit_Switches --
   -------------------

   procedure Emit_Switches
     (Wiz : access Prj_Wizard_Record'Class;
      Project : Project_Node_Id;
      Name : String;
      Tool : Tool_Names)
   is
      Pack, Var : Project_Node_Id;
      Arr : Argument_List := Get_Switches (Wiz.Switches, Tool);
   begin
      if Arr'Length /= 0 then
         Pack := Get_Or_Create_Package (Project, Name);
         Var := Create_Attribute
           (Pack, Get_Name_String (Name_Default_Switches), Ada_String, List);
         for J in Arr'Range loop
            Append_To_List (Var, Arr (J).all);
         end loop;
         Free (Arr);
      end if;
   end Emit_Switches;

   ------------------
   -- Generate_Prj --
   ------------------

   function Generate_Prj (W : access Gtk_Widget_Record'Class) return String is
      Wiz  : Prj_Wizard := Prj_Wizard (W);
      Project, Var : Project_Node_Id;
      File : File_Type;
      Dir : constant String := Get_Text (Wiz.Project_Location);
      Name : constant String := Get_Text (Wiz.Project_Name);

      procedure Write_Char (C : Character);
      procedure Write_Str  (S : String);
      --  Required functions to instanciate Pretty_Print

      ----------------
      -- Write_Char --
      ----------------

      procedure Write_Char (C : Character) is
      begin
         Put (File, C);
      end Write_Char;

      ---------------
      -- Write_Str --
      ---------------

      procedure Write_Str  (S : String) is
      begin
         Put (File, S);
      end Write_Str;

   begin
      --  ??? Shouldn'T use Hard-Coded Strings in here

      Project := Create_Project (Name => Name, Path => Dir);

      --  Append the source directories
      Var := Create_Attribute
        (Project, Get_Name_String (Name_Source_Dirs), Kind => List);
      declare
         Dirs : Argument_List := Get_Multiple_Selection
           (Wiz.Src_Dir_Selection);
      begin
         if Dirs'Length /= 0 then
            for J in Dirs'Range loop
               Append_To_List (Var, Dirs (J).all);
            end loop;
            Free (Dirs);
         else
            Append_To_List (Var, ".");
         end if;
      end;

      --  Append the build directory
      Var := Create_Attribute
        (Project, Get_Name_String (Name_Object_Dir), Kind => Single);
      declare
         Dir : constant String :=
           Get_Single_Selection (Wiz.Obj_Dir_Selection);
      begin
         if Dir = "" then
            Set_Value (Var, ".");
         else
            Set_Value (Var, Dir);
         end if;
      end;

      --  Append the switches
      Emit_Switches (Wiz, Project, Get_Name_String (Name_Builder), Gnatmake);

      if Get_Active (Wiz.Ada_Support) then
         Emit_Switches
           (Wiz, Project, Get_Name_String (Name_Compiler), Ada_Compiler);
      end if;

      Emit_Switches (Wiz, Project, Get_Name_String (Name_Binder), Binder);
      Emit_Switches (Wiz, Project, Get_Name_String (Name_Linker), Linker);

      --  Append the naming scheme
      Create_Project_Entry (Wiz.Naming, Project);

      if Dir (Dir'Last) = Directory_Separator then
         Create (File, Out_File, Dir & Name & Project_File_Extension);
      else
         Create (File, Out_File, Dir & Directory_Separator & Name
                 & Project_File_Extension);
      end if;

      Pretty_Print
        (Project, 3, True,
         Write_Char'Unrestricted_Access, Write_Str'Unrestricted_Access);
      Close (File);

      if Dir (Dir'Last) = Directory_Separator then
         return Dir & Name & Project_File_Extension;
      else
         return Dir & Directory_Separator & Name & Project_File_Extension;
      end if;
   end Generate_Prj;

   ---------
   -- Run --
   ---------

   function Run (Wiz : access Prj_Wizard_Record) return String is
   begin
      Show_All (Wiz);
      Set_Current_Page (Wiz, 1);

      if Run (Wiz) = Gtk_Response_Apply then
         return Generate_Prj (Wiz);
      else
         return "";
      end if;
   end Run;

end Creation_Wizard;
