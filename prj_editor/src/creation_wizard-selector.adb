-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004                            --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glide_Kernel;             use Glide_Kernel;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Paned;                use Gtk.Paned;
with Glib.Object;              use Glib.Object;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Box;                  use Gtk.Box;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;

with Ada.Exceptions;           use Ada.Exceptions;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Glide_Intl;               use Glide_Intl;
with Creation_Wizard.Full;     use Creation_Wizard.Full;
with Creation_Wizard.Adp;      use Creation_Wizard.Adp;
with Creation_Wizard.Simple;   use Creation_Wizard.Simple;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Traces;                   use Traces;
with Wizards;                  use Wizards;
with Creation_Wizard;          use Creation_Wizard;

package body Creation_Wizard.Selector is

   From_Sources_Label : constant String := "From existing Ada sources";
   From_Scratch_Label : constant String := "From scratch";
   From_Adp_Label     : constant String := "From .adp file";

   function Create_Tree_View
     (Column_Types   : Glib.GType_Array;
      Column_Names   : GNAT.OS_Lib.String_List;
      Show_Column_Titles : Boolean := True;
      Selection_Mode : Gtk.Enums.Gtk_Selection_Mode :=
        Gtk.Enums.Selection_Single)
      return Gtk.Tree_View.Gtk_Tree_View;
   --  Create a new simple tree view, where each column in the view is
   --  associated with a column in the model.
   --  Column_Names'Length is the number of columns in the view. If there are
   --  less columns in the view than in the model, then the matching columns
   --  in the model will not be visible on screen. There can't be more columns
   --  in the view than in the model, extra columns will simply be ignored.
   --  The caller is responsible for freeing Column_Names.
   --
   --  Columns associated with a boolean value will be rendered as a toggle
   --  button.
   --
   --  Limitations:
   --     Columns are not editable. Radio buttons not supported,
   --     Columns are not sortable

   type Wizard_Selector_Page is new Project_Wizard_Page_Record with record
      View          : Gtk_Tree_View;
      Description   : Gtk_Text_View;
      Last_Selected : Integer := -1;
      Name_And_Loc  : Name_And_Location_Page_Access;
   end record;
   type Wizard_Selector_Page_Access is access all Wizard_Selector_Page'Class;
   function Create_Content
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   procedure Generate_Project
     (Page    : access Wizard_Selector_Page;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project : in out Projects.Project_Type;
      Changed : in out Boolean);
   function Next_Page
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Wizard_Page;
   --  See inherited documentation

   procedure Selection_Changed
     (Selection : access Gtk_Widget_Record'Class;
      Page      : Project_Wizard_Page);
   --  Called when a new type of project is selected

   ----------------------
   -- Create_Tree_View --
   ----------------------

   function Create_Tree_View
     (Column_Types   : Glib.GType_Array;
      Column_Names   : GNAT.OS_Lib.String_List;
      Show_Column_Titles : Boolean := True;
      Selection_Mode : Gtk.Enums.Gtk_Selection_Mode :=
        Gtk.Enums.Selection_Single)
      return Gtk.Tree_View.Gtk_Tree_View
   is
      View            : Gtk_Tree_View;
      Col             : Gtk_Tree_View_Column;
      Col_Number      : Gint;
      Model           : Gtk_Tree_Store;
      Text_Render     : Gtk_Cell_Renderer_Text;
      Toggle_Render   : Gtk_Cell_Renderer_Toggle;
      pragma Unreferenced (Col_Number);
   begin
      Gtk_New (Model, Column_Types);
      Gtk_New (View, Model);
      Set_Mode (Get_Selection (View), Selection_Mode);
      Set_Headers_Visible (View, Show_Column_Titles);

      for N in 0
        .. Integer'Min (Column_Names'Length, Column_Types'Length) - 1
      loop
         Gtk_New           (Col);
         Set_Resizable     (Col, True);
         Set_Reorderable   (Col, True);

         Col_Number := Append_Column (View, Col);
         Set_Title (Col, Column_Names (Column_Names'First + N).all);

         case Column_Types (Column_Types'First + Guint (N)) is
            when GType_Boolean =>
               if Toggle_Render = null then
                  Gtk_New (Toggle_Render);
                  Set_Radio (Toggle_Render, False);
               end if;
               Pack_Start (Col, Toggle_Render, False);
               Add_Attribute (Col, Toggle_Render, "active", Gint (N));

            when GType_String =>
               if Text_Render = null then
                  Gtk_New (Text_Render);
               end if;
               Pack_Start (Col, Text_Render, False);
               Add_Attribute (Col, Text_Render, "text", Gint (N));

            when others =>
               raise Program_Error;
         end case;

      end loop;
      return View;
   end Create_Tree_View;

   ---------------
   -- Next_Page --
   ---------------

   function Next_Page
     (Page : access Wizard_Selector_Page;
      Wiz  : access Wizard_Record'Class) return Wizard_Page
   is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Selected : Integer;
   begin
      Get_Selected (Get_Selection (Page.View), Model, Iter);

      declare
         Project_T : constant String :=
           Get_String (Gtk_Tree_Store (Model), Iter, 0);
      begin
         if Project_T = -From_Sources_Label then
            Selected := 1;
         elsif Project_T = -From_Adp_Label then
            Selected := 2;
         else
            Selected := 3;
         end if;
      end;

      if Page.Last_Selected /= Selected then
         Page.Last_Selected := Selected;
         Remove_Pages (Wiz, After => Page.Name_And_Loc);
         case Selected is
            when 1 => Add_Simple_Wizard_Pages (Project_Wizard (Wiz));
            when 2 => Add_Adp_Wizard_Pages (Project_Wizard (Wiz));
            when others => Add_Full_Wizard_Pages
                 (Project_Wizard (Wiz), Page.Name_And_Loc);
         end case;
      end if;

      return null;
   end Next_Page;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page    : access Wizard_Selector_Page;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project : in out Projects.Project_Type;
      Changed : in out Boolean)
   is
      pragma Unreferenced (Page, Kernel, Scenario_Variables, Project, Changed);
   begin
      null;
   end Generate_Project;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed
     (Selection : access Gtk_Widget_Record'Class;
      Page      : Project_Wizard_Page)
   is
      pragma Unreferenced (Selection);
      P              : constant Wizard_Selector_Page_Access :=
        Wizard_Selector_Page_Access (Page);
      Buffer         : constant Gtk_Text_Buffer := Get_Buffer (P.Description);
      Selected       : Gtk_Tree_Iter;
      Selected_Model : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (P.View), Selected_Model, Selected);
      Set_Text (Buffer,
                Get_String (Gtk_Tree_Store (Get_Model (P.View)),
                            Selected, 1));
   end Selection_Changed;

   ------------------------
   -- Create_New_Project --
   ------------------------

   function Create_New_Project
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) return Boolean
   is
      Wiz  : Project_Wizard;
      P    : Wizard_Selector_Page_Access;
   begin
      Gtk_New (Wiz, Kernel);
      P := new Wizard_Selector_Page;
      Add_Page (Wiz,
                Page        => P,
                Description => -"Select the type of project to create",
                Toc         => -"Select project type");

      P.Name_And_Loc := Add_Name_And_Location_Page (Wiz);

      declare
         Name : constant String := Run (Wiz);
      begin
         if Name /= "" then
            Load_Project (Kernel, Name);
            return True;
         end if;
      end;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
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
      Project_Type_Cst : aliased String := -"Project type";
      Model  : Gtk_Tree_Store;
      From_Sources_Iter   : Gtk_Tree_Iter;
      From_Scratch_Iter   : Gtk_Tree_Iter;
      From_Adp_Iter       : Gtk_Tree_Iter;
      Paned  : Gtk_Paned;
      Button : Gtk_Widget;
      Box    : Gtk_Box;
      pragma Unreferenced (Button);
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);

      Gtk_New_Hpaned (Paned);
      Pack_Start (Box, Paned);

      Page.View := Create_Tree_View
        (Column_Types  => (1 => GType_String, 2 => GType_String),
         Column_Names  => (1 => Project_Type_Cst'Unchecked_Access),
         Show_Column_Titles => False);
      Add1 (Paned, Page.View);
      Model := Gtk_Tree_Store (Get_Model (Page.View));

      Append (Model, From_Sources_Iter, Null_Iter);
      Set (Model, From_Sources_Iter, 0, -From_Sources_Label);
      Set (Model, From_Sources_Iter, 1,
           -("Create a new set of projects given a set of source directories"
             & " and a set of object directories. GPS will try to create"
             & " projects so as to be able to get the same location for"
             & " object files when your application is build using project"
             & " files as it was when you build it previously."));

      Append (Model, From_Scratch_Iter, Null_Iter);
      Set (Model, From_Scratch_Iter, 0, -From_Scratch_Label);
      Set (Model, From_Scratch_Iter, 1,
           -("Create a new project file, where you can specify each of its"
             & " properties, like the set of source directories, its object"
             & " directory, compiler switches,..."));

      Append (Model, From_Adp_Iter, Null_Iter);
      Set (Model, From_Adp_Iter, 0, -From_Adp_Label);
      Set (Model, From_Adp_Iter, 1,
           -(".adp files are the project files used in the AdaCore's Glide"
             & " environment, based on Emacs. It is a very simple project."
             & ASCII.LF
             & "This wizard will allow you to easily convert such a file to"
             & " GPS's own format"));

      Gtk_New (Page.Description);
      Add2 (Paned, Page.Description);
      Set_Cursor_Visible (Page.Description, False);
      Set_Wrap_Mode (Page.Description, Wrap_Word);
      Set_Editable (Page.Description, False);

      Page_Handlers.Object_Connect
        (Get_Selection (Page.View), "changed",
         Page_Handlers.To_Marshaller (Selection_Changed'Access),
         Slot_Object => Page.View,
         User_Data => Project_Wizard_Page (Page));
      Select_Iter (Get_Selection (Page.View), From_Scratch_Iter);

      return Gtk_Widget (Box);
   end Create_Content;

   --------------------
   -- On_New_Project --
   --------------------

   procedure On_New_Project
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : Glide_Kernel.Kernel_Handle)
   is
      Tmp : Boolean;
      pragma Unreferenced (Widget, Tmp);
   begin
      Tmp := Create_New_Project (Kernel);
   end On_New_Project;

end Creation_Wizard.Selector;
