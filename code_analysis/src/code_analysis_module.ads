-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

--  <description>
--  This package defines the module for code analysis storage structure
--  It defines shell commands that allow to create graphical interfaces for
--  coverage informations inside GPS.
--  </description>

with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Ordered_Sets; use Ada.Containers;
with Ada.Calendar;         use Ada.Calendar;
with GNAT.Strings;

with Glib;
with Glib.Object;

with Gdk.Pixbuf;           use Gdk.Pixbuf;
with Gdk.Event;            use Gdk.Event;

with Gtk.Box;              use Gtk.Box;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Handlers;         use Gtk.Handlers;

with GPS.Kernel;           use GPS.Kernel;
with GPS.Kernel.Modules;   use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;   use GPS.Kernel.Scripts;
with GPS.Kernel.MDI;       use GPS.Kernel.MDI;
with GPS.Intl;             use GPS.Intl;

with VFS;                  use VFS;
with Traces;               use Traces;
with Code_Analysis;        use Code_Analysis;

package Code_Analysis_Module is

   ---------------
   -- Instances --
   ---------------

   function Less_Case_Insensitive
     (Left, Right : Class_Instance) return Boolean;
   function Equal_Case_Insensitive
     (Left, Right : Class_Instance) return Boolean;
   --  Use the Code_Analysis user property "Instance_Name" to perform the test

   package Code_Analysis_Class_Instance_Sets is new Indefinite_Ordered_Sets
     (Element_Type => Class_Instance,
      "<" => Less_Case_Insensitive,
      "=" => Equal_Case_Insensitive);
   --  Sets package for declared instances of the CodeAnalysis module.
   --  Allow to handle many instances.

   ------------------------
   -- Basic module stuff --
   ------------------------

   type Code_Analysis_Module_ID_Record is new Module_ID_Record with record
      Kernel         : Kernel_Handle;
      Class          : Class_Type;
      Instances      : Code_Analysis_Class_Instance_Sets.Set;
      Project_Pixbuf : Gdk_Pixbuf;
      File_Pixbuf    : Gdk_Pixbuf;
      Subp_Pixbuf    : Gdk_Pixbuf;
      Warn_Pixbuf    : Gdk_Pixbuf;
   end record;

   type Code_Analysis_Module_ID_Access is access all
     Code_Analysis_Module_ID_Record'Class;

   Code_Analysis_Module_ID : Code_Analysis_Module_ID_Access;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

private

   ---------------
   -- Tree view --
   ---------------

   type Code_Analysis_View_Record is new Gtk_Hbox_Record with record
      Tree        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
      Node_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cov_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cov_Percent : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Projects    : Code_Analysis_Tree;
   end record;

   type Code_Analysis_View is access all Code_Analysis_View_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Code_Analysis_View_Record, Code_Analysis_View);

   --------------
   -- Property --
   --------------

   type Code_Analysis_Property_Record is new Instance_Property_Record
   with record
      Projects      : Code_Analysis_Tree;
      View          : Code_Analysis_View;
      Child         : GPS_MDI_Child;
      Instance_Name : GNAT.Strings.String_Access;
      Date          : Time;
   end record;

   procedure Show_Analysis_Report
     (Instance : Class_Instance;
      Property : in out Code_Analysis_Property_Record;
      Context  : Selection_Context := No_Context);
   --  Call to build the tree view report and then populate it and expand the
   --  the appropriate item and finally insert it in the MDI. Should be called
   --  by Show_Analysis_Report_From_Context and Show_Analysis_Report_From_Shell

   procedure Build_Analysis_Report
     (Instance : Class_Instance;
      Property : in out Code_Analysis_Property_Record);
   --  Actually builds the tree view report. Should be called by
   --  Show_Analysis_Report or Show_Empty_Analysis_Report_From_Menu

   type Code_Analysis_Property is access all Code_Analysis_Property_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Code_Analysis_Property_Record, Code_Analysis_Property);

   ---------------------
   -- Contextual menu --
   ---------------------

   type Code_Analysis_Contextual_Menu is new Submenu_Factory_Record with null
     record;

   type Code_Analysis_Contextual_Menu_Access is access all
     Code_Analysis_Contextual_Menu;

   type Has_Coverage_Filter is new Action_Filter_Record with null record;

   function Filter_Matches_Primitive
     (Filter  : access Has_Coverage_Filter;
      Context : Selection_Context) return Boolean;
   --  True when the current context is associated with project, file or
   --  subprogram information

   procedure Append_To_Menu
     (Factory : access Code_Analysis_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Determines wether we add entries directly in the contextual menu, or in
   --  a generated submenu. Submenus are created if many instances are loaded

   type Context_And_Instance is record
      Context  : Selection_Context;
      Instance : Class_Instance;
   end record;

   package Context_And_Instance_CB is new User_Callback
     (Glib.Object.GObject_Record, Context_And_Instance);
   --  Used to connect handlers on the global Coverage contextual menu

   procedure Append_To_Contextual_Submenu
     (Cont_N_Inst  : Context_And_Instance;
      Submenu      : access Gtk_Menu_Record'Class;
      Project_Node : Project_Access);
   --  Allows to fill the given Submenu, in the appropriate order for
   --  contextual menu

   procedure Append_To_Submenu
     (Cont_N_Inst  : Context_And_Instance;
      Submenu      : access Gtk_Menu_Record'Class;
      Project_Node : Project_Access);
   --  Allows to fill the given Submenu, in the appropriate order for Tools
   --  menu

   procedure Append_Show_Analysis_Report_To_Menu
     (Cont_N_Inst : Context_And_Instance;
      Menu        : access Gtk_Menu_Record'Class);
   --  Actually fills the given Menu, with the "Show Analysis Report" entry
   --  With no context information, so the 1st node will be expanded

   procedure Append_Show_Empty_Analysis_Report
     (Cont_N_Inst : Context_And_Instance;
      Menu        : access Gtk_Menu_Record'Class);
   --  Actually fills the given Menu with the "Show Analysis Report" entry
   --  which deals with empty Instances (when there is no code_analysis entity
   --  present) by displaying an error message

   procedure Append_File_Menu_Entries
     (Cont_N_Inst   : Context_And_Instance;
      Submenu       : access Gtk_Menu_Record'Class;
      File_Node     : File_Access);
   --  Actually fills the given Submenu with the appropriate coverage action
   --  entries to handle files (Add/Remove coverage annotations,
   --  Show coverage report, ...)

   procedure Append_Project_Menu_Entries
     (Cont_N_Inst   : Context_And_Instance;
      Submenu       : access Gtk_Menu_Record'Class;
      Project_Node  : Project_Access;
      Is_Contextual : Boolean := True);
   --  Actually fills the given Submenu with the appropriate coverage action
   --  entries to handle projects (Add/Remove coverage annotations,
   --  Show coverage report, Load full data...)

   function Check_Context
     (Given_Context : Selection_Context) return Selection_Context;
   --  Check and correct the presence of project information in the
   --  Given_Context

   procedure Dynamic_Tools_Menu_Factory
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Determines wether we add entries directly in the "Tools/Coverage" menu,
   --  or in a generated submenu. Submenus are created if many instances are
   --  loaded

   procedure Dynamic_Views_Menu_Factory
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Determines wether we add entries directly in the "Tools/Views/Coverage"
   --  menu, or in a generated submenu. Submenus are created if many instances
   --  are loaded

   procedure Show_Empty_Analysis_Report_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Inst : Context_And_Instance);
   --  Call to build the tree view report and then put inside it an error
   --  message

   Code_Analysis_Cst_Str : constant String := "CodeAnalysis";
   Coverage_Category     : constant Glib.UTF8_String := -"Lines not covered";
   Me : constant Debug_Handle := Create (Code_Analysis_Cst_Str);

   --------------------
   -- Shell commands --
   --------------------

   procedure Create
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Create a new instance of Code_Analysis data structure from shell cmd

   procedure Create_From_Menu
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Create a new instance of Code_Analysis data structure from
   --  "Tools/Coverage" menu

   function Create_Instance return Class_Instance;
   --  Create a new instance of Code_Analysis data structure
   --  This sets its property with a date and a name
   --  Then, the instance is inserted in the Instances set of the Code_Analysis
   --  Module_ID

   procedure Add_Gcov_File_Info_From_Context
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Inst : Context_And_Instance);
   --  Looks for a corresponding GCOV file, and adds its node and coverage info
   --  into the current code_analysis structure

   procedure Add_Gcov_File_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add node and coverage info provided by a gcov file parsing

   procedure Add_Gcov_File_Info
     (Src_File     : VFS.Virtual_File;
      Cov_File     : VFS.Virtual_File;
      Project_Node : Project_Access);
   --  Actually adds the node and coverage info provided by the gcov file
   --  parsing. Should be called by Add_Gcov_File_Info_From_Shell or
   --  Add_Gcov_File_Info_From_Context

   procedure Add_Gcov_Project_Info_From_Context
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Inst : Context_And_Instance);
   --  Basically do an Add_Gcov_File_Info on every files of the project in
   --  Context

   procedure Add_Gcov_Project_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Basically do an Add_Gcov_File_Info on every files of the given project

   procedure Add_Gcov_Project_Info (Prj_Node : Project_Access);
   --  Actually look for project source files, and corresponding .gcov files
   --  And when its possible, add every file coverage information to the
   --  Code_Analysis structure of given Project_Node

   procedure List_Lines_Not_Covered_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add in the location view every not covered lines of any projects loaded
   --  in the Code_Analysis structure of the current Instance.

   procedure Show_Analysis_Report_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Create and display a Code_Analysis tree view within a MDI Child

   procedure Destroy
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Free memory used by a Code_Analysis instance

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Destroy (Widget      : access Glib.Object.GObject_Record'Class;
                         Cont_N_Inst : Context_And_Instance);
   --  Callback for the "destroy" signal that cleanly destroy the widget

   function On_Double_Click (View  : access Gtk_Widget_Record'Class;
                             Event : Gdk_Event) return Boolean;
   --  Callback for the "2button_press" signal that show the File or Subprogram
   --  indicated by the selected Report of Analysis tree node

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Determines the content of the contextual menu displayed on the Report of
   --  Analysis MDI child, using its selected node and sets project and file
   --  information to the given context

   procedure Open_File_Editor_On_File
     (Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter);
   --  Opens a file editor on the source file pointed out by Iter in Model

   procedure Open_File_Editor_On_Subprogram
     (Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter);
   --  Opens a file editor on the source file containing the Subprogram
   --  pointed out by Iter in Model

   procedure Add_Coverage_Annotations_From_Report
     (Object : access Gtk_Widget_Record'Class);
   --  Callback attached to the "View with coverage annotations" contextual
   --  menu entry of a File or Subprogram nodes in a Coverage Report

   procedure Add_Coverage_Annotations_From_Context
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Inst : Context_And_Instance);
   --  Add coverage annotations to source file editor
   --  Callback to the global contextual menu entry
   --  "View with coverage annotations"

   procedure Add_Coverage_Annotations
     (File_Node : Code_Analysis.File_Access);
   --  Actually add the annotation columns

   procedure Remove_Coverage_Annotations_From_Report
     (Object : access Gtk_Widget_Record'Class);
   --  Callback attached to the "Remove coverage annotations" contextual
   --  menu entry of a File or Subprogram nodes in a Coverage Report

   procedure Remove_Coverage_Annotations_From_Context
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Inst : Context_And_Instance);
   --  Callback attached to the "Remove coverage annotations" contextual
   --  menu entry of every objects that have File or Subprogram information in
   --  its context

   procedure Remove_Coverage_Annotations
     (File_Node : Code_Analysis.File_Access);
   --  Actually removes coverage annotations of src_editors of file represented
   --  by File_Node

   procedure List_Lines_Not_Covered_In_Project_From_Report
     (Object : access Gtk_Widget_Record'Class);
   --  Callback of contextual menu for Project rows in a Coverage Report.
   --  Add to the location view the unexecuted lines of all files of the
   --  selected Project.

   procedure List_Lines_Not_Covered_In_Project_From_Context
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Inst : Context_And_Instance);
   --  Callback of the "List lines not covered" entry of the global "Coverage"
   --  submenu when the Context only contains Project information.
   --  Just call the subprogram List_Lines_Not_Covered_In_File

   procedure List_Lines_Not_Covered_In_File_From_Report
     (Object : access Gtk_Widget_Record'Class);
   --  Callback of contextual menu for File rows in a Coverage Report. Opens
   --  the select File in a source editor and call
   --  List_Lines_Not_Covered_In_File.

   procedure List_Lines_Not_Covered_In_File_From_Context
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Inst : Context_And_Instance);
   --  Callback of the "List lines not covered" entry of the global "Coverage"
   --  submenu when the Context contains file information.
   --  Just call the List_Lines_Not_Covered_In_File subprogram

   procedure List_Lines_Not_Covered_In_Project
     (Project_Node : Project_Access);
   --  Add to the location view the unexecuted lines of the given Project of a
   --  Coverage Report.

   procedure List_Lines_Not_Covered_In_File
     (File_Node : Code_Analysis.File_Access);
   --  Add to the location view the unexecuted lines of the given File of a
   --  Coverage Report.

   procedure Expand_All_From_Report (Object : access Gtk_Widget_Record'Class);
   --  Expand the whole tree vien in a code_analysis report

   procedure Collapse_All_From_Report
     (Object : access Gtk_Widget_Record'Class);
   --  Collapse the whole tree vien in a code_analysis report

   procedure Show_Full_Tree (Object : access Gtk_Widget_Record'Class);
   --  Fill again the Gtk_Tree_Store with the full tree

   procedure Show_Flat_List_Of_Files (Object : access Gtk_Widget_Record'Class);
   --  Fill the Gtk_Tree_Store with only on level of file

   procedure Show_Flat_List_Of_Subprograms
     (Object : access Gtk_Widget_Record'Class);
   --  Fill the Gtk_Tree_Store with only on level of subprograms

   procedure Show_Analysis_Report_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Inst : Context_And_Instance);
   --  Menu callback that calls Show_Analysis_Report with no context info

end Code_Analysis_Module;
