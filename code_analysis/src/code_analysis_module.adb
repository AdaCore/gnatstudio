-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2007, AdaCore                 --
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

with Ada.Calendar;                           use Ada.Calendar;
with Ada.Containers.Indefinite_Ordered_Sets; use Ada.Containers;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;
with Basic_Types;                            use Basic_Types;
with Code_Analysis;                          use Code_Analysis;
with Code_Analysis_Tree_Model;               use Code_Analysis_Tree_Model;
with Code_Coverage;                          use Code_Coverage;
with Entities;                               use Entities;
with GNAT.OS_Lib;                            use GNAT.OS_Lib;
with GNAT.Scripts;                           use GNAT.Scripts;
with GNAT.Strings;
with GNAT.Traces;
with GPS.Intl;                               use GPS.Intl;
with GPS.Kernel;                             use GPS.Kernel;
with GPS.Kernel.Console;
with GPS.Kernel.Contexts;                    use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;                     use GPS.Kernel.Modules;
with GPS.Kernel.Project;                     use GPS.Kernel.Project;
with GPS.Kernel.Scripts;                     use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;              use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;                      use GPS.Kernel.Styles;
with GPS.Location_View;                      use GPS.Location_View;
with Gdk.Event;                              use Gdk.Event;
with Gdk.Pixbuf;                             use Gdk.Pixbuf;
with Glib.Object;
with Glib.Properties;
with Glib;                                   use Glib;
with Gtk.Box;                                use Gtk.Box;
with Gtk.Button;                             use Gtk.Button;
with Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Pixbuf;               use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Progress;             use Gtk.Cell_Renderer_Progress;
with Gtk.Cell_Renderer_Text;                 use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                              use Gtk.Enums;
with Gtk.Handlers;                           use Gtk.Handlers;
with Gtk.Image;                              use Gtk.Image;
with Gtk.Label;                              use Gtk.Label;
with Gtk.Menu;                               use Gtk.Menu;
with Gtk.Menu_Item;                          use Gtk.Menu_Item;
with Gtk.Object;                             use Gtk.Object;
with Gtk.Scrolled_Window;                    use Gtk.Scrolled_Window;
with Gtk.Stock;                              use Gtk.Stock;
with Gtk.Tree_Model;                         use Gtk.Tree_Model;
with Gtk.Tree_Selection;                     use Gtk.Tree_Selection;
with Gtk.Tree_Store;                         use Gtk.Tree_Store;
with Gtk.Tree_View;                          use Gtk.Tree_View;
with Gtk.Tree_View_Column;                   use Gtk.Tree_View_Column;
with Gtk.Widget;                             use Gtk.Widget;
with Gtk.Window;                             use Gtk.Window;
with Gtkada.Dialogs;                         use Gtkada.Dialogs;
with Gtkada.Handlers;                        use Gtkada.Handlers;
with Gtkada.MDI;                             use Gtkada.MDI;
with Language.Tree.Database;
with Language.Tree;                          use Language.Tree;
with Language;                               use Language;
with Language_Handlers;                      use Language_Handlers;
with Projects.Registry;                      use Projects.Registry;
with Projects;                               use Projects;
with Traces;                                 use Traces;
with VFS;                                    use VFS;

package body Code_Analysis_Module is

   Src_File_Cst : aliased   constant String := "src";
   --  Constant String that represents the name of the source file parameter
   --  of the GPS.CodeAnalysis.add_gcov_file_info subprogram
   Cov_File_Cst : aliased   constant String := "cov";
   --  Constant String that represents the name of the .gcov file parameter
   --  of the GPS.CodeAnalysis.add_gcov_file_info subprogram
   Prj_File_Cst : aliased   constant String := "prj";
   --  Constant String that represents the name of the .gpr file parameter
   --  of the GPS.CodeAnalysis.add_gcov_project_info subprogram
   Ana_Name_Cst : aliased   constant String := "name";
   --  Constant String that represents a name of Analysis_Instance in parameter
   --  of the GPS.CodeAnalysis.get_analysis command
   Gcov_Extension_Cst :     constant String := ".gcov";
   --  Constant String that represents the extension of GCOV files
   Progress_Bar_Width_Cst : constant Gint   := 150;
   --  Constant used to set the width of the progress bars of the analysis
   --  report

   Line_Info_Cst  : constant String := "Coverage Analysis";
   --  Name of the text info column used for the annotations
   Line_Icons_Cst : constant String := "Coverage Icons";
   --  Name of the pixmap column used for the annotations

   Prj_Pixbuf_Cst  : constant String := "gps-project-closed";
   --  Name of the pixbuf used for project node in the analysis report
   File_Pixbuf_Cst : constant String := "gps-file";
   --  Name of the pixbuf used for file node in the analysis report
   Subp_Pixbuf_Cst : constant String := "gps-entity-subprogram";
   --  Name of the pixbuf used for subprogram node in the analysis report

   Binary_Coverage_Trace : constant Debug_Handle :=
                             Create ("BINARY_COVERAGE_MODE", GNAT.Traces.On);
   Binary_Coverage_Mode  : Boolean;
   --  Boolean that allows to determine wether we are in binary coverage mode
   --  or not.

   Code_Analysis_Cst_Str : constant String := "CodeAnalysis";
   Coverage_Category     : constant Glib.UTF8_String := -"Uncovered lines";

   ---------------
   -- Tree view --
   ---------------

   type Code_Analysis_View_Record is new Gtk_Vbox_Record with record
      Tree        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
      Node_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cov_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cov_Percent : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Error_Box   : Gtk_Hbox;
      Icons       : Code_Analysis_Icons;
      Projects    : Code_Analysis_Tree;  -- Used by Show_Flat_List_* callbacks
   end record;

   type Code_Analysis_View is access all Code_Analysis_View_Record;

   package Kernel_Return_Cb is new User_Return_Callback
     (Gtk.Widget.Gtk_Widget_Record, Boolean, Kernel_Handle);

   ------------------------
   -- Analysis instances --
   ------------------------

   type Code_Analysis_Instance_Record is record
      Instances : GNAT.Scripts.Instance_List_Access;
      Projects  : Code_Analysis_Tree;
      View      : Code_Analysis_View;
      Child     : GPS_MDI_Child;
      Name      : GNAT.Strings.String_Access;
      Date      : Time;
   end record;

   type Code_Analysis_Instance is access Code_Analysis_Instance_Record;

   --------------
   -- Analyzes --
   --------------

   Miss_Analysis_Name : exception;

   function Less_Case_Insensitive
     (Left, Right : Code_Analysis_Instance) return Boolean;
   function Equal_Case_Insensitive
     (Left, Right : Code_Analysis_Instance) return Boolean;
   --  Use the Code_Analysis user property "Instance_Name" to perform the test

   package Code_Analysis_Instances is new Indefinite_Ordered_Sets
     (Element_Type => Code_Analysis_Instance,
      "<" => Less_Case_Insensitive,
      "=" => Equal_Case_Insensitive);
   --  Sets package for declared instances of the CodeAnalysis module.
   --  Allow to handle many instances.

   ------------------------
   -- Basic module stuff --
   ------------------------

   type Code_Analysis_Module_ID_Record is new Module_ID_Record with record
      Class    : Class_Type;
      Analyzes : Code_Analysis_Instances.Set;
      Count    : Integer := 0; -- To name the analysis instances
   end record;

   type Code_Analysis_Module_ID_Access is access all
     Code_Analysis_Module_ID_Record'Class;

   Code_Analysis_Module_ID : Code_Analysis_Module_ID_Access;

   type Code_Analysis_Property_Record is new Instance_Property_Record
   with record
      Analysis : Code_Analysis_Instance;
   end record;

   type Code_Analysis_Property is access all
     Code_Analysis_Property_Record'Class;

   type Context_And_Analysis is record
      Context  : Selection_Context;
      Analysis : Code_Analysis_Instance;
   end record;

   package Context_And_Analysis_CB is new User_Callback
     (Glib.Object.GObject_Record, Context_And_Analysis);
   --  Used to connect handlers on the global Coverage contextual menu

   function Get_Iter_From_Context
     (Context : Selection_Context;
      Model   : Gtk_Tree_Store) return Gtk_Tree_Iter;
   --  Return the Gtk_Tree_Iter of the context described entity in the Report
   --  of Analysis
   --  Return Null_Iter if Model is empty

   procedure Show_Empty_Analysis_Report
     (Kernel : Kernel_Handle; Analysis : Code_Analysis_Instance);
   --  Build an error stating Report of Analysis and insert it in the MDI

   procedure Show_Analysis_Report
     (Kernel       : Kernel_Handle;
      Cont_N_Anal  : Context_And_Analysis;
      Raise_Report : Boolean := True);
   --  Check if the context pointed project has data in the current
   --  code_analysis instance, if not, tries to find a project that has some
   --  and if it's not possible, show an error report
   --  Then Build the Report of Analysis N, populate it, expand the
   --  the appropriate item following given context information and finally
   --  insert the new Report in the MDI.
   --  If Raise_Report is set to True, the Report of Analysis N will be raised
   --  at the end of the treatment
   --  Should be called by :
   --   - Show_Analysis_Report_From_Menu
   --   - Show_Analysis_Report_From_Shell
   --   - and at every addition of data to code_analysis from file or projects

   procedure Build_Analysis_Report
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance);
   --  Actually builds the tree view report.
   --  If Is_Error is True, then the Report of Analysis will be built with an
   --  emptiness warning header.
   --  Should be called by Show_Analysis_Report or Show_Empty_Analysis_Report

   procedure List_Uncovered_Lines_In_All_Projects
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance);
   --  Call List_Lines_Not_Covered_In_Project for every projects of the given
   --  code analysis instance

   function First_Project_With_Coverage_Data
     (Analysis : Code_Analysis_Instance) return Project_Type;
   --  Return the 1st project that contains coverage data from the given
   --  analysis.
   --  Return No_Project if no project contains such data

   ---------------------
   -- Contextual menu --
   ---------------------

   type Code_Analysis_Contextual_Menu is new
     Submenu_Factory_Record with null record;

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

   procedure Append_To_Contextual_Submenu
     (Cont_N_Anal  : Context_And_Analysis;
      Submenu      : access Gtk_Menu_Record'Class;
      Project_Node : Project_Access);
   --  Allows to fill the given Submenu, in the appropriate order for
   --  contextual menu

   procedure Append_To_Submenu
     (Cont_N_Anal  : Context_And_Analysis;
      Submenu      : access Gtk_Menu_Record'Class;
      Project_Node : Project_Access);
   --  Allows to fill the given Submenu, in the appropriate order for Tools
   --  menu

   procedure Append_Show_Analysis_Report_To_Menu
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class);
   --  Actually fills the given Menu, with the "Show Analysis Report" entry
   --  With no context information, so the 1st node will be expanded

   procedure Append_Load_Data_For_All_Projects
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class);
   --  Actually fills the given Menu with the "Load data for all projecs" entry
   --  This entry try to load coverage data for root project and every imported
   --  projects.

   procedure Append_Subprogram_Menu_Entries
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class;
      Subp_Node   : Subprogram_Access);
   --  Actually fills the given Submenu with the appropriate coverage action
   --  entries to handle subprograms (List line not covered, Remove data of...)

   procedure Append_File_Menu_Entries
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class;
      File_Node   : File_Access);
   --  Actually fills the given Submenu with the appropriate coverage action
   --  entries to handle files (Add/Remove coverage annotations,
   --  Show coverage report, ...)

   procedure Append_Project_Menu_Entries
     (Cont_N_Anal  : Context_And_Analysis;
      Submenu      : access Gtk_Menu_Record'Class;
      Project_Node : Project_Access);
   --  Actually fills the given Submenu with the appropriate coverage action
   --  entries to handle projects (Add/Remove coverage annotations,
   --  Show coverage report, Load full data...)

   function Check_Context
     (Kernel        : Kernel_Handle;
      Given_Context : Selection_Context) return Selection_Context;
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
      Cont_N_Anal : Context_And_Analysis);
   --  Call to build the tree view report and then put inside it an error
   --  message

   procedure Shell_CodeAnalysis_Constructor
     (Data : in out Callback_Data'Class; Command : String);
   --  Empty subprogram that just raise an exception in order to prevent users
   --  from using the default shell constructor
   --  The Shell_Get_Command should be used instead.

   procedure Shell_Get_Command
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Create a shell scripting instance of the module

   procedure Create_Analysis_From_Menu
     (Widget : access Gtk_Widget_Record'Class);
   --  Create a new analysis instance from the "Tools/Coverage" menu

   function Create_Analysis_Instance
     (Name : String := "") return Code_Analysis_Instance;
   --  Create a new analysis instance, intended to contain analysis data
   --  The instance is inserted in the Instances set of the Module_ID

   procedure Add_Gcov_File_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Looks for a corresponding GCOV file, and adds its node and coverage info
   --  into the current code_analysis structure

   procedure Add_Gcov_File_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add node and coverage info provided by a gcov file parsing

   procedure Add_Gcov_File_Info
     (Kernel       : Kernel_Handle;
      Src_File     : VFS.Virtual_File;
      Cov_File     : VFS.Virtual_File;
      Project_Node : Project_Access);
   --  Actually adds the node and coverage info provided by the gcov file
   --  parsing. Should be called by Add_Gcov_File_Info_From_Shell or
   --  Add_Gcov_File_Info_From_Menu

   procedure Add_Gcov_Project_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Basically do an Add_Gcov_File_Info on every files of the project in
   --  Context

   procedure Add_Gcov_Project_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Basically do an Add_Gcov_File_Info on every files of the given project

   procedure Add_Gcov_Project_Info
     (Kernel   : Kernel_Handle;
      Prj_Node : Project_Access);
   --  Actually looks for a corresponding .gcov files to every source files in
   --  the given project and when its possible, add every file coverage
   --  information to the Code_Analysis structure of given Project_Node

   procedure Add_All_Gcov_Project_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Try to load gcov info for every file of the Root_Project and every
   --  imported projects

   procedure Add_All_Gcov_Project_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Try to load gcov info for every file of the Root_Project and every
   --  imported projects

   procedure List_Uncovered_Lines_In_All_Projects_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add in the location view every not covered lines of any projects loaded
   --  in the Code_Analysis structure of the current Instance.

   procedure Clear_All_Project_Locations_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Shell command callback
   --  Remove from the Locations view the listed uncovered lines of each files
   --  of each loaded projects
   --  Does nothing if the lines are not listed in

   procedure Clear_All_Project_Locations
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance);
   --  Remove from the Locations view the listed uncovered lines of each files
   --  of each loaded projects
   --  Does nothing if the lines are not listed in

   procedure Show_Analysis_Report_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Create and display a Code_Analysis tree view within a MDI Child

   procedure Destroy_Analysis_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Call Destroy_Analysis_Instance

   procedure Destroy_All_Analyzes_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Call Destroy_All_Instances

   procedure Destroy_All_Analyzes_From_Project_Changing_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Call Destroy_All_Instances

   procedure Destroy_All_Analyzes (Kernel : Kernel_Handle);
   --  Call Destroy_Analysis_Instance for every element in
   --  Code_Analysis_Module_ID.Instances

   procedure Destroy_Analysis_Instance
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance);
   --  Free the memory used by the given analysis instance

   procedure On_Destroy (Widget      : access Glib.Object.GObject_Record'Class;
                         Cont_N_Anal : Context_And_Analysis);
   --  Callback for the "destroy" signal that cleanly destroy the widget

   function On_Double_Click (Object : access Gtk_Widget_Record'Class;
                             Event  : Gdk_Event;
                             Kernel : Kernel_Handle) return Boolean;
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
     (Kernel : Kernel_Handle; Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter);
   --  Opens a file editor on the source file pointed out by Iter in Model

   procedure Open_File_Editor_On_Subprogram
     (Kernel : Kernel_Handle; Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter);
   --  Opens a file editor on the source file containing the Subprogram
   --  pointed out by Iter in Model

   procedure Add_Coverage_Annotations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Add coverage annotations to source file editor
   --  Callback to the global contextual menu entry
   --  "View with coverage annotations"

   procedure Add_Coverage_Annotations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access);
   --  Actually add the annotation columns

   procedure Remove_Coverage_Annotations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback attached to the "Remove coverage annotations" contextual
   --  menu entry of every objects that have File or Subprogram information in
   --  its context

   procedure Remove_Coverage_Annotations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access);
   --  Actually removes coverage annotations of src_editors of file represented
   --  by File_Node

   procedure List_Uncovered_Lines_In_Subprogram_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback of the "List lines not covered" entry of the global "Coverage"
   --  submenu when the Context contains subprogram information.
   --  Add to the location view the unexecuted lines of the given current
   --  entity, that is a subprogram and has associated coverage information

   procedure Clear_Subprogram_Locations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback that aim to remove from the Locations view the lines
   --  corresponding to uncovered lines of the contextual subprogram
   --  Does nothing if the lines are absent from the Locations view

   procedure List_Uncovered_Lines_In_File_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback of the "List lines not covered" entry of the global "Coverage"
   --  submenu when the Context contains file information.
   --  Just call the List_Lines_Not_Covered_In_File subprogram

   procedure Clear_File_Locations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback that aim to remove from the Locations view the lines
   --  corresponding to uncovered lines of the contextual file
   --  Does nothing if the file is absent from the Locations view

   procedure List_Uncovered_Lines_In_Project_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback of the "List lines not covered" entry of the global "Coverage"
   --  submenu when the Context only contains Project information.
   --  Just call the subprogram List_Lines_Not_Covered_In_File

   procedure Clear_Project_Locations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback that aim to remove from the Locations view the lines
   --  corresponding to uncovered lines of the contextual project
   --  Does nothing if the files of the project are absent from the Locations
   --  view

   procedure List_Uncovered_Lines_In_All_Projects_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback of the "List lines not covered in all projects" menu entry
   --  Calls List_Lines_Not_Covered_In_All_Projects

   procedure Clear_All_Project_Locations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback that aim to remove from the Locations view the lines
   --  corresponding to uncovered lines of all projects
   --  Does nothing if the files of the projects are absent from the Locations
   --  view

   procedure List_Uncovered_Lines_In_Project
     (Kernel       : Kernel_Handle;
      Project_Node : Project_Access);
   --  Add to the location view the unexecuted lines of the given Project of a
   --  Coverage Report

   procedure Clear_Project_Locations
     (Kernel       : Kernel_Handle;
      Project_Node : Project_Access);
   --  Remove from the Locations view the uncovered lines of each files of the
   --  given project_node
   --  Does nothing if the uncovered lines aren't listed there

   procedure List_Uncovered_Lines_In_File
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access);
   --  Add to the location view the unexecuted lines of the given File of a
   --  Coverage Report.

   procedure Clear_File_Locations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access);
   --  Remove from the Locations view the uncovered lines of the given
   --  file_node
   --  Does nothing if the uncovered lines aren't listed there

   procedure Remove_Subprogram_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Remove the selected subprogram node from the related report and instance

   procedure Remove_File_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Remove the selected file node from the related report and instance

   procedure Remove_Project_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Remove the selected project node from the related report and instance

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
      Cont_N_Anal : Context_And_Analysis);
   --  Menu callback that calls Show_Analysis_Report with no context info

   --------------------------------------------
   -- CodeAnalysis_Default_Shell_Constructor --
   --------------------------------------------

   procedure Shell_CodeAnalysis_Constructor
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Set_Error_Msg (Data, -"Default constructor can't be used to create " &
      (-"CodeAnalysis shell instances. Consider using static " &
       (-"GPS.CodeAnalysis.get (name:) instead.")));
   end Shell_CodeAnalysis_Constructor;

   -----------------------
   -- Shell_Get_Command --
   -----------------------

   procedure Shell_Get_Command
     (Data : in out Callback_Data'Class; Command : String)
   is

      procedure Attach_Instance_And_Analysis
        (Data     : in out Callback_Data'Class;
         Instance : Class_Instance;
         Analysis : Code_Analysis_Instance);
      --  Set the Instance in the instance list of Analysis
      --  Set Analysis in the created property for Instance

      procedure Attach_Instance_And_Analysis
        (Data     : in out Callback_Data'Class;
         Instance : Class_Instance;
         Analysis : Code_Analysis_Instance)
      is
         Property : Code_Analysis_Property;
      begin
         Set (Analysis.Instances.all, Get_Script (Data), Instance);
         Property := new Code_Analysis_Property_Record;
         Property.Analysis := Analysis;
         Set_Data (Instance, Code_Analysis_Cst_Str,
                   Instance_Property_Record (Property.all));
      end Attach_Instance_And_Analysis;

      pragma Unreferenced (Command);
      use Code_Analysis_Instances;
      Cur      : Cursor := Code_Analysis_Module_ID.Analyzes.First;
      Analysis : Code_Analysis_Instance := null;
      Instance : Class_Instance;
   begin
      Name_Parameters (Data, (1 => Ana_Name_Cst'Access));

      declare
         Analysis_Name : constant String := Nth_Arg (Data, 1);
      begin

         --  If the given name correspond to an existing Analysis use it
         --  Else create one using the givent instance
         loop
            exit when Analysis /= null or else Cur = No_Element;
            Analysis := Element (Cur);
            Next (Cur);

            if Analysis.Name.all /= Analysis_Name then
               Analysis := null;
            end if;
         end loop;

         if Analysis = null then
            --  Create one
            Analysis := Create_Analysis_Instance (Analysis_Name);
            Instance := New_Instance
              (Get_Script (Data), Code_Analysis_Module_ID.Class);
            Attach_Instance_And_Analysis (Data, Instance, Analysis);
         else
            --  Set the current instance to corresponding instance of the
            --  Analysis of the given name
            declare
               Stored_Instance : Class_Instance := Get
                 (Analysis.Instances.all, Get_Script (Data));
            begin
               if Stored_Instance = No_Class_Instance then
                  --  Attach the current instance to
                  Instance := New_Instance
                    (Get_Script (Data), Code_Analysis_Module_ID.Class);
                  Attach_Instance_And_Analysis (Data, Instance, Analysis);
               else
                  Instance := Stored_Instance;
               end if;
            end;
         end if;

         Set_Return_Value (Data, Instance);
      end;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Shell_Get_Command;

   -------------------------------
   -- Create_Analysis_From_Menu --
   -------------------------------

   procedure Create_Analysis_From_Menu
     (Widget : access Gtk_Widget_Record'Class)
   is
      Dummy : Code_Analysis_Instance;
      pragma Unreferenced (Dummy, Widget);
   begin
      Dummy := Create_Analysis_Instance;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Create_Analysis_From_Menu;

   ------------------------------
   -- Create_Analysis_Instance --
   ------------------------------

   function Create_Analysis_Instance
     (Name : String := "") return Code_Analysis_Instance
   is
      Analysis : constant Code_Analysis_Instance
        := new Code_Analysis_Instance_Record;
      Date     : Time;
   begin
      Date := Clock;
      Analysis.Date := Date;
      Code_Analysis_Module_ID.Count := Code_Analysis_Module_ID.Count + 1;

      if Name = "" then
         Analysis.Name   := new String'
           (-"Code Coverage" & Integer'Image (Code_Analysis_Module_ID.Count));
      else
         Analysis.Name   := new String'(Name);
      end if;

      Analysis.Instances := new Instance_List;
      Analysis.Projects  := new Project_Maps.Map;
      Code_Analysis_Module_ID.Analyzes.Insert (Analysis);
      return Analysis;
   end Create_Analysis_Instance;

   ----------------------------------
   -- Add_Gcov_File_Info_From_Menu --
   ----------------------------------

   procedure Add_Gcov_File_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Prj_Name : constant Project_Type :=
                   Project_Information (Cont_N_Anal.Context);
      Prj_Node : Project_Access;
      Src_File : constant VFS.Virtual_File :=
                   File_Information (Cont_N_Anal.Context);
      Cov_File : VFS.Virtual_File;
      Analysis : Code_Analysis_Instance;
   begin
      if Cont_N_Anal.Analysis = null then
         Analysis := Create_Analysis_Instance;
      else
         Analysis := Cont_N_Anal.Analysis;
      end if;

      Prj_Node := Get_Or_Create
        (Analysis.Projects, Prj_Name);
      Cov_File := Create (Object_Path (Prj_Name, False, False) &
                              "/" & Base_Name (Src_File) & Gcov_Extension_Cst);

      if not Is_Regular_File (Cov_File) then
         GPS.Kernel.Console.Insert
           (Get_Kernel (Cont_N_Anal.Context),
            -"There is no loadable GCOV information" &
            (-" associated with " & Base_Name (Src_File)),
            Mode => GPS.Kernel.Console.Error);

         declare
            File_Node : constant Code_Analysis.File_Access
              := Get_Or_Create (Prj_Node, Src_File);
         begin
            Set_Error (File_Node, File_Not_Found);
            --  Set an empty line array in order to make File_Node a valid
            --  Code_Analysis node
            File_Node.Lines := new Line_Array (1 .. 1);
         end;
      else
         Add_Gcov_File_Info
           (Get_Kernel (Cont_N_Anal.Context), Src_File, Cov_File, Prj_Node);
         Compute_Project_Coverage (Prj_Node);
      end if;

      --  Build/Refresh Report of Analysis
      Show_Analysis_Report
        (Get_Kernel (Cont_N_Anal.Context),
         Context_And_Analysis'(Cont_N_Anal.Context, Analysis));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Gcov_File_Info_From_Menu;

   -----------------------------------
   -- Add_Gcov_File_Info_From_Shell --
   -----------------------------------

   procedure Add_Gcov_File_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property : Instance_Property;
      Instance : Class_Instance;
      Context  : Selection_Context;
      Src_Inst : Class_Instance;
      Cov_Inst : Class_Instance;
      Src_File : VFS.Virtual_File;
      Cov_File : VFS.Virtual_File;
      Prj_Name : Project_Type;
      Prj_Node : Project_Access;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, Code_Analysis_Cst_Str);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      Name_Parameters (Data, (2 => Src_File_Cst'Access,
                              3 => Cov_File_Cst'Access));
      Src_Inst := Nth_Arg
        (Data, 2, Get_File_Class (Get_Kernel (Data)),
         Default => No_Class_Instance, Allow_Null => True);

      if Src_Inst = No_Class_Instance then
         Src_File := VFS.No_File;
      else
         Src_File := Get_Data (Src_Inst);
      end if;

      if not Is_Regular_File (Src_File) then
         Set_Error_Msg (Data, -"The name given for 'src' file is wrong");
         return;
      end if;

      Cov_Inst := Nth_Arg
        (Data, 3, Get_File_Class (Get_Kernel (Data)),
         Default => No_Class_Instance, Allow_Null => True);

      if Cov_Inst = No_Class_Instance then
         Cov_File := VFS.No_File;
      else
         Cov_File := Get_Data (Cov_Inst);
      end if;

      Prj_Name  := Get_Project_From_File
        (Get_Registry (Get_Kernel (Data)).all, Src_File);
      Prj_Node  := Get_Or_Create
        (Code_Analysis_Property (Property).Analysis.Projects, Prj_Name);

      if not Is_Regular_File (Cov_File) then
         Set_Error_Msg (Data, -"The name given for 'cov' file is wrong");

         declare
            File_Node : constant Code_Analysis.File_Access
              := Get_Or_Create (Prj_Node, Src_File);
         begin
            Set_Error (File_Node, File_Not_Found);
            --  Set an empty line array in order to make File_Node a valid
            --  Code_Analysis node
            File_Node.Lines := new Line_Array (1 .. 1);
         end;
         return;
      end if;

      Add_Gcov_File_Info
        (Get_Kernel (Data), Src_File, Cov_File, Prj_Node);
      Compute_Project_Coverage (Prj_Node);
      --  Build/Refresh Report of Analysis
      Context := Get_Current_Context (Get_Kernel (Data));
      Set_File_Information
        (Context, Project => Prj_Name, File => Src_File);
      Show_Analysis_Report
        (Get_Kernel (Data), Context_And_Analysis'(Context,
         Code_Analysis_Property (Property).Analysis));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Gcov_File_Info_From_Shell;

   ------------------------
   -- Add_Gcov_File_Info --
   ------------------------

   procedure Add_Gcov_File_Info
     (Kernel       : Kernel_Handle;
      Src_File     : VFS.Virtual_File;
      Cov_File     : VFS.Virtual_File;
      Project_Node : Project_Access)
   is
      use Language.Tree.Database;
      File_Contents : GNAT.Strings.String_Access;
      File_Node     : constant Code_Analysis.File_Access
        := Get_Or_Create (Project_Node, Src_File);
      Handler       : constant Language_Handler
        := Get_Language_Handler (Kernel);
      Database      : constant Construct_Database_Access
        := Get_Construct_Database (Kernel);
      Tree_Lang     : constant Tree_Language_Access
        := Get_Tree_Language_From_File (Handler, Src_File, False);
      Data_File     : constant Structured_File_Access
        := Language.Tree.Database.Get_Or_Create
          (Db   => Database,
           File => Src_File,
           Lang => Tree_Lang);
   begin
      if File_Time_Stamp (Src_File) > File_Time_Stamp (Cov_File) then
         GPS.Kernel.Console.Insert
           (Kernel, Base_Name (Src_File) &
         (-" has been modified since GCOV information were generated.") &
         (-" Skipped."),
            Mode => GPS.Kernel.Console.Error);
         Set_Error (File_Node, File_Corrupted);
         --  Set an empty line array in order to make File_Node a valid
         --  Code_Analysis node
         File_Node.Lines := new Line_Array (1 .. 1);
      else
         File_Contents := Read_File (Cov_File);
         Add_File_Info (File_Node, File_Contents);

         --  Check for project runs info
         if File_Node.Analysis_Data.Coverage_Data.Status = Valid and then
           Project_Node.Analysis_Data.Coverage_Data = null then

            Project_Node.Analysis_Data.Coverage_Data := new Project_Coverage;
            Get_Runs_Info_From_File
              (File_Contents,
               Project_Coverage
                 (Project_Node.Analysis_Data.Coverage_Data.all).Runs,
               Project_Coverage
                 (Project_Node.Analysis_Data.Coverage_Data.all).Have_Runs);
         end if;

         if File_Node.Analysis_Data.Coverage_Data.Status = Valid then
            Add_Subprogram_Info (File_Node, Data_File);
         end if;

         Free (File_Contents);
      end if;
   end Add_Gcov_File_Info;

   -------------------------------------
   -- Add_Gcov_Project_Info_From_Menu --
   -------------------------------------

   procedure Add_Gcov_Project_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Analysis : Code_Analysis_Instance;
      Prj_Node : Project_Access;
      Prj_Name : constant Project_Type :=
                   Project_Information (Cont_N_Anal.Context);
   begin
      if Cont_N_Anal.Analysis = null then
         Analysis := Create_Analysis_Instance;
      else
         Analysis := Cont_N_Anal.Analysis;
      end if;

      Prj_Node := Get_Or_Create
        (Analysis.Projects, Prj_Name);
      Add_Gcov_Project_Info (Get_Kernel (Cont_N_Anal.Context), Prj_Node);
      --  Build/Refresh Report of Analysis
      Show_Analysis_Report
        (Get_Kernel (Cont_N_Anal.Context),
         Context_And_Analysis'(Cont_N_Anal.Context, Analysis));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Gcov_Project_Info_From_Menu;

   --------------------------------------
   -- Add_Gcov_Project_Info_From_Shell --
   --------------------------------------

   procedure Add_Gcov_Project_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property : Instance_Property;
      Instance : Class_Instance;
      Context  : Selection_Context;
      Prj_Inst : Class_Instance;
      Prj_File : VFS.Virtual_File;
      Prj_Name : Project_Type;
      Prj_Node : Project_Access;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, Code_Analysis_Cst_Str);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      Name_Parameters (Data, (2 => Prj_File_Cst'Access));
         Prj_Inst := Nth_Arg
           (Data, 2, Get_File_Class (Get_Kernel (Data)),
            Default => No_Class_Instance, Allow_Null => True);

      if Prj_Inst = No_Class_Instance then
         Prj_File := VFS.No_File;
      else
         Prj_File := Get_Data (Prj_Inst);
      end if;

      if not Is_Regular_File (Prj_File) then
         Set_Error_Msg (Data, -"The name given for 'prj' file is wrong");
         return;
      end if;

      Prj_Name  := Load_Or_Find (Get_Registry (Get_Kernel (Data)).all,
                                 Locale_Full_Name (Prj_File));
      Prj_Node  := Get_Or_Create
        (Code_Analysis_Property (Property).Analysis.Projects, Prj_Name);
      Add_Gcov_Project_Info (Get_Kernel (Data), Prj_Node);

      --  Build/Refresh Report of Analysis
      Context := Get_Current_Context (Get_Kernel (Data));
      Set_File_Information (Context, Project => Prj_Name);
      Show_Analysis_Report
        (Get_Kernel (Data), Context_And_Analysis'(Context,
         Code_Analysis_Property (Property).Analysis));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Gcov_Project_Info_From_Shell;

   ---------------------------
   -- Add_Gcov_Project_Info --
   ---------------------------

   procedure Add_Gcov_Project_Info
     (Kernel   : Kernel_Handle;
      Prj_Node : Project_Access)
   is
      Src_Files : VFS.File_Array_Access;
      Src_File  : VFS.Virtual_File;
      Cov_File  : VFS.Virtual_File;
   begin
      --  get every source files of the project
      --  check if they are associated with gcov info
      --  load their info
      Src_Files := Get_Source_Files (Prj_Node.Name, Recursive => False);

      for J in Src_Files'First .. Src_Files'Last loop
         Src_File := Src_Files (J);
         Cov_File := Create (Object_Path (Prj_Node.Name, False, False) &
                             "/" & Base_Name (Src_File) & Gcov_Extension_Cst);

         if Is_Regular_File (Cov_File) then
            Add_Gcov_File_Info (Kernel, Src_File, Cov_File, Prj_Node);
         else
            GPS.Kernel.Console.Insert
              (Kernel, -"There is no loadable GCOV information" &
               (-" associated with " & Base_Name (Src_File)),
               Mode => GPS.Kernel.Console.Error);

            declare
               File_Node : constant Code_Analysis.File_Access
                 := Get_Or_Create (Prj_Node, Src_File);
            begin
               Set_Error (File_Node, File_Not_Found);
               --  Set an empty line array in order to make File_Node a valid
               --  Code_Analysis node
               File_Node.Lines := new Line_Array (1 .. 1);
            end;
         end if;
      end loop;

      Compute_Project_Coverage (Prj_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Gcov_Project_Info;

   -----------------------------------------
   -- Add_All_Gcov_Project_Info_From_Menu --
   -----------------------------------------

   procedure Add_All_Gcov_Project_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Analysis : Code_Analysis_Instance;
      Prj_Iter : Imported_Project_Iterator;
      Prj_Node : Project_Access;
      Prj_Name : constant Project_Type :=
                   Project_Information (Cont_N_Anal.Context);
   begin
      if Cont_N_Anal.Analysis = null then
         Analysis := Create_Analysis_Instance;
      else
         Analysis := Cont_N_Anal.Analysis;
      end if;

      Prj_Iter := Start (Prj_Name);

      loop
         exit when Current (Prj_Iter) = No_Project;
         Prj_Node := Get_Or_Create
           (Analysis.Projects, Current (Prj_Iter));
         Add_Gcov_Project_Info (Get_Kernel (Cont_N_Anal.Context), Prj_Node);
         Next (Prj_Iter);
      end loop;

      --  Build/Refresh Report of Analysis
      Show_Analysis_Report
        (Get_Kernel (Cont_N_Anal.Context),
         Context_And_Analysis'(No_Context, Analysis));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_All_Gcov_Project_Info_From_Menu;

   ------------------------------------------
   -- Add_All_Gcov_Project_Info_From_Shell --
   ------------------------------------------

   procedure Add_All_Gcov_Project_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property : Instance_Property;
      Instance : Class_Instance;
      Prj_Name : Project_Type;
      Prj_Node : Project_Access;
      Prj_Iter : Imported_Project_Iterator;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, Code_Analysis_Cst_Str);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      Prj_Name := Get_Project (Get_Kernel (Data));
      Prj_Iter := Start (Prj_Name);

      loop
         exit when Current (Prj_Iter) = No_Project;
         Prj_Node := Get_Or_Create
           (Code_Analysis_Property (Property).Analysis.Projects,
            Current (Prj_Iter));
         Add_Gcov_Project_Info (Get_Kernel (Data), Prj_Node);
         Next (Prj_Iter);
      end loop;

      --  Build/Refresh Report of Analysis
      Show_Analysis_Report
        (Get_Kernel (Data), Context_And_Analysis'(No_Context,
         Code_Analysis_Property (Property).Analysis));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_All_Gcov_Project_Info_From_Shell;

   -----------------------------------------------------
   -- List_Uncovered_Lines_In_All_Projects_From_Shell --
   -----------------------------------------------------

   procedure List_Uncovered_Lines_In_All_Projects_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property : Instance_Property;
      Instance : Class_Instance;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, Code_Analysis_Cst_Str);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      List_Uncovered_Lines_In_All_Projects
        (Get_Kernel (Data), Code_Analysis_Property (Property).Analysis);
   exception
      when E : others => Trace (Exception_Handle, E);
   end List_Uncovered_Lines_In_All_Projects_From_Shell;

   --------------------------------------------
   -- Clear_All_Project_Locations_From_Shell --
   --------------------------------------------

   procedure Clear_All_Project_Locations_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property : Instance_Property;
      Instance : Class_Instance;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, Code_Analysis_Cst_Str);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      Clear_All_Project_Locations
        (Get_Kernel (Data), Code_Analysis_Property (Property).Analysis);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Clear_All_Project_Locations_From_Shell;

   ----------------------------------------------------
   -- List_Uncovered_Lines_In_All_Projects_From_Menu --
   ----------------------------------------------------

   procedure List_Uncovered_Lines_In_All_Projects_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      List_Uncovered_Lines_In_All_Projects
        (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal.Analysis);
   exception
      when E : others => Trace (Exception_Handle, E);
   end List_Uncovered_Lines_In_All_Projects_From_Menu;

   -------------------------------------------
   -- Clear_All_Project_Locations_From_Menu --
   -------------------------------------------

   procedure Clear_All_Project_Locations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      Clear_All_Project_Locations
        (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal.Analysis);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Clear_All_Project_Locations_From_Menu;

   ------------------------------------------
   -- List_Uncovered_Lines_In_All_Projects --
   ------------------------------------------

   procedure List_Uncovered_Lines_In_All_Projects
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance)
   is
      use Project_Maps;
      Map_Cur  : Project_Maps.Cursor;
      Sort_Arr : Project_Array (1 .. Integer (Analysis.Projects.Length));
   begin
      Map_Cur  := Analysis.Projects.First;

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Projects (Sort_Arr);

      for J in Sort_Arr'Range loop
         List_Uncovered_Lines_In_Project (Kernel, Sort_Arr (J));
      end loop;
   end List_Uncovered_Lines_In_All_Projects;

   ---------------------------------
   -- Clear_All_Project_Locations --
   ---------------------------------

   procedure Clear_All_Project_Locations
   (Kernel   : Kernel_Handle;
    Analysis : Code_Analysis_Instance)
   is
      use Project_Maps;
      Map_Cur  : Project_Maps.Cursor := Analysis.Projects.First;
   begin
      for J in 1 .. Integer (Analysis.Projects.Length) loop
         Clear_Project_Locations (Kernel, Element (Map_Cur));
         Next (Map_Cur);
      end loop;
   end Clear_All_Project_Locations;

   -------------------------------------
   -- Show_Analysis_Report_From_Shell --
   -------------------------------------

   procedure Show_Analysis_Report_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Instance : Class_Instance;
      Property : Instance_Property;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, Code_Analysis_Cst_Str);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      Show_Analysis_Report
        (Get_Kernel (Data), Context_And_Analysis'(No_Context,
         Code_Analysis_Property (Property).Analysis));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Analysis_Report_From_Shell;

   ------------------------------------
   -- Show_Analysis_Report_From_Menu --
   ------------------------------------

   procedure Show_Analysis_Report_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      Show_Analysis_Report (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Show_Analysis_Report_From_Menu;

   ------------------------------------------
   -- Show_Empty_Analysis_Report_From_Menu --
   ------------------------------------------

   procedure Show_Empty_Analysis_Report_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Analysis : constant Code_Analysis_Instance := Create_Analysis_Instance;
   begin
      Show_Empty_Analysis_Report (Get_Kernel (Cont_N_Anal.Context), Analysis);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Show_Empty_Analysis_Report_From_Menu;

   --------------------------------
   -- Show_Empty_Analysis_Report --
   --------------------------------

   procedure Show_Empty_Analysis_Report
     (Kernel : Kernel_Handle; Analysis : Code_Analysis_Instance) is
   begin
      if Analysis.View = null then
         Build_Analysis_Report (Kernel, Analysis);
      end if;

      if Get_No_Show_All (Analysis.View.Error_Box) then
         Set_No_Show_All (Analysis.View.Error_Box, False);
      end if;

      Show_All (Analysis.View.Error_Box);
      Raise_Child (Analysis.Child);
   end Show_Empty_Analysis_Report;

   --------------------------------------
   -- First_Project_With_Coverage_Data --
   --------------------------------------

   function First_Project_With_Coverage_Data
     (Analysis : Code_Analysis_Instance) return Project_Type
   is
      use Project_Maps;
      Prj_Node : Code_Analysis.Project_Access;
      Prj_Cur  : Project_Maps.Cursor := Analysis.Projects.First;
   begin
      if Prj_Cur /= No_Element then
         Prj_Node := Element (Prj_Cur);
      else
         return No_Project;
      end if;

      loop
         exit when Prj_Node.Analysis_Data.Coverage_Data /= null
           or else Prj_Cur = No_Element;
         Prj_Node := Element (Prj_Cur);
         Next (Prj_Cur);
      end loop;

      if Prj_Cur /= No_Element then
         return Prj_Node.Name;
      else
         return No_Project;
      end if;
   end First_Project_With_Coverage_Data;

   ---------------------------
   -- Get_Iter_From_Context --
   ---------------------------

   function Get_Iter_From_Context
     (Context : Selection_Context;
      Model   : Gtk_Tree_Store) return Gtk_Tree_Iter
   is
      Iter    : Gtk_Tree_Iter := Get_Iter_First (Model);
      Num_Col : constant Gint := 1;
   begin
      if Iter /= Null_Iter then
         if not Has_Child (Model, Iter) then
            --  So we are in a flat list
            if Has_Entity_Name_Information (Context) then
               declare
                  Entity : constant Entities.Entity_Information :=
                             Get_Entity (Context);
               begin
                  if (Entity /= null
                      and then Is_Subprogram (Entity))
                    or else Get_Creator (Context) = Abstract_Module_ID
                    (Code_Analysis_Module_ID) then
                     --  So we have a subprogram information
                     --  Find in the list the context's subprogram
                     loop
                        exit when Get_String (Model, Iter, Num_Col) =
                          Entity_Name_Information (Context);
                        Next (Model, Iter);
                     end loop;
                  end if;
               end;
            elsif Has_File_Information (Context) then
               --  Find in the list the context's file
               loop
                  exit when Get_String (Model, Iter, Num_Col) =
                    Base_Name (File_Information (Context));
                  Next (Model, Iter);
               end loop;
            else
               --  Find in the list the context's project
               loop
                  exit when Get_String (Model, Iter, Num_Col) =
                    Project_Name (Project_Information (Context));
                  Next (Model, Iter);
               end loop;
            end if;
         else
            --  Find in the tree the context's project
            loop
               exit when Get_String (Model, Iter, Num_Col) =
                 Project_Name (Project_Information (Context));
               Next (Model, Iter);
            end loop;

            if Has_File_Information (Context) then
               --  So we also have file information
               Iter := Children (Model, Iter);

               --  Find in the tree the context's file
               loop
                  exit when Get_String (Model, Iter, Num_Col) =
                    Base_Name (File_Information (Context));
                  Next (Model, Iter);
               end loop;
            end if;

            if Has_Entity_Name_Information (Context) then
               declare
                  Entity : constant Entities.Entity_Information :=
                             Get_Entity (Context);
               begin
                  if (Entity /= null
                      and then Is_Subprogram (Entity))
                    or else Get_Creator (Context) = Abstract_Module_ID
                    (Code_Analysis_Module_ID) then
                     --  So we have a subprogram information
                     Iter := Children (Model, Iter);
                     --  Find in the tree the context's subprogram
                     loop
                        exit when Get_String (Model, Iter, Num_Col) =
                          Entity_Name_Information (Context);
                        Next (Model, Iter);
                     end loop;
                  end if;
               end;
            end if;
         end if;
      end if;

      return Iter;
   end Get_Iter_From_Context;

   --------------------------
   -- Show_Analysis_Report --
   --------------------------

   procedure Show_Analysis_Report
     (Kernel       : Kernel_Handle;
      Cont_N_Anal  : Context_And_Analysis;
      Raise_Report : Boolean := True)
   is
      Local_Context : Selection_Context;
      Iter          : Gtk_Tree_Iter;
      Path          : Gtk_Tree_Path;
   begin

      --------------------------------------
      --  Check for analysis information  --
      --------------------------------------

      if Local_Context = No_Context then
         Local_Context := Check_Context (Kernel, No_Context);
      end if;

      declare
         Prj_Name : Project_Type;
         Prj_Node : Code_Analysis.Project_Access;
      begin
         Prj_Node := Get_Or_Create
           (Cont_N_Anal.Analysis.Projects,
            Project_Information (Local_Context));

         if Prj_Node.Analysis_Data.Coverage_Data = null then
            --  If the current context's project has no coverage data, it has
            --  to be modified or an erro message is shown
            Prj_Name := First_Project_With_Coverage_Data
              (Cont_N_Anal.Analysis);

            if Prj_Name /= No_Project then
               --  Set in the context the 1st project that has analysis
               --  data
               Set_File_Information
                 (Local_Context, Project => Prj_Name);
            else
               Show_Empty_Analysis_Report (Kernel, Cont_N_Anal.Analysis);
               return;
            end if;
         end if;
      end;

      --  Here we have a context that point on elements that will be added to
      --  the report of analysis

      --------------------------
      --  Building the report --
      --------------------------

      if Cont_N_Anal.Analysis.View = null then
         Build_Analysis_Report (Kernel, Cont_N_Anal.Analysis);
      elsif Cont_N_Anal.Analysis.View.Error_Box /= null then
         Hide_All (Cont_N_Anal.Analysis.View.Error_Box);
      end if;

      Clear (Cont_N_Anal.Analysis.View.Model);
      Iter := Get_Iter_First (Cont_N_Anal.Analysis.View.Model);
      Fill_Iter (Cont_N_Anal.Analysis.View.Model, Iter,
                 Cont_N_Anal.Analysis.Projects, Binary_Coverage_Mode,
                 Cont_N_Anal.Analysis.View.Icons);

      --------------------------------------
      --  Selection of the context caller --
      --------------------------------------

      Iter := Get_Iter_From_Context
        (Local_Context, Cont_N_Anal.Analysis.View.Model);

      if Iter = Null_Iter then
         Show_Empty_Analysis_Report (Kernel, Cont_N_Anal.Analysis);
         return;
      end if;

      Path := Get_Path (Cont_N_Anal.Analysis.View.Model, Iter);
      Collapse_All (Cont_N_Anal.Analysis.View.Tree);
      Expand_To_Path (Cont_N_Anal.Analysis.View.Tree, Path);
      Select_Path (Get_Selection (Cont_N_Anal.Analysis.View.Tree), Path);
      Path_Free (Path);

      if Raise_Report then
         Raise_Child (Cont_N_Anal.Analysis.Child);
      end if;
   end Show_Analysis_Report;

   ---------------------------
   -- Build_Analysis_Report --
   ---------------------------

   procedure Build_Analysis_Report
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance)
   is
      Scrolled    : Gtk_Scrolled_Window;
      Text_Render : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Bar_Render  : Gtk_Cell_Renderer_Progress;
      Dummy       : Gint;
      Cont_N_Anal : constant Context_And_Analysis :=
                      (Context => No_Context, Analysis => Analysis);
      pragma Unreferenced (Dummy);
      --  Error box widgets
      Warning_Image    : Gtk_Image;
      Error_Label      : Gtk_Label;
      Label_And_Button : Gtk_Vbox;
      Button_Box       : Gtk_Hbox;
      Load_Data_Button : Gtk_Button;
   begin
      Analysis.View := new Code_Analysis_View_Record;
      Analysis.View.Icons.Prj_Pixbuf  := Render_Icon
        (Get_Main_Window (Kernel), Prj_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      Analysis.View.Icons.File_Pixbuf := Render_Icon
        (Get_Main_Window (Kernel), File_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      Analysis.View.Icons.Subp_Pixbuf := Render_Icon
        (Get_Main_Window (Kernel), Subp_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      Initialize_Vbox (Analysis.View, False, 0);
      Analysis.View.Projects := Analysis.Projects;
      Gtk_New (Analysis.View.Model, GType_Array'
          (Pix_Col     => Gdk.Pixbuf.Get_Type,
           Name_Col    => GType_String,
           Node_Col    => GType_Pointer,
           File_Col    => GType_Pointer,
           Prj_Col     => GType_Pointer,
           Cov_Col     => GType_String,
           Cov_Sort    => GType_Int,
           Cov_Bar_Txt => GType_String,
           Cov_Bar_Val => GType_Int));
      Gtk_New (Analysis.View.Tree, Gtk_Tree_Model (Analysis.View.Model));
      Set_Name (Analysis.View.Tree, Analysis.Name.all); --  testsuite

      --  Create and append an error box in every cases
      --  Set its children visible or not after
      Gtk_New_Hbox (Analysis.View.Error_Box, False, 7);
            Gtk_New_Vbox (Label_And_Button, False, 7);
      Gtk_New_Hbox (Button_Box);
      Gtk_New_From_Icon_Name
        (Warning_Image, Stock_Dialog_Warning, Icon_Size_Dialog);
      Gtk_New
        (Error_Label,
         -"This analysis report is empty. You can populate it with the "
         & '"' & (-"Load data..." & '"' &
           (-" entries of the /Tools/Coverage menu or the button below."
             )));
      Set_Line_Wrap (Error_Label, True);
      Set_Justify (Error_Label, Justify_Left);
      Gtk_New (Load_Data_Button, -"Load data for all projects");
      Context_And_Analysis_CB.Connect
        (Load_Data_Button, Gtk.Button.Signal_Clicked,
         Context_And_Analysis_CB.To_Marshaller
           (Add_All_Gcov_Project_Info_From_Menu'Access),
         Context_And_Analysis'
           (Check_Context (Kernel, Get_Current_Context (Kernel)), Analysis));
      Pack_Start (Analysis.View.Error_Box, Warning_Image, False, False, 7);
      Pack_Start (Label_And_Button, Error_Label, False, True, 7);
      Pack_Start (Button_Box, Load_Data_Button, False, False, 0);
      Pack_Start (Label_And_Button, Button_Box, False, True, 0);
      Pack_Start (Analysis.View.Error_Box, Label_And_Button, False, True, 0);
      Pack_Start (Analysis.View, Analysis.View.Error_Box, False, True, 0);
      Set_No_Show_All (Analysis.View.Error_Box, True);

      -----------------
      -- Node column --
      -----------------

      Gtk_New (Analysis.View.Node_Column);
      Gtk_New (Pixbuf_Rend);
      Pack_Start (Analysis.View.Node_Column, Pixbuf_Rend, False);
      Add_Attribute
        (Analysis.View.Node_Column, Pixbuf_Rend, "pixbuf", Pix_Col);
      Gtk_New (Text_Render);
      Pack_Start (Analysis.View.Node_Column, Text_Render, False);
      Add_Attribute
        (Analysis.View.Node_Column, Text_Render, "text", Name_Col);
      Dummy := Append_Column
        (Analysis.View.Tree, Analysis.View.Node_Column);
      Set_Title (Analysis.View.Node_Column, -"Entities");
      Set_Resizable (Analysis.View.Node_Column, True);
      Set_Sort_Column_Id (Analysis.View.Node_Column, Name_Col);

      ----------------------
      -- Coverage columns --
      ----------------------

      Gtk_New (Analysis.View.Cov_Column);
      Dummy :=
        Append_Column (Analysis.View.Tree, Analysis.View.Cov_Column);
      Gtk_New (Text_Render);
      Pack_Start (Analysis.View.Cov_Column, Text_Render, False);
      Add_Attribute
        (Analysis.View.Cov_Column, Text_Render, "text", Cov_Col);
      Set_Title (Analysis.View.Cov_Column, -"Coverage");
      Set_Sort_Column_Id (Analysis.View.Cov_Column, Cov_Sort);
      Gtk_New (Analysis.View.Cov_Percent);
      Dummy :=
        Append_Column (Analysis.View.Tree, Analysis.View.Cov_Percent);
      Gtk_New (Bar_Render);
      Glib.Properties.Set_Property
        (Bar_Render,
         Gtk.Cell_Renderer.Width_Property,
         Progress_Bar_Width_Cst);
      Pack_Start (Analysis.View.Cov_Percent, Bar_Render, False);
      Add_Attribute
        (Analysis.View.Cov_Percent, Bar_Render, "text", Cov_Bar_Txt);
      Add_Attribute
        (Analysis.View.Cov_Percent, Bar_Render, "value", Cov_Bar_Val);
      Set_Title (Analysis.View.Cov_Percent, -"Coverage Percentage");
      Set_Sort_Column_Id (Analysis.View.Cov_Percent, Cov_Bar_Val);
      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Add (Scrolled, Analysis.View.Tree);
      Add (Analysis.View, Scrolled);

      ---------------
      -- MDI child --
      ---------------

      GPS.Kernel.MDI.Gtk_New
        (Analysis.Child, Analysis.View,
         Group  => Group_VCS_Explorer,
         Module => Code_Analysis_Module_ID);
      Set_Title
        (Analysis.Child, -"Report of " & Analysis.Name.all);
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Analysis.View.Tree,
         Object          => Analysis.View,
         ID              => Module_ID (Code_Analysis_Module_ID),
         Context_Func    => Context_Func'Access);
      Kernel_Return_Cb.Object_Connect
        (Analysis.View.Tree, Signal_Button_Press_Event,
         Kernel_Return_Cb.To_Marshaller
           (On_Double_Click'Access), Analysis.View.Tree, Kernel);
      Context_And_Analysis_CB.Connect
        (Analysis.View, Signal_Destroy, Context_And_Analysis_CB.To_Marshaller
           (On_Destroy'Access), Cont_N_Anal);
      Put (Get_MDI (Kernel), Analysis.Child);
   end Build_Analysis_Report;

   --------------------------------
   -- Destroy_Analysis_From_Menu --
   --------------------------------

   procedure Destroy_Analysis_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      if Message_Dialog
        ((-"Destroy ")
         & Cont_N_Anal.Analysis.Name.all & (-"?"),
         Confirmation, Button_Yes or Button_No, Justification => Justify_Left,
         Title => Cont_N_Anal.Analysis.Name.all
            & (-" destruction?")) = 1
      then
         Destroy_Analysis_Instance
           (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal.Analysis);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Destroy_Analysis_From_Menu;

   ------------------------------------
   -- Destroy_All_Analyzes_From_Menu --
   ------------------------------------

   procedure Destroy_All_Analyzes_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Analysis_Nb : constant Integer := Integer
        (Code_Analysis_Module_ID.Analyzes.Length);
   begin
      if Message_Dialog
        ((-"Destroy") & Integer'Image (Analysis_Nb) & (-" analysis?"),
         Confirmation, Button_Yes or Button_No, Justification => Justify_Left,
         Title => Integer'Image (Analysis_Nb) & (-" destructions?")) = 1
      then
         Destroy_All_Analyzes (Get_Kernel (Cont_N_Anal.Context));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Destroy_All_Analyzes_From_Menu;

   -----------------------------------------------------
   -- Destroy_All_Analyzes_From_Project_Changing_Hook --
   -----------------------------------------------------

   procedure Destroy_All_Analyzes_From_Project_Changing_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Data);
   begin
      Destroy_All_Analyzes (Kernel_Handle (Kernel));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Destroy_All_Analyzes_From_Project_Changing_Hook;

   --------------------------
   -- Destroy_All_Analyzes --
   --------------------------

   procedure Destroy_All_Analyzes (Kernel : Kernel_Handle) is
      use Code_Analysis_Instances;
      Cur      : Cursor := Code_Analysis_Module_ID.Analyzes.First;
      Analysis : Code_Analysis_Instance;
   begin
      loop
         exit when Cur = No_Element;
         Analysis := Element (Cur);
         Next (Cur);
         Destroy_Analysis_Instance (Kernel, Analysis);
      end loop;
   end Destroy_All_Analyzes;

   -------------------------------
   -- Destroy_Analysis_Instance --
   -------------------------------

   procedure Destroy_Analysis_Instance
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance)
   is
      Instances : constant Instance_Array := Get_Instances
        (Analysis.Instances.all);
      Property  : Instance_Property;
   begin
      if Analysis.View /= null then
         Close_Child (Analysis.Child, Force => True);
      end if;

      Remove_Location_Category (Kernel, Coverage_Category);
      Remove_Line_Information_Column (Kernel, No_File, Line_Icons_Cst);
      Remove_Line_Information_Column (Kernel, No_File, Line_Info_Cst);
      Free_Code_Analysis (Analysis.Projects);

      --  For each shell instance, get its property and set the Analysis field
      --  to null
      for J in Instances'Range loop

         Property := Get_Data (Instances (J), Code_Analysis_Cst_Str);

         if Property /= null then
            Code_Analysis_Property (Property).Analysis := null;
         end if;
      end loop;

      if Code_Analysis_Module_ID.Analyzes.Contains (Analysis) then
         Code_Analysis_Module_ID.Analyzes.Delete (Analysis);
      end if;

      GNAT.Strings.Free (Analysis.Name);
   end Destroy_Analysis_Instance;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      Cont_N_Anal.Analysis.View := null;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Destroy;

   ---------------------
   -- On_Double_Click --
   ---------------------

   function On_Double_Click
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Kernel : Kernel_Handle) return Boolean
   is
      use Code_Analysis_Tree_Model.File_Set;
      Tree  : constant Gtk_Tree_View := Gtk_Tree_View (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         Get_Selected (Get_Selection (Tree), Model, Iter);

         declare
            Node : constant Node_Access := Code_Analysis.Node_Access
              (Node_Set.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
         begin
            if Node.all in Code_Analysis.Project'Class then
               --  So we are on a project node
               null;
            elsif Node.all in Code_Analysis.File'Class then
               --  So we are on a file node
               Open_File_Editor_On_File (Kernel, Model, Iter);
            elsif Node.all in Code_Analysis.Subprogram'Class then
               --  So we are on a subprogram node
               Open_File_Editor_On_Subprogram (Kernel, Model, Iter);
            end if;
         end;

         return True;
      end if;

      return False;
   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end On_Double_Click;

   ------------------------------
   -- Open_File_Editor_On_File --
   ------------------------------

   procedure Open_File_Editor_On_File
     (Kernel : Kernel_Handle; Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter)
   is
      File_Node : constant File_Access := File_Access
        (File_Set.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
   begin
      Open_File_Editor (Kernel, File_Node.Name);
   end Open_File_Editor_On_File;

   ------------------------------------
   -- Open_File_Editor_On_Subprogram --
   ------------------------------------

   procedure Open_File_Editor_On_Subprogram
     (Kernel : Kernel_Handle; Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter)
   is
      File_Node : constant File_Access := File_Access
        (File_Set.Get (Gtk_Tree_Store (Model), Iter, File_Col));
      Subp_Node : constant Subprogram_Access := Subprogram_Access
        (Subprogram_Set.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
   begin
      Open_File_Editor
        (Kernel, File_Node.Name, Subp_Node.Line,
         Basic_Types.Visible_Column_Type (Subp_Node.Column));
   end Open_File_Editor_On_Subprogram;

   ----------------------------------------
   -- Add_Coverage_Annotations_From_Menu --
   ----------------------------------------

   procedure Add_Coverage_Annotations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Project_Node : Project_Access;
      File_Node    : Code_Analysis.File_Access;
   begin
      Project_Node := Get_Or_Create
        (Cont_N_Anal.Analysis.Projects,
         Project_Information (Cont_N_Anal.Context));
      File_Node := Get_Or_Create
        (Project_Node, File_Information (Cont_N_Anal.Context));
      Open_File_Editor (Get_Kernel (Cont_N_Anal.Context), File_Node.Name);
      Add_Coverage_Annotations
        (Get_Kernel (Cont_N_Anal.Context), File_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Coverage_Annotations_From_Menu;

   ------------------------------
   -- Add_Coverage_Annotations --
   ------------------------------

   procedure Add_Coverage_Annotations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access)
   is
      Line_Info  : Line_Information_Data;
   begin
      Line_Info  := new Line_Information_Array (File_Node.Lines'Range);

      for J in File_Node.Lines'Range loop
         if File_Node.Lines (J) /= Null_Line then
            Line_Info (J).Text := Line_Coverage_Info
              (File_Node.Lines (J).Analysis_Data.Coverage_Data,
               Binary_Coverage_Mode);
         else
            Line_Info (J).Text := new String'("-");
         end if;
      end loop;

      Create_Line_Information_Column (Kernel, File_Node.Name, Line_Info_Cst);
      Add_Line_Information (Kernel, File_Node.Name, Line_Info_Cst, Line_Info);
      Unchecked_Free (Line_Info);
   end Add_Coverage_Annotations;

   -------------------------------------------
   -- Remove_Coverage_Annotations_From_Menu --
   -------------------------------------------

   procedure Remove_Coverage_Annotations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Project_Node : Project_Access;
      File_Node    : Code_Analysis.File_Access;
   begin
      Project_Node := Get_Or_Create
        (Cont_N_Anal.Analysis.Projects,
         Project_Information (Cont_N_Anal.Context));
      File_Node := Get_Or_Create
        (Project_Node, File_Information (Cont_N_Anal.Context));
      Open_File_Editor (Get_Kernel (Cont_N_Anal.Context), File_Node.Name);
      Remove_Coverage_Annotations
        (Get_Kernel (Cont_N_Anal.Context), File_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Remove_Coverage_Annotations_From_Menu;

   ---------------------------------
   -- Remove_Coverage_Annotations --
   ---------------------------------

   procedure Remove_Coverage_Annotations
     (Kernel : Kernel_Handle;
      File_Node : Code_Analysis.File_Access) is
   begin
      Remove_Line_Information_Column (Kernel, File_Node.Name, Line_Icons_Cst);
      Remove_Line_Information_Column (Kernel, File_Node.Name, Line_Info_Cst);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Remove_Coverage_Annotations;

   -----------------------------------------------
   -- List_Uncovered_Lines_In_Project_From_Menu --
   -----------------------------------------------

   procedure List_Uncovered_Lines_In_Project_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Project_Node : Project_Access;
   begin
      Project_Node := Get_Or_Create
        (Cont_N_Anal.Analysis.Projects,
         Project_Information (Cont_N_Anal.Context));
      List_Uncovered_Lines_In_Project
        (Get_Kernel (Cont_N_Anal.Context), Project_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end List_Uncovered_Lines_In_Project_From_Menu;

   ---------------------------------------
   -- Clear_Project_Locations_From_Menu --
   ---------------------------------------

   procedure Clear_Project_Locations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Project_Node : Project_Access;
   begin
      Project_Node := Get_Or_Create
        (Cont_N_Anal.Analysis.Projects,
         Project_Information (Cont_N_Anal.Context));
      Clear_Project_Locations
        (Get_Kernel (Cont_N_Anal.Context), Project_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Clear_Project_Locations_From_Menu;

   --------------------------------------------
   -- List_Uncovered_Lines_In_File_From_Menu --
   --------------------------------------------

   procedure List_Uncovered_Lines_In_File_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Prj_Node  : constant Project_Access := Get_Or_Create
           (Cont_N_Anal.Analysis.Projects,
            Project_Information (Cont_N_Anal.Context));
      File_Node : constant Code_Analysis.File_Access := Get_Or_Create
        (Prj_Node, File_Information (Cont_N_Anal.Context));
   begin
      Open_File_Editor (Get_Kernel (Cont_N_Anal.Context), File_Node.Name);
      List_Uncovered_Lines_In_File
        (Get_Kernel (Cont_N_Anal.Context), File_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end List_Uncovered_Lines_In_File_From_Menu;

   ------------------------------------
   -- Clear_File_Locations_From_Menu --
   ------------------------------------

   procedure Clear_File_Locations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Prj_Node  : constant Project_Access := Get_Or_Create
           (Cont_N_Anal.Analysis.Projects,
            Project_Information (Cont_N_Anal.Context));
      File_Node : constant Code_Analysis.File_Access := Get_Or_Create
        (Prj_Node, File_Information (Cont_N_Anal.Context));
   begin
      Clear_File_Locations (Get_Kernel (Cont_N_Anal.Context), File_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Clear_File_Locations_From_Menu;

   -------------------------------------
   -- List_Uncovered_Lines_In_Project --
   -------------------------------------

   procedure List_Uncovered_Lines_In_Project
     (Kernel : Kernel_Handle; Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur  : File_Maps.Cursor := Project_Node.Files.First;
      Sort_Arr : Code_Analysis.File_Array
        (1 .. Integer (Project_Node.Files.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         if Sort_Arr (J).Analysis_Data.Coverage_Data /= null then
            List_Uncovered_Lines_In_File (Kernel, Sort_Arr (J));
         end if;
      end loop;
   end List_Uncovered_Lines_In_Project;

   -----------------------------
   -- Clear_Project_Locations --
   -----------------------------

   procedure Clear_Project_Locations
     (Kernel : Kernel_Handle; Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur : File_Maps.Cursor := Project_Node.Files.First;
   begin
      for J in 1 .. Integer (Project_Node.Files.Length) loop
         Clear_File_Locations (Kernel, Element (Map_Cur));
         Next (Map_Cur);
      end loop;
   end Clear_Project_Locations;

   ----------------------------------
   -- List_Uncovered_Lines_In_File --
   ----------------------------------

   procedure List_Uncovered_Lines_In_File
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access)
   is
      No_File_Added : Boolean := True;
   begin
      if File_Node.Analysis_Data.Coverage_Data.Status = Valid then
         for J in File_Node.Lines'Range loop
            if File_Node.Lines (J) /= Null_Line then
               if File_Node.Lines (J).Analysis_Data.Coverage_Data.Coverage
                 = 0 then
                  No_File_Added := False;
                  Insert_Location
                    (Kernel             => Kernel,
                     Category           => Coverage_Category,
                     File               => File_Node.Name,
                     Text               => File_Node.Lines (J).Contents.all,
                     Line               => J,
                     Column             => 1,
                     Highlight          => True,
                     Highlight_Category => Builder_Warnings_Style);
               end if;
            end if;
         end loop;

         if No_File_Added then
            GPS.Kernel.Console.Insert
              (Kernel, -"There is no uncovered line in " &
               Base_Name (File_Node.Name));
         end if;
      else
         GPS.Kernel.Console.Insert
           (Kernel, -"There is no Gcov information associated with " &
            Base_Name (File_Node.Name),
            Mode => GPS.Kernel.Console.Error);
      end if;
   end List_Uncovered_Lines_In_File;

   --------------------------
   -- Clear_File_Locations --
   --------------------------

   procedure Clear_File_Locations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access) is
   begin
      if File_Node.Analysis_Data.Coverage_Data.Status = Valid then
         Remove_Location_Category (Kernel, Coverage_Category, File_Node.Name);
      end if;
   end Clear_File_Locations;

   --------------------------------------------------
   -- List_Uncovered_Lines_In_Subprogram_From_Menu --
   --------------------------------------------------

   procedure List_Uncovered_Lines_In_Subprogram_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Cont_N_Anal.Context);
      Prj_Node  : constant Project_Access := Get_Or_Create
        (Cont_N_Anal.Analysis.Projects,
         Project_Information (Cont_N_Anal.Context));
      File_Node : constant Code_Analysis.File_Access := Get_Or_Create
        (Prj_Node, File_Information (Cont_N_Anal.Context));
      Subp_Node : constant Code_Analysis.Subprogram_Access := Get_Or_Create
        (File_Node, new String'(Entity_Name_Information
         (Cont_N_Anal.Context)));
   begin
      if File_Node.Analysis_Data.Coverage_Data.Status = Valid then
         for J in Subp_Node.Start .. Subp_Node.Stop loop
            if File_Node.Lines (J) /= Null_Line then
               if File_Node.Lines (J).Analysis_Data.Coverage_Data.Coverage
                 = 0 then
                  Insert_Location
                    (Kernel             => Kernel,
                     Category           => Coverage_Category,
                     File               => File_Node.Name,
                     Text               => File_Node.Lines (J).Contents.all,
                     Line               => J,
                     Column             => 1,
                     Highlight          => True,
                     Highlight_Category => Builder_Warnings_Style);
               end if;
            end if;
         end loop;
      else
         GPS.Kernel.Console.Insert
           (Kernel, -"There is no Gcov information associated with " &
            Base_Name (File_Node.Name),
            Mode => GPS.Kernel.Console.Error);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end List_Uncovered_Lines_In_Subprogram_From_Menu;

   ------------------------------------------
   -- Clear_Subprogram_Locations_From_Menu --
   ------------------------------------------

   procedure Clear_Subprogram_Locations_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Cont_N_Anal.Context);
      Prj_Node  : constant Project_Access := Get_Or_Create
        (Cont_N_Anal.Analysis.Projects,
         Project_Information (Cont_N_Anal.Context));
      File_Node : constant Code_Analysis.File_Access := Get_Or_Create
        (Prj_Node, File_Information (Cont_N_Anal.Context));
      Subp_Node : constant Code_Analysis.Subprogram_Access := Get_Or_Create
        (File_Node, new String'(Entity_Name_Information
         (Cont_N_Anal.Context)));
   begin
      if File_Node.Analysis_Data.Coverage_Data.Status = Valid then
         for J in Subp_Node.Start .. Subp_Node.Stop loop
            if File_Node.Lines (J) /= Null_Line then
               if File_Node.Lines (J).Analysis_Data.Coverage_Data.Coverage
                 = 0 then
                  Remove_Location_Category
                    (Kernel,
                     Coverage_Category,
                     File_Node.Name,
                     File_Node.Lines (J).Number);
               end if;
            end if;
         end loop;
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Clear_Subprogram_Locations_From_Menu;

   ---------------------------------
   -- Remove_Subprogram_From_Menu --
   ---------------------------------

   procedure Remove_Subprogram_From_Menu
      (Widget      : access Glib.Object.GObject_Record'Class;
       Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Prj_Node  : constant Project_Access := Get_Or_Create
        (Cont_N_Anal.Analysis.Projects,
         Project_Information (Cont_N_Anal.Context));
      File_Node : constant Code_Analysis.File_Access := Get_Or_Create
        (Prj_Node, File_Information (Cont_N_Anal.Context));
      Subp_Node : Code_Analysis.Subprogram_Access := Get_Or_Create
        (File_Node, new String'(Entity_Name_Information
         (Cont_N_Anal.Context)));
      Prj_Iter   : Gtk_Tree_Iter;
      File_Iter  : Gtk_Tree_Iter;
      Subp_Iter  : Gtk_Tree_Iter;
   begin
      if Cont_N_Anal.Analysis.View = null then
         Show_Analysis_Report
           (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal, False);
      end if;

      --  Update project coverage information
      Prj_Node.Analysis_Data.Coverage_Data.Coverage :=
        Prj_Node.Analysis_Data.Coverage_Data.Coverage -
          Subp_Node.Analysis_Data.Coverage_Data.Coverage;
      Project_Coverage
        (Prj_Node.Analysis_Data.Coverage_Data.all).Children :=
        Project_Coverage
          (Prj_Node.Analysis_Data.Coverage_Data.all).Children -
          Subprogram_Coverage
            (Subp_Node.Analysis_Data.Coverage_Data.all).Children;

      if Project_Coverage
        (Prj_Node.Analysis_Data.Coverage_Data.all).Children = 0 then
         --  No more children means no more usable coverage data
         Unchecked_Free (Prj_Node.Analysis_Data.Coverage_Data);
      end if;

      --  Update file coverage information
      File_Node.Analysis_Data.Coverage_Data.Coverage :=
        File_Node.Analysis_Data.Coverage_Data.Coverage -
          Subp_Node.Analysis_Data.Coverage_Data.Coverage;
      Node_Coverage (File_Node.Analysis_Data.Coverage_Data.all).Children :=
        Node_Coverage (File_Node.Analysis_Data.Coverage_Data.all).Children -
        Subprogram_Coverage
          (Subp_Node.Analysis_Data.Coverage_Data.all).Children;

      if Node_Coverage
        (File_Node.Analysis_Data.Coverage_Data.all).Children = 0 then
         --  No more children means no more usable coverage data
         Unchecked_Free (File_Node.Analysis_Data.Coverage_Data);
      end if;

      Subp_Iter := Get_Iter_From_Context
        (Cont_N_Anal.Context, Cont_N_Anal.Analysis.View.Model);
      File_Iter := Parent (Cont_N_Anal.Analysis.View.Model, Subp_Iter);
      Fill_Iter (Cont_N_Anal.Analysis.View.Model, File_Iter,
                 File_Node.Analysis_Data, Binary_Coverage_Mode);
      Prj_Iter  := Parent (Cont_N_Anal.Analysis.View.Model, File_Iter);
      Fill_Iter (Cont_N_Anal.Analysis.View.Model, Prj_Iter,
                 Prj_Node.Analysis_Data, Binary_Coverage_Mode);
      --  Removes Subp_Iter from the report
      Remove (Cont_N_Anal.Analysis.View.Model, Subp_Iter);
      --  Removes Subp_Iter from its container
      Subprogram_Maps.Delete (File_Node.Subprograms, Subp_Node.Name.all);
      --  Free Subprogram analysis node
      Free_Subprogram (Subp_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Remove_Subprogram_From_Menu;

   ---------------------------
   -- Remove_File_From_Menu --
   ---------------------------

   procedure Remove_File_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Prj_Node  : constant Project_Access := Get_Or_Create
        (Cont_N_Anal.Analysis.Projects,
         Project_Information (Cont_N_Anal.Context));
      File_Node : Code_Analysis.File_Access := Get_Or_Create
        (Prj_Node, File_Information (Cont_N_Anal.Context));
      File_Iter : Gtk_Tree_Iter;
      Prj_Iter  : Gtk_Tree_Iter;
   begin
      if Cont_N_Anal.Analysis.View = null then
         Show_Analysis_Report
           (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal, False);
      end if;

      --  Update project coverage information
      Prj_Node.Analysis_Data.Coverage_Data.Coverage :=
        Prj_Node.Analysis_Data.Coverage_Data.Coverage -
          File_Node.Analysis_Data.Coverage_Data.Coverage;
      Project_Coverage
        (Prj_Node.Analysis_Data.Coverage_Data.all).Children :=
        Project_Coverage
          (Prj_Node.Analysis_Data.Coverage_Data.all).Children -
          Node_Coverage
            (File_Node.Analysis_Data.Coverage_Data.all).Children;

      if Project_Coverage
        (Prj_Node.Analysis_Data.Coverage_Data.all).Children = 0 then
         --  No more children means no more usable coverage data
         Unchecked_Free (Prj_Node.Analysis_Data.Coverage_Data);
      end if;

      File_Iter := Get_Iter_From_Context
        (Cont_N_Anal.Context, Cont_N_Anal.Analysis.View.Model);
      Prj_Iter  := Parent (Cont_N_Anal.Analysis.View.Model, File_Iter);
      Fill_Iter (Cont_N_Anal.Analysis.View.Model, Prj_Iter,
                 Prj_Node.Analysis_Data, Binary_Coverage_Mode);
      --  Removes File_Iter from the report
      Remove (Cont_N_Anal.Analysis.View.Model, File_Iter);
      --  Removes File_Iter from its container
      File_Maps.Delete (Prj_Node.Files, File_Node.Name);
      --  Free the file analysis node
      Free_File (File_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Remove_File_From_Menu;

   ------------------------------
   -- Remove_Project_From_Menu --
   ------------------------------

   procedure Remove_Project_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Iter     : Gtk_Tree_Iter;
      Prj_Node : Project_Access :=
                   Get_Or_Create (Cont_N_Anal.Analysis.Projects,
                                  Project_Information (Cont_N_Anal.Context));
   begin
      if Cont_N_Anal.Analysis.View = null then
         Show_Analysis_Report
           (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal, False);
      end if;

      Iter := Get_Iter_From_Context
        (Cont_N_Anal.Context, Cont_N_Anal.Analysis.View.Model);
      --  Removes it from the report
      Remove (Cont_N_Anal.Analysis.View.Model, Iter);
      --  Removes it from its container
      Project_Maps.Delete
        (Cont_N_Anal.Analysis.Projects.all, Prj_Node.Name);
      --  Free it
      Free_Project (Prj_Node);

      if Project_Maps.Length (Cont_N_Anal.Analysis.Projects.all) = 0 then
         Show_Empty_Analysis_Report
           (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal.Analysis);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Remove_Project_From_Menu;

   ----------------------------
   -- Expand_All_From_Report --
   ----------------------------

   procedure Expand_All_From_Report (Object : access Gtk_Widget_Record'Class)
   is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
   begin
      Expand_All (View.Tree);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Expand_All_From_Report;

   ------------------------------
   -- Collapse_All_From_Report --
   ------------------------------

   procedure Collapse_All_From_Report (Object : access Gtk_Widget_Record'Class)
   is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
   begin
      Collapse_All (View.Tree);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Collapse_All_From_Report;

   --------------------
   -- Show_Full_Tree --
   --------------------

   procedure Show_Full_Tree (Object : access Gtk_Widget_Record'Class) is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter : Gtk_Tree_Iter := Get_Iter_First (View.Model);
      Path : Gtk_Tree_Path;
   begin
      Clear (View.Model);
      Fill_Iter
        (View.Model, Iter, View.Projects, Binary_Coverage_Mode, View.Icons);
      Iter := Get_Iter_First (View.Model);
      Path := Get_Path (View.Model, Iter);
      Collapse_All (View.Tree);
      Expand_To_Path (View.Tree, Path);
      Select_Path (Get_Selection (View.Tree), Path);
      Path_Free (Path);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Full_Tree;

   -----------------------------
   -- Show_Flat_List_Of_Files --
   -----------------------------

   procedure Show_Flat_List_Of_Files (Object : access Gtk_Widget_Record'Class)
   is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter : Gtk_Tree_Iter := Get_Iter_First (View.Model);
   begin
      Clear (View.Model);
      Fill_Iter_With_Files
        (View.Model, Iter, View.Projects, Binary_Coverage_Mode, View.Icons);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Flat_List_Of_Files;

   -----------------------------------
   -- Show_Flat_List_Of_Subprograms --
   -----------------------------------

   procedure Show_Flat_List_Of_Subprograms
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter : Gtk_Tree_Iter := Get_Iter_First (View.Model);
   begin
      Clear (View.Model);
      Fill_Iter_With_Subprograms
        (View.Model, Iter, View.Projects, Binary_Coverage_Mode, View.Icons);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Flat_List_Of_Subprograms;

   ------------------
   -- Context_Func --
   ------------------

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk_Event;
      Menu         : Gtk_Menu)
   is
      pragma Unreferenced (Kernel, Event_Widget);
      View      : constant Code_Analysis_View := Code_Analysis_View (Object);
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Path      : Gtk_Tree_Path;
      Prj_Node  : Code_Analysis.Project_Access;
      File_Node : Code_Analysis.File_Access;
      Column    : Gtk_Tree_View_Column;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Item      : Gtk_Menu_Item;
      Iter      : Gtk_Tree_Iter := Get_Iter_First (View.Model);
   begin

      Get_Path_At_Pos (View.Tree, Gint (X), Gint (Y), Path, Column,
                       Buffer_X, Buffer_Y, Row_Found);

      ---------------------------------------------------------------
      --  Insert Report of Analysis # specific contextual entries  --
      ---------------------------------------------------------------

      Gtk_New (Item, -"Show flat list of files");
      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Show_Flat_List_Of_Files'Access, View);
      Append (Menu, Item);
      Gtk_New (Item, -"Show flat list of subprograms");
      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Show_Flat_List_Of_Subprograms'Access, View);
      Append (Menu, Item);

      if not Has_Child (View.Model, Iter) then
         Gtk_New (Item, -"Show full tree");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate, Show_Full_Tree'Access, View);
         Append (Menu, Item);
      else
         Gtk_New (Item, -"Expand all");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Expand_All_From_Report'Access, View);
         Append (Menu, Item);
         Gtk_New (Item, -"Collapse all");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Collapse_All_From_Report'Access, View);
         Append (Menu, Item);
      end if;

      ----------------------------------
      --  Set up context information  --
      ----------------------------------

      if Path /= null then
         Gtk_New (Item);
         Append (Menu, Item);
         Select_Path (Get_Selection (View.Tree), Path);
         Iter := Get_Iter (Gtk_Tree_Model (View.Model), Path);

         declare
            Node : constant Node_Access := Code_Analysis.Node_Access
              (Node_Set.Get (Gtk_Tree_Store (View.Model), Iter, Node_Col));
         begin
            if Node.all in Code_Analysis.Project'Class then
               --  So we are on a project node
               --  Context receive project information
               Set_File_Information
                 (Context, Project => Project_Access (Node).Name);
            elsif Node.all in Code_Analysis.File'Class then
               --  So we are on a file node
               --  Context receive project and file information
               Prj_Node := Project_Access
                 (Project_Set.Get
                    (Gtk_Tree_Store (View.Model), Iter, Prj_Col));
               Set_File_Information
                 (Context,
                  File    => Code_Analysis.File_Access (Node).Name,
                  Project => Prj_Node.Name);
            elsif Node.all in Code_Analysis.Subprogram'Class then
               --  So we are on a subprogram node
               --  Context receive project, file and entity information
               File_Node := Code_Analysis.File_Access
                 (File_Set.Get (Gtk_Tree_Store (View.Model),
                  Iter, File_Col));
               Prj_Node  := Project_Access
                 (Project_Set.Get (Gtk_Tree_Store (View.Model),
                  Iter, Prj_Col));
               Set_File_Information (Context, File_Node.Name, Prj_Node.Name);
               Set_Entity_Information
                 (Context, Subprogram_Access (Node).Name.all);
            end if;
         end;
      end if;
   end Context_Func;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Has_Coverage_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      if Has_Project_Information (Context)
        or else Has_File_Information (Context)
      then
         return True;

      elsif Has_Entity_Name_Information (Context) then
         Entity := Get_Entity (Context);

         return (Entity /= null
                 and then Is_Subprogram (Entity))
           or else Get_Creator (Context) = Abstract_Module_ID
           (Code_Analysis_Module_ID);
      end if;

      return False;
   end Filter_Matches_Primitive;

   --------------------
   -- Append_To_Menu --
   --------------------

   procedure Append_To_Menu
     (Factory : access Code_Analysis_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      use Code_Analysis_Instances;
      pragma Unreferenced (Factory, Object);
      Cont_N_Anal  : Context_And_Analysis;
      Project_Node : Project_Access;
      Submenu      : Gtk_Menu;
      Item         : Gtk_Menu_Item;
      Cur          : Cursor := Code_Analysis_Module_ID.Analyzes.First;
   begin
      Cont_N_Anal.Context := Context;

      loop
         exit when Cur = No_Element;

         Cont_N_Anal.Analysis := Element (Cur);
         Project_Node         := Get_Or_Create
           (Cont_N_Anal.Analysis.Projects,
            Project_Information (Cont_N_Anal.Context));

         if Code_Analysis_Module_ID.Analyzes.Length > 1 then
            Gtk_New (Item, -(Cont_N_Anal.Analysis.Name.all));
            Append (Menu, Item);
            Gtk_New (Submenu);
            Set_Submenu (Item, Submenu);
            Set_Sensitive (Item, True);
            Append_To_Contextual_Submenu (Cont_N_Anal, Submenu, Project_Node);
         else
            Append_To_Contextual_Submenu (Cont_N_Anal, Menu, Project_Node);
         end if;

         Next (Cur);
      end loop;
   end Append_To_Menu;

   ----------------------------------
   -- Append_To_Contextual_Submenu --
   ----------------------------------

   procedure Append_To_Contextual_Submenu
     (Cont_N_Anal  : Context_And_Analysis;
      Submenu      : access Gtk_Menu_Record'Class;
      Project_Node : Project_Access)
   is
      Item : Gtk_Menu_Item;
   begin
      if Has_File_Information (Cont_N_Anal.Context) then
         declare
            File_Node : constant Code_Analysis.File_Access := Get_Or_Create
              (Project_Node, File_Information (Cont_N_Anal.Context));
         begin
            if Has_Entity_Name_Information (Cont_N_Anal.Context) then
               declare
                  Entity    : constant Entities.Entity_Information :=
                                Get_Entity (Cont_N_Anal.Context);
                  Subp_Node : Subprogram_Access;
               begin
                  if (Entity /= null
                      and then Is_Subprogram (Entity))
                    or else  Get_Creator (Cont_N_Anal.Context) =
                    Abstract_Module_ID (Code_Analysis_Module_ID)
                  then
                     --  So we have a subprogram information
                     Subp_Node := Get_Or_Create
                       (File_Node, new String'(Entity_Name_Information
                        (Cont_N_Anal.Context)));
                     Append_Subprogram_Menu_Entries
                       (Cont_N_Anal, Submenu, Subp_Node);
                  else
                     Append_File_Menu_Entries
                       (Cont_N_Anal, Submenu, File_Node);
                  end if;
               end;
            else
               Append_File_Menu_Entries (Cont_N_Anal, Submenu, File_Node);
            end if;
         end;
      else
         Append_Project_Menu_Entries (Cont_N_Anal, Submenu, Project_Node);
      end if;

      if Get_Creator (Cont_N_Anal.Context) /=
        Abstract_Module_ID (Code_Analysis_Module_ID) then
         Gtk_New (Item);
         Append (Submenu, Item);
         Append_Show_Analysis_Report_To_Menu (Cont_N_Anal, Submenu);
      end if;
   end Append_To_Contextual_Submenu;

   ------------------------------------
   -- Append_Subprogram_Menu_Entries --
   ------------------------------------

   procedure Append_Subprogram_Menu_Entries
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class;
      Subp_Node   : Subprogram_Access)
   is
      Item : Gtk_Menu_Item;
   begin
      if Subp_Node.Analysis_Data.Coverage_Data /= null then
         Gtk_New (Item, -"List uncovered lines");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (List_Uncovered_Lines_In_Subprogram_From_Menu'Access),
            Cont_N_Anal);
         Gtk_New (Item, -"Clear listed locations");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Clear_Subprogram_Locations_From_Menu'Access),
            Cont_N_Anal);
         Gtk_New (Item, -"Remove data of " & Entity_Name_Information
                  (Cont_N_Anal.Context));
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Remove_Subprogram_From_Menu'Access), Cont_N_Anal);
      else
         Gtk_New (Item, -"Load data for " &
                  Locale_Base_Name (File_Information (Cont_N_Anal.Context)));
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Add_Gcov_File_Info_From_Menu'Access), Cont_N_Anal);
      end if;
   end Append_Subprogram_Menu_Entries;

   ------------------------------
   -- Append_File_Menu_Entries --
   ------------------------------

   procedure Append_File_Menu_Entries
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class;
      File_Node   : Code_Analysis.File_Access)
   is
      Item : Gtk_Menu_Item;
   begin
      if File_Node.Analysis_Data.Coverage_Data /= null and then
        File_Node.Analysis_Data.Coverage_Data.Status = Valid then
         Gtk_New (Item, -"Reload data for " &
                  Locale_Base_Name (File_Information (Cont_N_Anal.Context)));
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Add_Gcov_File_Info_From_Menu'Access), Cont_N_Anal);
         Gtk_New (Item, -"Add coverage annotations");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Add_Coverage_Annotations_From_Menu'Access), Cont_N_Anal);
         Gtk_New (Item, -"Remove coverage annotations");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Remove_Coverage_Annotations_From_Menu'Access), Cont_N_Anal);
         Gtk_New (Item, -"List uncovered lines");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (List_Uncovered_Lines_In_File_From_Menu'Access),
            Cont_N_Anal);
         Gtk_New (Item, -"Clear listed locations");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Clear_File_Locations_From_Menu'Access),
            Cont_N_Anal);
         Gtk_New (Item, -"Remove data of " &
                  Base_Name (File_Information (Cont_N_Anal.Context)));
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Remove_File_From_Menu'Access), Cont_N_Anal);
      else
         Gtk_New (Item, -"Load data for " &
                  Locale_Base_Name (File_Information (Cont_N_Anal.Context)));
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Add_Gcov_File_Info_From_Menu'Access), Cont_N_Anal);
      end if;
   end Append_File_Menu_Entries;

   ---------------------------------
   -- Append_Project_Menu_Entries --
   ---------------------------------

   procedure Append_Project_Menu_Entries
     (Cont_N_Anal  : Context_And_Analysis;
      Submenu      : access Gtk_Menu_Record'Class;
      Project_Node : Project_Access)
   is
      Item : Gtk_Menu_Item;
   begin
      if Project_Node.Analysis_Data.Coverage_Data /= null then
         Gtk_New (Item, -"Reload data for project " &
                  Project_Name (Project_Information (Cont_N_Anal.Context)));
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Add_Gcov_Project_Info_From_Menu'Access), Cont_N_Anal);
         Gtk_New (Item, -"List uncovered lines");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (List_Uncovered_Lines_In_Project_From_Menu'Access),
            Cont_N_Anal);
         Gtk_New (Item, -"Clear listed locations");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Clear_Project_Locations_From_Menu'Access),
            Cont_N_Anal);
         Gtk_New (Item, -"Remove data of project " &
                  Project_Name (Project_Information (Cont_N_Anal.Context)));
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Remove_Project_From_Menu'Access), Cont_N_Anal);
      else
         Gtk_New (Item, -"Load data for project " &
                  Project_Name (Project_Information (Cont_N_Anal.Context)));
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Add_Gcov_Project_Info_From_Menu'Access), Cont_N_Anal);
      end if;
   end Append_Project_Menu_Entries;

   ------------------
   -- Menu entries --
   ------------------

   -------------------
   -- Check_Context --
   -------------------

   function Check_Context
     (Kernel        : Kernel_Handle;
      Given_Context : Selection_Context) return Selection_Context
   is
      Checked_Context : Selection_Context;
   begin
      if Given_Context = No_Context then
         Checked_Context := Get_Current_Context (Kernel);
      else
         Checked_Context := Given_Context;
      end if;

      if not Has_Project_Information (Checked_Context) then
         if Has_File_Information (Checked_Context) then
            declare
               Prj_Info  : Project_Type;
               File_Info : constant VFS.Virtual_File :=
                             File_Information (Checked_Context);
            begin
               Prj_Info := Get_Project_From_File
                 (Get_Registry (Kernel).all,
                  File_Info);
               Set_File_Information (Checked_Context, File_Info, Prj_Info);
            end;
         else
            Set_File_Information
              (Checked_Context,
               Project => Get_Project (Kernel));
         end if;
      end if;

      return Checked_Context;
   end Check_Context;

   --------------------------------
   -- Dynamic_Tools_Menu_Factory --
   --------------------------------

   procedure Dynamic_Tools_Menu_Factory
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      use Code_Analysis_Instances;
      Cont_N_Anal : Context_And_Analysis;
      Prj_Node    : Project_Access;
      Submenu     : Gtk_Menu;
      Item        : Gtk_Menu_Item;
      Cur         : Cursor := Code_Analysis_Module_ID.Analyzes.First;
   begin
      Cont_N_Anal.Context := Check_Context (Kernel_Handle (Kernel), Context);

      if Cur = No_Element then
         Cont_N_Anal.Analysis := null;
         Append_Load_Data_For_All_Projects (Cont_N_Anal, Menu);
         Gtk_New (Item, -"Load data for project " &
                  Project_Name (Project_Information (Cont_N_Anal.Context)));
         Append (Menu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Add_Gcov_Project_Info_From_Menu'Access), Cont_N_Anal);

         if Has_File_Information (Cont_N_Anal.Context) then
            Gtk_New
              (Item, -"Load data for " &
               Locale_Base_Name (File_Information (Cont_N_Anal.Context)));
            Append (Menu, Item);
            Context_And_Analysis_CB.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               Context_And_Analysis_CB.To_Marshaller
                 (Add_Gcov_File_Info_From_Menu'Access), Cont_N_Anal);
         end if;

         Gtk_New (Item);
         Append (Menu, Item);
      else
         loop
            exit when Cur = No_Element;
            Cont_N_Anal.Analysis := Element (Cur);
            Next (Cur);
            Prj_Node             := Get_Or_Create
              (Cont_N_Anal.Analysis.Projects,
               Project_Information (Cont_N_Anal.Context));

            if Code_Analysis_Module_ID.Analyzes.Length > 1 then
               Gtk_New (Item, -(Cont_N_Anal.Analysis.Name.all));
               Append (Menu, Item);
               Gtk_New (Submenu);
               Set_Submenu (Item, Submenu);
               Set_Sensitive (Item, True);
               Append_To_Submenu (Cont_N_Anal, Submenu, Prj_Node);
            else
               Append_To_Submenu (Cont_N_Anal, Menu, Prj_Node);
               Gtk_New (Item);
               Append (Menu, Item);
            end if;
         end loop;
      end if;

      Gtk_New (Item, -"Create code analysis");
      Append (Menu, Item);
      Gtkada.Handlers.Widget_Callback.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Create_Analysis_From_Menu'Access);

      if Code_Analysis_Module_ID.Analyzes.Length > 1 then
         Gtk_New (Item, -"Remove all analysis");
         Append (Menu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Destroy_All_Analyzes_From_Menu'Access), Cont_N_Anal);
      end if;
   end Dynamic_Tools_Menu_Factory;

   --------------------------------
   -- Dynamic_Views_Menu_Factory --
   --------------------------------

   procedure Dynamic_Views_Menu_Factory
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Kernel);
      use Code_Analysis_Instances;
      Cur      : Cursor := Code_Analysis_Module_ID.Analyzes.First;
      Item     : Gtk_Menu_Item;
      Analysis : Code_Analysis_Instance;
   begin
      if Cur = No_Element then
         --  So there's currently no instances
         Gtk_New (Item, -"Show Report of Analysis");
         Append (Menu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Show_Empty_Analysis_Report_From_Menu'Access),
            Context_And_Analysis'(Context, Analysis));
         return;
      end if;

      loop
         exit when Cur = No_Element;
         Analysis := Element (Cur);
         Append_Show_Analysis_Report_To_Menu
           (Context_And_Analysis'(Context, Analysis), Menu);
         Next (Cur);
      end loop;
   end Dynamic_Views_Menu_Factory;

   -----------------------
   -- Append_To_Submenu --
   -----------------------

   procedure Append_To_Submenu
     (Cont_N_Anal  : Context_And_Analysis;
      Submenu      : access Gtk_Menu_Record'Class;
      Project_Node : Project_Access)
   is
      Item     : Gtk_Menu_Item;
   begin
      Append_Show_Analysis_Report_To_Menu (Cont_N_Anal, Submenu);
      Gtk_New (Item, -"Remove " & Cont_N_Anal.Analysis.Name.all);
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Destroy_Analysis_From_Menu'Access), Cont_N_Anal);
      Gtk_New (Item);
      Append (Submenu, Item);
      Append_Load_Data_For_All_Projects (Cont_N_Anal, Submenu);

      if First_Project_With_Coverage_Data (Cont_N_Anal.Analysis) /=
        No_Project then
         Gtk_New (Item, -"List uncovered lines in all projects");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (List_Uncovered_Lines_In_All_Projects_From_Menu'Access),
            Cont_N_Anal);
         Gtk_New (Item, -"Clear all project listed locations");
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Clear_All_Project_Locations_From_Menu'Access),
            Cont_N_Anal);
      end if;

      Append_Project_Menu_Entries (Cont_N_Anal, Submenu, Project_Node);

      if Has_File_Information (Cont_N_Anal.Context) then
         declare
            File_Node : constant Code_Analysis.File_Access := Get_Or_Create
              (Project_Node, File_Information (Cont_N_Anal.Context));
         begin
            Gtk_New (Item);
            Append (Submenu, Item);
            Append_File_Menu_Entries (Cont_N_Anal, Submenu, File_Node);
         end;
      end if;
   end Append_To_Submenu;

   -----------------------------------------
   -- Append_Show_Analysis_Report_To_Menu --
   -----------------------------------------

   procedure Append_Show_Analysis_Report_To_Menu
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class)
   is
      Item     : Gtk_Menu_Item;
   begin
      Gtk_New (Item, -"Show Report of " &
               Cont_N_Anal.Analysis.Name.all);
      Append (Menu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Show_Analysis_Report_From_Menu'Access), Cont_N_Anal);
   end Append_Show_Analysis_Report_To_Menu;

   ---------------------------------------
   -- Append_Load_Data_For_All_Projects --
   ---------------------------------------

   procedure Append_Load_Data_For_All_Projects
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class)
   is
      Cont_N_Anal_Root_Prj : Context_And_Analysis;
      Item                 : Gtk_Menu_Item;
   begin
      Cont_N_Anal_Root_Prj.Analysis := Cont_N_Anal.Analysis;
      Cont_N_Anal_Root_Prj.Context
        := Get_Current_Context (Get_Kernel (Cont_N_Anal.Context));
      --  Have a new context, in order to modify it independently
      Set_File_Information
        (Cont_N_Anal_Root_Prj.Context,
         Project => Get_Project (Get_Kernel (Cont_N_Anal.Context)));
      Gtk_New (Item, -"Load data for all projects");
      Append (Menu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Add_All_Gcov_Project_Info_From_Menu'Access),
         Cont_N_Anal_Root_Prj);
   end Append_Load_Data_For_All_Projects;

   ---------------------------
   -- Less_Case_Insensitive --
   ---------------------------

   function Less_Case_Insensitive
     (Left, Right : Code_Analysis_Instance) return Boolean is
   begin

      if Left.Name = null then
         raise Miss_Analysis_Name with "Property_Left.Instance_Name is null";
      end if;

      if Right.Name = null then
         raise Miss_Analysis_Name with "Property_Right.Instance_Name is null";
      end if;

      return Ada.Strings.Less_Case_Insensitive
        (Left.Name.all, Right.Name.all);
   end Less_Case_Insensitive;

   ----------------------------
   -- Equal_Case_Insensitive --
   ----------------------------

   function Equal_Case_Insensitive
     (Left, Right : Code_Analysis_Instance) return Boolean is
   begin

      if Left.Name = null then
         raise Miss_Analysis_Name with "Property_Left.Instance_Name is null";
      end if;

      if Right.Name = null then
         raise Miss_Analysis_Name with "Property_Right.Instance_Name is null";
      end if;

      return Ada.Strings.Equal_Case_Insensitive
        (Left.Name.all, Right.Name.all);
   end Equal_Case_Insensitive;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Contextual_Menu     : Code_Analysis_Contextual_Menu_Access;
      Code_Analysis_Class : constant Class_Type
        := New_Class (Kernel, Code_Analysis_Cst_Str);
   begin
      Binary_Coverage_Mode := Active (Binary_Coverage_Trace);
      Code_Analysis_Module_ID := new Code_Analysis_Module_ID_Record;
      Code_Analysis_Module_ID.Class  := Code_Analysis_Class;
      Contextual_Menu := new Code_Analysis_Contextual_Menu;
      Register_Module
        (Module      => Code_Analysis_Module_ID,
         Kernel      => Kernel,
         Module_Name => Code_Analysis_Cst_Str);
      Register_Contextual_Submenu
        (Kernel      => Kernel,
         Name        => -"Coverage",
         Filter      => new Has_Coverage_Filter,
         Submenu     => Submenu_Factory (Contextual_Menu));
      Register_Dynamic_Menu
        (Kernel      => Kernel,
         Parent_Path => '/' & (-"Tools"),
         Text        => -"Covera_ge",
         Ref_Item    => -"Documentation",
         Add_Before  => False,
         Factory     => Dynamic_Tools_Menu_Factory'Access);
      Register_Dynamic_Menu
        (Kernel      => Kernel,
         Parent_Path => '/' & (-"Tools" & ('/' & (-"Views"))),
         Text        => -"Covera_ge",
         Ref_Item    => -"Clipboard",
         Add_Before  => False,
         Factory     => Dynamic_Views_Menu_Factory'Access);
      Add_Hook
        (Kernel  => Kernel,
         Hook    => Project_Changing_Hook,
         Func    =>
           Wrapper (Destroy_All_Analyzes_From_Project_Changing_Hook'Access),
         Name    => "destroy_all_code_analysis");
      Register_Command
        (Kernel, Constructor_Method,
         Class   => Code_Analysis_Class,
         Handler => Shell_CodeAnalysis_Constructor'Access);
      Register_Command
        (Kernel, "get",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Code_Analysis_Class,
         Handler       => Shell_Get_Command'Access,
         Static_Method => True);
      Register_Command
        (Kernel, "add_all_gcov_project_info",
         Class   => Code_Analysis_Class,
         Handler => Add_All_Gcov_Project_Info_From_Shell'Access);
      Register_Command
        (Kernel, "add_gcov_project_info",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Code_Analysis_Class,
         Handler      => Add_Gcov_Project_Info_From_Shell'Access);
      Register_Command
        (Kernel, "add_gcov_file_info",
         Minimum_Args => 2,
         Maximum_Args => 2,
         Class        => Code_Analysis_Class,
         Handler      => Add_Gcov_File_Info_From_Shell'Access);
      Register_Command
        (Kernel, "list_uncovered_lines",
         Class   => Code_Analysis_Class,
         Handler => List_Uncovered_Lines_In_All_Projects_From_Shell'Access);
      Register_Command
        (Kernel, "clear_locations",
         Class   => Code_Analysis_Class,
         Handler =>
           Clear_All_Project_Locations_From_Shell'Access);
      Register_Command
        (Kernel, "show_analysis_report",
         Class   => Code_Analysis_Class,
         Handler => Show_Analysis_Report_From_Shell'Access);
   end Register_Module;

end Code_Analysis_Module;
