-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2006-2008, AdaCore                  --
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
with GNAT.Strings;
with GNATCOLL.Scripts;                           use GNATCOLL.Scripts;
with GNATCOLL.Traces;
with Glib;                                   use Glib;
with Glib.Object;                            use Glib.Object;
with Glib.Xml_Int;                           use Glib.Xml_Int;
with Gtk.Button;
with Gtk.Enums;                              use Gtk.Enums;
with Gtk.Handlers;                           use Gtk.Handlers;
with Gtk.Menu;                               use Gtk.Menu;
with Gtk.Menu_Item;                          use Gtk.Menu_Item;
with Gtk.Tree_Selection;                     use Gtk.Tree_Selection;
with Gtk.Tree_View;                          use Gtk.Tree_View;
with Gtk.Tree_Model;                         use Gtk.Tree_Model;
with Gtk.Tree_Store;                         use Gtk.Tree_Store;
with Gtk.Tree_View_Column;                   use Gtk.Tree_View_Column;
with Gtk.Widget;                             use Gtk.Widget;
with Gtk.Box;                                use Gtk.Box;
with Gtk.Object;                             use Gtk.Object;
with Gtk.Label;                              use Gtk.Label;
with Gtkada.Dialogs;                         use Gtkada.Dialogs;
with Gtkada.Handlers;                        use Gtkada.Handlers;
with Gtkada.MDI;                             use Gtkada.MDI;
with GPS.Kernel.Console;
with GPS.Intl;                               use GPS.Intl;
with GPS.Kernel;                             use GPS.Kernel;
with GPS.Kernel.MDI;                         use GPS.Kernel.MDI;
with GPS.Kernel.Contexts;                    use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                       use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;                     use GPS.Kernel.Modules;
with GPS.Kernel.Project;                     use GPS.Kernel.Project;
with GPS.Kernel.Scripts;                     use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;              use GPS.Kernel.Standard_Hooks;
with GPS.Location_View;                      use GPS.Location_View;
with Projects.Registry;                      use Projects.Registry;
with Projects;                               use Projects;
with VFS;                                    use VFS;
with Entities;                               use Entities;
with Traces;                                 use Traces;
with Code_Coverage;                          use Code_Coverage;
with Code_Analysis;                          use Code_Analysis;
with Code_Analysis_GUI;                      use Code_Analysis_GUI;
with Code_Analysis_XML;                      use Code_Analysis_XML;
with Code_Analysis_Tree_Model;               use Code_Analysis_Tree_Model;
with Coverage_GUI;                           use Coverage_GUI;

package body Code_Analysis_Module is

   Src_File_Cst : aliased constant String := "src";
   --  Constant String that represents the name of the source file parameter
   --  of the GPS.CodeAnalysis.add_gcov_file_info subprogram.
   Cov_File_Cst : aliased constant String := "cov";
   --  Constant String that represents the name of the .gcov file parameter
   --  of the GPS.CodeAnalysis.add_gcov_file_info subprogram.
   Prj_File_Cst : aliased constant String := "prj";
   --  Constant String that represents the name of the .gpr file parameter
   --  of the GPS.CodeAnalysis.add_gcov_project_info subprogram.
   Xml_File_Cst : aliased constant String := "xml";
   --  Constant String that represents a name of the xml file to dump to in
   --  parameters of the GPS.CodeAnalysis.dump_to_file command.
   Ana_Name_Cst : aliased constant String := "name";
   --  Constant String that represents a name of Analysis_Instance in parameter
   --  of the GPS.CodeAnalysis.get command.

   Single_Analysis_Trace : constant Debug_Handle :=
                         Create ("SINGLE_ANALYSIS_MODE", GNATCOLL.Traces.On);
   Single_Analysis_Mode  : Boolean;
   --  Boolean that allows to determine wether we should display only one
   --  analysis at a time or if we can display more, if true the user wont be
   --  able to create more than one analysis structure.

   package Kernel_Return_Cb is new User_Return_Callback
     (Gtk.Widget.Gtk_Widget_Record, Boolean, Kernel_Handle);

   ------------------------
   -- Analysis instances --
   ------------------------

   type Code_Analysis_Instance_Record is record
      Instances : GNATCOLL.Scripts.Instance_List_Access;
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

   function Less (Left, Right : Code_Analysis_Instance) return Boolean;
   function Equal (Left, Right : Code_Analysis_Instance) return Boolean;
   --  Use the Code_Analysis_Instance.Date to perform the test

   package Code_Analysis_Instances is new Indefinite_Ordered_Sets
     (Element_Type => Code_Analysis_Instance, "<" => Less, "=" => Equal);
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
   --  Return the Gtk_Tree_Iter of the Gtk_Tree_Store corresponding to the
   --  contextual elements (project, file, subprogram) of the coverage report.

   ---------------------
   -- Contextual menu --
   ---------------------

   type Code_Analysis_Contextual_Menu is new
     Submenu_Factory_Record with null record;

   type Code_Analysis_Contextual_Menu_Access is access all
     Code_Analysis_Contextual_Menu;

   procedure Append_To_Menu
     (Factory : access Code_Analysis_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Determine wether we add entries directly in the contextual menu, or in
   --  a generated submenu. Submenus are created if many instances are loaded.

   procedure Append_To_Contextual_Submenu
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class);
   --  Allow to fill the given Submenu with coverage entries in the appropriate
   --  order for a contextual menu.

   procedure Append_To_Submenu
     (Cont_N_Anal  : Context_And_Analysis;
      Submenu      : access Gtk_Menu_Record'Class);
   --  Allow to fill the given Submenu, in the appropriate order for Tools
   --  menu.

   procedure Append_Show_Analysis_Report_To_Menu
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class;
      Label       : String);
   --  Fill the given Menu, with the "Show report" entry with no context
   --  information, so the 1st node will be expanded.

   procedure Append_Load_Data_For_All_Projects
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class);
   --  Fill the given Menu with the "Load data for all projecs" entry
   --  The associated callback tries to load coverage data for root project
   --  and every imported projects.

   procedure Append_Load_Project_Data
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class);
   --  Fill the given Menu with the "Load data for project XXX" entry
   --  The associated callback tries to load coverage data of the context
   --  pointed project.

   procedure Append_Load_File_Data
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class);
   --  Fill the given Menu with the "Load data for XXX.ad[b,s]" entry
   --  The associated callback tries to load coverage data of the context
   --  pointed file.

   procedure Append_Subprogram_Menu_Entries
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class);
   --  Fill the given Submenu with the coverage entries that allow to handle
   --  subprograms.
   pragma Unreferenced (Append_Subprogram_Menu_Entries);

   procedure Append_File_Menu_Entries
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class);
   --  Fill the given Submenu with the coverage entries that allow to handle
   --  files.

   procedure Append_Project_Menu_Entries
     (Cont_N_Anal  : Context_And_Analysis;
      Submenu      : access Gtk_Menu_Record'Class);
   --  Fill the given Submenu with the coverage entries that allow to handle
   --  projects.

   function Check_Context
     (Kernel        : Kernel_Handle;
      Given_Context : Selection_Context) return Selection_Context;
   --  Check and correct the presence of project information in the
   --  Given_Context.

   procedure Dynamic_Tools_Menu_Factory
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   pragma Unreferenced (Dynamic_Tools_Menu_Factory);
   --  Determine wether we add entries directly in the "Tools/Coverage" menu,
   --  or in a generated submenu. Submenus are created if many instances are
   --  loaded.

   procedure Dynamic_Views_Menu_Factory
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Determine wether we add entries directly in the "Tools/Views/Coverage"
   --  menu, or in a generated submenu. Submenus are created if many instances
   --  are loaded.

   procedure On_Single_View_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Show the coverage report when we are in single analysis mode

   procedure On_Load_All_Projects_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Show the coverage report when we are in single analysis mode

   procedure On_Load_Current_Project_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Show the coverage report when we are in single analysis mode

   procedure On_Load_Current_File_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Show the coverage report when we are in single analysis mode

   procedure On_Clear_Coverage_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Show the coverage report when we are in single analysis mode

   procedure Show_Analysis_Report_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Menu callback that calls Show_Analysis_Report with no context info

   procedure Show_Analysis_Report_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Create and display a Coverage Report

   procedure Show_Analysis_Report
     (Kernel       : Kernel_Handle;
      Cont_N_Anal  : Context_And_Analysis;
      Raise_Report : Boolean := True);
   --  Check if the context pointed project has data in the current
   --  code_analysis instance, if not, tries to find a project that has some
   --  and if it's not possible, show the empty report warning board.
   --  Then Build the Coverage Report, populate it, expand the
   --  the appropriate item following given context information. Finally
   --  insert the new Report in the MDI. (via Connect_Report)
   --  If Raise_Report is True, the Coverage Report will be raised.
   --  Cont_N_Anal.Context must be fulfilled with a context that contains at
   --  least a valid project info, or a valid file info belonging to an
   --  Ada project loaded in GPS.

   procedure Connect_Report
     (Kernel : Kernel_Handle; Cont_N_Anal : Context_And_Analysis);
   --  Connect some signals to the widgets of the Cont_N_Anal.Analysis.View.
   --  Also create and put the report in an MDI child.

   procedure Refresh_Analysis_Report (Cont_N_Anal : Context_And_Analysis);
   --  Reload the Coverage Report contens from given Analysis instance if the
   --  report is built
   --  Cont_N_Anal.Context must be fulfilled with a context that contains at
   --  least a valid project info, or a valid file info belonging to an
   --  Ada project loaded in GPS.

   procedure On_Destroy (Widget      : access Glib.Object.GObject_Record'Class;
                         Cont_N_Anal : Context_And_Analysis);
   --  Callback for the "destroy" signal that mark the report as destroyed

   procedure Shell_CodeAnalysis_Constructor
     (Data : in out Callback_Data'Class; Command : String);
   --  Empty subprogram that just raise an exception in order to prevent users
   --  from using the default shell constructor.
   --  The Shell_Get_Command should be used instead.

   procedure Shell_Get_Command
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Create a shell scripting instance of the module

   procedure Attach_Instance_And_Analysis
     (Data     : in out Callback_Data'Class;
      Instance : Class_Instance;
      Analysis : Code_Analysis_Instance);
   --  Set the Instance in the instance list of Analysis
   --  Set Analysis in the created property for Instance.

   procedure Create_Analysis_From_Menu
     (Widget : access Gtk_Widget_Record'Class);
   --  Create a new analysis instance from the "Tools/Coverage" menu

   function Create_Analysis_Instance
     (Name : String := "") return Code_Analysis_Instance;
   --  Create a new analysis instance.
   --  The instance is inserted in the Instances set of the Module_ID

   procedure Add_Gcov_File_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Simple wrapper for Add_Gcov_File_Info_In_Callback
   --  Also calls Show_Analysis_Report.

   procedure Add_Gcov_File_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add node and coverage info provided by a gcov file parsing

   procedure Add_Gcov_File_Info_In_Callback
     (Cont_N_Anal : Context_And_Analysis);
   --  Allow to add Gcov file info in any file level callback that should
   --  use gcov info.
   --  Looks for Gcov files corresponding to the contextual file.
   --  Then call Add_Gcov_File_Info on it.

   procedure Add_Gcov_Project_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Call Add_Gcov_File_Info on every files of the contextual project

   procedure Add_Gcov_Project_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Call Add_Gcov_File_Info on every files of the given project

   procedure Add_Gcov_Project_Info_In_Callback
     (Cont_N_Anal : Context_And_Analysis);
   --  Allow to add Gcov project info in any project level callback that should
   --  use gcov info.
   --  Looks for Gcov files corresponding to every files of the contextual
   --  project and call Add_Gcov_File_Info on it.

   procedure Add_All_Gcov_Project_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Wrapper for Add_All_Gcov_Project_Info_In_Callback

   procedure Add_All_Gcov_Project_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Wrapper for Add_All_Gcov_Project_Info_In_Callback

   procedure Add_All_Gcov_Project_Info_In_Callback
     (Cont_N_Anal : Context_And_Analysis);
   --  Try to load gcov info for every files of the Root_Project and every
   --  imported projects.

   procedure Show_All_Coverage_Information_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add in the location view every not covered lines of any projects loaded
   --  in the Code_Analysis structure of the current Instance.

   procedure Hide_All_Coverage_Information_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Shell command callback
   --  Remove from the Locations view the listed uncovered lines of each files
   --  of each loaded projects.
   --  Does nothing if the lines are not listed in.
   --  Remove every coverage annotations of opened source file editors.

   procedure Destroy_Analysis_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Call Destroy_Analysis_Instance

   procedure Clear_Analysis_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Call Clear_Analysis_Instance

   procedure Destroy_All_Analyzes_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Call Destroy_All_Analyzes

   procedure Destroy_All_Analyzes_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Call Destroy_All_Analyzes

   procedure On_Project_Changing_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Call Destroy_All_Analyzes.
   --  Then create a new analysis.

   procedure Destroy_All_Analyzes (Kernel : Kernel_Handle);
   --  Call Destroy_Analysis_Instance for every element in
   --  Code_Analysis_Module_ID.Instances.

   procedure Destroy_Analysis_Instance
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance);
   --  Free the memory used by the given analysis instance

   procedure Clear_Analysis_Instance
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance);
   --  Remove informations from the given analysis.
   --  Destroy the associated Coverage Report if built.

   procedure Show_Subprogram_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback of the "Show subprogram coverage information" contextual entry.
   --  Add to the location view the uncovered lines of the given current
   --  contextual subprogram.
   --  Add to the related file a coverage annotations column.

   procedure Hide_Subprogram_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Remove from the Locations view the uncovered lines of the contextual
   --  subprogram.
   --  Does nothing if the lines are absent from the Locations view.

   procedure Show_File_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback of the "Show file coverage information" contextual entry.
   --  Add to the location view the uncovered lines of the given file.
   --  Also add a coverage annotations column to the corresponding src_editor.

   procedure Hide_File_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Remove from the Locations view the uncovered lines of the contextual
   --  file.
   --  Does nothing if the file is absent from the Locations view.

   procedure Show_Project_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback of the "Show project coverage information" contextual entry.
   --  Add to the Locations view the uncovered lines of every files of the
   --  given project.
   --  Also add a coverage annotations column to every opened src_editor.

   procedure Hide_Project_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Remove from the Location view the uncovered lines of the files of the
   --  contextual project.
   --  Also tries to remove the scoped src_editor coverage annotations column.
   --  Does nothing if the files of the project are absent from the Locations
   --  view, or not fitted with an annotations column.

   procedure Show_All_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Callback of the "List lines not covered in all projects" menu entry.
   --  Calls List_Lines_Not_Covered_In_All_Projects.

   procedure Hide_All_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis);
   --  Remove from the Locations view the uncovered lines of all projects.
   --  Does nothing if the corresponding files are absent from the Locations
   --  view.

   pragma Unreferenced (Show_All_Coverage_Information_From_Menu,
                        Hide_All_Coverage_Information_From_Menu);
   --  On G917-013 request, these functions are not used anymore
   --  but they are functional, and could be needed soon.
   --  ??? Should be totally removed after beta-testing campaign.

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

   procedure Activate_Pango_Markup (Item : Gtk_Menu_Item);
   --  Allow to use pango markup when setting the item label

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   --  Save the status of the code analysis view to an XML tree

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the code analysis view from a saved XML tree

   procedure Dump_To_File
     (Analysis : Code_Analysis_Instance;
      File     : VFS.Virtual_File);
   --  Dump the given analysis structure to File in XML format

   procedure Dump_To_File_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Dump the current analysis (the 1st of the set) to the shell given file:
   --  in XML format

   procedure Load_From_File_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Replace the current coverage information in memory with the given
   --  xml-formated file one

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
         --  Else create one using the given instance
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

   ----------------------------------
   -- Attach_Instance_And_Analysis --
   ----------------------------------

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
      Set_Data (Instance, CodeAnalysis_Cst,
                Instance_Property_Record (Property.all));
   end Attach_Instance_And_Analysis;

   -------------------------------
   -- Create_Analysis_From_Menu --
   -------------------------------

   procedure Create_Analysis_From_Menu
     (Widget : access Gtk_Widget_Record'Class)
   is
      Dummy : constant Code_Analysis_Instance := Create_Analysis_Instance;
      pragma Unreferenced (Dummy, Widget);
   begin
      null;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Create_Analysis_From_Menu;

   ------------------------------
   -- Create_Analysis_Instance --
   ------------------------------

   function Create_Analysis_Instance
     (Name : String := "") return Code_Analysis_Instance
   is
      Analysis : constant Code_Analysis_Instance := new
        Code_Analysis_Instance_Record;
      Date     : Time;
   begin
      Date := Clock;
      Analysis.Date := Date;
      Code_Analysis_Module_ID.Count := Code_Analysis_Module_ID.Count + 1;

      if Name = "" then
         if Code_Analysis_Module_ID.Count = 1 then
            Analysis.Name := new String'(-"Coverage");
         else
            Analysis.Name := new String'
              (-"Coverage" & Integer'Image
                 (Code_Analysis_Module_ID.Count));
         end if;
      else
         Analysis.Name    := new String'(Name);
      end if;

      Analysis.Instances  := new Instance_List;
      Analysis.Projects   := new Project_Maps.Map;
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
   begin
      Add_Gcov_File_Info_In_Callback (Cont_N_Anal);
      --  Build/Refresh Report of Analysis
      Show_Analysis_Report (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal);
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
      Property := Get_Data (Instance, CodeAnalysis_Cst);

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
         end;
         return;
      end if;

      Add_Gcov_File_Info
        (Get_Kernel (Data), Src_File, Cov_File, Prj_Node);
      Compute_Project_Coverage (Prj_Node);
      --  Build/Refresh Report of Analysis
      Context := Get_Current_Context (Get_Kernel (Data));
      Set_File_Information
        (Context, Project => Prj_Name, Files => (1 => Src_File));
      Show_Analysis_Report
        (Get_Kernel (Data), Context_And_Analysis'(Context,
         Code_Analysis_Property (Property).Analysis));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Gcov_File_Info_From_Shell;

   ------------------------------------
   -- Add_Gcov_File_Info_In_Callback --
   ------------------------------------

   procedure Add_Gcov_File_Info_In_Callback
     (Cont_N_Anal : Context_And_Analysis)
   is
      Prj_Name : constant Project_Type :=
                   Project_Information (Cont_N_Anal.Context);
      Prj_Node : Project_Access;
      Src_File : constant VFS.Virtual_File :=
                   File_Information (Cont_N_Anal.Context);
      Cov_File : VFS.Virtual_File;
   begin
      Prj_Node := Get_Or_Create (Cont_N_Anal.Analysis.Projects, Prj_Name);
      Cov_File := Find_Gcov_File (Get_Kernel (Cont_N_Anal.Context), Src_File);

      if not Is_Regular_File (Cov_File) then
         GPS.Kernel.Console.Insert
           (Get_Kernel (Cont_N_Anal.Context),
            -"Could not find coverage file " & Full_Name (Cov_File).all);

         declare
            File_Node : constant Code_Analysis.File_Access
              := Get_Or_Create (Prj_Node, Src_File);
         begin
            Set_Error (File_Node, File_Not_Found);
         end;
      else
         Add_Gcov_File_Info
           (Get_Kernel (Cont_N_Anal.Context), Src_File, Cov_File, Prj_Node);
         Compute_Project_Coverage (Prj_Node);
         Refresh_Analysis_Report (Cont_N_Anal);
      end if;
   end Add_Gcov_File_Info_In_Callback;

   -------------------------------------
   -- Add_Gcov_Project_Info_From_Menu --
   -------------------------------------

   procedure Add_Gcov_Project_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      Add_Gcov_Project_Info_In_Callback (Cont_N_Anal);
      --  Build/Refresh Report of Analysis
      Show_Analysis_Report (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal);
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
      Property := Get_Data (Instance, CodeAnalysis_Cst);

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

   ---------------------------------------
   -- Add_Gcov_Project_Info_In_Callback --
   ---------------------------------------

   procedure Add_Gcov_Project_Info_In_Callback
     (Cont_N_Anal : Context_And_Analysis)
   is
      Prj_Node : Project_Access;
   begin
      Prj_Node := Get_Or_Create (Cont_N_Anal.Analysis.Projects,
                                 Project_Information (Cont_N_Anal.Context));
      Add_Gcov_Project_Info (Get_Kernel (Cont_N_Anal.Context), Prj_Node);
      Refresh_Analysis_Report (Cont_N_Anal);
   end Add_Gcov_Project_Info_In_Callback;

   -----------------------------------------
   -- Add_All_Gcov_Project_Info_From_Menu --
   -----------------------------------------

   procedure Add_All_Gcov_Project_Info_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      Add_All_Gcov_Project_Info_In_Callback (Cont_N_Anal);

      --  Build/Refresh Report of Analysis
      Show_Analysis_Report
        (Get_Kernel (Cont_N_Anal.Context),
         Context_And_Analysis'(No_Context, Cont_N_Anal.Analysis));
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
      Property := Get_Data (Instance, CodeAnalysis_Cst);

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

   -------------------------------------------
   -- Add_All_Gcov_Project_Info_In_Callback --
   -------------------------------------------

   procedure Add_All_Gcov_Project_Info_In_Callback
     (Cont_N_Anal : Context_And_Analysis)
   is
      Prj_Iter : Imported_Project_Iterator;
      Prj_Node : Project_Access;
      Prj_Name : constant Project_Type :=
                   Project_Information (Cont_N_Anal.Context);
   begin
      Prj_Iter := Start (Prj_Name);

      loop
         exit when Current (Prj_Iter) = No_Project;
         Prj_Node := Get_Or_Create
           (Cont_N_Anal.Analysis.Projects, Current (Prj_Iter));
         Add_Gcov_Project_Info (Get_Kernel (Cont_N_Anal.Context), Prj_Node);
         Next (Prj_Iter);
      end loop;

      Refresh_Analysis_Report (Cont_N_Anal);
   end Add_All_Gcov_Project_Info_In_Callback;

   ----------------------------------------------
   -- Show_All_Coverage_Information_From_Shell --
   ----------------------------------------------

   procedure Show_All_Coverage_Information_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property : Instance_Property;
      Instance : Class_Instance;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, CodeAnalysis_Cst);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      Show_All_Coverage_Information
        (Get_Kernel (Data),
         Code_Analysis_Property (Property).Analysis.Projects);
      --  Build/Refresh the Coverage Report
      Show_Analysis_Report (Get_Kernel (Data),
        Context_And_Analysis'(No_Context,
          Code_Analysis_Property (Property).Analysis));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_All_Coverage_Information_From_Shell;

   ----------------------------------------------
   -- Hide_All_Coverage_Information_From_Shell --
   ----------------------------------------------

   procedure Hide_All_Coverage_Information_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property : Instance_Property;
      Instance : Class_Instance;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, CodeAnalysis_Cst);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      Hide_All_Coverage_Information
        (Get_Kernel (Data),
         Code_Analysis_Property (Property).Analysis.Projects);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Hide_All_Coverage_Information_From_Shell;

   ---------------------------------------------
   -- Show_All_Coverage_Information_From_Menu --
   ---------------------------------------------

   procedure Show_All_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      if First_Project_With_Coverage_Data (Cont_N_Anal.Analysis.Projects) =
        No_Project then
         Add_All_Gcov_Project_Info_In_Callback (Cont_N_Anal);

         if First_Project_With_Coverage_Data (Cont_N_Anal.Analysis.Projects) =
           No_Project then
            GPS.Kernel.Console.Insert
              (Get_Kernel (Cont_N_Anal.Context),
               -"No coverage information available");
            return;
         end if;
      end if;

      Show_All_Coverage_Information
        (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal.Analysis.Projects);
      --  Build/Refresh the Coverage Report
      Show_Analysis_Report (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_All_Coverage_Information_From_Menu;

   ---------------------------------------------
   -- Hide_All_Coverage_Information_From_Menu --
   ---------------------------------------------

   procedure Hide_All_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      Hide_All_Coverage_Information
        (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal.Analysis.Projects);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Hide_All_Coverage_Information_From_Menu;

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
      Property := Get_Data (Instance, CodeAnalysis_Cst);

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

   --------------------------
   -- Show_Analysis_Report --
   --------------------------

   procedure Show_Analysis_Report
     (Kernel       : Kernel_Handle;
      Cont_N_Anal  : Context_And_Analysis;
      Raise_Report : Boolean := True)
   is
      Local_Context : Selection_Context;
      Iter          : Gtk_Tree_Iter := Null_Iter;
      Path          : Gtk_Tree_Path;
   begin

      -----------------------
      --  Build the report --
      -----------------------

      if Cont_N_Anal.Context = No_Context then
         Local_Context := Check_Context (Kernel, No_Context);
      else
         Local_Context := Cont_N_Anal.Context;
      end if;

      if Cont_N_Anal.Analysis.View = null then
         Cont_N_Anal.Analysis.View := Build_Analysis_Report
           (Kernel,
            Cont_N_Anal.Analysis.Name,
            Cont_N_Anal.Analysis.Projects,
            Binary_Coverage_Mode);
         Connect_Report
           (Kernel,
            Context_And_Analysis'(Local_Context, Cont_N_Anal.Analysis));
      else
         --  If Report already existed, clear its Gtk_Tree_Store
         Clear (Cont_N_Anal.Analysis.View.Model);
      end if;

      --------------------------------------
      --  Check for analysis information  --
      --------------------------------------

      declare
         Prj_Name : Project_Type;
         Prj_Node : Project_Access;
      begin
         Prj_Node := Get_Or_Create (Cont_N_Anal.Analysis.Projects,
                                    Project_Information (Local_Context));

         if Prj_Node.Analysis_Data.Coverage_Data = null then
            --  If the current context's project has no coverage data, it has
            --  to be modified or an error message is shown
            Prj_Name := First_Project_With_Coverage_Data
              (Cont_N_Anal.Analysis.Projects);

            if Prj_Name /= No_Project then
               --  Set in the context the 1st project that has analysis
               --  data
               Set_File_Information (Local_Context, Project => Prj_Name);
            else
               --  Show the empty report warning board
               if Get_No_Show_All (Cont_N_Anal.Analysis.View.Error_Board) then
                  Set_No_Show_All
                    (Cont_N_Anal.Analysis.View.Error_Board, False);
               end if;

               Show_All (Cont_N_Anal.Analysis.View.Error_Board);
               --  Removes Prj_Node from its container as we just created it
               Project_Maps.Delete
                 (Cont_N_Anal.Analysis.Projects.all, Prj_Node.Name);
               --  Free Prj_Node
               Free_Project (Prj_Node);

               if Raise_Report then
                  Raise_Child (Cont_N_Anal.Analysis.Child);
               end if;

               return;
            end if;
         else
            Hide_All (Cont_N_Anal.Analysis.View.Error_Board);
         end if;
      end;

      --  Here we have a context that point on elements that will be added to
      --  the coverage report

      ----------------------
      --  Fill the report --
      ----------------------

      Fill_Iter (Cont_N_Anal.Analysis.View.Model, Iter,
                 Cont_N_Anal.Analysis.Projects, Binary_Coverage_Mode,
                 Cont_N_Anal.Analysis.View.Icons);

      --------------------------------------
      --  Selection of the context caller --
      --------------------------------------

      Iter := Get_Iter_From_Context
        (Local_Context, Cont_N_Anal.Analysis.View.Model);

      if Iter /= Null_Iter then
         Path := Get_Path (Cont_N_Anal.Analysis.View.Model, Iter);
      else
         Path := Get_Path (Cont_N_Anal.Analysis.View.Model,
                           Get_Iter_First (Cont_N_Anal.Analysis.View.Model));
      end if;

      Collapse_All (Cont_N_Anal.Analysis.View.Tree);
      Expand_To_Path (Cont_N_Anal.Analysis.View.Tree, Path);
      Select_Path (Get_Selection (Cont_N_Anal.Analysis.View.Tree), Path);
      Path_Free (Path);

      if Raise_Report then
         Raise_Child (Cont_N_Anal.Analysis.Child);
      end if;
   end Show_Analysis_Report;

   --------------------
   -- Connect_Report --
   --------------------

   procedure Connect_Report
     (Kernel : Kernel_Handle; Cont_N_Anal : Context_And_Analysis) is
   begin
      Context_And_Analysis_CB.Connect
        (Cont_N_Anal.Analysis.View.Load_Button,
         Gtk.Button.Signal_Clicked,
         Context_And_Analysis_CB.To_Marshaller
           (Add_All_Gcov_Project_Info_From_Menu'Access), Cont_N_Anal);

      ---------------
      -- MDI child --
      ---------------

      GPS.Kernel.MDI.Gtk_New
        (Cont_N_Anal.Analysis.Child, Cont_N_Anal.Analysis.View,
         Group  => Group_VCS_Explorer,
         Module => Code_Analysis_Module_ID);
      Set_Title
        (Cont_N_Anal.Analysis.Child,
         Cont_N_Anal.Analysis.Name.all & (-" Report"));
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Cont_N_Anal.Analysis.View.Tree,
         Object          => Cont_N_Anal.Analysis.View,
         ID              => Module_ID (Code_Analysis_Module_ID),
         Context_Func    => Context_Func'Access);
      Kernel_Return_Cb.Object_Connect
        (Cont_N_Anal.Analysis.View.Tree, Signal_Button_Press_Event,
         Kernel_Return_Cb.To_Marshaller
           (On_Double_Click'Access), Cont_N_Anal.Analysis.View.Tree, Kernel);
      Context_And_Analysis_CB.Connect
        (Cont_N_Anal.Analysis.View, Signal_Destroy,
         Context_And_Analysis_CB.To_Marshaller (On_Destroy'Access),
         Cont_N_Anal);
      Put (Get_MDI (Kernel), Cont_N_Anal.Analysis.Child);
   end Connect_Report;

   -----------------------------
   -- Refresh_Analysis_Report --
   -----------------------------

   procedure Refresh_Analysis_Report (Cont_N_Anal : Context_And_Analysis) is
   begin
      if Cont_N_Anal.Analysis.View /= null then
         Show_Analysis_Report
           (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal, False);
      end if;
   end Refresh_Analysis_Report;

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
                        exit when Iter = Null_Iter or else
                        Get_String (Model, Iter, Num_Col) =
                          Entity_Name_Information (Context);
                        Next (Model, Iter);
                     end loop;
                  end if;
               end;
            elsif Has_File_Information (Context) then
               --  Find in the list the context's file
               loop
                  exit when Iter = Null_Iter or else
                  Get_String (Model, Iter, Num_Col) =
                    Base_Name (File_Information (Context));
                  Next (Model, Iter);
               end loop;
            else
               --  Find in the list the context's project
               loop
                  exit when Iter = Null_Iter or else
                  Get_String (Model, Iter, Num_Col) =
                    Project_Name (Project_Information (Context));
                  Next (Model, Iter);
               end loop;
            end if;
         else
            --  Find in the tree the context's project
            loop
               exit when Iter = Null_Iter or else
               Get_String (Model, Iter, Num_Col) =
                 Project_Name (Project_Information (Context));
               Next (Model, Iter);
            end loop;

            if Has_File_Information (Context) then
               --  So we also have file information
               Iter := Children (Model, Iter);

               --  Find in the tree the context's file
               loop
                  exit when Iter = Null_Iter or else
                  Get_String (Model, Iter, Num_Col) =
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
                        exit when Iter = Null_Iter or else
                        Get_String (Model, Iter, Num_Col) =
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

   ------------------------------
   -- Clear_Analysis_From_Menu --
   ------------------------------

   procedure Clear_Analysis_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
   begin
      Clear_Analysis_Instance
        (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal.Analysis);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Clear_Analysis_From_Menu;

   ------------------------------------
   -- Destroy_All_Analyzes_From_Menu --
   ------------------------------------

   procedure Destroy_All_Analyzes_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      Dummy       : Code_Analysis_Instance;
      Analysis_Nb : constant Integer := Integer
        (Code_Analysis_Module_ID.Analyzes.Length);
      pragma Unreferenced (Widget, Dummy);
   begin
      if Message_Dialog
        ((-"Destroy") & Integer'Image (Analysis_Nb) & (-" analysis?"),
         Confirmation, Button_Yes or Button_No, Justification => Justify_Left,
         Title => Integer'Image (Analysis_Nb) & (-" destructions?")) = 1
      then
         Destroy_All_Analyzes (Get_Kernel (Cont_N_Anal.Context));
         Code_Analysis_Module_ID.Count := 0;
         Dummy := Create_Analysis_Instance;
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Destroy_All_Analyzes_From_Menu;

   -------------------------------------
   -- Destroy_All_Analyzes_From_Shell --
   -------------------------------------

   procedure Destroy_All_Analyzes_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Instance : Class_Instance;
      Analysis : Code_Analysis_Instance;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);

      declare
         Property : Instance_Property;
         --  Limited lifetime for Property as it is no longer valid after
         --  Destroy_All_Analyzes call
      begin
         Property := Get_Data (Instance, CodeAnalysis_Cst);

         if Code_Analysis_Property (Property).Analysis = null then
            Set_Error_Msg (Data, -"The analysis no longer exists");
            return;
         end if;
      end;

      Destroy_All_Analyzes (Get_Kernel (Data));
      Code_Analysis_Module_ID.Count := 0;
      Analysis := Create_Analysis_Instance;
      Attach_Instance_And_Analysis (Data, Instance, Analysis);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Destroy_All_Analyzes_From_Shell;

   ------------------------------
   -- On_Project_Changing_Hook --
   ------------------------------

   procedure On_Project_Changing_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Dummy  : Code_Analysis_Instance;
      pragma Unreferenced (Data, Dummy);
   begin
      Destroy_All_Analyzes (Kernel_Handle (Kernel));
      Code_Analysis_Module_ID.Count := 0;
      Dummy  := Create_Analysis_Instance;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Project_Changing_Hook;

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
      Remove_Line_Information_Column (Kernel, No_File, CodeAnalysis_Cst);
      Free_Code_Analysis (Analysis.Projects);

      --  For each shell instance, get its property and set the Analysis field
      --  to null
      for J in Instances'Range loop
         Property := Get_Data (Instances (J), CodeAnalysis_Cst);

         if Property /= null then
            Code_Analysis_Property (Property).Analysis := null;
         end if;
      end loop;

      if Code_Analysis_Module_ID.Analyzes.Contains (Analysis) then
         Code_Analysis_Module_ID.Analyzes.Delete (Analysis);
      end if;

      GNAT.Strings.Free (Analysis.Name);
   end Destroy_Analysis_Instance;

   -----------------------------
   -- Clear_Analysis_Instance --
   -----------------------------

   procedure Clear_Analysis_Instance
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance) is
   begin
      if Analysis.View /= null then
         Close_Child (Analysis.Child, Force => True);
      end if;

      Remove_Location_Category (Kernel, Coverage_Category);
      Remove_Line_Information_Column (Kernel, No_File, CodeAnalysis_Cst);
      Free_Code_Analysis (Analysis.Projects);
   end Clear_Analysis_Instance;

   -------------------------------------------------
   -- Show_Project_Coverage_Information_From_Menu --
   -------------------------------------------------

   procedure Show_Project_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Prj_Node  : constant Project_Access := Get_Or_Create
           (Cont_N_Anal.Analysis.Projects,
            Project_Information (Cont_N_Anal.Context));
   begin
      if not Have_Gcov_Info
        (Cont_N_Anal.Analysis.Projects, Cont_N_Anal.Context) then
         Add_Gcov_Project_Info_In_Callback (Cont_N_Anal);

         if not Have_Gcov_Info
           (Cont_N_Anal.Analysis.Projects, Cont_N_Anal.Context) then
            GPS.Kernel.Console.Insert
              (Get_Kernel (Cont_N_Anal.Context),
               -"No coverage information to display for "
               & Project_Name (Prj_Node.Name));
            return;
         end if;
      end if;

      List_Project_Uncovered_Lines
        (Get_Kernel (Cont_N_Anal.Context), Prj_Node);
      Add_Project_Coverage_Annotations
        (Get_Kernel (Cont_N_Anal.Context), Prj_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Project_Coverage_Information_From_Menu;

   -------------------------------------------------
   -- Hide_Project_Coverage_Information_From_Menu --
   -------------------------------------------------

   procedure Hide_Project_Coverage_Information_From_Menu
     (Widget      : access Glib.Object.GObject_Record'Class;
      Cont_N_Anal : Context_And_Analysis)
   is
      pragma Unreferenced (Widget);
      Project_Node : constant Project_Access := Get_Or_Create
        (Cont_N_Anal.Analysis.Projects,
         Project_Information (Cont_N_Anal.Context));
   begin
      Clear_Project_Locations
        (Get_Kernel (Cont_N_Anal.Context), Project_Node);
      Remove_Project_Coverage_Annotations
        (Get_Kernel (Cont_N_Anal.Context), Project_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Hide_Project_Coverage_Information_From_Menu;

   ----------------------------------------------
   -- Show_File_Coverage_Information_From_Menu --
   ----------------------------------------------

   procedure Show_File_Coverage_Information_From_Menu
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
      if not Have_Gcov_Info
        (Cont_N_Anal.Analysis.Projects, Cont_N_Anal.Context) then
         Add_Gcov_File_Info_In_Callback (Cont_N_Anal);

         if not Have_Gcov_Info
           (Cont_N_Anal.Analysis.Projects, Cont_N_Anal.Context) then
            GPS.Kernel.Console.Insert
              (Get_Kernel (Cont_N_Anal.Context),
               -"No coverage information to display for "
               & Base_Name (File_Node.Name));
            return;
         end if;
      end if;

      --  Call Open_File_Editor with Line = 0 so that, if the editor is already
      --  open, we do not jump to line 1.
      Open_File_Editor
        (Get_Kernel (Cont_N_Anal.Context), File_Node.Name,
         Line => 0);
      List_File_Uncovered_Lines
        (Get_Kernel (Cont_N_Anal.Context), File_Node);
      Add_File_Coverage_Annotations
        (Get_Kernel (Cont_N_Anal.Context), File_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_File_Coverage_Information_From_Menu;

   ----------------------------------------------
   -- Hide_File_Coverage_Information_From_Menu --
   ----------------------------------------------

   procedure Hide_File_Coverage_Information_From_Menu
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
      Remove_File_Coverage_Annotations
        (Get_Kernel (Cont_N_Anal.Context), File_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Hide_File_Coverage_Information_From_Menu;

   ----------------------------------------------------
   -- Show_Subprogram_Coverage_Information_From_Menu --
   ----------------------------------------------------

   procedure Show_Subprogram_Coverage_Information_From_Menu
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
      if not Have_Gcov_Info
        (Cont_N_Anal.Analysis.Projects, Cont_N_Anal.Context) then
         Add_Gcov_File_Info_In_Callback (Cont_N_Anal);

         if not Have_Gcov_Info
           (Cont_N_Anal.Analysis.Projects, Cont_N_Anal.Context) then
            GPS.Kernel.Console.Insert
              (Get_Kernel (Cont_N_Anal.Context),
               -"No coverage information to display for "
               & Subp_Node.Name.all);
            return;
         end if;
      end if;

      List_Subprogram_Uncovered_Lines (Kernel, File_Node, Subp_Node);
      Add_File_Coverage_Annotations (Kernel, File_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Subprogram_Coverage_Information_From_Menu;

   ----------------------------------------------------
   -- Hide_Subprogram_Coverage_Information_From_Menu --
   ----------------------------------------------------

   procedure Hide_Subprogram_Coverage_Information_From_Menu
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
      Clear_Subprogram_Locations (Kernel, File_Node, Subp_Node);
      Remove_File_Coverage_Annotations (Kernel, File_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Hide_Subprogram_Coverage_Information_From_Menu;

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
      Prj_Iter  : Gtk_Tree_Iter;
      File_Iter : Gtk_Tree_Iter;
      Subp_Iter : Gtk_Tree_Iter;
   begin
      if Cont_N_Anal.Analysis.View = null then
         Show_Analysis_Report
           (Get_Kernel (Cont_N_Anal.Context), Cont_N_Anal, False);
      end if;

      if Have_Gcov_Info
        (Cont_N_Anal.Analysis.Projects, Cont_N_Anal.Context) then
         --  Remove potential listed locations
         Clear_Subprogram_Locations
           (Get_Kernel (Cont_N_Anal.Context), File_Node, Subp_Node);
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
            Remove_File_Coverage_Annotations
              (Get_Kernel (Cont_N_Anal.Context), File_Node);
            Unchecked_Free (File_Node.Analysis_Data.Coverage_Data);
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
      end if;

      Subp_Iter := Get_Iter_From_Context
        (Cont_N_Anal.Context, Cont_N_Anal.Analysis.View.Model);

      if Subp_Iter /= Null_Iter then
         File_Iter := Parent (Cont_N_Anal.Analysis.View.Model, Subp_Iter);
         Fill_Iter (Cont_N_Anal.Analysis.View.Model, File_Iter,
                    File_Node.Analysis_Data, Binary_Coverage_Mode);
         Prj_Iter  := Parent (Cont_N_Anal.Analysis.View.Model, File_Iter);
         Fill_Iter (Cont_N_Anal.Analysis.View.Model, Prj_Iter,
                    Prj_Node.Analysis_Data, Binary_Coverage_Mode);
         --  Removes Subp_Iter from the report
         Remove (Cont_N_Anal.Analysis.View.Model, Subp_Iter);
      end if;

      --  Removes Subp_Node from its container
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

      if Have_Gcov_Info
        (Cont_N_Anal.Analysis.Projects, Cont_N_Anal.Context) then
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

         --  Removes potential listed locations
         Clear_File_Locations (Get_Kernel (Cont_N_Anal.Context), File_Node);
         --  Removes potential annotations
         Remove_File_Coverage_Annotations
           (Get_Kernel (Cont_N_Anal.Context), File_Node);
      end if;

      File_Iter := Get_Iter_From_Context
        (Cont_N_Anal.Context, Cont_N_Anal.Analysis.View.Model);

      if File_Iter /= Null_Iter then
         Prj_Iter  := Parent (Cont_N_Anal.Analysis.View.Model, File_Iter);
         Fill_Iter (Cont_N_Anal.Analysis.View.Model, Prj_Iter,
                    Prj_Node.Analysis_Data, Binary_Coverage_Mode);
         --  Removes File_Iter from the report
         Remove (Cont_N_Anal.Analysis.View.Model, File_Iter);
      end if;

      --  Removes File_Node from its container
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

      if Iter /= Null_Iter then
         --  Removes Iter from the report
         Remove (Cont_N_Anal.Analysis.View.Model, Iter);
      end if;

      --  Removes potential listed locations
      Clear_Project_Locations (Get_Kernel (Cont_N_Anal.Context), Prj_Node);
      --  Removes potential src_editor annotations
      Remove_Project_Coverage_Annotations
        (Get_Kernel (Cont_N_Anal.Context), Prj_Node);
      --  Removes Prj_Node from its container
      Project_Maps.Delete
        (Cont_N_Anal.Analysis.Projects.all, Prj_Node.Name);
      --  Free Prj_Node
      Free_Project (Prj_Node);

      if Project_Maps.Length (Cont_N_Anal.Analysis.Projects.all) = 0 then
         Refresh_Analysis_Report (Cont_N_Anal);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Remove_Project_From_Menu;

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
      Cont_N_Anal : Context_And_Analysis;
      Submenu     : Gtk_Menu;
      Item        : Gtk_Menu_Item;
      Cur         : Cursor := Code_Analysis_Module_ID.Analyzes.First;
   begin
      Cont_N_Anal.Context := Context;

      loop
         exit when Cur = No_Element;

         Cont_N_Anal.Analysis := Element (Cur);

         if Code_Analysis_Module_ID.Analyzes.Length > 1 then
            Gtk_New (Item, Cont_N_Anal.Analysis.Name.all);
            Append (Menu, Item);
            Gtk_New (Submenu);
            Set_Submenu (Item, Submenu);
            Set_Sensitive (Item, True);
            Append_To_Contextual_Submenu (Cont_N_Anal, Submenu);
         else
            Append_To_Contextual_Submenu (Cont_N_Anal, Menu);
         end if;

         Next (Cur);
      end loop;
   end Append_To_Menu;

   ----------------------------------
   -- Append_To_Contextual_Submenu --
   ----------------------------------

   procedure Append_To_Contextual_Submenu
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class)
   is
      Item : Gtk_Menu_Item;
   begin
      if Has_File_Information (Cont_N_Anal.Context) then
         --  Comment out the code making a distinction between clicking on
         --  a subprogram entity and clicking anywhere else. We do not want
         --  this refinement for the first release.

--           if Has_Entity_Name_Information (Cont_N_Anal.Context) then
--              declare
--                 Entity : constant Entities.Entity_Information :=
--                            Get_Entity (Cont_N_Anal.Context);
--              begin
--                 if (Entity /= null
--                      and then Is_Subprogram (Entity))
--                   or else  Get_Creator (Cont_N_Anal.Context) =
--                   Abstract_Module_ID (Code_Analysis_Module_ID)
--                 then
--                    --  So we have a subprogram information
--                    Append_Subprogram_Menu_Entries (Cont_N_Anal, Submenu);
--                 else
--                    Append_File_Menu_Entries (Cont_N_Anal, Submenu);
--                 end if;
--              end;
--           else
         Append_File_Menu_Entries (Cont_N_Anal, Submenu);
--           end if;
      else
         Append_Project_Menu_Entries (Cont_N_Anal, Submenu);
      end if;

      if Get_Creator (Cont_N_Anal.Context) /=
        Abstract_Module_ID (Code_Analysis_Module_ID)
      then
         Gtk_New (Item);
         Append (Submenu, Item);
         Append_Show_Analysis_Report_To_Menu
           (Cont_N_Anal, Submenu, -"Show Coverage Report");
      end if;
   end Append_To_Contextual_Submenu;

   ------------------------------------
   -- Append_Subprogram_Menu_Entries --
   ------------------------------------

   procedure Append_Subprogram_Menu_Entries
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class)
   is
      Item : Gtk_Menu_Item;
   begin
      Gtk_New (Item, -"Show coverage information");
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Show_Subprogram_Coverage_Information_From_Menu'Access),
         Context_And_Analysis'(Cont_N_Anal.Context, Cont_N_Anal.Analysis));

      Gtk_New (Item, -"Hide coverage information");
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Hide_Subprogram_Coverage_Information_From_Menu'Access),
         Context_And_Analysis'(Cont_N_Anal.Context, Cont_N_Anal.Analysis));

      Gtk_New (Item);
      Append (Submenu, Item);

      Gtk_New (Item, -"Load data for " & Emphasize (Locale_Base_Name
        (File_Information (Cont_N_Anal.Context))));
      Activate_Pango_Markup (Item);
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Add_Gcov_File_Info_From_Menu'Access), Cont_N_Anal);

      Gtk_New (Item, -"Remove data of " & Emphasize (Entity_Name_Information
        (Cont_N_Anal.Context)));
      Activate_Pango_Markup (Item);
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Remove_Subprogram_From_Menu'Access), Cont_N_Anal);
   end Append_Subprogram_Menu_Entries;

   ------------------------------
   -- Append_File_Menu_Entries --
   ------------------------------

   procedure Append_File_Menu_Entries
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class)
   is
      Item    : Gtk_Menu_Item;
      Context : Selection_Context :=
                  Get_Current_Context (Get_Kernel (Cont_N_Anal.Context));
   begin
      --  Remove entity information
      Set_Entity_Information (Context);

      if Get_Creator (Cont_N_Anal.Context) =
        Abstract_Module_ID (Code_Analysis_Module_ID) then
         --  If context has been filled by Coverage Report, then fill it again
         --  because the new gotten context from kernel will be empty too
         Set_File_Information
           (Context,
            File_Information (Cont_N_Anal.Context),
            Project_Information (Cont_N_Anal.Context));
      end if;

      Gtk_New (Item, -"Show coverage information");
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Show_File_Coverage_Information_From_Menu'Access),
         Context_And_Analysis'(Context, Cont_N_Anal.Analysis));

      Gtk_New (Item, -"Hide coverage information");
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Hide_File_Coverage_Information_From_Menu'Access),
         Context_And_Analysis'(Context, Cont_N_Anal.Analysis));

      Gtk_New (Item);
      Append (Submenu, Item);

      Append_Load_File_Data (Cont_N_Anal, Submenu);

      Gtk_New (Item, -"Remove data of " & Emphasize (Base_Name
        (File_Information (Context))));
      Activate_Pango_Markup (Item);
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Remove_File_From_Menu'Access),
         Context_And_Analysis'(Context, Cont_N_Anal.Analysis));
   end Append_File_Menu_Entries;

   ---------------------------------
   -- Append_Project_Menu_Entries --
   ---------------------------------

   procedure Append_Project_Menu_Entries
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class)
   is
      Item    : Gtk_Menu_Item;
      Context : Selection_Context :=
                  Get_Current_Context (Get_Kernel (Cont_N_Anal.Context));
   begin
      Set_File_Information
        (Context, Project => Project_Information (Cont_N_Anal.Context));

      Gtk_New (Item, -"Show coverage information");
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Show_Project_Coverage_Information_From_Menu'Access),
         Context_And_Analysis'(Context, Cont_N_Anal.Analysis));

      Gtk_New (Item, -"Hide coverage information");
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Hide_Project_Coverage_Information_From_Menu'Access),
         Context_And_Analysis'(Context, Cont_N_Anal.Analysis));

      Gtk_New (Item);
      Append (Submenu, Item);

      Append_Load_Project_Data (Cont_N_Anal, Submenu);

      Gtk_New (Item, -"Remove data of project " & Emphasize (Project_Name
        (Project_Information (Cont_N_Anal.Context))));
      Activate_Pango_Markup (Item);
      Append (Submenu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Remove_Project_From_Menu'Access),
         Context_And_Analysis'(Context, Cont_N_Anal.Analysis));
   end Append_Project_Menu_Entries;

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
               Set_File_Information
                 (Checked_Context, (1 => File_Info), Prj_Info);
            end;

         else
            Set_File_Information
              (Checked_Context, Project => Get_Project (Kernel));
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
      Submenu     : Gtk_Menu;
      Item        : Gtk_Menu_Item;
      Cur         : Cursor := Code_Analysis_Module_ID.Analyzes.First;
   begin
      Cont_N_Anal.Context := Check_Context (Kernel_Handle (Kernel), Context);

      loop
         exit when Cur = No_Element;
         Cont_N_Anal.Analysis := Element (Cur);
         Next (Cur);

         if Code_Analysis_Module_ID.Analyzes.Length > 1 then
            Gtk_New (Item, -(Cont_N_Anal.Analysis.Name.all));
            Append (Menu, Item);
            Gtk_New (Submenu);
            Set_Submenu (Item, Submenu);
            Set_Sensitive (Item, True);
            Append_To_Submenu (Cont_N_Anal, Submenu);
         else
            Append_To_Submenu (Cont_N_Anal, Menu);
         end if;
      end loop;

      if not Single_Analysis_Mode then
         Gtk_New (Item);
         Append (Menu, Item);
         Gtk_New (Item, -"Create code analysis");
         Append (Menu, Item);
         Gtkada.Handlers.Widget_Callback.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Create_Analysis_From_Menu'Access);

         if Code_Analysis_Module_ID.Analyzes.Length > 1 then
            Gtk_New (Item, -"Remove all analyzes");
            Append (Menu, Item);
            Context_And_Analysis_CB.Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               Context_And_Analysis_CB.To_Marshaller
                 (Destroy_All_Analyzes_From_Menu'Access), Cont_N_Anal);
         end if;
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
      use Code_Analysis_Instances;
      Cont_N_Anal : Context_And_Analysis;
      Cur         : Cursor := Code_Analysis_Module_ID.Analyzes.Last;
   begin
      Cont_N_Anal.Context := Check_Context (Kernel_Handle (Kernel), Context);

      loop
         exit when Cur = No_Element;
         Cont_N_Anal.Analysis := Element (Cur);
         Append_Show_Analysis_Report_To_Menu
           (Cont_N_Anal, Menu,
            -(Cont_N_Anal.Analysis.Name.all & (-" Report")));
         Next (Cur);
      end loop;
   end Dynamic_Views_Menu_Factory;

   -------------------------
   -- On_Single_View_Menu --
   -------------------------

   procedure On_Single_View_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      use Code_Analysis_Instances;
      Cur : constant Cursor := Code_Analysis_Module_ID.Analyzes.First;
   begin
      Show_Analysis_Report (Kernel, Context_And_Analysis'(Check_Context
        (Kernel, No_Context), Element (Cur)));
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Single_View_Menu;

   -------------------------------
   -- On_Load_All_Projects_Menu --
   -------------------------------

   procedure On_Load_All_Projects_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Cont_N_Anal : Context_And_Analysis;
      use Code_Analysis_Instances;
      Cur         : constant Cursor := Code_Analysis_Module_ID.Analyzes.First;
   begin
      if Cur = No_Element then
         return;
      end if;

      Cont_N_Anal.Analysis := Element (Cur);
      Cont_N_Anal.Context  :=
        Check_Context (Kernel, Get_Current_Context (Kernel));

      Set_File_Information
        (Cont_N_Anal.Context,
         Project => Get_Project (Get_Kernel (Cont_N_Anal.Context)));
      Add_All_Gcov_Project_Info_From_Menu
        (Widget      => Widget,
         Cont_N_Anal => Cont_N_Anal);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Load_All_Projects_Menu;

   ----------------------------------
   -- On_Load_Current_Project_Menu --
   ----------------------------------

   procedure On_Load_Current_Project_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Cont_N_Anal : Context_And_Analysis;
      use Code_Analysis_Instances;
      Cur         : constant Cursor := Code_Analysis_Module_ID.Analyzes.First;
   begin
      if Cur = No_Element then
         return;
      end if;

      Cont_N_Anal.Analysis := Element (Cur);
      Cont_N_Anal.Context := Check_Context
        (Kernel, Get_Current_Context (Kernel));

      Add_Gcov_Project_Info_From_Menu
        (Widget      => Widget,
         Cont_N_Anal => Cont_N_Anal);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Load_Current_Project_Menu;

   -------------------------------
   -- On_Load_Current_File_Menu --
   -------------------------------

   procedure On_Load_Current_File_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Cont_N_Anal : Context_And_Analysis;
      use Code_Analysis_Instances;
      Cur         : constant Cursor := Code_Analysis_Module_ID.Analyzes.First;
   begin
      if Cur = No_Element then
         return;
      end if;

      Cont_N_Anal.Analysis := Element (Cur);
      Cont_N_Anal.Context := Check_Context
        (Kernel, Get_Current_Context (Kernel));

      Add_Gcov_File_Info_From_Menu
        (Widget      => Widget,
         Cont_N_Anal => Cont_N_Anal);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Load_Current_File_Menu;

   ----------------------------
   -- On_Clear_Coverage_Menu --
   ----------------------------

   procedure On_Clear_Coverage_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      use Code_Analysis_Instances;
      Cur         : constant Cursor := Code_Analysis_Module_ID.Analyzes.First;
      Cont_N_Anal : Context_And_Analysis;
   begin
      if Cur = No_Element then
         return;
      end if;

      Cont_N_Anal.Analysis := Element (Cur);
      Cont_N_Anal.Context := Check_Context
        (Kernel, Get_Current_Context (Kernel));

      Clear_Analysis_From_Menu
        (Widget      => Widget,
         Cont_N_Anal => Cont_N_Anal);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Clear_Coverage_Menu;

   -----------------------
   -- Append_To_Submenu --
   -----------------------

   procedure Append_To_Submenu
     (Cont_N_Anal : Context_And_Analysis;
      Submenu     : access Gtk_Menu_Record'Class)
   is
      use Code_Analysis_Instances;
      Item : Gtk_Menu_Item;
   begin
      Append_Show_Analysis_Report_To_Menu
        (Cont_N_Anal, Submenu, -"Show report");
      Gtk_New (Item);
      Append (Submenu, Item);
      Append_Load_Data_For_All_Projects (Cont_N_Anal, Submenu);
      Append_Load_Project_Data (Cont_N_Anal, Submenu);

      if Has_File_Information (Cont_N_Anal.Context) then
         Append_Load_File_Data (Cont_N_Anal, Submenu);
      end if;

      Gtk_New (Item);
      Append (Submenu, Item);

      if Code_Analysis_Module_ID.Analyzes.Length > 1 then
         Gtk_New (Item, -"Remove " & Cont_N_Anal.Analysis.Name.all);
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Destroy_Analysis_From_Menu'Access), Cont_N_Anal);
      else
         Gtk_New (Item, -"Clear " & Cont_N_Anal.Analysis.Name.all);
         Append (Submenu, Item);
         Context_And_Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Context_And_Analysis_CB.To_Marshaller
              (Clear_Analysis_From_Menu'Access), Cont_N_Anal);
      end if;
   end Append_To_Submenu;

   -----------------------------------------
   -- Append_Show_Analysis_Report_To_Menu --
   -----------------------------------------

   procedure Append_Show_Analysis_Report_To_Menu
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class;
      Label       : String)
   is
      Item : Gtk_Menu_Item;
   begin
      Gtk_New (Item, -Label);
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

   ------------------------------
   -- Append_Load_Project_Data --
   ------------------------------

   procedure Append_Load_Project_Data
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class)
   is
      Item : Gtk_Menu_Item;
   begin
      Gtk_New (Item, -"Load data for project " & Emphasize (Project_Name
        (Project_Information (Cont_N_Anal.Context))));
      Activate_Pango_Markup (Item);
      Append (Menu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Add_Gcov_Project_Info_From_Menu'Access),
         Context_And_Analysis'(Cont_N_Anal.Context, Cont_N_Anal.Analysis));
   end Append_Load_Project_Data;

   ---------------------------
   -- Append_Load_File_Data --
   ---------------------------

   procedure Append_Load_File_Data
     (Cont_N_Anal : Context_And_Analysis;
      Menu        : access Gtk_Menu_Record'Class)
   is
      Item : Gtk_Menu_Item;
   begin
      Gtk_New (Item, -"Load data for " & Emphasize (Locale_Base_Name
        (File_Information (Cont_N_Anal.Context))));
      Activate_Pango_Markup (Item);
      Append (Menu, Item);
      Context_And_Analysis_CB.Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Context_And_Analysis_CB.To_Marshaller
           (Add_Gcov_File_Info_From_Menu'Access),
         Context_And_Analysis'(Cont_N_Anal.Context, Cont_N_Anal.Analysis));
   end Append_Load_File_Data;

   ---------------------------
   -- Activate_Pango_Markup --
   ---------------------------

   procedure Activate_Pango_Markup (Item : Gtk_Menu_Item) is
      Label : constant Gtk_Label := Gtk_Label (Get_Child (Item));
   begin
      if Label /= null then
         Set_Use_Markup (Label, True);
      end if;
   end Activate_Pango_Markup;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);
      Root : Node_Ptr;
      View : Code_Analysis_View;
   begin
      if Widget.all in Code_Analysis_View_Record'Class then
         View     := Code_Analysis_View (Widget);
         Root     := new Glib.Xml_Int.Node;
         Root.Tag := new String'("Code_Analysis_Tree");
         Set_Attribute (Root, "name", Get_Name (View.Tree));
         Dump_XML (View.Projects, Root);
         return Root;
      else
         return null;
      end if;
   end Save_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
      Cont_N_Anal : Context_And_Analysis;
      Raised_View : Boolean;
   begin
      if Node.Tag.all = "Code_Analysis_Tree" then
         if Code_Analysis_Module_ID.Count = 1 then
            --  There's only one analysis created, we fill it
            Code_Analysis_Module_ID.Count := 2;
            Cont_N_Anal.Analysis :=
              Code_Analysis_Module_ID.Analyzes.First_Element;
            Cont_N_Anal.Analysis.Name :=
              new String'(Get_Attribute (Node, "name"));
         else
            --  Count is more than one, so the first analysis has already been
            --  filled, we create a new one
            Cont_N_Anal.Analysis :=
              Create_Analysis_Instance (Get_Attribute (Node, "name"));
         end if;

         Parse_XML
           (Get_Project (User), Cont_N_Anal.Analysis.Projects, Node.Child);

         if Get_Attribute (Node.Parent, "Raised") = "True" then
            Raised_View := True;
         end if;

         Show_Analysis_Report (User, Cont_N_Anal, Raised_View);
         return MDI_Child (Cont_N_Anal.Analysis.Child);
      else
         return null;
      end if;
   end Load_Desktop;

   ------------------
   -- Dump_To_File --
   ------------------

   procedure Dump_To_File
     (Analysis : Code_Analysis_Instance;
      File     : VFS.Virtual_File)
   is
      Root : Node_Ptr;
   begin
      Root     := new Glib.Xml_Int.Node;
      Root.Tag := new String'("Code_Analysis_Tree");
      Set_Attribute (Root, "name", Analysis.Name.all);
      Dump_XML (Analysis.Projects, Root);
      Print (Root, Full_Name (File).all);
      Free (Root);
   end Dump_To_File;

   -----------------------------
   -- Dump_To_File_From_Shell --
   -----------------------------

   procedure Dump_To_File_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property  : Instance_Property;
      Instance  : Class_Instance;
      File_Inst : Class_Instance;
      File_Dump : VFS.Virtual_File;
   begin
      --  Check if the attached Analysis is still there
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, CodeAnalysis_Cst);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      --  Check the parameters of the command
      Name_Parameters (Data, (2 => Xml_File_Cst'Access));
      File_Inst := Nth_Arg
        (Data, 2, Get_File_Class (Get_Kernel (Data)),
         Default => No_Class_Instance, Allow_Null => True);

      if File_Inst = No_Class_Instance then
         Set_Error_Msg
           (Data, -"You must give a VFS.Virtual_File the for 'xml' argument");
         return;
      else
         File_Dump := Get_Data (File_Inst);
      end if;

      Dump_To_File (Code_Analysis_Property (Property).Analysis, File_Dump);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Dump_To_File_From_Shell;

   -------------------------------
   -- Load_From_File_From_Shell --
   -------------------------------

   procedure Load_From_File_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property    : Instance_Property;
      Instance    : Class_Instance;
      File_Inst   : Class_Instance;
      Loaded_File : VFS.Virtual_File;
      Root_Node   : Node_Ptr;
   begin
      --  Check if the attached Analysis is still there
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Get_Data (Instance, CodeAnalysis_Cst);

      if Code_Analysis_Property (Property).Analysis = null then
         Set_Error_Msg (Data, -"The analysis no longer exists");
         return;
      end if;

      --  Check the parameters of the command
      Name_Parameters (Data, (2 => Xml_File_Cst'Access));
      File_Inst := Nth_Arg
        (Data, 2, Get_File_Class (Get_Kernel (Data)),
         Default => No_Class_Instance, Allow_Null => True);

      if File_Inst = No_Class_Instance then
         Set_Error_Msg
           (Data, -"You must give a VFS.Virtual_File the for 'xml' argument");
         return;
      else
         Loaded_File := Get_Data (File_Inst);
      end if;

      if not Is_Regular_File (Loaded_File) then
         Set_Error_Msg (Data, -"The name given for 'xml' file is wrong");
         return;
      end if;

      Root_Node := Parse (Full_Name (Loaded_File).all);
      Parse_XML
        (Get_Project (Get_Kernel (Data)),
         Code_Analysis_Property (Property).Analysis.Projects, Root_Node.Child);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Load_From_File_From_Shell;

   ----------
   -- Less --
   ----------

   function Less (Left, Right : Code_Analysis_Instance) return Boolean is
   begin
      return Left.Date < Right.Date;
   end Less;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Code_Analysis_Instance) return Boolean is
   begin
      return Left.Date = Right.Date;
   end Equal;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Contextual_Menu     : Code_Analysis_Contextual_Menu_Access;
      Code_Analysis_Class : constant Class_Type :=
                              New_Class (Kernel, CodeAnalysis_Cst);
      Analysis            : Code_Analysis_Instance;
      pragma Unreferenced (Analysis);
      Tools               : constant String := '/' & (-"Tools");
      Coverage            : constant String := -"Covera_ge";
      Views               : constant String := -"Views";
      Mitem               : Gtk_Menu_Item;
   begin
      Binary_Coverage_Mode          := Active (Binary_Coverage_Trace);
      Single_Analysis_Mode          := Active (Single_Analysis_Trace);
      Code_Analysis_Module_ID       := new Code_Analysis_Module_ID_Record;
      Code_Analysis_Module_ID.Class := Code_Analysis_Class;
      Analysis                      := Create_Analysis_Instance;
      Contextual_Menu               := new Code_Analysis_Contextual_Menu;
      Register_Module
        (Module      => Code_Analysis_Module_ID,
         Kernel      => Kernel,
         Module_Name => CodeAnalysis_Cst);
      Register_Contextual_Submenu
        (Kernel      => Kernel,
         Name        => -"Coverage",
         Filter      => Action_Filter (Lookup_Filter (Kernel, "Project only")
           or Lookup_Filter (Kernel, "In project")),
         Submenu     => Submenu_Factory (Contextual_Menu));

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Tools & '/' & Coverage,
         Text        => -"_Show report",
         Callback    => On_Single_View_Menu'Access,
         Ref_Item    => -"Documentation",
         Add_Before  => False);

      --  Deactivate the dynamic menu for now.
      --  Use a static menu, so that the "gcov.py" plug-in can add menus to
      --  the menu "/Tools/Coverage/".

      --  ??? The current static menus work only in "single analysis" mode

      --        Register_Dynamic_Menu
      --          (Kernel      => Kernel,
      --           Parent_Path => Tools & '/' & Coverage,
      --           Text        => -"Data",
      --           Add_Before  => False,
      --           Factory     => Dynamic_Tools_Menu_Factory'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Tools & '/' & Coverage, Mitem);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Tools & '/' & Coverage,
         Text        => -"Load data for _all projects",
         Callback    => On_Load_All_Projects_Menu'Access,
         Ref_Item    => -"Documentation",
         Add_Before  => False);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Tools & '/' & Coverage,
         Text        => -"Load data for current _project",
         Callback    => On_Load_Current_Project_Menu'Access,
         Ref_Item    => -"Documentation",
         Add_Before  => False);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Tools & '/' & Coverage,
         Text        => -"Load data for current _file",
         Callback    => On_Load_Current_File_Menu'Access,
         Ref_Item    => -"Documentation",
         Add_Before  => False);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Tools & '/' & Coverage, Mitem);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Tools & '/' & Coverage,
         Text        => -"C_lear coverage from memory",
         Callback    => On_Clear_Coverage_Menu'Access,
         Ref_Item    => -"Documentation",
         Add_Before  => False);

      if not Single_Analysis_Mode then
         Register_Dynamic_Menu
           (Kernel      => Kernel,
            Parent_Path => Tools & '/' & Views,
            Text        => -"Covera_ge",
            Ref_Item    => -"Clipboard",
            Add_Before  => False,
            Factory     => Dynamic_Views_Menu_Factory'Access);
      else
         Register_Menu
           (Kernel      => Kernel,
            Parent_Path => Tools & '/' & Views,
            Text        => -"Covera_ge Report",
            Callback    => On_Single_View_Menu'Access);
      end if;

      Add_Hook
        (Kernel  => Kernel,
         Hook    => Project_Changing_Hook,
         Func    =>
           Wrapper (On_Project_Changing_Hook'Access),
         Name    => "destroy_all_code_analysis");
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
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
        (Kernel, "show_coverage_information",
         Class   => Code_Analysis_Class,
         Handler => Show_All_Coverage_Information_From_Shell'Access);
      Register_Command
        (Kernel, "hide_coverage_information",
         Class   => Code_Analysis_Class,
         Handler => Hide_All_Coverage_Information_From_Shell'Access);
      Register_Command
        (Kernel, "show_analysis_report",
         Class   => Code_Analysis_Class,
         Handler => Show_Analysis_Report_From_Shell'Access);
      Register_Command
        (Kernel, "dump_to_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class   => Code_Analysis_Class,
         Handler       => Dump_To_File_From_Shell'Access);
      Register_Command
        (Kernel, "load_from_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Code_Analysis_Class,
         Handler       => Load_From_File_From_Shell'Access);
      Register_Command
        (Kernel, "clear",
         Class   => Code_Analysis_Class,
         Handler => Destroy_All_Analyzes_From_Shell'Access);
   end Register_Module;

end Code_Analysis_Module;
