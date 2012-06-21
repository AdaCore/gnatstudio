------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Ada.Calendar;                           use Ada.Calendar;
with Ada.Containers.Indefinite_Ordered_Sets; use Ada.Containers;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;
with GNAT.Strings;
with GNATCOLL.Projects;                      use GNATCOLL.Projects;
with GNATCOLL.Scripts;                       use GNATCOLL.Scripts;
with GNATCOLL.VFS;                           use GNATCOLL.VFS;
with Glib;                                   use Glib;
with Glib.Object;                            use Glib.Object;
with XML_Utils;                              use XML_Utils;
with Gtk.Button;
with Gtk.Enums;                              use Gtk.Enums;
with Gtk.Handlers;                           use Gtk.Handlers;
with Gtk.Menu;                               use Gtk.Menu;
with Gtk.Menu_Item;                          use Gtk.Menu_Item;
with Gtk.Separator_Menu_Item;                use Gtk.Separator_Menu_Item;
with Gtk.Tree_Selection;                     use Gtk.Tree_Selection;
with Gtk.Tree_View;                          use Gtk.Tree_View;
with Gtk.Tree_Model;                         use Gtk.Tree_Model;
with Gtk.Tree_Store;                         use Gtk.Tree_Store;
with Gtk.Tree_View_Column;                   use Gtk.Tree_View_Column;
with Gtk.Widget;                             use Gtk.Widget;
with Gtk.Box;                                use Gtk.Box;
with Gtk.Label;                              use Gtk.Label;
with Gtkada.MDI;                             use Gtkada.MDI;
with GPS.Intl;                               use GPS.Intl;
with GPS.Kernel;                             use GPS.Kernel;
with GPS.Kernel.Console;
with GPS.Kernel.Contexts;                    use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                         use GPS.Kernel.MDI;
with GPS.Kernel.Messages;                    use GPS.Kernel.Messages;
with GPS.Kernel.Modules;                     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;                  use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;                     use GPS.Kernel.Project;
with GPS.Kernel.Scripts;                     use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;              use GPS.Kernel.Standard_Hooks;
with Projects;                               use Projects;
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

   package Kernel_Return_Cb is new User_Return_Callback
     (Gtk.Widget.Gtk_Widget_Record, Boolean, Kernel_Handle);

   ------------------------
   -- Analysis instances --
   ------------------------

   type Code_Analysis_Instance_Record is record
      Projects  : Code_Analysis_Tree;
      Name      : GNAT.Strings.String_Access;
      Date      : Time;
   end record;

   type Code_Analysis_Instance is access Code_Analysis_Instance_Record;

   function Get_Analysis
     (View : Code_Analysis_View) return Code_Analysis_Instance;
   --  Retrieve the analysis from the view.

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

   package String_Set is new Indefinite_Ordered_Sets
     (Element_Type => Unbounded_String);

   ------------------------
   -- Basic module stuff --
   ------------------------

   type Code_Analysis_Module_ID_Record is new Module_ID_Record with record
      Class    : Class_Type;
      Registered_Analysis : String_Set.Set;
      Analyzes : Code_Analysis_Instances.Set;
   end record;

   type Code_Analysis_Module_ID_Access is access all
     Code_Analysis_Module_ID_Record'Class;

   overriding procedure Destroy
     (Module : in out Code_Analysis_Module_ID_Record);

   Code_Analysis_Module_ID : Code_Analysis_Module_ID_Access;

   type CB_Data_Record is record
      Kernel   : Kernel_Handle;
      Analysis : Unbounded_String;
      Project  : Project_Type;
      File     : GNATCOLL.VFS.Virtual_File;
   end record;

   package Analysis_CB is new User_Callback
     (Glib.Object.GObject_Record, CB_Data_Record);
   --  Used to connect handlers on the global Coverage contextual menu

   function Get_Iter_From_Context
     (Project : Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Model   : Gtk_Tree_Store) return Gtk_Tree_Iter;
   --  Return the Gtk_Tree_Iter of the Gtk_Tree_Store corresponding to the
   --  contextual elements (project, file) of the coverage report.

   ---------------------
   -- Contextual menu --
   ---------------------

   type Code_Analysis_Contextual_Menu is new
     Submenu_Factory_Record with null record;

   type Code_Analysis_Contextual_Menu_Access is access all
     Code_Analysis_Contextual_Menu;

   overriding procedure Append_To_Menu
     (Factory : access Code_Analysis_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Submenu : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Determine wether we add entries directly in the contextual menu, or in
   --  a generated submenu. Submenus are created if many instances are loaded.

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
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
   --  Menu callback that calls Show_Analysis_Report with no context info

   procedure Show_Analysis_Report_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Create and display a Coverage Report

   procedure Show_Analysis_Report
     (Kernel       : Kernel_Handle;
      Analysis     : Code_Analysis_Instance;
      Project      : Project_Type := No_Project;
      File         : GNATCOLL.VFS.Virtual_File := No_File;
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

   function Get_Or_Create
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance;
      Create   : Boolean) return GPS_MDI_Child;
   --  Get the report widget for the given context. If no such widget exist,
   --  and 'Create' is true, then this creates the report.

   procedure Refresh_Analysis_Report
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance);
   --  Reload the Coverage Report contens from given Analysis instance if the
   --  report is built
   --  Cont_N_Anal.Context must be fulfilled with a context that contains at
   --  least a valid project info, or a valid file info belonging to an
   --  Ada project loaded in GPS.

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
     (Instance : Class_Instance;
      Analysis : String);
   --  Set the Instance in the instance list of Analysis
   --  Set Analysis in the created property for Instance.

   function Get_Or_Create
     (Name : String := -"Coverage") return Code_Analysis_Instance;
   --  Create a new analysis instance.
   --  The instance is inserted in the Instances set of the Module_ID.
   --  This function will always return a non null value.

   procedure Add_Gcov_File_Info_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
   --  Add Gcov information for a given file, if present

   procedure Add_Gcov_File_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add node and coverage info provided by a gcov file parsing

   procedure Add_Gcov_File_Info_In_Callback
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance;
      Project  : Project_Type;
      File     : Virtual_File;
      From_XML : Boolean := False);
   --  Allow to add Gcov file info in any file level callback that should
   --  use gcov info.
   --  Looks for Gcov files corresponding to the contextual file.
   --  Then call Add_Gcov_File_Info on it.

   procedure Add_Gcov_Project_Info_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
   --  Call Add_Gcov_File_Info on every files of the contextual project

   procedure Add_Gcov_Project_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Call Add_Gcov_File_Info on every files of the given project

   procedure Add_Gcov_Project_Info_In_Callback
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance;
      Project  : Project_Type);
   --  Allow to add Gcov project info in any project level callback that should
   --  use gcov info.
   --  Looks for Gcov files corresponding to every files of the contextual
   --  project and call Add_Gcov_File_Info on it.

   procedure Add_All_Gcov_Project_Info_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
   --  Wrapper for Add_All_Gcov_Project_Info_In_Callback

   procedure Add_All_Gcov_Project_Info_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Wrapper for Add_All_Gcov_Project_Info_In_Callback

   procedure Add_All_Gcov_Project_Info_In_Callback
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance);
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

   procedure Destroy_All_Analyzes_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Call Destroy_All_Analyzes

   procedure On_Project_Changing_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Call Destroy_All_Analyzes.
   --  Then create a new analysis.

   procedure On_Project_View_Changed_Hook
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when a project view is changed. Allows to fill the view with new
   --  datas.

   procedure Destroy_All_Analyzes
     (Kernel     : Kernel_Handle;
      Close_View : Boolean := True);
   --  Call Destroy_Analysis_Instance for every element in
   --  Code_Analysis_Module_ID.Instances.

   procedure Destroy_Analysis_Instance
     (Kernel     : Kernel_Handle;
      Analysis   : in out Code_Analysis_Instance;
      Close_View : Boolean);
   --  Free the memory used by the given analysis instance

   procedure Show_File_Coverage_Information_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
   --  Callback of the "Show file coverage information" contextual entry.
   --  Add to the location view the uncovered lines of the given file.
   --  Also add a coverage annotations column to the corresponding src_editor.

   procedure Hide_File_Coverage_Information_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
   --  Remove from the Locations view the uncovered lines of the contextual
   --  file.
   --  Does nothing if the file is absent from the Locations view.

   procedure Show_Project_Coverage_Information_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
   --  Callback of the "Show project coverage information" contextual entry.
   --  Add to the Locations view the uncovered lines of every files of the
   --  given project.
   --  Also add a coverage annotations column to every opened src_editor.

   procedure Hide_Project_Coverage_Information_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
   --  Remove from the Location view the uncovered lines of the files of the
   --  contextual project.
   --  Also tries to remove the scoped src_editor coverage annotations column.
   --  Does nothing if the files of the project are absent from the Locations
   --  view, or not fitted with an annotations column.

   procedure Remove_File_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
   --  Remove the selected file node from the related report and instance

   procedure Remove_Project_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record);
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
      File     : GNATCOLL.VFS.Virtual_File);
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

   ------------------
   -- Get_Analysis --
   ------------------

   function Get_Analysis
     (View : Code_Analysis_View) return Code_Analysis_Instance
   is
      use Code_Analysis_Instances;
      Cursor : Code_Analysis_Instances.Cursor :=
                 Code_Analysis_Module_ID.Analyzes.First;
   begin
      while Has_Element (Cursor) loop
         if View.Name = Element (Cursor).Name.all then
            return Element (Cursor);
         end if;

         Next (Cursor);
      end loop;

      return null;
   end Get_Analysis;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Module : in out Code_Analysis_Module_ID_Record) is
   begin
      --  The view is already closed at this point, and so certainly is the gps
      --  main window.
      Destroy_All_Analyzes (Get_Kernel (Module), Close_View => False);
   end Destroy;

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
      Instance : Class_Instance;

   begin
      Name_Parameters (Data, (1 => Ana_Name_Cst'Access));

      Instance := New_Instance
        (Get_Script (Data), Code_Analysis_Module_ID.Class);
      Attach_Instance_And_Analysis (Instance, Nth_Arg (Data, 1));

      Set_Return_Value (Data, Instance);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Shell_Get_Command;

   ----------------------------------
   -- Attach_Instance_And_Analysis --
   ----------------------------------

   procedure Attach_Instance_And_Analysis
     (Instance : Class_Instance;
      Analysis : String)
   is
   begin
      Set_Data (Instance, Code_Analysis_Module_ID.Class, Analysis);
   end Attach_Instance_And_Analysis;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Name : String := -"Coverage") return Code_Analysis_Instance
   is
      use Code_Analysis_Instances;
      Cur      : Code_Analysis_Instances.Cursor;
      Analysis : Code_Analysis_Instance := null;
      Date     : Time;

   begin
      Cur := Code_Analysis_Module_ID.Analyzes.First;

      --  If the given name correspond to an existing Analysis use it
      --  Else create one using the given instance
      while Has_Element (Cur) loop
         Analysis := Element (Cur);

         if Element (Cur).Name.all = Name then
            Analysis := Element (Cur);

            exit;
         end if;

         Next (Cur);
      end loop;

      if Analysis = null then
         Analysis := new Code_Analysis_Instance_Record;
         Date := Clock;
         Analysis.Date := Date;
         Code_Analysis_Module_ID.Analyzes.Insert (Analysis);

         Analysis.Name      := new String'(Name);
         Analysis.Projects  := new Project_Maps.Map;
      end if;

      return Analysis;
   end Get_Or_Create;

   -------------------------------------
   -- Add_Gcov_Project_Info_From_Menu --
   -------------------------------------

   procedure Add_Gcov_File_Info_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Analysis : constant Code_Analysis_Instance :=
                   Get_Or_Create (To_String (CB_Data.Analysis));
   begin
      Add_Gcov_File_Info_In_Callback
        (CB_Data.Kernel, Analysis, CB_Data.Project, CB_Data.File);
      --  Build/Refresh Report of Analysis
      Show_Analysis_Report
        (CB_Data.Kernel, Analysis, CB_Data.Project, CB_Data.File);
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
      Analysis : Code_Analysis_Instance;
      Instance : Class_Instance;
      Src_Inst : Class_Instance;
      Cov_Inst : Class_Instance;
      Src_File : GNATCOLL.VFS.Virtual_File;
      Cov_File : GNATCOLL.VFS.Virtual_File;
      Prj_Name : Project_Type;
      Prj_Node : Project_Access;

   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Analysis := Get_Or_Create
        (Name => Get_Data (Instance, Code_Analysis_Module_ID.Class));

      Name_Parameters (Data, (2 => Src_File_Cst'Access,
                              3 => Cov_File_Cst'Access));
      Src_Inst := Nth_Arg
        (Data, 2, Get_File_Class (Get_Kernel (Data)),
         Default => No_Class_Instance, Allow_Null => True);

      if Src_Inst = No_Class_Instance then
         Src_File := GNATCOLL.VFS.No_File;
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
         Cov_File := GNATCOLL.VFS.No_File;
      else
         Cov_File := Get_Data (Cov_Inst);
      end if;

      Prj_Name  :=
        Get_Registry (Get_Kernel (Data)).Tree.Info (Src_File).Project;
      Prj_Node  := Get_Or_Create (Analysis.Projects, Prj_Name);

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

      Show_Analysis_Report
        (Get_Kernel (Data), Analysis, Prj_Name, Src_File);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Gcov_File_Info_From_Shell;

   ------------------------------------
   -- Add_Gcov_File_Info_In_Callback --
   ------------------------------------

   procedure Add_Gcov_File_Info_In_Callback
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance;
      Project  : Project_Type;
      File     : Virtual_File;
      From_XML : Boolean := False)
   is
      Prj_Node  : constant Code_Analysis.Project_Access :=
                    Get_Or_Create (Analysis.Projects, Project);
      File_Node : constant Code_Analysis.File_Access :=
                    Get_Or_Create (Prj_Node, File);
      Cov_File  : GNATCOLL.VFS.Virtual_File;

   begin
      Cov_File := Find_Gcov_File (Kernel, File);

      if not Is_Regular_File (Cov_File) then
         GPS.Kernel.Console.Insert
           (Kernel,
            -"Could not find coverage file " & Display_Full_Name (Cov_File));

         Set_Error (File_Node, File_Not_Found);

      else
         Add_Gcov_File_Info (Kernel, File, Cov_File, Prj_Node);
         Compute_Project_Coverage (Prj_Node);

         Coverage_GUI.Clear_File_Locations (Kernel, File_Node);
         Coverage_GUI.List_File_Uncovered_Lines (Kernel, File_Node, False);

         --  Refresh source editor annotations and locations information.
         if not From_XML then
            Coverage_GUI.Remove_File_Coverage_Annotations (Kernel, File_Node);
            Coverage_GUI.Add_File_Coverage_Annotations (Kernel, File_Node);

            Refresh_Analysis_Report (Kernel, Analysis);
         end if;
      end if;
   end Add_Gcov_File_Info_In_Callback;

   -------------------------------------
   -- Add_Gcov_Project_Info_From_Menu --
   -------------------------------------

   procedure Add_Gcov_Project_Info_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Analysis : constant Code_Analysis_Instance :=
                   Get_Or_Create (To_String (CB_Data.Analysis));

   begin
      Add_Gcov_Project_Info_In_Callback
        (CB_Data.Kernel, Analysis, CB_Data.Project);
      --  Build/Refresh Report of Analysis
      Show_Analysis_Report (CB_Data.Kernel, Analysis, CB_Data.Project);
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
      Analysis : Code_Analysis_Instance;
      Instance : Class_Instance;
      Context  : Selection_Context;
      Prj_Inst : Class_Instance;
      Prj_File : GNATCOLL.VFS.Virtual_File;
      Prj_Name : Project_Type;
      Prj_Node : Project_Access;

   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Analysis := Get_Or_Create
        (Name => Get_Data (Instance, Code_Analysis_Module_ID.Class));

      Name_Parameters (Data, (2 => Prj_File_Cst'Access));
         Prj_Inst := Nth_Arg
           (Data, 2, Get_File_Class (Get_Kernel (Data)),
            Default => No_Class_Instance, Allow_Null => True);

      if Prj_Inst = No_Class_Instance then
         Prj_File := GNATCOLL.VFS.No_File;
      else
         Prj_File := Get_Data (Prj_Inst);
      end if;

      if not Is_Regular_File (Prj_File) then
         Set_Error_Msg (Data, -"The name given for 'prj' file is wrong");
         return;
      end if;

      --  ??? We used to call Load_Or_Find, which would load the new project if
      --  it could not be found. That's seems incorrect though, since that
      --  changed the project the user had loaded

      Prj_Name := Get_Registry (Get_Kernel (Data)).Tree.Project_From_Name
        (+Prj_File.Base_Name (Suffix => Project_File_Extension));
      Prj_Node  := Get_Or_Create (Analysis.Projects, Prj_Name);
      Add_Gcov_Project_Info (Get_Kernel (Data), Prj_Node);

      --  Build/Refresh Report of Analysis
      Context := Get_Current_Context (Get_Kernel (Data));
      Set_File_Information (Context, Project => Prj_Name);
      Show_Analysis_Report (Get_Kernel (Data), Analysis, Prj_Name);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Gcov_Project_Info_From_Shell;

   ---------------------------------------
   -- Add_Gcov_Project_Info_In_Callback --
   ---------------------------------------

   procedure Add_Gcov_Project_Info_In_Callback
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance;
      Project  : Project_Type)
   is
      Prj_Node : Project_Access;
   begin
      Prj_Node := Get_Or_Create (Analysis.Projects, Project);
      Add_Gcov_Project_Info (Kernel, Prj_Node);
      Refresh_Analysis_Report (Kernel, Analysis);
   end Add_Gcov_Project_Info_In_Callback;

   -----------------------------------------
   -- Add_All_Gcov_Project_Info_From_Menu --
   -----------------------------------------

   procedure Add_All_Gcov_Project_Info_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Analysis : constant Code_Analysis_Instance :=
                   Get_Or_Create (To_String (CB_Data.Analysis));

   begin
      Add_All_Gcov_Project_Info_In_Callback
        (CB_Data.Kernel, Analysis);

      --  Build/Refresh Report of Analysis
      Show_Analysis_Report (CB_Data.Kernel, Analysis);
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
      Analysis : Code_Analysis_Instance;
      Instance : Class_Instance;
      Prj_Name : Project_Type;
      Prj_Node : Project_Access;
      Prj_Iter : Project_Iterator;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Analysis := Get_Or_Create
        (Name => Get_Data (Instance, Code_Analysis_Module_ID.Class));

      Prj_Name := Get_Project (Get_Kernel (Data));
      Prj_Iter := Start (Prj_Name);

      loop
         exit when Current (Prj_Iter) = No_Project;
         Prj_Node := Get_Or_Create (Analysis.Projects, Current (Prj_Iter));
         Add_Gcov_Project_Info (Get_Kernel (Data), Prj_Node);
         Next (Prj_Iter);
      end loop;

      --  Build/Refresh Report of Analysis
      Show_Analysis_Report (Get_Kernel (Data), Analysis);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_All_Gcov_Project_Info_From_Shell;

   -------------------------------------------
   -- Add_All_Gcov_Project_Info_In_Callback --
   -------------------------------------------

   procedure Add_All_Gcov_Project_Info_In_Callback
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance)
   is
      Prj_Iter : Project_Iterator;
      Prj_Node : Project_Access;

   begin
      Prj_Iter := Start (Get_Project (Kernel));

      while Current (Prj_Iter) /= No_Project loop
         Prj_Node := Get_Or_Create
           (Analysis.Projects, Current (Prj_Iter));
         Add_Gcov_Project_Info (Kernel, Prj_Node);
         Next (Prj_Iter);
      end loop;

      Refresh_Analysis_Report (Kernel, Analysis);
   end Add_All_Gcov_Project_Info_In_Callback;

   ----------------------------------------------
   -- Show_All_Coverage_Information_From_Shell --
   ----------------------------------------------

   procedure Show_All_Coverage_Information_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Analysis : Code_Analysis_Instance;
      Instance : Class_Instance;

   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Analysis := Get_Or_Create
        (Name => Get_Data (Instance, Code_Analysis_Module_ID.Class));

      Show_All_Coverage_Information (Get_Kernel (Data), Analysis.Projects);

      --  Build/Refresh the Coverage Report
      Show_Analysis_Report (Get_Kernel (Data), Analysis);
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
      Analysis : Code_Analysis_Instance;
      Instance : Class_Instance;

   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Analysis := Get_Or_Create
        (Name => Get_Data (Instance, Code_Analysis_Module_ID.Class));

      Hide_All_Coverage_Information (Get_Kernel (Data), Analysis.Projects);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Hide_All_Coverage_Information_From_Shell;

   -------------------------------------
   -- Show_Analysis_Report_From_Shell --
   -------------------------------------

   procedure Show_Analysis_Report_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Analysis : Code_Analysis_Instance;
      Instance : Class_Instance;

   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Analysis := Get_Or_Create
        (Name => Get_Data (Instance, Code_Analysis_Module_ID.Class));

      Show_Analysis_Report (Get_Kernel (Data), Analysis);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Analysis_Report_From_Shell;

   ------------------------------------
   -- Show_Analysis_Report_From_Menu --
   ------------------------------------

   procedure Show_Analysis_Report_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Analysis : constant Code_Analysis_Instance :=
                   Get_Or_Create (To_String (CB_Data.Analysis));

   begin
      Show_Analysis_Report (CB_Data.Kernel, Analysis);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Show_Analysis_Report_From_Menu;

   --------------------------
   -- Show_Analysis_Report --
   --------------------------

   procedure Show_Analysis_Report
     (Kernel       : Kernel_Handle;
      Analysis     : Code_Analysis_Instance;
      Project      : Project_Type := No_Project;
      File         : GNATCOLL.VFS.Virtual_File := No_File;
      Raise_Report : Boolean := True)
   is
      Local_Project : Project_Type := Project;
      Iter          : Gtk_Tree_Iter := Null_Iter;
      Path          : Gtk_Tree_Path;
      Child         : GPS_MDI_Child;
      View          : Code_Analysis_View;

   begin
      --  Build the report view

      Child := Get_Or_Create (Kernel, Analysis, Create => True);
      View := Code_Analysis_View (Get_Widget (Child));
      Clear (View.Model);

      --  Check for analysis information:

      if Local_Project = No_Project then
         Local_Project := Get_Project (Kernel);
      end if;

      declare
         Prj_Name : Project_Type;
         Prj_Node : Project_Access;
      begin
         Prj_Node := Get_Or_Create (Analysis.Projects, Local_Project);

         if Prj_Node.Analysis_Data.Coverage_Data = null then
            --  If the current context's project has no coverage data, it has
            --  to be modified or an error message is shown
            Prj_Name := First_Project_With_Coverage_Data (Analysis.Projects);

            if Prj_Name = No_Project then
               --  Show the empty report warning board
               if Get_No_Show_All (View.Error_Board) then
                  Set_No_Show_All (View.Error_Board, False);
               end if;

               Show_All (View.Error_Board);
               --  Removes Prj_Node from its container as we just created it
               Project_Maps.Delete (Analysis.Projects.all, Prj_Node.Name);
               --  Free Prj_Node
               Free_Project (Prj_Node);

               if Raise_Report then
                  Raise_Child (Child);
               end if;
            end if;
         else
            Hide_All (View.Error_Board);
         end if;
      end;

      --  Here we have a context that point on elements that will be added to
      --  the coverage report

      --  Fill the report:

      Fill_Iter
        (View.Model, Iter,
         Analysis.Projects,
         Binary_Coverage_Mode,
         View.Icons);

      --  Selection of the context caller:

      Iter := Get_Iter_From_Context (Local_Project, File, View.Model);

      if Iter = Null_Iter then
         Iter := Get_Iter_First (View.Model);
      end if;

      if Iter /= Null_Iter then
         Path := Get_Path (View.Model, Iter);
         Collapse_All (View.Tree);
         Expand_To_Path (View.Tree, Path);
         Select_Path (Get_Selection (View.Tree), Path);
         Path_Free (Path);
      end if;

      if Raise_Report then
         Raise_Child (Child);
      end if;

      return;
   end Show_Analysis_Report;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance;
      Create   : Boolean) return GPS_MDI_Child
   is
      Child : GPS_MDI_Child;
      View  : Code_Analysis_View;

   begin
      if Kernel.Is_In_Destruction then
         return null;
      end if;

      Child := GPS_MDI_Child
        (Find_MDI_Child_By_Name
           (Get_MDI (Kernel),
            Analysis.Name.all & (-" Report")));

      if Child = null and then Create then
         --  Create the report view

         View := Build_Analysis_Report
           (Kernel,
            Analysis.Name,
            Analysis.Projects,
            Binary_Coverage_Mode);

         --  Create the MDI child

         GPS.Kernel.MDI.Gtk_New
           (Child, View,
            Group  => Group_Default,
            Module => Code_Analysis_Module_ID);
         Set_Title
           (Child,
            Analysis.Name.all & (-" Report"));

         --  Connect handlers

         Analysis_CB.Connect
           (View.Load_Button,
            Gtk.Button.Signal_Clicked,
            Analysis_CB.To_Marshaller
              (Add_All_Gcov_Project_Info_From_Menu'Access),
            CB_Data_Record'
              (Kernel, To_Unbounded_String (Analysis.Name.all),
               No_Project, No_File));

         Register_Contextual_Menu
           (Kernel          => Kernel,
            Event_On_Widget => View.Tree,
            Object          => View,
            ID              => Module_ID (Code_Analysis_Module_ID),
            Context_Func    => Context_Func'Access);
         Kernel_Return_Cb.Object_Connect
           (View.Tree, Signal_Button_Press_Event,
            Kernel_Return_Cb.To_Marshaller
              (On_Double_Click'Access), View, Kernel);

         Put (Get_MDI (Kernel), Child);
      end if;

      return Child;
   end Get_Or_Create;

   -----------------------------
   -- Refresh_Analysis_Report --
   -----------------------------

   procedure Refresh_Analysis_Report
     (Kernel   : Kernel_Handle;
      Analysis : Code_Analysis_Instance)
   is
   begin
      if Get_Or_Create (Kernel, Analysis, False) /= null then
         Show_Analysis_Report (Kernel, Analysis, No_Project, No_File, False);
      end if;
   end Refresh_Analysis_Report;

   ---------------------------
   -- Get_Iter_From_Context --
   ---------------------------

   function Get_Iter_From_Context
     (Project : Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Model   : Gtk_Tree_Store) return Gtk_Tree_Iter
   is
      Iter    : Gtk_Tree_Iter := Get_Iter_First (Model);
      Num_Col : constant Gint := 1;
   begin
      if Iter /= Null_Iter then
         if not Has_Child (Model, Iter) then
            --  So we are in a flat list
            if File /= No_File then
               --  Find in the list the context's file
               loop
                  exit when Iter = Null_Iter or else
                  Get_String (Model, Iter, Num_Col) = Display_Base_Name (File);
                  Next (Model, Iter);
               end loop;
            else
               --  Find in the list the context's project
               loop
                  exit when Iter = Null_Iter or else
                  Get_String (Model, Iter, Num_Col) = Project.Name;
                  Next (Model, Iter);
               end loop;
            end if;
         else
            --  Find in the tree the context's project
            loop
               exit when Iter = Null_Iter or else
               Get_String (Model, Iter, Num_Col) = Project.Name;
               Next (Model, Iter);
            end loop;

            if File /= No_File then
               --  So we also have file information
               Iter := Children (Model, Iter);

               --  Find in the tree the context's file
               loop
                  exit when Iter = Null_Iter or else
                  Get_String (Model, Iter, Num_Col) = Display_Base_Name (File);
                  Next (Model, Iter);
               end loop;
            end if;
         end if;
      end if;

      return Iter;
   end Get_Iter_From_Context;

   -------------------------------------
   -- Destroy_All_Analyzes_From_Shell --
   -------------------------------------

   procedure Destroy_All_Analyzes_From_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);

   begin
      Destroy_All_Analyzes (Get_Kernel (Data));

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
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Project_Changing_Hook;

   ----------------------------
   -- On_Project_Loaded_Hook --
   ----------------------------

   procedure On_Project_View_Changed_Hook
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Node     : Node_Ptr;
      Analysis : Code_Analysis_Instance;

      procedure On_New_File (Project : Project_Type; File : Virtual_File);
      --  Called when a File node is found while parsing XML.

      -----------------
      -- On_New_File --
      -----------------

      procedure On_New_File (Project : Project_Type; File : Virtual_File) is
      begin
         Add_Gcov_File_Info_In_Callback
           (Kernel_Handle (Kernel), Analysis, Project, File, True);
      end On_New_File;

      procedure Parse_XML is new Code_Analysis_XML.Parse_Desktop_XML
        (On_New_File);

   begin
      Node := Get_XML_Content
        (Get_MDI (Kernel), "Code_Analysis_Tree");

      if Node /= null then
         Analysis :=
           Get_Or_Create
             (To_String
                  (Code_Analysis_Module_ID.Registered_Analysis.First_Element));
         Parse_XML
           (Get_Project (Kernel),
            Node);
         Refresh_Analysis_Report (Kernel_Handle (Kernel), Analysis);
      end if;
   end On_Project_View_Changed_Hook;

   --------------------------
   -- Destroy_All_Analyzes --
   --------------------------

   procedure Destroy_All_Analyzes
     (Kernel     : Kernel_Handle;
      Close_View : Boolean := True)
   is
      use Code_Analysis_Instances;
      Cur      : Cursor := Code_Analysis_Module_ID.Analyzes.First;
      Analysis : Code_Analysis_Instance;

   begin
      while Has_Element (Cur) loop
         Analysis := Element (Cur);
         Next (Cur);
         Destroy_Analysis_Instance (Kernel, Analysis, Close_View);
      end loop;

      Code_Analysis_Module_ID.Analyzes.Clear;
   end Destroy_All_Analyzes;

   -------------------------------
   -- Destroy_Analysis_Instance --
   -------------------------------

   procedure Destroy_Analysis_Instance
     (Kernel     : Kernel_Handle;
      Analysis   : in out Code_Analysis_Instance;
      Close_View : Boolean)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Code_Analysis_Instance_Record, Code_Analysis_Instance);
      Child     : GPS_MDI_Child;
      View      : Code_Analysis_View;

   begin
      Child := Get_Or_Create (Kernel, Analysis, False);

      if Child /= null then
         if Close_View then
            Close_Child (Child, Force => True);
         else
            View := Code_Analysis_View (Get_Widget (Child));
            View.Clear;
         end if;
      end if;

      Get_Messages_Container (Kernel).Remove_Category
        (Uncovered_Category, Coverage_Message_Flags);
      Get_Messages_Container (Kernel).Remove_Category
        (Partially_Covered_Category, Coverage_Message_Flags);
      Remove_Line_Information_Column (Kernel, No_File, CodeAnalysis_Cst);
      Free_Code_Analysis (Analysis.Projects);

      if Code_Analysis_Module_ID.Analyzes.Contains (Analysis) then
         Code_Analysis_Module_ID.Analyzes.Delete (Analysis);
      end if;

      GNAT.Strings.Free (Analysis.Name);
      Unchecked_Free (Analysis);
   end Destroy_Analysis_Instance;

   -------------------------------------------------
   -- Show_Project_Coverage_Information_From_Menu --
   -------------------------------------------------

   procedure Show_Project_Coverage_Information_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Analysis : constant Code_Analysis_Instance :=
                   Get_Or_Create (To_String (CB_Data.Analysis));
      Prj_Node  : constant Project_Access :=
                    Get_Or_Create (Analysis.Projects, CB_Data.Project);
   begin
      if not Have_Gcov_Info (Analysis.Projects, CB_Data.Project) then
         Add_Gcov_Project_Info_In_Callback
           (CB_Data.Kernel, Analysis, CB_Data.Project);

         if not Have_Gcov_Info (Analysis.Projects, CB_Data.Project) then
            GPS.Kernel.Console.Insert
              (CB_Data.Kernel,
               -"No coverage information to display for "
               & Prj_Node.Name.Name);

            return;
         end if;
      end if;

      List_Project_Uncovered_Lines (CB_Data.Kernel, Prj_Node);
      Add_Project_Coverage_Annotations (CB_Data.Kernel, Prj_Node);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Project_Coverage_Information_From_Menu;

   -------------------------------------------------
   -- Hide_Project_Coverage_Information_From_Menu --
   -------------------------------------------------

   procedure Hide_Project_Coverage_Information_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Analysis : constant Code_Analysis_Instance :=
                   Get_Or_Create (To_String (CB_Data.Analysis));
      Project_Node : constant Project_Access :=
                       Get_Or_Create (Analysis.Projects, CB_Data.Project);

   begin
      Clear_Project_Locations (CB_Data.Kernel, Project_Node);
      Remove_Project_Coverage_Annotations (CB_Data.Kernel, Project_Node);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Hide_Project_Coverage_Information_From_Menu;

   ----------------------------------------------
   -- Show_File_Coverage_Information_From_Menu --
   ----------------------------------------------

   procedure Show_File_Coverage_Information_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Analysis  : constant Code_Analysis_Instance :=
                    Get_Or_Create (To_String (CB_Data.Analysis));
      Prj_Node  : constant Project_Access :=
                    Get_Or_Create (Analysis.Projects, CB_Data.Project);
      File_Node : constant Code_Analysis.File_Access :=
                    Get_Or_Create (Prj_Node, CB_Data.File);

   begin
      if not Have_Gcov_Info
        (Analysis.Projects, CB_Data.Project, CB_Data.File)
      then
         Add_Gcov_File_Info_In_Callback
           (CB_Data.Kernel, Analysis, CB_Data.Project, CB_Data.File);

         if not Have_Gcov_Info
           (Analysis.Projects, CB_Data.Project, CB_Data.File)
         then
            GPS.Kernel.Console.Insert
              (CB_Data.Kernel,
               -"No coverage information to display for "
               & Display_Base_Name (File_Node.Name));
            return;
         end if;
      end if;

      --  Call Open_File_Editor with Line = 0 so that, if the editor is already
      --  open, we do not jump to line 1.
      Open_File_Editor (CB_Data.Kernel, File_Node.Name, Line => 0);
      List_File_Uncovered_Lines (CB_Data.Kernel, File_Node, False);
      Add_File_Coverage_Annotations (CB_Data.Kernel, File_Node);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_File_Coverage_Information_From_Menu;

   ----------------------------------------------
   -- Hide_File_Coverage_Information_From_Menu --
   ----------------------------------------------

   procedure Hide_File_Coverage_Information_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Analysis  : constant Code_Analysis_Instance :=
                    Get_Or_Create (To_String (CB_Data.Analysis));
      Prj_Node  : constant Project_Access :=
                    Get_Or_Create (Analysis.Projects, CB_Data.Project);
      File_Node : constant Code_Analysis.File_Access :=
                    Get_Or_Create (Prj_Node, CB_Data.File);

   begin
      Clear_File_Locations (CB_Data.Kernel, File_Node);
      Remove_File_Coverage_Annotations (CB_Data.Kernel, File_Node);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Hide_File_Coverage_Information_From_Menu;

   ---------------------------
   -- Remove_File_From_Menu --
   ---------------------------

   procedure Remove_File_From_Menu
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Analysis  : constant Code_Analysis_Instance :=
                    Get_Or_Create (To_String (CB_Data.Analysis));
      Prj_Node  : constant Project_Access :=
                    Get_Or_Create (Analysis.Projects, CB_Data.Project);
      File_Node : Code_Analysis.File_Access :=
                    Get_Or_Create (Prj_Node, CB_Data.File);
      File_Iter : Gtk_Tree_Iter;
      Prj_Iter  : Gtk_Tree_Iter;
      Child     : GPS_MDI_Child;
      View      : Code_Analysis_View;

   begin
      Child := Get_Or_Create (CB_Data.Kernel, Analysis, False);

      if Child = null then
         Show_Analysis_Report
           (CB_Data.Kernel, Analysis, CB_Data.Project, CB_Data.File);
         Child := Get_Or_Create (CB_Data.Kernel, Analysis, False);
      end if;

      View := Code_Analysis_View (Get_Widget (Child));

      if Have_Gcov_Info
        (Analysis.Projects, CB_Data.Project, CB_Data.File)
      then
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
         Clear_File_Locations (CB_Data.Kernel, File_Node);
         --  Removes potential annotations
         Remove_File_Coverage_Annotations (CB_Data.Kernel, File_Node);
      end if;

      File_Iter := Get_Iter_From_Context
        (CB_Data.Project, CB_Data.File, View.Model);

      if File_Iter /= Null_Iter then
         Prj_Iter  := Parent (View.Model, File_Iter);
         Fill_Iter (View.Model, Prj_Iter,
                    Prj_Node.Analysis_Data, Binary_Coverage_Mode);
         --  Removes File_Iter from the report
         Remove (View.Model, File_Iter);
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
     (Widget  : access Glib.Object.GObject_Record'Class;
      CB_Data : CB_Data_Record)
   is
      pragma Unreferenced (Widget);
      Iter     : Gtk_Tree_Iter;
      Analysis : constant Code_Analysis_Instance :=
                   Get_Or_Create (To_String (CB_Data.Analysis));
      Prj_Node : Project_Access :=
                   Get_Or_Create (Analysis.Projects, CB_Data.Project);
      Child    : GPS_MDI_Child;
      View     : Code_Analysis_View;

   begin
      Child := Get_Or_Create (CB_Data.Kernel, Analysis, False);

      if Child = null then
         Show_Analysis_Report (CB_Data.Kernel, Analysis, CB_Data.Project);
         Child := Get_Or_Create (CB_Data.Kernel, Analysis, False);
      end if;

      View := Code_Analysis_View (Get_Widget (Child));

      Iter := Get_Iter_From_Context (CB_Data.Project, No_File, View.Model);

      if Iter /= Null_Iter then
         --  Removes Iter from the report
         Remove (View.Model, Iter);
      end if;

      --  Removes potential listed locations
      Clear_Project_Locations (CB_Data.Kernel, Prj_Node);
      --  Removes potential src_editor annotations
      Remove_Project_Coverage_Annotations (CB_Data.Kernel, Prj_Node);
      --  Removes Prj_Node from its container
      Project_Maps.Delete (Analysis.Projects.all, Prj_Node.Name);
      --  Free Prj_Node
      Free_Project (Prj_Node);

      if Project_Maps.Length (Analysis.Projects.all) = 0 then
         Refresh_Analysis_Report (CB_Data.Kernel, Analysis);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Remove_Project_From_Menu;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access Code_Analysis_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Submenu : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      use Code_Analysis_Instances;
      pragma Unreferenced (Factory, Object);
      Analysis : constant String :=
         To_String (Code_Analysis_Module_ID.Registered_Analysis.First_Element);
      Item     : Gtk_Menu_Item;
      Sep      : Gtk_Separator_Menu_Item;

   begin
      if Has_File_Information (Context) then
         Gtk_New (Item, -"Show coverage information");
         Append (Submenu, Item);
         Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Analysis_CB.To_Marshaller
              (Show_File_Coverage_Information_From_Menu'Access),
            CB_Data_Record'
              (Kernel   => Get_Kernel (Context),
               Analysis => To_Unbounded_String (Analysis),
               Project  => Project_Information (Context),
               File     => File_Information (Context)));

         Gtk_New (Item, -"Hide coverage information");
         Append (Submenu, Item);
         Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Analysis_CB.To_Marshaller
              (Hide_File_Coverage_Information_From_Menu'Access),
            CB_Data_Record'
              (Kernel   => Get_Kernel (Context),
               Analysis => To_Unbounded_String (Analysis),
               Project  => Project_Information (Context),
               File     => File_Information (Context)));

         Gtk_New (Sep);
         Append (Submenu, Sep);

         Gtk_New
           (Item, -"Load data for " &
            Emphasize (Display_Base_Name (File_Information (Context))));
         Activate_Pango_Markup (Item);
         Append (Submenu, Item);
         Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Analysis_CB.To_Marshaller
              (Add_Gcov_File_Info_From_Menu'Access),
            CB_Data_Record'
              (Kernel   => Get_Kernel (Context),
               Analysis => To_Unbounded_String (Analysis),
               Project  => Project_Information (Context),
               File     => File_Information (Context)));

         Gtk_New
           (Item, -"Remove data of " &
            Emphasize (Display_Base_Name (File_Information (Context))));
         Activate_Pango_Markup (Item);
         Append (Submenu, Item);
         Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Analysis_CB.To_Marshaller
              (Remove_File_From_Menu'Access),
            CB_Data_Record'
              (Kernel   => Get_Kernel (Context),
               Analysis => To_Unbounded_String (Analysis),
               Project  => Project_Information (Context),
               File     => File_Information (Context)));

      else
         Gtk_New (Item, -"Show coverage information");
         Append (Submenu, Item);
         Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Analysis_CB.To_Marshaller
              (Show_Project_Coverage_Information_From_Menu'Access),
            CB_Data_Record'
              (Kernel   => Get_Kernel (Context),
               Analysis => To_Unbounded_String (Analysis),
               Project  => Project_Information (Context),
               File     => No_File));

         Gtk_New (Item, -"Hide coverage information");
         Append (Submenu, Item);
         Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Analysis_CB.To_Marshaller
              (Hide_Project_Coverage_Information_From_Menu'Access),
            CB_Data_Record'
              (Kernel   => Get_Kernel (Context),
               Analysis => To_Unbounded_String (Analysis),
               Project  => Project_Information (Context),
               File     => No_File));

         Gtk_New (Sep);
         Append (Submenu, Sep);

         Gtk_New
           (Item, -"Load data for project " &
            Emphasize (Project_Information (Context).Name));
         Activate_Pango_Markup (Item);
         Append (Submenu, Item);
         Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Analysis_CB.To_Marshaller
              (Add_Gcov_Project_Info_From_Menu'Access),
            CB_Data_Record'
              (Kernel   => Get_Kernel (Context),
               Analysis => To_Unbounded_String (Analysis),
               Project  => Project_Information (Context),
               File     => No_File));

         Gtk_New
           (Item, -"Remove data of project " &
            Emphasize (Project_Information (Context).Name));
         Activate_Pango_Markup (Item);
         Append (Submenu, Item);
         Analysis_CB.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Analysis_CB.To_Marshaller
              (Remove_Project_From_Menu'Access),
            CB_Data_Record'
              (Kernel   => Get_Kernel (Context),
               Analysis => To_Unbounded_String (Analysis),
               Project  => Project_Information (Context),
               File     => No_File));
      end if;

      if Get_Creator (Context) /=
        Abstract_Module_ID (Code_Analysis_Module_ID)
      then
         Gtk_New (Sep);
         Append (Submenu, Sep);

         Gtk_New (Item, -"Show Coverage Report");
         Append (Submenu, Item);
         Analysis_CB.Connect
           (Item,
            Gtk.Menu_Item.Signal_Activate,
            Analysis_CB.To_Marshaller
              (Show_Analysis_Report_From_Menu'Access),
            CB_Data_Record'
              (Kernel   => Get_Kernel (Context),
               Analysis => To_Unbounded_String (Analysis),
               Project  => No_Project,
               File     => No_File));
      end if;
   end Append_To_Menu;

   -------------------------
   -- On_Single_View_Menu --
   -------------------------

   procedure On_Single_View_Menu
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Show_Analysis_Report (Kernel, Get_Or_Create);
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
      use Code_Analysis_Instances;

   begin
      Add_All_Gcov_Project_Info_From_Menu
        (Widget  => Widget,
         CB_Data => CB_Data_Record'
           (Kernel   => Kernel,
            Analysis =>
              Code_Analysis_Module_ID.Registered_Analysis.First_Element,
            Project  => Get_Project (Kernel),
            File     => No_File));

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
      use Code_Analysis_Instances;
      Prj : Project_Type := Project_Information (Get_Current_Context (Kernel));

   begin
      if Prj = No_Project then
         Prj := Get_Project (Kernel);
      end if;

      Add_Gcov_Project_Info_From_Menu
        (Widget  => Widget,
         CB_Data => CB_Data_Record'
           (Kernel   => Kernel,
            Analysis =>
              Code_Analysis_Module_ID.Registered_Analysis.First_Element,
            Project  => Prj,
            File     => No_File));

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
      Prj      : Project_Type :=
                   Project_Information (Get_Current_Context (Kernel));
      File     : constant Virtual_File :=
                   File_Information (Get_Current_Context (Kernel));

   begin
      if Prj = No_Project then
         Prj := Get_Project (Kernel);
      end if;

      Add_Gcov_File_Info_From_Menu
        (Widget => Widget,
         CB_Data => CB_Data_Record'
           (Kernel   => Kernel,
            Analysis =>
              Code_Analysis_Module_ID.Registered_Analysis.First_Element,
            Project  => Prj,
            File     => File));

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
      pragma Unreferenced (Widget);

   begin
      Destroy_All_Analyzes (Kernel, False);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Clear_Coverage_Menu;

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
      Root     : Node_Ptr;
      Analysis : Code_Analysis_Instance;

   begin
      if Widget.all in Code_Analysis_View_Record'Class then
         Analysis := Get_Analysis (Code_Analysis_View (Widget));
         Root     := new XML_Utils.Node;
         Root.Tag := new String'("Code_Analysis_Tree");
         Set_Attribute (Root, "name", Analysis.Name.all);
         Dump_Desktop_XML (Analysis.Projects, Root);

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
      Analysis    : Code_Analysis_Instance;

   begin
      if Node.Tag.all = "Code_Analysis_Tree" then
         Analysis := Get_Or_Create (Get_Attribute (Node, "name"));

         --  We do not parse the subtree, as the project is not loaded yet,
         --  and we need it loaded to restore the view. This will be done later
         --  upon "project_changed" hook.
         --  We just create the report view and display it.
         return MDI_Child (Get_Or_Create (User, Analysis, Create => True));
      else
         return null;
      end if;
   end Load_Desktop;

   ------------------
   -- Dump_To_File --
   ------------------

   procedure Dump_To_File
     (Analysis : Code_Analysis_Instance;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Root : Node_Ptr;
   begin
      Root     := new XML_Utils.Node;
      Root.Tag := new String'("Code_Analysis_Tree");
      Set_Attribute (Root, "name", Analysis.Name.all);
      Dump_Full_XML (Analysis.Projects, Root);
      Print (Root, File);
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
      Analysis  : Code_Analysis_Instance;
      Instance  : Class_Instance;
      File_Inst : Class_Instance;
      File_Dump : GNATCOLL.VFS.Virtual_File;

   begin
      --  Check if the attached Analysis is still there
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Analysis := Get_Or_Create
        (Name => Get_Data (Instance, Code_Analysis_Module_ID.Class));

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

      Dump_To_File (Analysis, File_Dump);
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
      Analysis    : Code_Analysis_Instance;
      Instance    : Class_Instance;
      File_Inst   : Class_Instance;
      Loaded_File : GNATCOLL.VFS.Virtual_File;
      Root_Node   : Node_Ptr;

   begin
      --  Check if the attached Analysis is still there
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Analysis := Get_Or_Create
        (Name => Get_Data (Instance, Code_Analysis_Module_ID.Class));

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

      Root_Node := Parse (Loaded_File);
      Parse_Full_XML
        (GPS.Kernel.Project.Get_Registry (Get_Kernel (Data)),
         Analysis.Projects,
         Root_Node.Child);

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
      Tools               : constant String := '/' & (-"Tools");
      Coverage            : constant String := -"Cov_erage";
      Views               : constant String := -"Views";
      Sep                 : Gtk_Separator_Menu_Item;

   begin
      Binary_Coverage_Mode          := Active (Binary_Coverage_Trace);

      Code_Analysis_Module_ID       := new Code_Analysis_Module_ID_Record;
      Code_Analysis_Module_ID.Class := Code_Analysis_Class;
      Code_Analysis_Module_ID.Registered_Analysis.Insert
        (To_Unbounded_String ("Coverage"));

      Contextual_Menu               := new Code_Analysis_Contextual_Menu;
      Register_Module
        (Module      => Code_Analysis_Module_ID,
         Kernel      => Kernel,
         Module_Name => CodeAnalysis_Cst);
      Register_Contextual_Submenu
        (Kernel      => Kernel,
         Name        => -"Coverage",
         Filter      => Lookup_Filter (Kernel, "Project only")
                          or Lookup_Filter (Kernel, "In project"),
         Submenu     => Submenu_Factory (Contextual_Menu));

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Tools & '/' & Coverage,
         Text        => -"_Show report",
         Callback    => On_Single_View_Menu'Access,
         Ref_Item    => -"Documentation",
         Add_Before  => True);

      Gtk_New (Sep);
      Register_Menu (Kernel, Tools & '/' & Coverage, Sep);

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

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Tools & '/' & Coverage,
         Text        => -"C_lear coverage from memory",
         Callback    => On_Clear_Coverage_Menu'Access,
         Ref_Item    => -"Documentation",
         Add_Before  => False);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Tools & '/' & Views,
         Text        => -"Coverage Repor_t",
         Ref_Item    => -"Clipboard",
         Add_Before  => False,
         Callback    => On_Single_View_Menu'Access);

      Add_Hook
        (Kernel  => Kernel,
         Hook    => Project_Changing_Hook,
         Func    => Wrapper (On_Project_Changing_Hook'Access),
         Name    => "code_analysis.project_changing");
      Add_Hook
        (Kernel  => Kernel,
         Hook    => Project_View_Changed_Hook,
         Func    => Wrapper (On_Project_View_Changed_Hook'Access),
         Name    => "code_analysis.project_view_changed");
      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);

      --  Shell commands registration
      Register_Command
        (Kernel, Constructor_Method,
         Class         => Code_Analysis_Class,
         Handler       => Shell_CodeAnalysis_Constructor'Access);
      Register_Command
        (Kernel, "get",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Code_Analysis_Class,
         Handler       => Shell_Get_Command'Access,
         Static_Method => True);
      Register_Command
        (Kernel, "add_all_gcov_project_info",
         Class         => Code_Analysis_Class,
         Handler       => Add_All_Gcov_Project_Info_From_Shell'Access);
      Register_Command
        (Kernel, "add_gcov_project_info",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Code_Analysis_Class,
         Handler       => Add_Gcov_Project_Info_From_Shell'Access);
      Register_Command
        (Kernel, "add_gcov_file_info",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Code_Analysis_Class,
         Handler       => Add_Gcov_File_Info_From_Shell'Access);
      Register_Command
        (Kernel, "show_coverage_information",
         Class         => Code_Analysis_Class,
         Handler       => Show_All_Coverage_Information_From_Shell'Access);
      Register_Command
        (Kernel, "hide_coverage_information",
         Class         => Code_Analysis_Class,
         Handler       => Hide_All_Coverage_Information_From_Shell'Access);
      Register_Command
        (Kernel, "show_analysis_report",
         Class         => Code_Analysis_Class,
         Handler       => Show_Analysis_Report_From_Shell'Access);
      Register_Command
        (Kernel, "dump_to_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Code_Analysis_Class,
         Handler       => Dump_To_File_From_Shell'Access);
      Register_Command
        (Kernel, "load_from_file",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Code_Analysis_Class,
         Handler       => Load_From_File_From_Shell'Access);
      Register_Command
        (Kernel, "clear",
         Class         => Code_Analysis_Class,
         Handler       => Destroy_All_Analyzes_From_Shell'Access);

      Coverage_GUI.Register_Module (Kernel);
   end Register_Module;

end Code_Analysis_Module;
