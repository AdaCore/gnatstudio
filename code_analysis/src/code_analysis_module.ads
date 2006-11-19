-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

with Traces;               use Traces;
with Code_Analysis;        use Code_Analysis;

with GPS.Kernel;           use GPS.Kernel;
with GPS.Kernel.Modules;   use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;   use GPS.Kernel.Scripts;
with GPS.Kernel.MDI;       use GPS.Kernel.MDI;
with GPS.Intl;             use GPS.Intl;

with Glib;
with Glib.Object;

with Gdk.Pixbuf;           use Gdk.Pixbuf;
with Gdk.Event;            use Gdk.Event;

with Gtk.Box;              use Gtk.Box;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Handlers;         use Gtk.Handlers;

package Code_Analysis_Module is

   type Code_Analysis_Module_ID_Record is new Module_ID_Record with record
      Kernel         : Kernel_Handle;
      Class          : Class_Type;
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
   type Code_Analysis_View_Record is new Gtk_Hbox_Record with record
      Tree        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
      Iter        : Gtk_Tree_Iter;
      Node_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cov_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cov_Percent : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Instance    : Class_Instance;
   end record;

   type Code_Analysis_View is access all Code_Analysis_View_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Code_Analysis_View_Record, Code_Analysis_View);

   type Code_Analysis_Class_Record is new Instance_Property_Record with record
      Projects : Code_Analysis_Tree;
      View     : Code_Analysis_View;
      Child    : GPS_MDI_Child;
   end record;

   type Code_Analysis_Class is access all Code_Analysis_Class_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Code_Analysis_Class_Record, Code_Analysis_Class);

   type Has_Coverage_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Has_Coverage_Filter;
      Context : Selection_Context) return Boolean;
   --  True when the current context is associated with project, file or
   --  subprogram information

   type Coverage_Contextual_Menu is new Submenu_Factory_Record
   with record
      Code_Analysis : Code_Analysis_Class;
   end record;

   type Coverage_Contextual_Menu_Access is access all Coverage_Contextual_Menu;

   procedure Append_To_Menu
     (Factory : access Coverage_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Fill Menu with the contextual menu for the Coverage module in ,
   --  if Context is appropriate.

   type Context_And_Code_Analysis is record
      Context       : Selection_Context;
      Code_Analysis : Code_Analysis_Class;
   end record;

   package Context_And_Code_Analysis_CB is new User_Callback
     (Glib.Object.GObject_Record, Context_And_Code_Analysis);
   --  Used to connect handlers on the global Coverage contextual menu

   Code_Analysis_Cst_Str : constant String := "code_analysis";
   Coverage_Category     : constant Glib.UTF8_String := -"Not covered lines";
   Me : constant Debug_Handle := Create (Code_Analysis_Cst_Str);

   procedure Create
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Create a new instance of Code_Analysis data structure

   procedure Shell_Add_Gcov_Info
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add node and coverage info provided by a gcov file parsing

   procedure Shell_List_Lines_Not_Covered
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add in the location view every not covered lines of any projects loaded
   --  in the Code_Analysis structure of the current Instance.

   procedure Shell_Show_Tree_View
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Creata an display a Code_Analysis tree view within a MDI Child

   procedure Destroy
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Free memory used by a Code_Analysis instance

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal that cleanly destroy the widget

   function On_Double_Click (View  : access Gtk_Widget_Record'Class;
                             Event : Gdk_Event) return Boolean;
   --  Callback for the "2button_press" signal that show the File or Subprogram
   --  indicated by the selected Coverage Report tree node

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Determine the content of the contextual menu displays for the Coverage
   --  Report MDI child nodes

   procedure Add_Coverage_Annotations_From_Report
     (Object : access Gtk_Widget_Record'Class);
   --  Callback attached to the "View with coverage annotations" contextual
   --  menu entry of a File or Subprogram nodes in a Coverage Report

   procedure Add_Coverage_Annotations_From_Context
     (Widget : access Glib.Object.GObject_Record'Class;
      C      : Context_And_Code_Analysis);
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
     (Widget : access Glib.Object.GObject_Record'Class;
      C      : Context_And_Code_Analysis);
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

   procedure List_Lines_Not_Covered_In_File_From_Report
     (Object : access Gtk_Widget_Record'Class);
   --  Callback of contextual menu for File rows in a Coverage Report. Opens
   --  the select File in a source editor and call
   --  List_Lines_Not_Covered_In_File.

   procedure List_Lines_Not_Covered_In_Project
     (Project_Node : Project_Access);
   --  Add to the location view the unexecuted lines of the given Project of a
   --  Coverage Report.

   procedure List_Lines_Not_Covered_In_File
     (File_Node : Code_Analysis.File_Access);
   --  Add to the location view the unexecuted lines of the given File of a
   --  Coverage Report.

   procedure Load_Coverage_Information
     (Widget : access Glib.Object.GObject_Record'Class;
      C      : Context_And_Code_Analysis);
   --  Not yet implemented
   --  Will try to load every .gcov files of the project in the
   --  Selection_Context of the Context_And_Code_Analysis in its Code_Analysis
   --  structure
end Code_Analysis_Module;
