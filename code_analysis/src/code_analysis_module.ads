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
--  </description>

with Ada.Unchecked_Deallocation;

with Traces;               use Traces;
with Code_Analysis;        use Code_Analysis;

with GPS.Kernel;           use GPS.Kernel;
with GPS.Kernel.Modules;   use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;   use GPS.Kernel.Scripts;
with GPS.Kernel.MDI;       use GPS.Kernel.MDI;

with Gdk.Pixbuf;           use Gdk.Pixbuf;

with Gtk.Box;              use Gtk.Box;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Menu;             use Gtk.Menu;
with Gdk.Event;            use Gdk.Event;
with Glib;
with Glib.Object;

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
   type Code_Analysis_Class_Record;

   type Code_Analysis_Class is access all Code_Analysis_Class_Record;

   type Code_Analysis_View_Record is new Gtk_Hbox_Record with record
      Tree        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
      Iter        : Gtk_Tree_Iter;
      Node_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cov_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
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

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Code_Analysis_Class_Record, Code_Analysis_Class);

   Code_Analysis_Cst_Str      : constant String := "code_analysis";
   Me : constant Debug_Handle := Create (Code_Analysis_Cst_Str);

   procedure Create
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Create a new instance of Code_Analysis data structure

   procedure Add_Gcov_Info
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Add node and coverage info provided by a gcov file parsing

   procedure Show_Tree_View
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Creata an display a Code_Analysis tree view within a MDI Child

   procedure Destroy
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Free memory used by a Code_Analysis instance

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Determine the content of the contextual menu displays for the Coverage
   --  Report MDI child nodes

   procedure Add_Gcov_File_Annotations
     (Object : access Gtk_Widget_Record'Class);
   --  Callback attached to the "View with coverage annotations" contextual
   --  menu entry of a File node in the Coverage Report

   procedure Add_Gcov_Subp_Annotations
     (Object : access Gtk_Widget_Record'Class);
   --  Callback attached to the "View with coverage annotations" contextual
   --  menu entry of a Subprogram node in the Coverage Report

   procedure Add_Gcov_Annotations (File_Node : Code_Analysis.File_Access);
   --  Actually add the coverage annotation columns to a VFS.Virtual_File
   --  displayed in a source editor of the MDI
   --  Is called by both Add_Gcov_File_Annotations & Add_Gcov_Subp_Annotations

   procedure Remove_Gcov_File_Annotations
     (Object : access Gtk_Widget_Record'Class);
   --  Callback attached to the "Remove coverage annotations" contextual
   --  menu entry of a File node in the Coverage Report

   procedure Remove_Gcov_Subp_Annotations
     (Object : access Gtk_Widget_Record'Class);
   --  Callback attached to the "Remove coverage annotations" contextual
   --  menu entry of a Subprogram node in the Coverage Report

   procedure Remove_Gcov_Annotations (File_Node : Code_Analysis.File_Access);
   --  Actually remove the coverage annotation columns to a VFS.Virtual_File
   --  displayed in a source editor of the MDI
   --  Is called by both Remove_Gcov_File_Annotations
   --  & Remove_Gcov_Subp_Annotations
end Code_Analysis_Module;
