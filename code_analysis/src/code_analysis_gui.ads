-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2008, AdaCore                 --
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
--  This package provides the graphical user interface subprograms for
--  Code Analysis Module use
--  </description>

with GNAT.Strings;         use GNAT.Strings;
with Glib.Object;
with Glib;                 use Glib;
with Gdk.Event;            use Gdk.Event;
with Gdk.Pixbuf;           use Gdk.Pixbuf;
with Gtk.Menu;
with Gtk.Box;              use Gtk.Box;
with Gtk.Button;           use Gtk.Button;
with Gtk.Object;           use Gtk.Object;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget;           use Gtk.Widget;
with GPS.Kernel;           use GPS.Kernel;
with Code_Analysis;        use Code_Analysis;

package Code_Analysis_GUI is

   Prj_Pixbuf_Cst  : constant String := "gps-project-closed";
   --  Name of the pixbuf used for project node in the analysis report
   File_Pixbuf_Cst : constant String := "gps-file";
   --  Name of the pixbuf used for file node in the analysis report
   Subp_Pixbuf_Cst : constant String := "gps-entity-subprogram";
   --  Name of the pixbuf used for subprogram node in the analysis report

   Pix_Col  : constant := 0;
   --  Gtk_Tree_Model column number dedicated to the icons associated with each
   --  node of code_analysis data structure
   Name_Col : constant := 1;
   --  Gtk_Tree_Model column number dedicated to the name of the nodes of
   --  code_analysis structure
   Node_Col : constant := 2;
   --  Gtk_Tree_Model column number dedicated to the nodes of code_analysis
   --  structure
   File_Col : constant := 3;
   --  Gtk_Tree_Model column number dedicated to the node corresponding file
   --  of the code_analysis structure (usefull for flat views)
   --  It is filled with :
   --   - nothing if the node is a project
   --   - the file_node itself if its a file
   --   - the parent file_node if its a subprogram
   Prj_Col  : constant := 4;
   --  Gtk_Tree_Model column number dedicated to the node corresponding project
   --  of the code_analysis structure in every circumstance
   --  (usefull for flat views)
   Cov_Col  : constant := 5;
   --  Gtk_Tree_Model column number dedicated to the coverage information
   --  contained in the node coverage records
   Cov_Sort : constant := 6;
   --  Gtk_Tree_Model column number dedicated to some raw coverage information
   --  used to sort rows by not covered lines amount order
   Cov_Bar_Txt : constant := 7;
   --  Ctk_Tree_Model column number dedicated to the coverage percentage column
   Cov_Bar_Val : constant := 8;
   --  Gtk_Tree_Model column number dedicated to the raw coverage percentage
   --  values, in order to be use in sorting operations

   Progress_Bar_Width_Cst : constant Gint := 150;
   --  Constant used to set the width of the progress bars of the analysis
   --  report

   Covered_Line_Pixbuf   : Gdk_Pixbuf := null;
   Uncovered_Line_Pixbuf : Gdk_Pixbuf := null;
   --  Pixbufs containing the line information icons.
   --  Call Initialize_Graphics before referencing these variables.

   procedure Initialize_Graphics (Kernel : Kernel_Handle);
   --  Initialize the code coverage icons.

   type Code_Analysis_Icons is record
      Prj_Pixbuf  : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      Subp_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
   end record;

   type Code_Analysis_View_Record is new Gtk_Vbox_Record with record
      Tree        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
      Node_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cov_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cov_Percent : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Error_Board : Gtk_Hbox; --  when there's no data
      Load_Button : Gtk_Button;
      Empty_Board : Gtk_Hbox; --  when flat view doesn't allow to see data
      Icons       : Code_Analysis_Icons;
      Projects    : Code_Analysis_Tree;
      --  Used by Show_Flat_List_* and Save_Desktop callbacks
      Binary_Mode : Boolean := True;
   end record;

   type Code_Analysis_View is access all Code_Analysis_View_Record;

   function Build_Analysis_Report
     (Kernel   : Kernel_Handle;
      Name     : String_Access;
      Projects : Code_Analysis_Tree;
      Binary   : Boolean) return Code_Analysis_View;
   --  Actually builds the tree view report.
   --  If Is_Error is True, then the Report of Analysis will be built with an
   --  emptiness warning header.
   --  Should be called by Show_Analysis_Report or Show_Empty_Analysis_Report

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

end Code_Analysis_GUI;
