-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
--                             AdaCore                               --
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

with Gdk.Pixbuf;
with Gdk.Event;      use Gdk.Event;
with Glib;           use Glib;
with Gtk.Menu;       use Gtk.Menu;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View;  use Gtk.Tree_View;
with Gtk.Widget;     use Gtk.Widget;
with Gtkada.MDI;     use Gtkada.MDI;

with GPS.Kernel;     use GPS.Kernel;
with GPS.Kernel.MDI; use GPS.Kernel.MDI;
with Language;       use Language;
with Projects;
with VFS;

package Project_Explorers_Common is

   type MDI_Explorer_Child_Record is
     new GPS.Kernel.MDI.GPS_MDI_Child_Record with private;
   type MDI_Explorer_Child is access all MDI_Explorer_Child_Record'Class;

   function Dnd_Data
     (Child : access MDI_Explorer_Child_Record; Copy : Boolean)
      return Gtkada.MDI.MDI_Child;
   procedure Child_Drag_Finished (Child  : access MDI_Explorer_Child_Record);
   --  See inherited documentation

   ------------------------
   -- Column definitions --
   ------------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.

   Icon_Column          : constant := 0;
   Base_Name_Column     : constant := 1;
   Absolute_Name_Column : constant := 2;
   Node_Type_Column     : constant := 3;
   User_Data_Column     : constant := 4;
   Line_Column          : constant := 5;
   Column_Column        : constant := 6;
   Project_Column       : constant := 7;
   Category_Column      : constant := 8;
   Up_To_Date_Column    : constant := 9;
   Entity_Base_Column   : constant := 10;
   Timestamp_Column     : constant := 11;

   ----------------------
   -- Node definitions --
   ----------------------

   type Node_Types is
     (Project_Node,
      Extends_Project_Node,
      Directory_Node,
      Obj_Directory_Node,
      Exec_Directory_Node,
      File_Node,
      Category_Node,
      Entity_Node,
      Modified_Project_Node);
   subtype Directory_Node_Types
     is Node_Types range Directory_Node .. Exec_Directory_Node;
   --  The kind of nodes one might find in the tree

   --------------
   -- Graphics --
   --------------

   type Pixbuf_Array is array (Node_Types) of Gdk.Pixbuf.Gdk_Pixbuf;

   Open_Pixbufs  : Pixbuf_Array;
   Close_Pixbufs : Pixbuf_Array;

   procedure Init_Graphics (Widget : Gtk_Widget);
   --  Initialize the pixbufs.

   ---------------------------------
   -- Tree manipulation functions --
   ---------------------------------

   --  The following functions are intended to work on a Model that
   --  has been initialized with the columns described above.

   procedure Append_File
     (Kernel : Kernel_Handle;
      Model  : Gtk_Tree_Store;
      Base   : Gtk_Tree_Iter;
      File   : VFS.Virtual_File;
      Sorted : Boolean := False);
   --  Append a file node to Base in the model.
   --  File must be an absolute file name.

   procedure Append_Dummy_Iter
     (Model : Gtk_Tree_Store;
      Base  : Gtk_Tree_Iter);
   --  Append an empty iter to Base.

   function Append_Category_Node
     (Model       : Gtk_Tree_Store;
      File        : VFS.Virtual_File;
      Category    : Language_Category;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Add a category node in the model.

   function Append_Entity_Node
     (Model       : Gtk_Tree_Store;
      File        : VFS.Virtual_File;
      Construct   : Construct_Information;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Add an entity node in the model.

   procedure Append_File_Info
     (Kernel    : Kernel_Handle;
      Model     : Gtk_Tree_Store;
      Node      : Gtk_Tree_Iter;
      File_Name : VFS.Virtual_File);
   --  Add info to a file node in the model.

   function Get_Node_Type
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Node_Types;
   --  Return the type of Node.

   procedure Set_Node_Type
     (Model    : Gtk_Tree_Store;
      Node     : Gtk_Tree_Iter;
      N_Type   : Node_Types;
      Expanded : Boolean);
   --  Set the Node type and the pixmap accordingly.

   function Get_Category_Type
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Language_Category;
   --  Return the type of category.

   function Is_Up_To_Date
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Boolean;
   --  Return the state of the Up_To_Date flag for Node.

   procedure Set_Up_To_Date
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter;
      State : Boolean);
   --  Set the state of the Up_To_Date flag for Node.

   function Get_Base_Name
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return String;
   --  Return the base name for Node.
   --  Returns a UTF8-encoded string.

   function Get_Absolute_Name
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return String;
   --  Return the absolute name for Node.

   function Get_Directory_From_Node
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter)
      return String;
   --  Return the name of the directory to which Node belongs. This returns the
   --  full directory name, relative to the project.
   --  The return strings always ends with a directory separator.

   function Get_File_From_Node
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return VFS.Virtual_File;
   --  Return the name of the file containing Node (or, in case Node is an
   --  Entity_Node, the name of the file that contains the entity).

   function Get_Project_From_Node
     (Model     : Gtk_Tree_Store;
      Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Node      : Gtk_Tree_Iter;
      Importing : Boolean) return Projects.Project_Type;
   --  Return the name of the project that Node belongs to. If Importing is
   --  True, we return the importing project, not the one associated with Node.

   ----------
   -- Misc --
   ----------

   function Entity_Name_Of
     (Construct     : Construct_Information;
      Show_Profiles : Boolean) return String;
   --  Return the string to use to display the entity.
   --  This returns a string in Pango Markup format.

   function Entity_Icon_Of
     (Construct : Construct_Information) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Return the icon associated with Construct.

   function Filter_Category
     (Category : Language.Language_Category) return Language.Language_Category;
   --  Return the category to use when an entity is Category.
   --  This is used to group subprograms (procedures and functions together),
   --  or remove unwanted categories (in which case Cat_Unknown is returned).

   function On_Button_Press
     (Kernel    : Kernel_Handle;
      Child     : access MDI_Explorer_Child_Record'Class;
      Tree      : access Gtk_Tree_View_Record'Class;
      Model     : Gtk_Tree_Store;
      Event     : Gdk_Event;
      Add_Dummy : Boolean) return Boolean;
   --  If the Event is a button click, expand the node or jump to the
   --  location accordingly, and return whether the event should be propagated.
   --  If Add_Dummy is true, a dummy node will be added to nodes collapsed
   --  by this call.

   function On_Key_Press
     (Kernel    : Kernel_Handle;
      Tree      : access Gtk_Tree_View_Record'Class;
      Event    : Gdk_Event) return Boolean;
   --  React to key press event on the tree.

   procedure Context_Factory
     (Context : in out Selection_Context;
      Kernel  : Kernel_Handle;
      Tree    : access Gtk_Tree_View_Record'Class;
      Model   : Gtk_Tree_Store;
      Event   : Gdk_Event;
      Menu    : Gtk_Menu);
   --  Return the context to use for the contextual menu.

private
   type MDI_Explorer_Child_Record is
     new GPS.Kernel.MDI.GPS_MDI_Child_Record with
      record
         Kernel        : GPS.Kernel.Kernel_Handle;
         Dnd_From_File : VFS.Virtual_File := VFS.No_File;
         --  The file from which we started a Dnd operation
      end record;

end Project_Explorers_Common;
