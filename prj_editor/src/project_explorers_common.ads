------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Symbols;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Gdk.Event;      use Gdk.Event;
with Glib;           use Glib;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View;  use Gtk.Tree_View;
with Gtk.Widget;     use Gtk.Widget;
with Gtkada.MDI;     use Gtkada.MDI;

with GPS.Kernel;     use GPS.Kernel;
with GPS.Kernel.MDI; use GPS.Kernel.MDI;
with Language;       use Language;

package Project_Explorers_Common is

   type MDI_Explorer_Child_Record is
     new GPS.Kernel.MDI.GPS_MDI_Child_Record with private;
   type MDI_Explorer_Child is access all MDI_Explorer_Child_Record'Class;

   overriding function Dnd_Data
     (Child : access MDI_Explorer_Child_Record; Copy : Boolean)
      return Gtkada.MDI.MDI_Child;
   overriding procedure Child_Drag_Finished
     (Child  : access MDI_Explorer_Child_Record);
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
   Display_Name_Column  : constant := 1;
   File_Column          : constant := 2;
   Node_Type_Column     : constant := 3;
   Line_Column          : constant := 4;
   Column_Column        : constant := 5;
   Entity_Base_Column   : constant := 6;

   ----------------------
   -- Node definitions --
   ----------------------

   type Node_Types is
     (Project_Node,
      Root_Project_Node,
      Extends_Project_Node,
      Modified_Project_Node,
      Runtime_Node,
      Directory_Node,
      Obj_Directory_Node,
      Exec_Directory_Node,
      File_Node,
      Category_Node,
      Entity_Node,
      Dummy_Node);
   subtype Project_Node_Types
     is Node_Types range Project_Node .. Modified_Project_Node;
   subtype Directory_Node_Types
     is Node_Types range Directory_Node .. Exec_Directory_Node;
   --  The kind of nodes one might find in the tree

   function Stock_For_Node
     (Node : Node_Types; Expanded : Boolean) return String;
   --  Return the name of the stock icon to use.

   ---------------------------------
   -- Tree manipulation functions --
   ---------------------------------

   --  The following functions are intended to work on a Model that
   --  has been initialized with the columns described above.

   function Create_Or_Reuse_Node
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Kind   : Node_Types;
      Name   : String;
      File   : Virtual_File;
      Add_Dummy : Boolean := False) return Gtk_Tree_Iter;
   --  Check if Parent already has a child with the correct kind and name,
   --  and returns it. If not, creates a new node, where Name is set for the
   --  Display_Name_Column.
   --  If Add_Dummy is true and a new node is created, a dummy child is
   --  added to it so that the user can expand the node.

   procedure Create_Or_Reuse_File
     (Model  : Gtk_Tree_Store;
      Kernel : not null access Kernel_Handle_Record'Class;
      Dir    : Gtk_Tree_Iter;
      File   : Virtual_File);
   --  Create a new file node, or reuse one if it already exists

   procedure Append_File
     (Kernel : Kernel_Handle;
      Model  : Gtk_Tree_Store;
      Base   : Gtk_Tree_Iter;
      File   : GNATCOLL.VFS.Virtual_File;
      Sorted : Boolean := False);
   --  Append a file node to Base in the model.
   --  File must be an absolute file name.

   procedure Append_Dummy_Iter
     (Model : Gtk_Tree_Store;
      Base  : Gtk_Tree_Iter);
   function Has_Dummy_Iter
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter) return Boolean;
   procedure Remove_Dummy_Iter
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter);
   --  Append an empty iter to Base

   function Append_Category_Node
     (Model         : Gtk_Tree_Store;
      File          : GNATCOLL.VFS.Virtual_File;
      Category      : Language_Category;
      Category_Name : GNATCOLL.Symbols.Symbol;
      Parent_Iter   : Gtk_Tree_Iter;
      Sorted        : Boolean) return Gtk_Tree_Iter;
   --  Add a category node in the model

   function Append_Entity_Node
     (Model       : Gtk_Tree_Store;
      File        : GNATCOLL.VFS.Virtual_File;
      Construct   : Construct_Information;
      Parent_Iter : Gtk_Tree_Iter;
      Sorted      : Boolean) return Gtk_Tree_Iter;
   --  Add an entity node in the model

   procedure Append_File_Info
     (Kernel    : Kernel_Handle;
      Model     : Gtk_Tree_Store;
      Node      : Gtk_Tree_Iter;
      File_Name : GNATCOLL.VFS.Virtual_File;
      Sorted    : Boolean);
   --  Add info to a file node in the model

   procedure Append_Runtime_Info
     (Kernel    : Kernel_Handle;
      Model     : Gtk_Tree_Store;
      Node      : Gtk_Tree_Iter);
   --  Add runtime information

   function Get_Node_Type
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Node_Types;
   --  Return the type of Node

   procedure Set_Node_Type
     (Model    : Gtk_Tree_Store;
      Node     : Gtk_Tree_Iter;
      N_Type   : Node_Types;
      Expanded : Boolean);
   --  Set the Node type and the pixmap accordingly

   function Get_Base_Name
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Filesystem_String;
   --  Return the base name for Node.
   --  Returns a UTF8-encoded string.

   function Get_Absolute_Name
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File;
   --  Return the absolute name for Node

   function Get_Directory_From_Node
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the directory to which Node belongs.

   function Get_File_From_Node
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the file containing Node (or, in case Node is an
   --  Entity_Node, the name of the file that contains the entity).

   function Get_Project_From_Node
     (Model     : Gtk_Tree_Store;
      Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Node      : Gtk_Tree_Iter;
      Importing : Boolean) return Project_Type;
   --  Return the name of the project that Node belongs to. If Importing is
   --  True, we return the importing project, not the one associated with Node.

   ----------
   -- Misc --
   ----------

   function Entity_Name_Of
     (Construct          : Construct_Information;
      Show_Profiles      : Boolean;
      Max_Profile_Length : Positive := Positive'Last) return String;
   --  Return the string to use to display the entity.
   --  This returns a string in Pango Markup format. If Show_Profiles is true
   --  the the entity profile is returned. As this can be large it is possible
   --  to specify the maximum number of characters returned for the profile
   --  using Max_Profile_Length.

   function Entity_Icon_Of (Construct : Construct_Information) return String;
   --  Return the icon associated with Construct

   function Entity_Icon_Of
     (Construct : Simple_Construct_Information) return String;
   --  Return the icon associated with Construct

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
      Event     : Gdk_Event_Button;
      Add_Dummy : Boolean) return Boolean;
   --  If the Event is a button click, expand the node or jump to the
   --  location accordingly, and return whether the event should be propagated.
   --  If Add_Dummy is true, a dummy node will be added to nodes collapsed
   --  by this call.
   --  Model is the model where the insertion of dummy nodes should take place.
   --  This might not be the same as Tree.Get_Model, in case there is a filter
   --  model.

   function On_Key_Press
     (Kernel : Kernel_Handle;
      Tree   : access Gtk_Tree_View_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  React to key press event on the tree

   procedure Context_Factory
     (Context : in out Selection_Context;
      Kernel  : Kernel_Handle;
      Model   : Gtk_Tree_Store;
      Iter    : Gtk_Tree_Iter);
   --  Return the context to use for the contextual menu

private

   type MDI_Explorer_Child_Record is
     new GPS.Kernel.MDI.GPS_MDI_Child_Record with
      record
         Kernel        : GPS.Kernel.Kernel_Handle;
         Dnd_From_File : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
         Dnd_From_Project : GNATCOLL.Projects.Project_Type :=
           GNATCOLL.Projects.No_Project;
         --  The file from which we started a Dnd operation
      end record;

end Project_Explorers_Common;
