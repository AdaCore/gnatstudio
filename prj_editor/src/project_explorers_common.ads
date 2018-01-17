------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Gdk.Event;      use Gdk.Event;
with Glib;           use Glib;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Widget;     use Gtk.Widget;
with Gtkada.MDI;     use Gtkada.MDI;
with Gtkada.Tree_View; use Gtkada.Tree_View;

with GPS.Kernel;       use GPS.Kernel;
with GPS.Kernel.Hooks; use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;   use GPS.Kernel.MDI;
with GPS.VCS;          use GPS.VCS;
with Language;         use Language;
with Tooltips;         use Tooltips;

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

   type Base_Explorer_Tree_Record is new Tree_View_Record with record
      Kernel : Kernel_Handle;
   end record;

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
      Lib_Directory_Node,
      Exec_Directory_Node,
      File_Node);
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
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Parent : Gtk_Tree_Iter;
      Kind   : Node_Types;
      Name   : String;
      File   : Virtual_File) return Gtk_Tree_Iter;
   --  Check if Parent already has a child with the correct kind and name,
   --  and returns it. If not, creates a new node, where Name is set for the
   --  Display_Name_Column.

   function Create_Or_Reuse_File
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Dir    : Gtk_Tree_Iter;
      File   : Virtual_File) return Gtk_Tree_Iter;
   --  Create a new file node, or reuse one if it already exists

   function Create_Node
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Parent    : Gtk_Tree_Iter;
      Kind      : Node_Types;
      Name      : String;
      File      : Virtual_File;
      Icon_Name : String := "") return Gtk_Tree_Iter;
   --  Create a new node. Name is set for the Display_Name_Column.
   --  The icon_name will be computed from Kind if none is specified as
   --  argument.

   function Create_File
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Dir       : Gtk_Tree_Iter;
      File      : Virtual_File;
      Icon_Name : String := "gps-emblem-file-unmodified") return Gtk_Tree_Iter;
   --  Create a file node at the end of the children of Dir

   procedure Append_Runtime_Info
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Node      : Gtk_Tree_Iter);
   --  Add runtime information

   function Get_Node_Type
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Node_Types;
   function Get_Node_Type
     (Self  : not null access Base_Explorer_Tree_Record'Class;
      Node  : Gtk_Tree_Iter) return Node_Types
     is (Get_Node_Type (Self.Model, Node));
   --  Return the type of Node

   procedure Set_Node_Type
     (Self     : not null access Base_Explorer_Tree_Record'Class;
      Node     : Gtk_Tree_Iter;
      N_Type   : Node_Types;
      Expanded : Boolean);
   --  Set the Node type and the pixmap accordingly

   function Get_Directory_From_Node
     (Self  : not null access Base_Explorer_Tree_Record'Class;
      Node  : Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the directory to which Node belongs.

   function Get_File_From_Node
     (Self  : not null access Base_Explorer_Tree_Record'Class;
      Node  : Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the file containing Node

   function Get_Project_From_Node
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Node      : Gtk_Tree_Iter;
      Importing : Boolean) return Project_Type;
   --  Return the name of the project that Node belongs to. If Importing is
   --  True, we return the importing project, not the one associated with Node.

   ---------------
   -- Expansion --
   ---------------

   type Node_Id is record
      File  : Virtual_File;
      Depth : Natural;
   end record;
   --  A unique ID for nodes, so that we can restore the expansion status.
   --  We need to take the depth into account, because some files could be
   --  duplicated in the project view (when not using flat view).

   function Get_Id
     (Self : not null access Base_Explorer_Tree_Record'Class;
      Row  : Gtk_Tree_Iter) return Node_Id;
   function Hash (Self : Node_Id) return Ada.Containers.Hash_Type;
   package Explorer_Expansion is new Expansion_Support
     (Tree_Record => Base_Explorer_Tree_Record,
      Id          => Node_Id,
      Get_Id      => Get_Id,
      Hash        => Hash);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Explorer_Expansion.Detached_Model'Class,
      Explorer_Expansion.Detached_Model_Access);

   ---------
   -- VCS --
   ---------

   type On_VCS_Status_Changed is new Vcs_File_Status_Hooks_Function with record
      Tree     : access Base_Explorer_Tree_Record'Class;
   end record;
   overriding procedure Execute
     (Self          : On_VCS_Status_Changed;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Vcs           : not null access Abstract_VCS_Engine'Class;
      Files         : GPS.Kernel.File_Sets.Set;
      Props         : VCS_File_Properties);
   --  Should be set for the Vcs_File_Status_Changed_Hook.
   --  It will automatically update the icons for all displayed files in the
   --  tree

   --------------
   -- Tooltips --
   --------------

   type Explorer_Tooltips is new Tooltips.Tooltips with record
      Tree     : access Base_Explorer_Tree_Record'Class;
   end record;
   type Explorer_Tooltips_Access is access all Explorer_Tooltips'Class;
   overriding function Create_Contents
     (Self     : not null access Explorer_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation

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
     (Child     : access MDI_Explorer_Child_Record'Class;
      Tree      : not null access Base_Explorer_Tree_Record'Class;
      Event     : Gdk_Event_Button) return Boolean;
   --  If the Event is a button click, expand the node or jump to the
   --  location accordingly, and return whether the event should be propagated.

   function On_Key_Press
     (Tree   : not null access Base_Explorer_Tree_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  React to key press event on the tree

   procedure Context_Factory
     (Self    : not null access Base_Explorer_Tree_Record'Class;
      Context : in out Selection_Context;
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
