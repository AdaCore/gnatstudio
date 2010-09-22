-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2010, AdaCore                       --
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

--  This package provides classic Category/File/Message Gtk+ model to
--  be used in location views.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtkada.Abstract_Tree_Model;
with GNATCOLL.VFS;

with GPS.Kernel.Messages; use GPS.Kernel.Messages;

private with Gdk.Color;
private with Gdk.Pixbuf;
private with Glib.Values;
with Gtk.Tree_Model;

package GPS.Location_View.Listener is

   type Classic_Tree_Model_Record is
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
   with private;

   type Classic_Tree_Model is access all Classic_Tree_Model_Record'Class;

   type Locations_Listener is new Abstract_Listener with private;
   type Locations_Listener_Access is access all Locations_Listener'Class;

   function Register
     (Kernel : Kernel_Handle) return Locations_Listener_Access;
   --  Create a message listener, register it and return it.

   procedure Unregister
     (Kernel : Kernel_Handle;
      L      : in out Locations_Listener_Access);
   --  Unregister the listener and free memory associated to it

   function Get_Model
     (L : Locations_Listener_Access)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Return the model associated with the listener

   Category_Column           : constant Glib.Gint := 0;
   --  Contains name of the category.
   Weight_Column             : constant Glib.Gint := 1;
   --  Contains weight inside the category.
   File_Column               : constant Glib.Gint := 2;
   --  Contains name of the file in which message's location placed. For
   --  category and subcategory items the value is No_File. Note: for
   --  secondary messages it returns file where secondary location located,
   --  which can be differ from the file for parent's primary message.
   Line_Column               : constant Glib.Gint := 3;
   --  Contains line number of the message. For category/subcategory/file
   --  level nodes the value -1 is used.
   Column_Column             : constant Glib.Gint := 4;
   --  Contains column number of the message. For category/subcategory/file
   --  level nodes the value -1 os used.
   Text_Column               : constant Glib.Gint := 5;
   --  Contains plain text of the message.
   Node_Icon_Column          : constant Glib.Gint := 6;
   --  Contains icon for the node.
   Node_Markup_Column        : constant Glib.Gint := 7;
   --  Contains markup of the node. Markup includes line:column information
   --  and text of the message with potential highlighting of some parts
   --  (secondary locations for example) for messages nodes, basename of the
   --  file for file nodes and category's name for category node.
   Node_Foreground_Column    : constant Glib.Gint := 8;
   --  Contains Gdk color for the foreground of the node.
   Node_Tooltip_Column       : constant Glib.Gint := 9;
   --  Contains tooltip text for the node.
   Node_Mark_Column          : constant Glib.Gint := 10;
   --  Contains editor's mark of the current position of the location in the
   --  source file.
   Action_Pixbuf_Column      : constant Glib.Gint := 11;
   --  Contains pixmuf object of the associated action.
   Action_Command_Column     : constant Glib.Gint := 12;
   --  Contains command to be executed on action.
   Number_Of_Children_Column : constant Glib.Gint := 13;
   --  Contains number of children items. This number is useful for filtering
   --  purpose because it contains unmodified number of children items.
   Sort_Order_Hint_Column    : constant Glib.Gint := 14;
   --  Hint to the view how file level nodes must be sorted by default.
   Message_Column            : constant Glib.Gint := 15;
   --  Access to message converted to address.
   Total_Columns             : constant Glib.Gint := 16;
   --  Total number of columns.

private
   type Node_Record is tagged;
   type Node_Access is access all Node_Record'Class;

   package Node_Vectors is
     new Ada.Containers.Vectors (Positive, Node_Access);

   type Node_Kinds is (Node_Category, Node_File, Node_Message);

   type Node_Record (Kind : Node_Kinds) is tagged limited record
      Parent        : Node_Access := null;
      Children      : Node_Vectors.Vector;

      Message_Count : Natural := 0;

      case Kind is
         when Node_Category =>
            Name    : Ada.Strings.Unbounded.Unbounded_String;

         when Node_File =>
            File    : GNATCOLL.VFS.Virtual_File;

         when Node_Message =>
            Message : Message_Access;
      end case;
   end record;

   type Classic_Tree_Model_Record is
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record with record
      Kernel          : Kernel_Handle;

      Categories      : Node_Vectors.Vector;

      Stamp           : Glib.Gint := 1;
      --  Integer value to distinguish obsolete values of Gtk_Tree_Iter by
      --  Gtk+ convention.

      Non_Leaf_Color  : Gdk.Color.Gdk_Color;
      --  Foreground color for category and file nodes.

      Category_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Pixbuf     : Gdk.Pixbuf.Gdk_Pixbuf;
      --  Pixbufs to decorate category and file nodes.
   end record;

   procedure Initialize (Self : access Classic_Tree_Model_Record'Class);
   procedure Gtk_New
     (Object    : out Classic_Tree_Model;
      Kernel    : Kernel_Handle);

   procedure Node_Added
     (Self : not null access Classic_Tree_Model_Record;
      Node : not null Node_Access);
   --  Common implementation of node adding handling, it handles items of all
   --  levels. Category_Added and File_Added just a renaming of Node_Added;
   --  Message_Added can't be renamed due to difference in subprogram's
   --  profile.

   overriding function Get_N_Columns
     (Self : access Classic_Tree_Model_Record) return Glib.Gint;
   --  Override this to return the number of columns supported by Tree_Model.

   overriding function Get_Column_Type
     (Self  : access Classic_Tree_Model_Record;
      Index : Glib.Gint) return Glib.GType;
   --  Override this to return the type of the Index-th column in the model.

   overriding function Get_Iter
     (Self : access Classic_Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this return an iterator pointing to Path.
   --  Null_Iter is returned if Path was invalid or no iterator could be set.

   overriding function Get_Path
     (Self : access Classic_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Override this to return a newly created Gtk_Tree_Path referenced by
   --  Iter. This path will be freed with Path_Free by the caller.

   overriding procedure Get_Value
     (Self   : access Classic_Tree_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);
   --  Override this get a value from the model, at column Column and line
   --  Iter. Value must be freed by the caller.

   overriding procedure Next
     (Self : access Classic_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Override this to set Iter to point to the node following it at the
   --  current level. If there is none, Iter is set to Null_Iter.

   overriding function Children
     (Self   : access Classic_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the first child of Parent. If Parent has no
   --  children, return Null_Iter. Parent will remain a valid node after this
   --  function has been called.

   overriding function Has_Child
     (Self : access Classic_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Override this to return True if Iter has children, False otherwise.

   overriding function N_Children
     (Self : access Classic_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;
   --  Override this to return the number of children that Iter has.
   --  As a special case, if Iter is Null_Iter, then the number of toplevel
   --  nodes is returned.

   overriding function Nth_Child
     (Self   : access Classic_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the child of Parent, using the given index.
   --  The First index is 0. If Index is too big, or Parent has no children,
   --  return Null_Iter. If Parent is Null_Iter, then the nth root node is set.

   overriding function Parent
     (Self  : access Classic_Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the parent of Child. If Child is at the
   --  toplevel, and doesn't have a parent, then Null_Iter is returned.

   type Locations_Listener
     is new Abstract_Listener
   with record
      Model  : Classic_Tree_Model;
   end record;

   overriding procedure Message_Property_Changed
     (Self     : not null access Locations_Listener;
      Message  : not null access Abstract_Message'Class;
      Property : String);

   overriding procedure Message_Added
     (Self    : not null access Locations_Listener;
      Message : not null access Abstract_Message'Class);

   overriding procedure Message_Removed
     (Self    : not null access Locations_Listener;
      Message : not null access Abstract_Message'Class);

   overriding procedure File_Added
     (Self    : not null access Locations_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File);

   overriding procedure File_Removed
     (Self     : not null access Locations_Listener;
      Category : Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File);

   overriding procedure Category_Added
     (Self     : not null access Locations_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String);

end GPS.Location_View.Listener;
