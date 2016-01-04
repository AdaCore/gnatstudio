------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2016, AdaCore                     --
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

--  This package provides classic Category/File/Message Gtk+ model to
--  be used in location views.

private with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Tree_Store;
with GNATCOLL.VFS;

with GPS.Kernel.Messages;   use GPS.Kernel.Messages;

with Glib;
private with Glib.Main;
private with Gdk.RGBA;
with Gtk.Tree_Model;
private with Gtk.Tree_Row_Reference;

package GPS.Location_View.Listener is

   type Messages_Sort_Order is (By_Location, By_Weight);

   type File_Sort_Order is
     (Category_Default_Sort,
      Alphabetical);
   --  The sort order of files. Each category defines its own default order
   --  (which is set in the model itself), but users can override it if needed.

   type Classic_Tree_Model_Record is
     new Gtk.Tree_Store.Gtk_Tree_Store_Record with private;

   type Classic_Tree_Model is access all Classic_Tree_Model_Record'Class;

   procedure Set_Order
     (Self       : not null access Classic_Tree_Model_Record'Class;
      File_Order : File_Sort_Order;
      Msg_Order  : Messages_Sort_Order);
   --  Set the sort order for Self.

   type Locations_Listener is new Abstract_Listener with private;
   type Locations_Listener_Access is access all Locations_Listener'Class;

   function Register
     (Kernel : Kernel_Handle) return Locations_Listener_Access;
   --  Create a message listener, register it and return it.

   procedure Unregister
     (Kernel : Kernel_Handle;
      Self   : in out Locations_Listener_Access);
   --  Unregister the listener and free memory associated to it

   function Get_Model
     (L : Locations_Listener_Access)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Return the model associated with the listener

   Category_Column           : constant Glib.Gint  := 0;
   --  Contains name of the category.
   Weight_Column             : constant Glib.Gint  := 1;
   --  Contains weight inside the category.
   File_Column               : constant Glib.Gint  := 2;
   --  Contains name of the file in which message's location placed. For
   --  category and subcategory items the value is No_File. Note: for
   --  secondary messages it returns file where secondary location located,
   --  which can be differ from the file for parent's primary message.
   Line_Column               : constant Glib.Gint  := 3;
   --  Contains line number of the message. For category/subcategory/file
   --  level nodes the value -1 is used.
   Column_Column             : constant Glib.Gint  := 4;
   --  Contains column number of the message. For category/subcategory/file
   --  level nodes the value -1 os used.
   Text_Column               : constant Glib.Gint  := 5;
   --  Contains plain text of the message.
   Node_Icon_Name_Column     : constant Glib.Gint  := 6;
   --  Contains icon for the node.
   Node_Markup_Column        : constant Glib.Gint  := 7;
   --  Contains markup of the node. Markup includes line:column information
   --  and text of the message with potential highlighting of some parts
   --  (secondary locations for example) for messages nodes, basename of the
   --  file for file nodes and category's name for category node.
   Node_Tooltip_Column       : constant Glib.Gint  := 8;
   --  Contains tooltip text for the node.
   Node_Mark_Column          : constant Glib.Gint  := 9;
   --  Contains editor's mark of the current position of the location in the
   --  source file.
   Icon_Name_Column      : constant Glib.Gint  := 10;
   --  Contains the name of the icon to display for this action
   Action_Command_Column     : constant Glib.Gint  := 11;
   --  Contains command to be executed on action.
   Action_Tooltip_Column     : constant Glib.Gint  := 12;
   --  Contains tooltip text for the action of the node.
   Number_Of_Children_Column : constant Glib.Gint  := 13;
   --  Contains number of children items. This number is useful for filtering
   --  purpose because it contains unmodified number of children items.
   Sort_Order_Hint_Column    : constant Glib.Gint  := 14;
   --  Hint to the view how file level nodes must be sorted by default.
   Message_Column            : constant Glib.Gint  := 15;
   --  Access to message converted to address.
   Total_Columns             : constant Glib.Gint  := 16;
   --  Total number of columns.

   function Get_Message
     (Model  : Gtk_Tree_Model;
      Iter   : Gtk_Tree_Iter;
      Column : Glib.Gint) return Message_Access;
   --  Returns message at specified position

private

   package Row_Reference_Vectors is
     new Ada.Containers.Vectors
       (Positive,
        Gtk.Tree_Row_Reference.Gtk_Tree_Row_Reference,
        Gtk.Tree_Row_Reference."=");

   type Classic_Tree_Model_Record is
     new Gtk.Tree_Store.Gtk_Tree_Store_Record with record
      Messages_Order : Messages_Sort_Order := By_Location;
      --  Sort order for locations within a file.

      File_Order     : File_Sort_Order := Category_Default_Sort;
      --  Sort order for files within a category

      Idle_Handler   : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  Idle handle to enable sorting

      Sort_Column    : Glib.Gint;
      --  Column returned by freeze function to thaw sorting

      Removed_Rows   : Row_Reference_Vectors.Vector;
      --  List of message rows was requested to be removed. It is used to don't
      --  emit 'row_removed' signal for individual messages when file row is
      --  removed too.
   end record;

   procedure Initialize (Self : access Classic_Tree_Model_Record'Class);
   procedure Gtk_New (Object : out Classic_Tree_Model);

   type Locations_Listener is
     new GPS.Kernel.Messages.Abstract_Listener with record
      Kernel          : Kernel_Handle;
      Model           : Classic_Tree_Model;

      Non_Leaf_Color  : Gdk.RGBA.Gdk_RGBA;
      --  Foreground color for category and file nodes.
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
      Category : Ada.Strings.Unbounded.Unbounded_String;
      Allow_Auto_Jump_To_First : Boolean);

   overriding procedure Category_Removed
     (Self     : not null access Locations_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String);

end GPS.Location_View.Listener;
