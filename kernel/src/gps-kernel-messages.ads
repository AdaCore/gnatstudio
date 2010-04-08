-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2009-2010, AdaCore                  --
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
--  There are three user visible entities in the centralized messages
--  container: message, container and listener.
--
--  Abstract_Message is a root tagged type for all kinds of messages.
--  Each module can provide own types of messages which need to be
--  derived from Abstract_Message type. Modules can use standard
--  implementation which is provided in the child package Simple when
--  this implementation is suitable for module's purpose.
--
--  Messages_Container is a container for messages. It is an exclusive
--  owner of all messages.
--
--  Abstract_Listener is an interface to provide notify others modules
--  about changes in the state of container or individual messages.

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Hash;
private with Ada.Tags;

with Gtk.Tree_Model;
private with Gtkada.Abstract_Tree_Model;
with GNATCOLL.VFS;
with GPS.Styles;

package GPS.Kernel.Messages is

   type Messages_Container (<>) is tagged limited private;

   type Messages_Container_Access is
     access all Messages_Container'Class;

   type Message_Visibility_Kind is
     (Editor_Side, --  Messages displayed on the side of editors
      Locations    --  Messages displayed in the locations view
     );

   type Message_Flags is array (Message_Visibility_Kind) of Boolean;
   --  A list of potential locations where a message should be shown.

   function To_Int (Flags : Message_Flags) return Integer;
   function From_Int (Int : Integer) return Message_Flags;
   --  Utility functions to load/save flags to XML

   type Abstract_Listener is abstract tagged limited private;

   type Listener_Access is access all Abstract_Listener'Class;

   type Action_Item is access all GPS.Editors.Line_Information_Record;

   type Unbounded_String_Array is
     array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   type Virtual_File_Array is
     array (Positive range <>) of GNATCOLL.VFS.Virtual_File;

   type Sort_Order_Hint is (Chronological, Alphabetical);
   --  Hint for the view how it must sort items at file level by default.

   ----------------------
   -- Abstract Message --
   ----------------------

   type Message_Levels is (Primary, Secondary);

   type Abstract_Message (Level : Message_Levels) is
     abstract tagged limited private;

   type Message_Access is access all Abstract_Message'Class;

   subtype Primary_Abstract_Message is Abstract_Message (Primary);
   subtype Secondary_Abstract_Message is Abstract_Message (Secondary);

   type Message_Array is array (Positive range <>) of Message_Access;

   function Get_Category
     (Self : not null access constant Abstract_Message'Class)
      return Ada.Strings.Unbounded.Unbounded_String;

   function Get_Category
     (Self : not null access constant Abstract_Message'Class) return String;

   function Get_File
     (Self : not null access constant Abstract_Message'Class)
      return GNATCOLL.VFS.Virtual_File;

   function Get_Line
     (Self : not null access constant Abstract_Message'Class)
      return Natural;
   --  Returns the line number of the original location of the message

   function Get_Column
     (Self : not null access constant Abstract_Message'Class)
      return Basic_Types.Visible_Column_Type;
   --  Returns the column number of the original location of the message

   function Get_Text
     (Self : not null access constant Abstract_Message)
      return Ada.Strings.Unbounded.Unbounded_String is abstract;
   --  Returns plain text of the message

   function Get_Markup
     (Self : not null access constant Abstract_Message)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Returns text of the message with possible Pango markup. Default
   --  implementation returns escaped plain text.

   function Get_Parent
     (Self : not null access constant Abstract_Message'Class)
      return Message_Access;
   --  For secondary message it returns parent message, for primary message it
   --  returns null.

   procedure Set_Action
     (Self   : not null access Abstract_Message'Class;
      Action : Action_Item);
   --  Associate specified action with the message. Message takes ownership on
   --  the associated action. Previous action is deallocated.

   function Get_Action
     (Self : not null access constant Abstract_Message'Class)
      return Action_Item;
   --  Returns action associated with the message.

   procedure Set_Highlighting
     (Self   : not null access Abstract_Message'Class;
      Style  : GPS.Styles.Style_Access;
      Length : Positive);
   --  Set highlighting style and span to be used in the editor to highlight
   --  corresponding location.

   procedure Set_Highlighting
     (Self  : not null access Abstract_Message'Class;
      Style : GPS.Styles.Style_Access);
   --  Set style for line highlightinh in the editor.

   function Get_Highlighting_Style
     (Self : not null access constant Abstract_Message'Class)
      return GPS.Styles.Style_Access;
   --  Returns highlighting style to be used by source editor to highlight
   --  message.

   function Get_Highlighting_Length
     (Self : not null access constant Abstract_Message'Class) return Natural;
   --  Returns length of highlighting. Zero length means all line must be
   --  highlighted.

   function Get_Editor_Mark
     (Self : not null access constant Abstract_Message'Class)
      return GPS.Editors.Editor_Mark'Class;
   --  Returns editor's mark of the current location of the message.

   procedure Remove (Self : not null access Abstract_Message'Class);
   --  Removes message and deallocate it.

   procedure Initialize
     (Self          : not null access Abstract_Message'Class;
      Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Weight        : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags);
   --  Initialize message and connect it to container

   procedure Initialize
     (Self          : not null access Abstract_Message'Class;
      Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags);
   --  Initialize message and connect it to parent message

   procedure Finalize (Self : not null access Abstract_Message);
   --  Called to release resources occupied by the message before memory for
   --  message will be released. Derived types can override it to do additional
   --  actions, but must call this subprogram always to avoid memory leaks.

   ------------------------
   -- Messages Container --
   ------------------------

   function Get_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return not null Messages_Container_Access;
   --  Returns messages conntainer for the specified instance of the kernel.

   function New_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return not null Messages_Container_Access;
   --  Create a new empty messages container, with no models and no listeners

   function Get_Categories
     (Self : not null access constant Messages_Container'Class)
      return Unbounded_String_Array;
   --  Returns list of all categories.

   function Get_Files
     (Self     : not null access constant Messages_Container'Class;
      Category : Ada.Strings.Unbounded.Unbounded_String)
      return Virtual_File_Array;
   --  Returns list of files in the specified category. Empty array is returned
   --  when there is not such category.

   function Get_Messages
     (Self     : not null access constant Messages_Container'Class;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
      return Message_Array;
   --  Returns list of messages for the specified file in the specified
   --  category. Returns empty list when there is no file or category.

   procedure Remove_All_Messages
     (Self : not null access Messages_Container'Class);
   --  Removes all messages.

   procedure Remove_Category
     (Self     : not null access Messages_Container'Class;
      Category : String);
   --  Removes all messages in the specified category. Do nothing when there
   --  is no such category.

   procedure Remove_File
     (Self     : not null access Messages_Container'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File);
   --  Removes all messages for specified file in the specified category.
   --  Do nothing when there is no such category or file.

   function Get_Classic_Tree_Model
     (Self : not null access constant Messages_Container'Class)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns Gtk+ model for classic tree representation.

   procedure Set_Sort_Order_Hint
     (Self     : not null access Messages_Container'Class;
      Category : String;
      Hint     : Sort_Order_Hint);
   --  Sets sort order hint for the specified category. Hint must be set before
   --  first message in the category is created.

   procedure Register_Listener
     (Self     : not null access Messages_Container;
      Listener : not null Listener_Access;
      Flags    : Message_Flags);
   --  Register listener. It do nothing when listener is already registered.
   --  This listener will only receive messages the flags of which have one
   --  "True" field in common with Flags.

   procedure Unregister_Listener
     (Self     : not null access Messages_Container;
      Listener : not null Listener_Access);
   --  Unregister listener. It do nothing when listener is not registered.

   ----------------------
   -- Message Listener --
   ----------------------

   procedure Category_Added
     (Self     : not null access Abstract_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String) is null;

   procedure File_Added
     (Self     : not null access Abstract_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File) is null;

   procedure File_Removed
     (Self     : not null access Abstract_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File) is null;

   procedure Message_Added
     (Self    : not null access Abstract_Listener;
      Message : not null access Abstract_Message'Class) is null;

   procedure Message_Property_Changed
     (Self     : not null access Abstract_Listener;
      Message  : not null access Abstract_Message'Class;
      Property : String) is null;
   --  ??? It would be nice to use a type to represent Properties, rather
   --  than using strings

   procedure Message_Removed
     (Self    : not null access Abstract_Listener;
      Message : not null access Abstract_Message'Class) is null;

   ---------------------
   -- Gtk+ Tree Model --
   ---------------------

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
   Total_Columns             : constant Glib.Gint := 15;
   --  Total number of columns.

   --------------------------
   -- For private use only --
   --------------------------

   function Create_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return System.Address;
   --  Creates new nessages container and returns its address. Address is used
   --  to break circular dependency between Kernel and Messages_Container.

   procedure Save (Self : not null access Messages_Container'Class);
   --  Saves all messages for the current project

   procedure Save
     (Self  : not null access Messages_Container'Class;
      File  : GNATCOLL.VFS.Virtual_File;
      Debug : Boolean);
   --  Saves all messages into the specified file

   procedure Free_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Destroyes messages container

private

   type Abstract_Listener is abstract tagged limited record
      Flags : Message_Flags;
   end record;

   type Editor_Mark_Access is access all GPS.Editors.Editor_Mark'Class;

   type Node_Record is tagged;
   type Node_Access is access all Node_Record'Class;

   package Node_Vectors is
     new Ada.Containers.Vectors (Positive, Node_Access);

   package Category_Maps is
     new Ada.Containers.Hashed_Maps
       (Ada.Strings.Unbounded.Unbounded_String,
        Node_Access,
        Ada.Strings.Unbounded.Hash,
        Ada.Strings.Unbounded."=");

   package File_Maps is
     new Ada.Containers.Hashed_Maps
       (GNATCOLL.VFS.Virtual_File,
        Node_Access,
        GNATCOLL.VFS.Full_Name_Hash,
        GNATCOLL.VFS."=");

   type Node_Kinds is (Node_Category, Node_File, Node_Message);

   --  Declaration of Node_Record as tagged type with discriminant versus
   --  hierarchy of tagged types allows simplify its association with
   --  Gtk_Tree_Iter. The only one derived tagged type is Abstract_Message
   --  which is publicly visible as base for concrete types for messages.

   type Node_Record (Kind : Node_Kinds) is tagged limited record
      Parent        : Node_Access;
      Children      : Node_Vectors.Vector;
      Message_Count : Natural;
      Flags         : Message_Flags;

      case Kind is
         when Node_Category =>
            Container : Messages_Container_Access;
            Name      : Ada.Strings.Unbounded.Unbounded_String;
            File_Map  : File_Maps.Map;
            Sort_Hint : Sort_Order_Hint;

         when Node_File =>
            File : GNATCOLL.VFS.Virtual_File;

         when Node_Message =>
            Line   : Natural;
            Column : Basic_Types.Visible_Column_Type;
            Mark   : Editor_Mark_Access;
            Action : Action_Item;
            Style  : GPS.Styles.Style_Access;
            Length : Natural := 0;
      end case;
   end record;

   type Abstract_Message (Level : Message_Levels) is
     abstract new Node_Record (Node_Message)
   with record
      case Level is
         when Primary =>
            Weight : Natural;

         when Secondary =>
            Corresponding_File : GNATCOLL.VFS.Virtual_File;
      end case;
   end record;

   --------------------------
   -- Abstract Gtk+ models --
   --------------------------

   type Abstract_Messages_Tree_Model_Record
     (Container : not null access Messages_Container'Class) is
         abstract new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
   with record
      Flags : Message_Flags;
   end record;

   procedure Category_Added
     (Self     : not null access Abstract_Messages_Tree_Model_Record;
      Category : not null Node_Access) is null;

   procedure Category_Removed
     (Self  : not null access Abstract_Messages_Tree_Model_Record;
      Index : Positive) is null;

   procedure File_Added
     (Self     : not null access Abstract_Messages_Tree_Model_Record;
      File     : not null Node_Access) is null;

   procedure File_Removed
     (Self     : not null access Abstract_Messages_Tree_Model_Record;
      Category : not null Node_Access;
      Index    : Positive) is null;

   procedure Message_Added
     (Self    : not null access Abstract_Messages_Tree_Model_Record;
      Message : not null Message_Access) is null;

   procedure Message_Property_Changed
     (Self    : not null access Abstract_Messages_Tree_Model_Record;
      Message : not null Message_Access) is null;

   procedure Message_Removed
     (Self  : not null access Abstract_Messages_Tree_Model_Record;
      File  : not null Node_Access;
      Index : Positive) is null;

   type Messages_Model_Access is
     access all Abstract_Messages_Tree_Model_Record'Class;

   type Message_Save_Procedure is
     access procedure
       (Message_Node : not null Message_Access;
        XML_Node     : not null XML_Utils.Node_Ptr);

   type Primary_Message_Load_Procedure is
     access function
       (XML_Node      : not null XML_Utils.Node_Ptr;
        Container     : not null Messages_Container_Access;
        Category      : String;
        File          : GNATCOLL.VFS.Virtual_File;
        Line          : Natural;
        Column        : Basic_Types.Visible_Column_Type;
        Weight        : Natural;
        Actual_Line   : Integer;
        Actual_Column : Integer;
        Flags         : Message_Flags)
        return not null Message_Access;

   type Secondary_Message_Load_Procedure is
     access procedure
       (XML_Node      : not null XML_Utils.Node_Ptr;
        Parent        : not null Message_Access;
        File          : GNATCOLL.VFS.Virtual_File;
        Line          : Natural;
        Column        : Basic_Types.Visible_Column_Type;
        Actual_Line   : Integer;
        Actual_Column : Integer;
        Flags         : Message_Flags);

   package Model_Vectors is
     new Ada.Containers.Vectors (Positive, Messages_Model_Access);

   package Listener_Vectors is
     new Ada.Containers.Vectors (Positive, Listener_Access);

   function Hash (Item : Ada.Tags.Tag) return Ada.Containers.Hash_Type;

   package Primary_Message_Load_Maps is
     new Ada.Containers.Hashed_Maps
       (Ada.Tags.Tag, Primary_Message_Load_Procedure, Hash, Ada.Tags."=");

   package Secondary_Message_Load_Maps is
     new Ada.Containers.Hashed_Maps
       (Ada.Tags.Tag, Secondary_Message_Load_Procedure, Hash, Ada.Tags."=");

   package Message_Save_Maps is
     new Ada.Containers.Hashed_Maps
       (Ada.Tags.Tag, Message_Save_Procedure, Hash, Ada.Tags."=");

   package Sort_Order_Hint_Maps is
     new Ada.Containers.Hashed_Maps
       (Ada.Strings.Unbounded.Unbounded_String,
        Sort_Order_Hint,
        Ada.Strings.Unbounded.Hash,
        Ada.Strings.Unbounded."=");

   type Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
   is tagged limited record
      Project_File      : GNATCOLL.VFS.Virtual_File;
      Category_Map      : Category_Maps.Map;
      Categories        : Node_Vectors.Vector;
      Models            : Model_Vectors.Vector;
      Listeners         : Listener_Vectors.Vector;
      Savers            : Message_Save_Maps.Map;
      Primary_Loaders   : Primary_Message_Load_Maps.Map;
      Secondary_Loaders : Secondary_Message_Load_Maps.Map;
      Sort_Order_Hints  : Sort_Order_Hint_Maps.Map;
   end record;

   procedure Register_Message_Class
     (Self           : not null access Messages_Container'Class;
      Tag            : Ada.Tags.Tag;
      Save           : not null Message_Save_Procedure;
      Primary_Load   : Primary_Message_Load_Procedure;
      Secondary_Load : Secondary_Message_Load_Procedure);
   --  Registers save and load procedures for the specified class of messages

   function Match (A, B : Message_Flags) return Boolean;
   pragma Inline (Match);
   --  Return True if A and B have one "True" field in common

end GPS.Kernel.Messages;
