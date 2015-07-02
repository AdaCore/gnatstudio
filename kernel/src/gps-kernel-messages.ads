------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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
--  There are four user visible entities in the centralized messages
--  container: message, message's note, container and listener.
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
with Ada.Unchecked_Conversion;
with Ada.Tags;

with Default_Preferences;           use Default_Preferences;
with GNATCOLL.VFS;
with GPS.Editors.Line_Information;
with GPS.Kernel.Style_Manager;

package GPS.Kernel.Messages is

   Locations_Save_In_Desktop : Boolean_Preference;
   --  Whether we should save the locations between GPS sessions.

   type Messages_Container (<>) is tagged limited private;

   type Messages_Container_Access is
     access all Messages_Container'Class;

   type Message_Visibility_Kind is
     (Editor_Side, --  Messages displayed on the side of editors
      Locations    --  Messages displayed in the locations view
     );

   type Message_Flags is array (Message_Visibility_Kind) of Boolean;
   Empty_Message_Flags : constant Message_Flags := (others => False);
   --  A list of potential locations where a message should be shown.

   function To_Int (Flags : Message_Flags) return Integer;
   function From_Int (Int : Integer) return Message_Flags;
   --  Utility functions to load/save flags to XML

   type Abstract_Listener is abstract tagged limited private;

   type Listener_Access is access all Abstract_Listener'Class;

   subtype Action_Item is GPS.Editors.Line_Information.Action_Item;
   function To_Action_Item is new Ada.Unchecked_Conversion
      (System.Address, Action_Item);

   type Unbounded_String_Array is
     array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   type Virtual_File_Array is
     array (Positive range <>) of GNATCOLL.VFS.Virtual_File;

   type Sort_Order_Hint is (Chronological, Alphabetical);
   --  Hint for the view how it must sort items at file level by default.

   type Abstract_Note is abstract tagged limited null record;

   type Note_Access is access all Abstract_Note'Class;

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

   function Get_Flags
     (Self : not null access constant Abstract_Message'Class)
      return Message_Flags;

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

   function Get_Weight
     (Self : not null access constant Abstract_Message'Class)
      return Natural;
   --  Return the weight of the message
   --  ??? What is message weight?

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
      Style  : GPS.Kernel.Style_Manager.Style_Access;
      Length : Positive);
   --  Set highlighting style and span to be used in the editor to highlight
   --  corresponding location.

   procedure Set_Highlighting
     (Self  : not null access Abstract_Message'Class;
      Style : GPS.Kernel.Style_Manager.Style_Access);
   --  Set style for line highlighting in the editor.

   function Get_Highlighting_Style
     (Self : not null access constant Abstract_Message'Class)
      return GPS.Kernel.Style_Manager.Style_Access;
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

   procedure Set_Flags
     (Self  : not null access Abstract_Message'Class;
      Flags : Message_Flags);
   --  Sets flags of the message.

   procedure Remove (Self : not null access Abstract_Message'Class);
   --  Removes message and deallocate it.

   function Has_Note
     (Self : not null access constant Abstract_Message'Class;
      Tag  : Ada.Tags.Tag) return Boolean;
   --  Returns True when note of the given tagged type is allosicated with the
   --  messages, otherwise returns False.

   function Get_Note
     (Self : not null access constant Abstract_Message'Class;
      Tag  : Ada.Tags.Tag) return not null Note_Access;
   --  Returns note with the givent tagged type. Raises Constraint_Error when
   --  there is no note of the given tagged type associated with the message.

   procedure Set_Note
     (Self : not null access Abstract_Message'Class;
      Note : not null Note_Access);
   --  Sets note for the message. Any number of notes can be associated with
   --  the message, but each note must have own tagged type. If the note of the
   --  same tagged type already associated with the message it is destroyed
   --  and replaced by given one.

   procedure Remove_Note
     (Self : not null access Abstract_Message'Class;
      Tag  : Ada.Tags.Tag);
   --  Remove the note associated with Self

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
      Flags         : Message_Flags;
      Allow_Auto_Jump_To_First : Boolean);
   --  Initialize message and connect it to container.
   --  If Allow_Auto_Jump_To_First is True and the user preference is also true
   --  then the locations window will automatically jump to the first message
   --  when the category is created.
   --  ??? Need doc for Weight, Actual_Line, Actual_Column, not trivial

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

   -------------------
   -- Abstract_Note --
   -------------------

   procedure Finalize (Self : not null access Abstract_Note) is null;
   --  Called to release resources occupied by the note. It is called in two
   --  cases: when message is destroyed and when application request remove of
   --  the note. It is removed from the set of notes of message before call.

   ------------------------
   -- Messages Container --
   ------------------------

   function Get_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return not null Messages_Container_Access;
   --  Returns messages conntainer for the specified instance of the kernel.

   function Has_Category
     (Self     : not null access constant Messages_Container'Class;
      Category : String) return Boolean;
   --  Return True if Category is present in the container

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

   procedure Clear (Self : not null access Messages_Container'Class);
   --  Removes all messages, clear all internal structures.

   procedure Remove_All_Messages
     (Self  : not null access Messages_Container'Class;
      Flags : Message_Flags);
   --  Removes all messages that match Flags

   procedure Remove_Category
     (Self     : not null access Messages_Container'Class;
      Category : String;
      Flags    : Message_Flags);
   --  Removes all messages in the specified category. Do nothing when there
   --  is no such category.

   procedure Remove_File
     (Self     : not null access Messages_Container'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Flags    : Message_Flags);
   --  Removes all messages for specified file in the specified category.
   --  Do nothing when there is no such category or file.

   procedure Set_Sort_Order_Hint
     (Self     : not null access Messages_Container'Class;
      Category : String;
      Hint     : Sort_Order_Hint);
   --  Sets sort order hint for the specified category. Hint must be set before
   --  first message in the category is created.

   function Get_Sort_Order_Hint
     (Self     : not null access Messages_Container'Class;
      Category : String) return Sort_Order_Hint;
   --  Get the sort order hint for this category

   procedure Register_Listener
     (Self     : not null access Messages_Container;
      Listener : not null Listener_Access;
      Flags    : Message_Flags);
   --  Register listener. It do nothing when listener is already registered.
   --  This listener will only receive messages the flags of which have one
   --  "True" field in common with Flags. When all flags are set to "False",
   --  listener will always receive all notifications.

   procedure Unregister_Listener
     (Self     : not null access Messages_Container;
      Listener : not null Listener_Access);
   --  Unregister listener. It do nothing when listener is not registered.

   ----------------------
   -- Message Listener --
   ----------------------

   procedure Category_Added
     (Self     : not null access Abstract_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      Allow_Auto_Jump_To_First : Boolean) is null;
   --  If Allow_Auto_Jump_To_First is True and the user preference is also true
   --  then the locations window will automatically jump to the first message.

   procedure Category_Removed
     (Self     : not null access Abstract_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String) is null;
   --  Called on remove of category

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
   --  Called when messages is no longer visible for listener.
   --
   --  Note, it doesn't mean that messages will be destroyed immediately. Also,
   --  Message_Added subprogram can be called for this message again, when
   --  message will be visible to listener again.
   --
   --  To track phisical destruction of messages use Note mechanism.

   function Message_Can_Be_Destroyed
     (Self    : not null access Abstract_Listener;
      Message : not null access Abstract_Message'Class) return Boolean;
   --  Ask listener about destroy of the message. When one of listeners return
   --  False message is not destroyed but is made invisible.

   --------------------------
   -- For private use only --
   --------------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Create the preferences for this module

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
      Flags : Message_Flags;
      Debug : Boolean);
   --  Saves all messages into the specified file
   --  Save only categories which match Flags.

   procedure Free_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Destroyes messages container

private

   type Abstract_Reference is tagged;
   type Reference_Access is access all Abstract_Reference'Class;

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

   function Hash (Item : Ada.Tags.Tag) return Ada.Containers.Hash_Type;
   --  Hash function.

   package Note_Maps is
     new Ada.Containers.Hashed_Maps
       (Ada.Tags.Tag,
        Note_Access,
        Hash,
        Ada.Tags."=");

   type Message_Counters is array (Message_Visibility_Kind) of Natural;

   type Node_Kinds is (Node_Category, Node_File, Node_Message);

   --  Declaration of Node_Record as tagged type with discriminant versus
   --  hierarchy of tagged types allows simplify its association with
   --  Gtk_Tree_Iter. The only one derived tagged type is Abstract_Message
   --  which is publicly visible as base for concrete types for messages.

   type Node_Record (Kind : Node_Kinds) is tagged limited record
      Parent   : Node_Access;
      Children : Node_Vectors.Vector;

      case Kind is
         when Node_Category | Node_File =>
            Counters : Message_Counters;
            --  Number of messages of each visibility kinds. Used to manage
            --  notification about category and file addition/removal.

            case Kind is
               when Node_Category =>
                  Container : Messages_Container_Access;
                  Name      : Ada.Strings.Unbounded.Unbounded_String;
                  File_Map  : File_Maps.Map;
                  Sort_Hint : Sort_Order_Hint;

               when Node_File =>
                  File : GNATCOLL.VFS.Virtual_File;

               when Node_Message =>
                  null;
            end case;

         when Node_Message =>
            Line   : Natural;
            Column : Basic_Types.Visible_Column_Type;
            Mark   : Editor_Mark_Access;
            Action : Action_Item;
            Style  : GPS.Kernel.Style_Manager.Style_Access;
            Length : Natural := 0;
            Notes  : Note_Maps.Map;
            Flags  : Message_Flags;
      end case;
   end record;

   type Abstract_Message (Level : Message_Levels) is
     abstract new Node_Record (Node_Message)
   with record
      Head : Reference_Access;
      Tail : Reference_Access;
      --  Chain of references to this message

      case Level is
         when Primary =>
            Weight : Natural;

         when Secondary =>
            Corresponding_File : GNATCOLL.VFS.Virtual_File;
      end case;
   end record;

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
        Flags         : Message_Flags;
        Allow_Auto_Jump_To_First : Boolean)
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

   package Listener_Vectors is
     new Ada.Containers.Vectors (Positive, Listener_Access);

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
      Listeners         : Listener_Vectors.Vector;
      Savers            : Message_Save_Maps.Map;
      Primary_Loaders   : Primary_Message_Load_Maps.Map;
      Secondary_Loaders : Secondary_Message_Load_Maps.Map;
      Sort_Order_Hints  : Sort_Order_Hint_Maps.Map;
      Cleanup_Mode      : Boolean := False;
      --  When set to True, messages are removed even when listner requests
      --  to hide them.
      Messages_Loaded   : Boolean := False;
      --  Set to True when Project_Changed till Project_View_Changed hook
      --  to read messages only one per project when project view became
      --  ready.
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

   ------------------------
   -- Abstract_Reference --
   ------------------------

   type Abstract_Reference is
     abstract new Ada.Finalization.Controlled with record
      Message  : Message_Access;
      Previous : Reference_Access;
      Next     : Reference_Access;
   end record;

   overriding procedure Adjust (Self : in out Abstract_Reference);

   overriding procedure Finalize (Self : in out Abstract_Reference);

   procedure Set
     (Self    : in out Abstract_Reference;
      Message : not null Message_Access);
   --  Sets reference to the given message.

   procedure Unset (Self : in out Abstract_Reference);
   --  Unsets reference.

end GPS.Kernel.Messages;
