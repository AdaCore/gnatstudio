-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
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

with Gtk.Tree_Model;
private with Gtkada.Abstract_Tree_Model;
with GNATCOLL.VFS;

package GPS.Kernel.Messages is

   type Messages_Container (<>) is tagged limited private;

   type Messages_Container_Access is
     access all Messages_Container'Class;

   type Abstract_Listener is abstract tagged limited null record;

   type Listener_Access is access all Abstract_Listener'Class;

   ----------------------
   -- Abstract Message --
   ----------------------

   type Message_Levels is (Primary, Secondary);

   type Abstract_Message (Level : Message_Levels) is
     abstract tagged limited private;

   type Message_Access is access all Abstract_Message'Class;

   subtype Primary_Abstract_Message is Abstract_Message (Primary);
   subtype Secondary_Abstract_Message is Abstract_Message (Secondary);

   function Get_Category
     (Self : not null access constant Abstract_Message'Class)
      return Ada.Strings.Unbounded.Unbounded_String;

   function Get_Category
     (Self : not null access constant Abstract_Message'Class) return String;

   function Get_File
     (Self : not null access constant Abstract_Message'Class)
      return GNATCOLL.VFS.Virtual_File;

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

   procedure Initialize
     (Self      : not null access Abstract_Message'Class;
      Container : not null Messages_Container_Access;
      Category  : String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type);
   --  Initialize message and connect it to container

   procedure Initialize
     (Self   : not null access Abstract_Message'Class;
      Parent : not null Message_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type);
   --  Initialize message and connect it to parent message

   ------------------------
   -- Messages Container --
   ------------------------

   function Get_Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return not null Messages_Container_Access;
   --  Returns messages conntainer for the specified instance of the kernel.

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

   function Get_Message_At
     (Self     : not null access constant Messages_Container'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Basic_Types.Visible_Column_Type)
      return Message_Access;
   --  Returns last inserted mesage at the specified location if any;
   --  otherwise returns null.
   --  XXX It is used for implementation of legacy API, need to be reviewed.
   --  Please, avoid use of it or change this comment.

   function Get_Classic_Tree_Model
     (Self : not null access constant Messages_Container'Class)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns Gtk+ model for classic tree representation.

   procedure Register_Listener
     (Self     : not null access Messages_Container;
      Listener : not null Listener_Access);
   --  Register listener. It do nothing when listener is already registered.

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

   procedure Message_Added
     (Self    : not null access Abstract_Listener;
      Message : not null access Abstract_Message'Class) is null;

   --------------------------
   -- For private use only --
   --------------------------

   function Create_Message_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
      return System.Address;
   --  Creates new nessages container and returns its address. Address is used
   --  to break circular dependency between Kernel and Messages_Container.

private

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
   --  hierarhy of tagged types allows simplify its association with
   --  Gtk_Tree_Iter. The only one derived tagged type is Abstract_Message
   --  which is publically visible as base for concrete types for messages.

   type Node_Record (Kind : Node_Kinds) is tagged limited record
      Parent   : Node_Access;
      Children : Node_Vectors.Vector;

      case Kind is
         when Node_Category =>
            Container : Messages_Container_Access;
            Name      : Ada.Strings.Unbounded.Unbounded_String;
            File_Map  : File_Maps.Map;

         when Node_File =>
            File : GNATCOLL.VFS.Virtual_File;

         when Node_Message =>
            Line   : Natural;
            Column : Basic_Types.Visible_Column_Type;
      end case;
   end record;

   type Abstract_Message (Level : Message_Levels) is
     abstract new Node_Record (Node_Message) with record
      case Level is
         when Primary =>
            null;

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
            with null record;

   procedure Category_Added
     (Self     : not null access Abstract_Messages_Tree_Model_Record;
      Category : not null Node_Access) is null;

   procedure Category_Removed
     (Self  : not null access Abstract_Messages_Tree_Model_Record;
      Index : Positive) is null;

   procedure File_Added
     (Self : not null access Abstract_Messages_Tree_Model_Record;
      File : not null Node_Access) is null;

   procedure File_Removed
     (Self     : not null access Abstract_Messages_Tree_Model_Record;
      Category : not null Node_Access;
      Index    : Positive) is null;

   procedure Message_Added
     (Self    : not null access Abstract_Messages_Tree_Model_Record;
      Message : not null Message_Access) is null;

   procedure Message_Removed
     (Self  : not null access Abstract_Messages_Tree_Model_Record;
      File  : not null Node_Access;
      Index : Positive) is null;

   type Messages_Model_Access is
     access all Abstract_Messages_Tree_Model_Record'Class;

   package Model_Vectors is
     new Ada.Containers.Vectors (Positive, Messages_Model_Access);

   package Listener_Vectors is
     new Ada.Containers.Vectors (Positive, Listener_Access);

   type Messages_Container
     (Kernel : not null access Kernel_Handle_Record'Class)
   is tagged limited record
      Category_Map : Category_Maps.Map;
      Categories   : Node_Vectors.Vector;
      Models       : Model_Vectors.Vector;
      Listeners    : Listener_Vectors.Vector;
   end record;

end GPS.Kernel.Messages;
