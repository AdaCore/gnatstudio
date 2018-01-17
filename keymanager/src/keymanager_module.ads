------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;
with GNATCOLL.VFS;

with Gdk.Event;
with Gdk.Types;

with GPS.Kernel;
with HTables;

package KeyManager_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Register_Key_Menu
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus for KeyManager module

   procedure Load_Custom_Keys
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Load the customized key bindings. This needs to be done after all
   --  XML files and themes have been loaded, so that the user's choice
   --  overrides everything.

   type Key_Theme_Type is tagged private;
   --  Type representing a key theme

   Null_Key_Theme : constant Key_Theme_Type;

   function Get_Name (Theme : Key_Theme_Type) return String;
   --  Return the name of the given key theme

   function Is_User_Defined (Theme : Key_Theme_Type) return Boolean;
   --  Return True if the given the is user defined, False otherwise

   type Key_Theme_Type_List is private;
   type Key_Theme_Type_Cursor is private;
   --  Types used to iterate over key themes

   Null_Key_Theme_Type_Cursor : constant Key_Theme_Type_Cursor;

   function Get_First_Reference
     (Themes_List : Key_Theme_Type_List) return Key_Theme_Type_Cursor;
   procedure Next (Theme_Cursor : in out Key_Theme_Type_Cursor);
   function Find_By_Name
     (Themes_List : Key_Theme_Type_List;
      Name        : String) return Key_Theme_Type_Cursor;
   function Get_Key_Theme
     (Theme_Cursor : Key_Theme_Type_Cursor) return Key_Theme_Type;

   function Get_Key_Theme
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return String;
   procedure Set_Key_Theme
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String);
   --  Return the name of the key theme selected by the user

   function User_Key_Theme_Directory
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return GNATCOLL.VFS.Virtual_File;
   --  Return the directory used for user themes.

   procedure Load_Key_Theme
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Theme  : String := "");
   --  Load an XML file that contains a key theme. By default, it loads the
   --  key theme that the user has selected last. If Theme is specified, it is
   --  also stored as the default key theme for the next GPS session.
   --  This does not remove existing key bindings.

   type Remove_Mode is (All_Shortcuts, Standard_Shortcuts, User_Shortcuts);
   procedure Remove_Shortcuts
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Mode   : Remove_Mode);
   --  Remove key shortcuts:
   --  * either all shortcuts
   --  * or shortcuts loaded from key themes (but preserve user defined ones)
   --  * or only user defined shortcuts (and preserve standard ones).

   function List_Key_Themes
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return Key_Theme_Type_List;
   --  Return all known key themes.

   procedure Block_Key_Shortcuts
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Block all handling of key shortcuts defined in GPS. gtk+'s own key
   --  handling, though, will be performed as usual.

   procedure Unblock_Key_Shortcuts
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Reactivate the handling of key shortcuts

   type General_Event_Handler_Callback is access function
     (Event : Gdk.Event.Gdk_Event;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Boolean;
   --  A function called when any event reaches any of the GPS windows. It
   --  must return True if the event was processed, False otherwise (in which
   --  case the event will be further processed).

   procedure Add_Event_Handler
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Handler : General_Event_Handler_Callback);
   procedure Remove_Event_Handler
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Handler : General_Event_Handler_Callback);
   --  Add a new callback to be called when an event should be processed

private
   type Keys_Header_Num is range 0 .. 1000;
   type Key_Binding is record
      Key      : Gdk.Types.Gdk_Key_Type;
      Modifier : Gdk.Types.Gdk_Modifier_Type;
   end record;

   type Key_Theme_Type is tagged record
      Name         : Ada.Strings.Unbounded.Unbounded_String;
      User_Defined : Boolean;
   end record;

   package Key_Theme_Type_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Key_Theme_Type,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
      "="             => "=");

   type Key_Theme_Type_List is new Key_Theme_Type_Maps.Map with null record;
   type Key_Theme_Type_Cursor is record
      C : Key_Theme_Type_Maps.Cursor;
   end record;

   Null_Key_Theme : constant Key_Theme_Type := Key_Theme_Type'
     (Name         => Ada.Strings.Unbounded.Null_Unbounded_String,
      User_Defined => False);

   Null_Key_Theme_Type_Cursor : constant Key_Theme_Type_Cursor :=
                                  Key_Theme_Type_Cursor'
                                    (C => Key_Theme_Type_Maps.No_Element);

   type Keymap_Record;
   type Keymap_Access is access Keymap_Record;

   type Key_Description;
   type Key_Description_List is access Key_Description;
   type Key_Description is record
      Action  : GNAT.Strings.String_Access;
      Next    : Key_Description_List;
      Keymap  : Keymap_Access := null;
      --  This is the secondary keymap

      User_Defined : Boolean := False;
      --  True when this binding was loaded from ~/.gps/keys6.xml, as opposed
      --  to loaded from one of the predefined themes.
   end record;
   No_Key : constant Key_Description_List := null;

   function Hash (Key : Key_Binding) return Keys_Header_Num;
   procedure Free (Element : in out Key_Description_List);
   --  Support functions for creating the htable

   package Key_Htable is new HTables.Simple_HTable
     (Header_Num   => Keys_Header_Num,
      Element      => Key_Description_List,
      Free_Element => Free,
      No_Element   => No_Key,
      Key          => Key_Binding,
      Hash         => Hash,
      Equal        => "=");

   type HTable_Access is access Key_Htable.Instance;

   type Keymap_Record is record
      Table : Key_Htable.Instance;
   end record;
   for Keymap_Record'Alignment use
     Integer'Min (16, Standard'Maximum_Alignment);
   --  Mapping between keys and actions. This table doesn't include the
   --  shortcuts for menus in general, although it might after the key
   --  shortcuts editor has been opened.

   Disabled_String   : constant String := "";
   --  Displayed for the shortcut of unassigned actions

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Key_Htable.Instance, HTable_Access);

   function Lookup_Key_From_Action
     (Table             : HTable_Access;
      Action            : String;
      Default           : String := "none";
      Use_Markup        : Boolean := True;
      Return_Multiple   : Boolean := True;
      Is_User_Changed   : access Boolean) return String;
   --  Return the list of key bindings set for a specific action. The returned
   --  string can be displayed as is to the user, but is not suitable for
   --  parsing as a keybinding. The list of keybindings includes the
   --  accelerators set by gtk+ for its menus (in which case Accel_Path_Prefix
   --  needs to be defined)
   --  If Use_Markup is true, then the "or" that separates several shortcuts
   --  is displayed with a different font.
   --  If Return_Multiple is True and there are multiple shortcuts for this
   --  action, all are concatenated in the resulting string, otherwise only the
   --  first one found is returned.
   --  On exit, Is_User_Changed is set to true if at least one of the key
   --  bindings has been modified by the user (as opposed to being set by a
   --  script or by default in GPS)

   function Lookup_Action_From_Key
     (Key      : String;
      Bindings : HTable_Access) return String;
   --  Return the name of the action currently bound to Key (possibly a multi-
   --  key binding). The empty string is returned if the key is not bound yet.

   function Actions_With_Key_Prefix
     (Key       : String;
      Bindings  : HTable_Access;
      Separator : Character := ASCII.LF) return String;
   --  Returns all actions (and associated key shortcut) that are associated
   --  with key or are having key as a shortcut prefix. The list of actions are
   --  separated by the given character.

   procedure Bind_Default_Key_Internal
     (Kernel                    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Table                                : in out Key_Htable.Instance;
      Action                               : String;
      Key                                  : String;
      Save_In_Keys_XML                     : Boolean;
      Remove_Existing_Shortcuts_For_Action : Boolean;
      Remove_Existing_Actions_For_Shortcut : Boolean);
   --  Add a new key shortcut for Action.
   --
   --  If Remove_Existings_Shortcuts_For_Action, then any binding to this
   --  action is first cancelled. Otherwise, the binding is added to the list
   --  of valid shortcuts for this action.
   --
   --  If Remove_Existing_Actions_For_Shortcut is True, then any action bound
   --  to Key will be detached. Otherwise, the action will be executed in
   --  addition to all other actions and menus bound to this key.
   --
   --  Key can include secondary keymaps, as in "control-c control-k".
   --  If Key is the empty string, then any binding for the action is removed,
   --  and the action is saved in keys.xml so that it will be unattached the
   --  next time GPS is started.
   --
   --  Action can be the empty string to just remove existing bindings.
   --
   --  If Save_In_Keys_XML is true, then the action will be saved in keys.xml
   --  when GPS exits, and reloaded the next time it is started.
   --
   --  If Update_Menus is true, then gtk+ accelerators are immediately
   --  updated to reflect the change. This should be False when keys are
   --  edited in the key shortcut editor, since this is synchronized only when
   --  the editor is saved. If Update_Menus is True, then
   --  Remove_Existing_Actions_For_Shortcut also applies to menus.

   function Get_Shortcuts
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return HTable_Access;
   --  Return the list of all key shortcuts

   procedure Clone (From : Key_Htable.Instance; To : out Key_Htable.Instance);
   --  Deep copy of From

   procedure Save_Keys
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Save_All : Boolean;
      Filename : GNATCOLL.VFS.Virtual_File);
   --  save either all keys or only the modified ones in thegiven file.

   procedure Save_Custom_Keys
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Save the current custom keys

   procedure Set_GUI_Running (Running : Boolean);
   --  Inform the module whether the GUI is currently running

end KeyManager_Module;
