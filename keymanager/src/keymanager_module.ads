-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2006                       --
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

with Ada.Unchecked_Deallocation;
with Gdk.Event;
with Gdk.Types;
with GNAT.Strings;
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
   --  overrides everything

   procedure Block_Key_Shortcuts
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Block all handling of key shortcuts defined in GPS. gtk+'s own key
   --  handling, though, will be performed as usual

   procedure Unblock_Key_Shortcuts
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Reactivate the handling of key shortcuts

private
   type Keys_Header_Num is range 0 .. 1000;
   type Key_Binding is record
      Key      : Gdk.Types.Gdk_Key_Type;
      Modifier : Gdk.Types.Gdk_Modifier_Type;
   end record;

   type Keymap_Record;
   type Keymap_Access is access Keymap_Record;

   type Key_Description;
   type Key_Description_List is access Key_Description;
   type Key_Description is record
      Action  : GNAT.Strings.String_Access;
      Next    : Key_Description_List;
      Keymap  : Keymap_Access := null;
      --  This is the secondary keymap
      Changed : Boolean := False;
   end record;
   No_Key : constant Key_Description_List := null;
   --  Changed is set to True when the key was customized from within GPS
   --  itself, and should therefore be saved on exit. It is false for values
   --  read from the custom files.

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

   type HTable_Access is access Key_Htable.HTable;

   type Keymap_Record is record
      Table : Key_Htable.HTable;
   end record;
   for Keymap_Record'Alignment use
     Integer'Min (16, Standard'Maximum_Alignment);
   --  Mapping between keys and actions. This table doesn't include the
   --  shortcuts for menus in general, although it might after the key
   --  shortcuts editor has been opened.

   Disabled_String   : constant String := "";
   --  Displayed for the shortcut of unassigned actions

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Key_Htable.HTable, HTable_Access);

   function Lookup_Key_From_Action
     (Table             : HTable_Access;
      Action            : String;
      Default           : String := "none";
      Use_Markup        : Boolean := True;
      Return_Multiple   : Boolean := True;
      Default_On_Gtk    : Boolean := True;
      Is_User_Changed   : access Boolean) return String;
   --  Return the list of key bindings set for a specific action. The returned
   --  string can be displayed as is to the user, but is not suitable for
   --  parsing as a keybinding. The list of keybindings includes the
   --  accelerators set by gtk+ for its menus (in which case Accel_Path_Prefix
   --  needs to be defined)
   --  If Use_Markup is true, then the "or" that separates several shortcuts
   --  is displayed with a different font.
   --  If Default_On_Gtk is true and the action is not found in the table but
   --  corresponds to a menu, look it up using standard gtk+ mechanisms and
   --  insert the corresponding entry in Table to speed up further lookups.
   --  If Return_Multiple is True and there are multiple shortcuts for this
   --  action, all are concatenated in the resulting string, otherwise only the
   --  first one found is returned.
   --  On exit, Is_User_Changed is set to true if at least one of the key
   --  bindings has been modified by the user (as opposed to being set by a
   --  script or by default in GPS)

   procedure Bind_Default_Key_Internal
     (Kernel                    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Table                                : in out Key_Htable.HTable;
      Action                               : String;
      Key                                  : String;
      Save_In_Keys_XML                     : Boolean;
      Remove_Existing_Shortcuts_For_Action : Boolean;
      Remove_Existing_Actions_For_Shortcut : Boolean;
      Update_Menus                         : Boolean);
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
   --  If Save_In_Keys_XML is true, then the action will be saved in keys.xml
   --  when GPS exits, and reloaded the next time it is started.
   --
   --  If Update_Menus is true, then gtk+ accelerators are immediately
   --  updated to reflect the change. This should be False when keys are
   --  edited in the key shortcut editor, since this is synchronized only when
   --  the editor is saved. If Update_Menus is True, then
   --  Remove_Existing_Actions_For_Shortcut also applies to menus.

   procedure Block_Accel_Map_Refresh
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Block  : Boolean);
   --  Block the update of the internal key shortcuts table when the global
   --  gtk+ accel map is changed (in particular through the dynamic key
   --  binding feature, but also when key shortcuts are changed
   --  programmatically)

   function Get_Shortcuts
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return HTable_Access;
   --  Return the list of all key shortcuts

   procedure Clone (From : Key_Htable.HTable; To : out Key_Htable.HTable);
   --  Deep copy of From

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

end KeyManager_Module;
