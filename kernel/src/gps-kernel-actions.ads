------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

--  This package describes the use of Actions in GPS.
--
--  Actions are named commands (or list of commands) in GPS. These can
--  be associated with menus, keys and toolbar buttons among other things.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Commands.Interactive;  use Commands.Interactive;
with Gdk.Event;
with GNAT.Strings;
with String_Hash;

package GPS.Kernel.Actions is

   type Action_Record is private;
   type Action_Record_Access is access Action_Record;

   package Action_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
      (String);

   procedure Register_Action
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Command     : access Commands.Interactive.Interactive_Command'Class;
      Description : String := "";
      Filter      : Action_Filter := null;
      Category    : String := "General";
      Icon_Name   : String := "");
   --  Register a new named action in GPS.
   --  Only the actions that can be executed interactively by the user
   --  should be registered.
   --  Name must be unique in GPS.
   --  Action will be freed automatically by the kernel.
   --  Category is used in the key bindings editor to group actions and make
   --  them easier to find by the user. If it is the empty string, the action
   --  will not be shown in the keybinding editor.
   --  Command is then owned by the kernel, and will be freed when GPS exits.
   --  You must not call Unref withouth first calling Ref on that command.

   procedure Unregister_Action
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String;
      Remove_Menus_And_Toolbars : Boolean := True);
   --  Remove action named Name from the table of actions.
   --  Automatically remove the associated menus and toolbar items if
   --  Remove_Menus_And_Toolbars is true.

   function Lookup_Action
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return Action_Record_Access;
   --  Lookup a command by name. Return null if no such action has been
   --  registered.
   --  If Name represents the absolute path to a menu (starting with /), then
   --  an action is created dynamically as appropriate (but doesn't need to
   --  be freed explicitly by the caller)

   procedure Set_Action_Disabled
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Name     : String;
      Disabled : Boolean);
   --  Whether an action is allowed in GPS.
   --  This is used to disable the use of specific actions across GPS,
   --  including for the contextual menus.

   function Filter_Matches
     (Self    : access Action_Record;
      Context : Selection_Context) return Boolean;
   --  Whether the action can be executed in this context

   function "and"
     (Action : access Action_Record;
      Filter : access Action_Filter_Record'Class)
      return access Action_Filter_Record'Class;
   --  Combine the action's filter with another filter, and return the result.
   --  Any of the two parameters can be null

   function Get_Filter_Error
     (Self : access Action_Record) return Unbounded_String;
   --  Return the error message that might explain why the filter does not
   --  match the current context

   function Has_Filter (Self : not null access Action_Record) return Boolean;
   --  Whether there is a filter for this action.

   function Get_Name (Self : not null access Action_Record) return String;
   --  Return the name for the action

   function Get_Command
     (Self    : not null access Action_Record)
      return not null access Interactive_Command'Class;
   --  The command to execute for this action.
   --  ??? This subprogram is provided as a transition only. New code should
   --  always store the name of the action, and execute it via its name.
   --  The returned command might be free if the command is unregistered or
   --  overridden, and code that stores the command would access freed memory.

   function Get_Icon_Name (Self : access Action_Record) return String;
   --  Return the icon to use when this action is made visible to the user.

   function Execute_Action
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Action      : String;
      Context     : Selection_Context := No_Context;
      Event       : Gdk.Event.Gdk_Event := null;
      Repeat      : Positive := 1;
      Args        : access GNAT.Strings.String_List := null;
      Synchronous : Boolean := False;
      Show_Bar    : Boolean := False;
      Via_Menu    : Boolean := False;
      Block_Exit  : Boolean := False;
      Error_Msg_In_Console : Boolean := True) return Boolean;
   --  Execute the action if it is valid for the given context.
   --  If Context is null, it is computed automatically.
   --  Returns True if the command was executed, False if it did not apply to
   --  the context.
   --  Args are extra arguments that are passed to the command. It is
   --  automatically freed by this function. These are used from XML files
   --  using $1, $2,..
   --  If Synchronous is True, the command is executed immediately, and this
   --  function returns only when the action has finished. Otherwise, the
   --  action is executed in the background, and this function might return
   --  before it has finished.
   --  Show_Bar indicates whether to show a progress bar, in case the action
   --  is executed in the background.
   --  If Error_Msg_In_Console is true and the filter did not match, an error
   --  is displayed in the console.
   --  Via_Menu can be set to True to force the command to execute as if it
   --  was executed from a menu. By default, this is computed from the Event.
   --  If Block_Exit is true, GPS will not exit while the command is running.

   function Get_Category
     (Action : not null access Action_Record) return String;
   --  Return the name of the category for the action

   function Get_Full_Description
     (Action : not null access Action_Record;
      Kernel : access Kernel_Handle_Record'Class := null;
      Use_Markup : Boolean := True)
     return String;
   --  Return the full description (+ name + category + shortcut) for this
   --  action.
   --  If the Kernel is specified, the description includes the known
   --  keyboard shortcuts for this action.
   --  The result string includes pango markup.

   type Action_Iterator is private;

   function Start (Kernel : access Kernel_Handle_Record'Class)
      return Action_Iterator;
   --  Return the first action registered in the kernel (this is in no
   --  particular order).

   procedure Next
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : in out Action_Iterator);
   --  Move to the next action

   function Get (Iter : Action_Iterator) return String;
   function Get (Iter : Action_Iterator) return Action_Record_Access;
   --  Return the current action. The empty string or No_Action is returned if
   --  there are no more actions.

private

   type Action_Record is record
      Command     : access Interactive_Command'Class;
      Filter      : access Action_Filter_Record'Class;
      Description : GNAT.Strings.String_Access;
      Name        : GNAT.Strings.String_Access;
      Modified    : Boolean;
      Overridden  : Boolean;
      Category    : GNAT.Strings.String_Access;

      Disabled    : Boolean := False;
      --  Whether this command was disabled explicitly.

      Icon_Name   : GNAT.Strings.String_Access;
   end record;
   --  Command is freed automatically. We use an anonymous type so that calls
   --  to Register_Action can call "new ..." directly when passing a value to
   --  the parameter.
   --
   --  Filter indicates when the action can be executed. If null, this means
   --  the action can always be executed. The filter mustn't be deallocated
   --  in the life of GPS, since there might be actions bound to it at any
   --  time.
   --  Category is used in the key bindings editor to group actions. If null,
   --  the action should not be shown in the keybinding editor.
   --
   --  Icon_Name is the icon for this action (optional, might be null). See
   --  GPS.Stock_Icons for more information on icon themes.

   procedure Free (Action : in out Action_Record_Access);
   --  Free the memory occupied by the action

   package Actions_Htable is new String_Hash
     (Action_Record_Access, Free, null, Case_Sensitive => False);

   type Actions_Htable_Record is new Root_Table with record
      Table : Actions_Htable.String_Hash_Table.Instance;
   end record;
   type Actions_Htable_Access is access all Actions_Htable_Record'Class;

   overriding procedure Reset (X : access Actions_Htable_Record);
   --  Reset the table.

   type Action_Iterator is record
      Iterator : Actions_Htable.String_Hash_Table.Cursor;
   end record;

end GPS.Kernel.Actions;
