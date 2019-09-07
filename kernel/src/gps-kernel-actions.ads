------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Tags;
with GNAT.Strings;

with Gtkada.MDI;           use Gtkada.MDI;
with Gdk.Event;
with Gdk.Types;            use Gdk.Types;

with Commands.Interactive; use Commands.Interactive;
with String_Hash;

package GPS.Kernel.Actions is

   type Action_Record is private;
   type Action_Access is access Action_Record;

   procedure Register_Action
     (Kernel                   : access Kernel_Handle_Record'Class;
      Name                     : String;
      Command                  : Interactive_Command_Access;
      Description              : String := "";
      Filter                   : Action_Filter := null;
      Category                 : String := "General";
      Icon_Name                : String := "";
      For_Learning             : Boolean := False;
      Shortcut_Active_For_View : Ada.Tags.Tag := Ada.Tags.No_Tag;
      Log_On_Execute           : Boolean := True);
   --  Register a new named action in GPS.
   --  Only the actions that can be executed interactively by the user
   --  should be registered.
   --
   --  Name must be unique in GPS.
   --
   --  Action will be freed automatically by the kernel.
   --
   --  Category is used in the key bindings editor to group actions and make
   --  them easier to find by the user. If it is the empty string, the action
   --  will not be shown in the keybinding editor.
   --
   --  Command is then owned by the kernel, and will be freed when GPS exits.
   --  You must not call Unref withouth first calling Ref on that command.
   --
   --  When For_Learning is True, this action will be displayed in the GPS
   --  Learn view when it's valid in the current context.
   --
   --  Shortcut_Active_For_View Used to enable the key shortcut associated
   --  to this action only when the focus is within a view with a type
   --  contained in this tag's hierarchy (e.g: a view descendant of the
   --  Browser_Child_Record type). This is useful when the key shortcut does
   --  not contain any key modifier (e.g: if we want to use '+' as a key
   --  shortcut for a given action but still want to be able to write a '+'
   --  in an editor).
   --
   --  If Log_On_Execute is False, the action won't be logged when executed.
   --  Setting this falg to False can be useful for actions that are very
   --  sensitive regarding performance (e.g: basic editor actions).

   procedure Unregister_Action
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String;
      Remove_Menus_And_Toolbars : Boolean := True);
   --  Remove action named Name from the table of actions.
   --  Automatically remove the associated menus and toolbar items if
   --  Remove_Menus_And_Toolbars is true.

   function Lookup_Action
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return Action_Access;
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
      Filter : Action_Filter) return Action_Filter;
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

   function Get_Label (Self : not null access Action_Record) return String;
   --  Return a suitable label for the given action, starting with a capital
   --  letter and with the rest of the name being lowercase.

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

   function Is_Key_Shortcut_Active
     (Self  : access Action_Record;
      Child : access MDI_Child_Record'Class;
      Key   : Gdk_Key_Type;
      Modif : Gdk_Modifier_Type)
      return Boolean;
   --  Return True if the key shortcut should be active in the given MDI child,
   --  False otherwise.

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
   --  Action can be an action name or a menu path - but prefer to use the
   --  action name, which is more stable than menu names, as menus may be
   --  reordered by users.
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
     (Action           : not null access Action_Record;
      Kernel           : access Kernel_Handle_Record'Class := null;
      Use_Markup       : Boolean := True;
      Include_Name     : Boolean := True;
      Include_Category : Boolean := True;
      Include_Menus    : Boolean := True)
     return String;
   --  Return the full description for this action.
   --  If Include_Name, this includes the name of the action.
   --  If Include_Category, this includes the category of the action.
   --  If Include_Menus, this includes the menus associated to the action.
   --  If the Kernel is specified, the description includes the known
   --  keyboard shortcuts for this action.
   --  The result string includes pango markup if Use_Markup is True.

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
   function Get (Iter : Action_Iterator) return Action_Access;
   --  Return the current action. The empty string or No_Action is returned if
   --  there are no more actions.

   procedure Register_Actions_Learn_Provider
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Register all the actions that we want to display in the Learn view

private

   type Action_Record is record
      Command                  : access Interactive_Command'Class;
      Filter                   : Action_Filter;
      Description              : GNAT.Strings.String_Access;
      Name                     : GNAT.Strings.String_Access;
      Modified                 : Boolean;
      Overridden               : Boolean;
      Category                 : GNAT.Strings.String_Access;

      Disabled                 : Boolean := False;
      --  Whether this command was disabled explicitly.

      Icon_Name                : GNAT.Strings.String_Access;

      Shortcut_Active_For_View : Ada.Tags.Tag := Ada.Tags.No_Tag;
      --  Used to enable the key shortcut associated to this action only when
      --  the focus is within a view with a type contained in this tag's
      --  hierarchy (e.g: a view descendant of the Browser_Child_Record type).
      --  This is useful when the key shortcut does not contain any key
      --  modifier (e.g: if we want to use '+' as a key shortcut for a given
      --  action but still want to be able to write a '+' in an editor).

      Log_On_Execute           : Boolean := True;
      --  Used to know whether we should log this action when executing it.
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

   procedure Free (Action : in out Action_Access);
   --  Free the memory occupied by the action

   package Actions_Htable is new String_Hash
     (Action_Access, Free, null, Case_Sensitive => False);

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
