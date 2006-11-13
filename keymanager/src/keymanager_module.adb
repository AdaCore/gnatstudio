-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2006                      --
--                              AdaCore                              --
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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;             use GNAT.OS_Lib;

with System.Assertions;       use System.Assertions;

with Pango.Enums;             use Pango.Enums;

with Gdk.Event;               use Gdk.Event;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Gdk.Types;               use Gdk.Types;

with Glib.Convert;            use Glib.Convert;
with Glib.Object;             use Glib.Object;
with Glib.Xml_Int;            use Glib.Xml_Int;
with Glib.Values;             use Glib.Values;
with Glib;                    use Glib;

with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Accel_Map;           use Gtk.Accel_Map;
with Gtk.Arguments;           use Gtk.Arguments;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Cell_Renderer_Text;  use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;        use Gtk.Check_Button;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Event_Box;           use Gtk.Event_Box;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.Handlers;            use Gtk.Handlers;
with Gtk.Hbutton_Box;         use Gtk.Hbutton_Box;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main;                use Gtk.Main;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Paned;               use Gtk.Paned;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Separator;           use Gtk.Separator;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtk.Text_View;           use Gtk.Text_View;
with Gtk.Tooltips;            use Gtk.Tooltips;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;   use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort;     use Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection;      use Gtk.Tree_Selection;
with Gtk.Tree_Store;          use Gtk.Tree_Store;
with Gtk.Tree_View;           use Gtk.Tree_View;
with Gtk.Tree_View_Column;    use Gtk.Tree_View_Column;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

with Gtkada.File_Selector;    use Gtkada.File_Selector;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.Macro;            use Gtkada.Macro;
with Gtkada.MDI;              use Gtkada.MDI;

with Case_Handling;           use Case_Handling;
with Commands.Interactive;    use Commands, Commands.Interactive;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Kernel;              use GPS.Kernel;
with GUI_Utils;               use GUI_Utils;
with HTables;                 use HTables;
with String_Utils;            use String_Utils;
with Traces;                  use Traces;
with VFS;                     use VFS;
with XML_Parsers;

package body KeyManager_Module is

   Me : constant Debug_Handle := Create ("Keymanager");

   Use_Macro : constant Debug_Handle := Create ("Keymanager.Macro", Off);
   --  ??? For now disable by default since this is a work in progress

   File_Cst                  : aliased constant String := "file";
   Speed_Cst                 : aliased constant String := "speed";
   Command_Cst               : aliased constant String := "command";
   Key_Cst                   : aliased constant String := "key";
   Count_Cst                 : aliased constant String := "count";
   Load_Macro_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => File_Cst'Access);
   Play_Macro_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Speed_Cst'Access);

   Menu_Context_Name : constant String := "Menus";
   --  -"Menus" will need to be translated

   Disabled_String   : constant String := "";
   --  Displayed for the shortcut of unassigned actions

   type Keys_Header_Num is range 0 .. 1000;
   type Key_Binding is record
      Key      : Gdk_Key_Type;
      Modifier : Gdk_Modifier_Type;
   end record;

   type Keymap_Record;
   type Keymap_Access is access Keymap_Record;

   type Key_Description;
   type Key_Description_List is access Key_Description;
   type Key_Description is record
      Action  : String_Access;
      Next    : Key_Description_List;
      Keymap  : Keymap_Access := null;
      --  This is the secondary keymap
      Changed : Boolean := False;
   end record;
   No_Key : constant Key_Description_List := null;
   --  Changed is set to True when the key was customized from within GPS
   --  itself, and should therefore be saved on exit. It is false for values
   --  read from the custom files.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Key_Description, Key_Description_List);

   procedure Free_Non_Recursive (Element : in out Key_Description_List);
   --  Free Element, but not its sibling.
   --  Warning: this breaks the list in which Element was, since the previous
   --  element will still point to Element.

   function Hash (Key : Key_Binding) return Keys_Header_Num;
   procedure Free (Element : in out Key_Description_List);
   --  Support functions for creating the htable

   package Key_Htable is new Simple_HTable
     (Header_Num   => Keys_Header_Num,
      Element      => Key_Description_List,
      Free_Element => Free,
      No_Element   => No_Key,
      Key          => Key_Binding,
      Hash         => Hash,
      Equal        => "=");
   use Key_Htable;

   type HTable_Access is access Key_Htable.HTable;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Key_Htable.HTable, HTable_Access);

   procedure Clone
     (From : Key_Htable.HTable; To : out Key_Htable.HTable);
   procedure Clone
     (From : Key_Description_List; To : out Key_Description_List);
   --  Deep-copy of From

   type Keymap_Record is record
      Table : Key_Htable.HTable;
   end record;
   for Keymap_Record'Alignment use
     Integer'Min (16, Standard'Maximum_Alignment);
   --  Mapping between keys and actions. This table doesn't include the
   --  shortcuts for menus in general, although it might after the key
   --  shortcuts editor has been opened.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Keymap_Record, Keymap_Access);

   type Event_Set is record
      Events           : Macro_Item_Access;
      --  Set of events recorded.

      Last_Event       : Macro_Item_Access;
      --  Last event recorded.

      Current_Event    : Macro_Item_Access;
      --  Current event being replayed.

      Start_Clock       : Ada.Calendar.Time;
      --  Start time of event replay.

      Time_Spent        : Guint32;
      --  Virtual time spent so far in events (addition of Events.Time)

      Prev_Time         : Guint32;
      --  Time of previous event recorded.

      Speed            : Duration := 1.0;
      --  Speed at which replay is made. 1.0 means normal speed.
   end record;

   type Key_Manager_Record is record
      Kernel           : Kernel_Handle;
      Table            : HTable_Access;

      Custom_Keys_Loaded : Boolean := False;
      --  Whether the user's custom keys have been loaded

      Secondary_Keymap : Keymap_Access := null;
      --  The secondary keymap currently in use, or null if using the primary.

      Active           : Boolean := True;
      --  Whether the key manager should process the key events. This is only
      --  deactivated while editing the key bindings through the GUI.

      Recording        : Boolean := False;
      --  Whether the key manager is recording key events

      Events           : Event_Set;
      --  Handle record and replay of events
   end record;
   type Key_Manager_Access is access all Key_Manager_Record;

   function Is_Numeric_Key
     (Key      : Gdk_Key_Type;
      Modifier : Gdk_Modifier_Type) return Boolean;
   --  Whether Key is one of the numeric keys

   type Argument_Key_Validator is access function
     (Key : Gdk_Key_Type; Modifier : Gdk_Modifier_Type) return Boolean;
   --  Return True if Key is still valid for the current action argument we
   --  are reading

   type Argument_Read_Callback is access procedure
     (Command  : Interactive_Command'Class;
      Argument : String);
   --  Called when the user has finished entering the argument for the
   --  command (ie when Argument_Key_Validator returned False).

   procedure Read_Action_Argument
     (Validator : Argument_Key_Validator;
      Callback  : Argument_Read_Callback;
      Command   : access Interactive_Command'Class);
   --  Read an argument for the command. This includes all keys pressed while
   --  Validator returns True. Callback is called when the argument has been
   --  read.

   type Keymanager_Module_Record is new Module_ID_Record with record
      Key_Manager   : Key_Manager_Access;
      Accel_Map_Id  : Handler_Id;
      Menus_Created : Boolean := False;
      --  Indicates whether the initial set of menus has been created.

      Repeat_Count     : Positive := 1;
      --  Number of times that the next command should be repeated

      Argument_Validator : Argument_Key_Validator;
      Argument_Callback  : Argument_Read_Callback;
      Argument_Data      : Interactive_Command_Access;
      Argument_Current   : String_Access;
      --  Whether we are currently reading the argument for a command, see
      --  Read_Action_Argument, and the callbacks to use to check whether the
      --  user has finished entering the argument. Both callbacks are set to
      --  null when we are not reading an argument.

      Last_Command  : Cst_String_Access;
      --  The last action that was executed by the user. It can either be a
      --  precise action name when executed through a key binding, or null if
      --  the command is not known precisely.
      --  This string must never be freed and points to global data elsewhere
      --  in GPS.

      Last_User_Command : String_Access;
      --  This is the name of the last command set by the user. The idea is
      --  that in a callback the user can set this, which will be return by the
      --  shell command "last_command" if set (otherwise the field
      --  Last_Command) is returned. Last_User_Command is freed whenever a
      --  command different from Last_Command is executed by the user.
   end record;
   type Keymanager_Module_ID is access all Keymanager_Module_Record'Class;

   procedure Customize
     (Module : access Keymanager_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   procedure Destroy (Module : in out Keymanager_Module_Record);
   --  See doc for inherited subprogram

   --  ??? Global variable, could be queries from the kernel
   Keymanager_Module : Keymanager_Module_ID;

   function Process_Event
     (Handler : access Key_Manager_Record;
      Kernel  : access Kernel_Handle_Record'Class;
      Event   : Gdk_Event) return Boolean;
   --  Process the event and call the appropriate actions if needed

   procedure Free (Handler : in out Key_Manager_Record);
   --  Free the memoru occupied by the key manager

   procedure On_Accel_Map_Changed
     (Map    : access GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : Kernel_Handle);
   --  Monitor changes in the global gtk+ accelerator map. Any change in there,
   --  most notably through the dynamic key bindings feature, has impacts on
   --  the GPS shortcuts (since assigning a new accelerator to a menu should
   --  disable all actions currently associated with the same shortcut)

   procedure Bind_Default_Key_Internal
     (Kernel                               : access Kernel_Handle_Record'Class;
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

   procedure Macro_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the key manager module.

   procedure On_Edit_Keys
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open a GUI to edit the key bindings

   procedure Record_Macro (Kernel : Kernel_Handle);
   --  Start record of all events.

   procedure On_Start_Recording
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for starting record of all events for later re-play.

   procedure On_Stop_Recording
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Stop recording key events for later re-play.

   procedure On_Play_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for playing current set of events.

   procedure Play_Macro (Kernel : Kernel_Handle; Speed : Duration := 1.0);
   --  Play current set of events.

   procedure Load_Macro (File : Virtual_File; Success : out Boolean);
   --  Load macro file.

   procedure On_Load_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for loading set of events to replay.

   procedure On_Save_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Save last set of events recorded.

   function Play_Macro_Timer return Boolean;
   --  Timer used by On_Play_Macro

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class);
   procedure On_Remove_Key (Editor : access Gtk_Widget_Record'Class);
   --  Handle the "Grab", "Remove" and "Add" buttons

   function Grab_Multiple_Key
     (Widget         : access Gtk_Widget_Record'Class;
      Allow_Multiple : Boolean) return String;
   --  Grab a key binding, with support for multiple keymaps. Returns the
   --  empty string if no key could be grabbed.

   function Cancel_Grab return Boolean;
   --  Exit the current nest main loop, if any

   type Keys_Editor_Record is new Gtk_Dialog_Record with record
      Kernel             : Kernel_Handle;
      Bindings           : HTable_Access;
      View               : Gtk_Tree_View;
      Model              : Gtk_Tree_Store;
      Filter             : Gtk_Tree_Model_Filter;
      Sort               : Gtk_Tree_Model_Sort;
      Help               : Gtk_Text_Buffer;
      Action_Name        : Gtk_Label;
      With_Shortcut_Only : Gtk_Check_Button;
      Flat_List          : Gtk_Check_Button;
      Remove_Button      : Gtk_Button;
      Grab_Button        : Gtk_Button;

      Disable_Filtering  : Boolean := False;
   end record;
   type Keys_Editor is access all Keys_Editor_Record'Class;

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class);
   --  Fill the contents of the editor

   procedure Refresh_Editor (Editor : access Keys_Editor_Record'Class);
   --  Refresh the list of key bindings in editor. Better use this one than
   --  Fill_Editor when possible, since this will preserve expanded/closed
   --  nodes

   procedure Save_Editor (Editor : access Keys_Editor_Record'Class);
   --  Save the contents of the editor

   procedure On_Toggle_Flat_List (Editor : access Gtk_Widget_Record'Class);
   --  Called when the user toggles the "View Flat List" filter button

   procedure On_Toggle_Shortcuts_Only
     (Editor : access Gtk_Widget_Record'Class);
   --  Called when the user toggles "View only actions with shortcuts"

   package Keys_Editor_Visible_Funcs is new Gtk.Tree_Model_Filter.Visible_Funcs
     (Keys_Editor);
   function Action_Is_Visible
     (Model : access Gtk_Tree_Model_Record'Class;
      Iter  : Gtk_Tree_Iter;
      Data  : Keys_Editor) return Boolean;
   --  Selects whether a given row should be visible in the key shortcuts
   --  editor.

   function Set
     (Model      : Gtk_Tree_Store;
      Parent     : Gtk_Tree_Iter;
      Descr      : String;
      Key        : String := "") return Gtk_Tree_Iter;
   --  Add a new line into the model

   procedure Add_Selection_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the selection has changed

   procedure Get_Secondary_Keymap
     (Table  : in out Key_Htable.HTable;
      Key    : Gdk_Key_Type;
      Modif  : Gdk_Modifier_Type;
      Keymap : out Keymap_Access);
   --  Get or create a secondary keymap in Table.

   function Find_Parent
     (Model  : Gtk_Tree_Store;
      Action : Action_Record_Access) return Gtk_Tree_Iter;
   --  Find the parent node for Action.
   --  Create the parent node if needed

   pragma Warnings (Off);
   --  These two UCs are safe aliasing-wise, so kill warning
   function Convert is new Ada.Unchecked_Conversion
     (Kernel_Handle, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Kernel_Handle);
   pragma Warnings (On);

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure General_Event_Handler
     (Event : Gdk_Event; Kernel : System.Address);
   --  Event handler called before even gtk can do its dispatching. This
   --  intercepts all events going through the application

   procedure Keymanager_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Process shell commands associated with this module

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

   type Repeat_Next_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   function Execute
     (Command : access Repeat_Next_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   procedure On_Repeat_Next_Argument_Read
     (Command  : Interactive_Command'Class;
      Argument : String);
   --  This command reads a numeric argument, and will then execute the next
   --  action a number of times

   Action_Column     : constant := 0;
   Key_Column        : constant := 1;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Keymanager_Module_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Key_Manager_Record, Key_Manager_Access);
   begin
      Free (Module.Key_Manager.all);
      Unchecked_Free (Module.Key_Manager);
      Keymanager_Module := null;
   end Destroy;

   ---------------------------
   -- General_Event_Handler --
   ---------------------------

   procedure General_Event_Handler
     (Event : Gdk_Event; Kernel : System.Address)
   is
      Event_Type  : constant Gdk_Event_Type := Get_Event_Type (Event);
      Key_Item    : Macro_Item_Key_Access;
      Button_Item : Macro_Item_Mouse_Access;
      Motion_Item : Macro_Item_Motion_Access;
      Scroll_Item : Macro_Item_Scroll_Access;

      procedure Save_Item (Item : Macro_Item_Access);
      --  Save item in list of current events, if non null

      ---------------
      -- Save_Item --
      ---------------

      procedure Save_Item (Item : Macro_Item_Access) is
      begin
         if Item /= null then
            if Keymanager_Module.Key_Manager.Events.Events = null then
               Keymanager_Module.Key_Manager.Events.Events := Item;
               Keymanager_Module.Key_Manager.Events.Last_Event := Item;

            else
               --  Store the relative time, to ease replay.

               Item.Prev := Keymanager_Module.Key_Manager.Events.Last_Event;
               Keymanager_Module.Key_Manager.Events.Last_Event.Next := Item;
               Keymanager_Module.Key_Manager.Events.Last_Event := Item;
            end if;

            Keymanager_Module.Key_Manager.Events.Prev_Time := Get_Time (Event);
         end if;
      end Save_Item;

   begin
      if Keymanager_Module = null
        or else Keymanager_Module.Key_Manager = null
      then
         --  This can happen when GPS is exiting and modules have been
         --  deallocated already.

         Gtk.Main.Do_Event (Event);
         return;
      end if;

      --  We do not put a global exception handler in this procedure since
      --  it is called very often, so when using setjmp/longjmp, the cost
      --  may not be negligible.

      if Keymanager_Module.Key_Manager.Recording then
         begin
            case Event_Type is
               when Key_Press | Key_Release =>
                  Key_Item := Create_Item
                    (Event, Keymanager_Module.Key_Manager.Events.Prev_Time);
                  Save_Item (Macro_Item_Access (Key_Item));

                  if Process_Event
                    (Keymanager_Module.Key_Manager, Convert (Kernel), Event)
                  then
                     return;
                  end if;

               when Button_Press | Button_Release
                    | Gdk_2button_Press
                    | Gdk_3button_Press
               =>
                  Button_Item := Create_Item
                    (Event, Keymanager_Module.Key_Manager.Events.Prev_Time);
                  Save_Item (Macro_Item_Access (Button_Item));

               when Motion_Notify =>
                  Motion_Item := Create_Item
                    (Event, Keymanager_Module.Key_Manager.Events.Prev_Time);
                  Save_Item (Macro_Item_Access (Motion_Item));

               when Scroll =>
                  Scroll_Item := Create_Item
                    (Event, Keymanager_Module.Key_Manager.Events.Prev_Time);
                  Save_Item (Macro_Item_Access (Scroll_Item));

               --  Other events should not be needed: they will be generated as
               --  part the basic events handled above (keyboard/mouse events).

               when others =>
                  null;
            end case;

         exception
            when E : others =>
               Trace
                 (Exception_Handle,
                  "Unexpected exception: " & Exception_Information (E));
         end;

      elsif Event_Type = Key_Press or else Event_Type = Key_Release then
         if Keymanager_Module.Key_Manager.Events.Current_Event /= null
           and then not Get_Send_Event (Event)
           and then Get_Key_Val (Event) = GDK_Escape
         then
            Trace (Me, "Replay cancelled");
            Keymanager_Module.Key_Manager.Events.Current_Event := null;
         end if;

         if Process_Event
           (Keymanager_Module.Key_Manager, Convert (Kernel), Event)
         then
            return;
         end if;

      elsif Event_Type = Button_Release then
         --  The command will be executed by gtk, we don't know exactly how
         if Keymanager_Module.Last_Command /= null then
            Free (Keymanager_Module.Last_User_Command);
         end if;
         Keymanager_Module.Last_Command := null;
      end if;

      --  Dispatch the event in the standard gtk+ main loop
      Gtk.Main.Do_Event (Event);
   end General_Event_Handler;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Key_Binding) return Keys_Header_Num is
   begin
      return Keys_Header_Num
        ((Integer (Key.Key) + Integer (Key.Modifier) * 16#FFFF#)
          mod Integer (Keys_Header_Num'Last + 1));
   end Hash;

   ------------------------
   -- Free_Non_Recursive --
   ------------------------

   procedure Free_Non_Recursive (Element : in out Key_Description_List) is
   begin
      if Element.Action /= null then
         Free (Element.Action);
         Element.Action := null;
      end if;

      if Element.Keymap /= null then
         Reset (Element.Keymap.Table);
         Unchecked_Free (Element.Keymap);
         Element.Keymap := null;
      end if;

      Unchecked_Free (Element);
   end Free_Non_Recursive;

   ----------
   -- Free --
   ----------

   procedure Free (Element : in out Key_Description_List) is
      Current : Key_Description_List := Element;
      N       : Key_Description_List;
   begin
      while Current /= null loop
         N := Current.Next;
         Free_Non_Recursive (Current);
         Current := N;
      end loop;
   end Free;

   -----------
   -- Clone --
   -----------

   procedure Clone
     (From : Key_Description_List; To : out Key_Description_List)
   is
      Tmp    : Key_Description_List := From;
      Tmp_To : Key_Description_List;
   begin
      To := null;
      while Tmp /= null loop
         if To = null then
            Tmp_To := new Key_Description;
            To     := Tmp_To;
         else
            Tmp_To.Next := new Key_Description;
            Tmp_To      := Tmp_To.Next;
         end if;

         if Tmp.Action /= null then
            Tmp_To.Action := new String'(Tmp.Action.all);
         end if;

         if Tmp.Keymap /= null then
            Tmp_To.Keymap := new Keymap_Record;
            Clone (From => Tmp.Keymap.Table, To => Tmp_To.Keymap.Table);
         end if;

         Tmp_To.Changed := Tmp.Changed;
         Tmp := Tmp.Next;
      end loop;
   end Clone;

   -----------
   -- Clone --
   -----------

   procedure Clone
     (From : Key_Htable.HTable; To : out Key_Htable.HTable)
   is
      Iter : Key_Htable.Iterator;
      List : Key_Description_List;
   begin
      Reset (To);
      Get_First (From, Iter);
      while Get_Element (Iter) /= null loop
         Clone (From => Get_Element (Iter), To => List);
         Set (To, Get_Key (Iter), List);
         Get_Next (From, Iter);
      end loop;
   end Clone;

   -------------------------------
   -- Bind_Default_Key_Internal --
   -------------------------------

   procedure Bind_Default_Key_Internal
     (Kernel                               : access Kernel_Handle_Record'Class;
      Table                                : in out Key_Htable.HTable;
      Action                               : String;
      Key                                  : String;
      Save_In_Keys_XML                     : Boolean;
      Remove_Existing_Shortcuts_For_Action : Boolean;
      Remove_Existing_Actions_For_Shortcut : Boolean;
      Update_Menus                         : Boolean)
  is
      procedure Bind_Internal
        (Table       : in out Key_Htable.HTable;
         Default_Key : Gdk.Types.Gdk_Key_Type;
         Default_Mod : Gdk.Types.Gdk_Modifier_Type);
      --  Internal version that allows setting the Changed attribute.

      procedure Remove_In_Keymap (Table : in out Key_Htable.HTable);
      --  Remove all bindings to Action in Table and its secondary keymaps

      -------------------
      -- Bind_Internal --
      -------------------

      procedure Bind_Internal
        (Table       : in out Key_Htable.HTable;
         Default_Key : Gdk.Types.Gdk_Key_Type;
         Default_Mod : Gdk.Types.Gdk_Modifier_Type)
      is
         Tmp, Binding3, Binding2 : Key_Description_List;
         Success : Boolean;
         pragma Unreferenced (Success);
      begin
         if not Remove_Existing_Actions_For_Shortcut then
            Binding3 := Get (Table, Key_Binding'(Default_Key, Default_Mod));

            --  Check whether the same action is already attached to this key.
            --  ??? When we have a menu, we should check the underlying action
            --  if there is any.
            Tmp := Binding3;
            while Tmp /= null loop
               if Tmp.Action /= null
                 and then Equal
                   (Tmp.Action.all, Action, Case_Sensitive => False)
               then
                  return;
               end if;
               Tmp := Tmp.Next;
            end loop;

         elsif Update_Menus then
            --  Remove the gtk+ bindings
            Tmp := Get (Table, Key_Binding'(Default_Key, Default_Mod));
            while Tmp /= null loop
               if Tmp.Action /= null
                 and then Tmp.Action (Tmp.Action'First) = '/'
               then
                  Success := Change_Entry
                    ("<gps>" & Tmp.Action.all,
                     Accel_Key  => 0,
                     Accel_Mods => 0,
                     Replace    => True);
               end if;
               Tmp := Tmp.Next;
            end loop;
         end if;

         --  Put in front of the list if possible, so that when the user
         --  just added a new binding, it is visible first (in particular
         --  useful for menus).

         --  We need to clone memory associated to Binding3, since it is
         --  freed in the call to Set below.
         if Binding3 /= null then
            Clone (From => Binding3, To => Tmp);
         else
            Tmp := null;
         end if;

         Binding2 := new Key_Description'
           (Action         => new String'(Action),
            Changed        => Save_In_Keys_XML,
            Keymap         => null,
            Next           => Tmp);
         Set (Table, Key_Binding'(Default_Key, Default_Mod), Binding2);
      end Bind_Internal;

      ----------------------
      -- Remove_In_Keymap --
      ----------------------

      procedure Remove_In_Keymap
        (Table : in out Key_Htable.HTable)
      is
         Iter : Key_Htable.Iterator;
         List, Previous, Tmp : Key_Description_List;
      begin
         Get_First (Table, Iter);
         while Get_Element (Iter) /= null loop
            List := Get_Element (Iter);

            Previous := null;
            while List /= null loop
               if List.Keymap /= null then
                  Remove_In_Keymap (List.Keymap.Table);
                  Previous := List;
                  List := List.Next;

               elsif List.Action /= null
                 and then Equal (List.Action.all, Action,
                                 Case_Sensitive => False)
               then
                  if Previous = null then
                     if List.Next /= null then
                        --  Remove list from the list of keybindings, without
                        --  modifying the htable itself to avoid invalidating
                        --  the iterator
                        Tmp := List.Next;
                        Free (List.Action);
                        List.all := Tmp.all;
                        Unchecked_Free (Tmp);
                     else
                        --  There was a single element with this key binding:
                        Free (List.Action);
                        List := List.Next;
                     end if;

                  else
                     Previous.Next := List.Next;
                     Free_Non_Recursive (List);
                     List := Previous.Next;
                  end if;

               else
                  Previous := List;
                  List := List.Next;
               end if;
            end loop;

            Get_Next (Table, Iter);
         end loop;
      end Remove_In_Keymap;

      Partial_Key : Gdk_Key_Type;
      Modif, Mnemonic_Modif : Gdk_Modifier_Type;
      First, Last : Integer;
      Keymap  : Keymap_Access;
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      --  Are we trying to cancel all bindings to Action ?
      if Remove_Existing_Shortcuts_For_Action
        or else Key = ""
      then
         Remove_In_Keymap (Table);

         if Update_Menus and then Action (Action'First) = '/' then
            --  Guess the accel path from the menu
            Success := Change_Entry
              ("<gps>" & Action,
               Accel_Key  => 0,
               Accel_Mods => 0,
               Replace    => True);
         end if;

      end if;

      if Key = "" or else Key = -Disabled_String then
         --  Bind to an invalid key, so that when saving we know this should be
         --  removed
         Bind_Internal (Table, 0, 0);
         return;
      end if;

      First := Key'First;
      while First <= Key'Last loop
         Last := First + 1;
         while Last <= Key'Last and then Key (Last) /= ' ' loop
            Last := Last + 1;
         end loop;

         Value (Key (First .. Last - 1), Partial_Key, Modif);

         if Last > Key'Last then
            if Action (Action'First) = '/' then
               --  For a menu, ensure the accel map entry exists. If we don't
               --  do that, the following scenario will fail:
               --     - The emacs mode registers a binding for a menu that
               --       is defined in a not yet loaded python package
               --     - The other python package is loaded. It creates the menu
               --       which create the accel_path for it. As a result, a
               --       "changed" signal is propagated for the accel_map, and
               --       the callback On_Accel_Map_Changed will delete any
               --       binding associated with it (F720-010)
               Add_Entry ("<gps>" & Action, 0, 0);
            end if;

            if Keymap = null then
               Bind_Internal (Table, Partial_Key, Modif);

               if Update_Menus and then Action (Action'First) = '/' then
                  --  Guess the accel path from the menu. This operation might
                  --  fail if the shortcut is already used as a mnemonic for a
                  --  menu, so we temporarily change the modifier for mnemonics
                  --  All menus are assumed to be in the main GPS window at
                  --  this stage.

                  Mnemonic_Modif := Get_Mnemonic_Modifier
                    (Get_Main_Window (Kernel));

                  if Modif = Mnemonic_Modif then
                     Set_Mnemonic_Modifier
                       (Get_Main_Window (Kernel),
                        Modif or Control_Mask or Mod1_Mask or Shift_Mask);
                  end if;

                  if not Change_Entry
                    ("<gps>" & Action,
                     Accel_Key  => Partial_Key,
                     Accel_Mods => Modif,
                     Replace    => True)
                  then
                     --  If we still couldn't change it, at very least disable
                     --  it from the menu so as not to confuse users
                     Success := Change_Entry
                       ("<gps>" & Action,
                        Accel_Key  => 0,
                        Accel_Mods => 0,
                        Replace    => True);
                  end if;

                  if Modif = Mnemonic_Modif then
                     Set_Mnemonic_Modifier
                       (Get_Main_Window (Kernel), Mnemonic_Modif);
                  end if;
               end if;

            else
               Bind_Internal (Keymap.Table, Partial_Key, Modif);
            end if;

         else
            if Keymap = null then
               Get_Secondary_Keymap (Table, Partial_Key, Modif, Keymap);
            else
               Get_Secondary_Keymap (Keymap.Table, Partial_Key, Modif, Keymap);
            end if;
         end if;

         First := Last + 1;
      end loop;
   end Bind_Default_Key_Internal;

   --------------------------
   -- Get_Secondary_Keymap --
   --------------------------

   procedure Get_Secondary_Keymap
     (Table  : in out Key_Htable.HTable;
      Key    : Gdk_Key_Type;
      Modif  : Gdk_Modifier_Type;
      Keymap : out Keymap_Access)
   is
      Binding  : Key_Description_List := Get (Table, (Key, Modif));
      Binding2 : Key_Description_List;
   begin
      if Binding = null then
         Keymap := new Keymap_Record;
         Binding := new Key_Description'
           (Action  => null,
            Changed => False,
            Keymap  => Keymap,
            Next    => null);
         Set (Table, (Key, Modif), Binding);

      else
         Binding2 := Binding;
         while Binding2 /= null
           and then Binding2.Keymap = null
         loop
            Binding  := Binding2;  --  Last value where Next /= null
            Binding2 := Binding2.Next;
         end loop;

         --  If there is no secondary keymap yet, create one
         if Binding2 = null then
            Keymap   := new Keymap_Record;
            Binding.Next := new Key_Description'
              (Action  => null,
               Changed => False,
               Keymap  => Keymap,
               Next    => null);
         else
            Keymap := Binding2.Keymap;
         end if;
      end if;
   end Get_Secondary_Keymap;

   -------------------
   -- Process_Event --
   -------------------

   function Process_Event
     (Handler  : access Key_Manager_Record;
      Kernel   : access Kernel_Handle_Record'Class;
      Event    : Gdk.Event.Gdk_Event) return Boolean
   is
      Key     : Gdk_Key_Type;
      Modif   : Gdk_Modifier_Type;
      Binding : Key_Description_List;
      Command : Action_Record_Access;
      Has_Secondary : constant Boolean := Handler.Secondary_Keymap /= null;
      Context : Selection_Context;
      Context_Computed : Boolean := False;
      Found_Action : Boolean := False;
      Child    : GPS_MDI_Child;

      procedure Compute_Context;
      --  Compute the current context if not done already

      procedure Compute_Child;
      --  Compute the child that currently has the focus. If no such child, or
      --  this isn't a GPS_MDI_Child, null is set

      procedure Undo_Group (Start : Boolean);
      --  Start or end an undo group

      procedure Compute_Context is
      begin
         if not Context_Computed then
            Context := Get_Current_Context (Kernel);
            Context_Computed := True;
         end if;
      end Compute_Context;

      procedure Compute_Child is
         C : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
      begin
         if C /= null and then C.all in GPS_MDI_Child_Record'Class then
            Child := GPS_MDI_Child (C);
         end if;
      end Compute_Child;

      procedure Undo_Group (Start : Boolean) is
      begin
         if Start then
            if Keymanager_Module.Repeat_Count >= 2 then
               Compute_Child;
               if Child /= null then
                  Start_Group (Get_Command_Queue (Child));
               end if;
            end if;
         elsif Child /= null then
            End_Group (Get_Command_Queue (Child));
         end if;
      end Undo_Group;

   begin
      --  We could test Modif /= 0 if we allowed only key shortcuts with a
      --  modifier (control, alt, ...). However, this would prevent assigning
      --  key shortcuts to F1, F2, Home, PageUp,.. so is not desirable.

      if Handler.Active
        and then Get_Event_Type (Event) = Key_Press
      then
         --  Remove any num-lock and caps-lock modifiers.
         Modif := Get_State (Event) and Get_Default_Mod_Mask;
         Key   := Get_Key_Val (Event);

         --  Are we reading arguments for a command ?

         if Keymanager_Module.Argument_Validator /= null then
            if (Modif /= 0 or else Key /= GDK_Escape)
              and then Keymanager_Module.Argument_Validator (Key, Modif)
            then
               declare
                  Tmp : String_Access := Keymanager_Module.Argument_Current;
               begin
                  Keymanager_Module.Argument_Current :=
                    new String'(Tmp.all & Get_String (Event));
                  Free (Tmp);
               end;

               --  No longer process the current key
               return True;
            else
               Trace (Me, "Finished reading argument: "
                        & Keymanager_Module.Argument_Current.all);
               Keymanager_Module.Argument_Callback
                 (Keymanager_Module.Argument_Data.all,
                  Keymanager_Module.Argument_Current.all);
               Keymanager_Module.Argument_Validator := null;
               Free (Keymanager_Module.Argument_Current);
               --  Process the current key as usual
            end if;
         end if;

         --  Ignore when the key is just one of the modifier. No binding can
         --  be associated to them anyway, so this is slightly more efficient,
         --  and this also avoids resetting the last command.
         if Key >= GDK_Shift_L
           and then Key <= GDK_Hyper_R
         then
            return False;
         end if;

         if Handler.Secondary_Keymap = null then
            Binding := Get (Handler.Table.all, (Key, Modif));
         else
            Binding := Get (Handler.Secondary_Keymap.Table, (Key, Modif));
         end if;

         --  Ignore shift modifiers as well. Don't do it systematically to
         --  preserve backward compatibility. This way, using
         --  alt-shift-greater or alt-greater results in the same.

         if Binding = No_Key then
            Modif := Modif and not Shift_Mask;
            if Handler.Secondary_Keymap = null then
               Binding := Get (Handler.Table.all, (Key, Modif));
            else
               Binding := Get (Handler.Secondary_Keymap.Table, (Key, Modif));
            end if;
         end if;

         Handler.Secondary_Keymap := null;

         --  First try to activate the key shortcut using the standard
         --  Gtk+ mechanism.
         --  Do this lookup only if we are not currently processing a
         --  secondary key.

         if not Has_Secondary
           and then Accel_Groups_Activate
             (Get_Main_Window (Kernel), Key, Modif)
         then
            Undo_Group (Start => True);
            for R in 2 .. Keymanager_Module.Repeat_Count loop
               Found_Action := Accel_Groups_Activate
                 (Get_Main_Window (Kernel), Key, Modif);
            end loop;
            Undo_Group (Start => False);

            Found_Action := True;

            --  The command will be executed by gtk, we don't know exactly how
            if Keymanager_Module.Last_Command /= null then
               Free (Keymanager_Module.Last_User_Command);
            end if;
            Keymanager_Module.Last_Command := null;
            Keymanager_Module.Repeat_Count := 1;

         else
            --  Execute all commands bound to this key. The order is somewhat
            --  random, since it depends in what order the key shortcuts were
            --  defined.
            while Binding /= No_Key loop
               if Binding.Action = null then
                  Handler.Secondary_Keymap := Binding.Keymap;
                  Found_Action := True;

               else
                  --  If we have not found the accelerator using the Gtk+
                  --  mechanism, fallback on the standard mechanism to lookup
                  --  the action.
                  Command := Lookup_Action (Kernel, Binding.Action.all);

                  if Command = null then
                     Insert
                       (Kernel, -"Action not defined: " & Binding.Action.all);

                  elsif Command.Command /= null then
                     Compute_Context;

                     if Command.Filter = null
                       or else
                         (Context /= No_Context
                          and then Filter_Matches (Command.Filter, Context))
                     then
                        Trace (Me, "Executing action " & Binding.Action.all
                               & Keymanager_Module.Repeat_Count'Img
                               & " times");

                        if Keymanager_Module.Last_Command /=
                          Cst_String_Access (Binding.Action)
                        then
                           Free (Keymanager_Module.Last_User_Command);
                        end if;
                        Keymanager_Module.Last_Command :=
                          Cst_String_Access (Binding.Action);

                        Undo_Group (Start => True);
                        for R in 1 .. Keymanager_Module.Repeat_Count loop
                           Launch_Background_Command
                             (Kernel,
                              Create_Proxy
                                (Command.Command,
                                 (Event       => Event,
                                  Context     => Context,
                                  Synchronous => False,
                                  Dir     => null,
                                  Args    => null,
                                  Label   => new String'(Binding.Action.all),
                                  Repeat_Count => R,
                                  Remaining_Repeat =>
                                    Keymanager_Module.Repeat_Count - R)),
                              Destroy_On_Exit => False,
                              Active          => True,
                              Show_Bar        => False,
                              Queue_Id        => "");
                        end loop;
                        Undo_Group (Start => False);

                        Found_Action := True;
                        Keymanager_Module.Repeat_Count := 1;
                     end if;
                  end if;
               end if;

               Binding := Binding.Next;
            end loop;
         end if;

         if not Found_Action then
            --  The command will be executed by gtk, we don't know exactly how
            if Keymanager_Module.Last_Command /= null then
               Free (Keymanager_Module.Last_User_Command);
            end if;
            Keymanager_Module.Last_Command := null;

            --  To repeat this one, we need to requeue the event...
            Undo_Group (Start => True);
            for R in 2 .. Keymanager_Module.Repeat_Count loop
               declare
                  Ev : Gdk_Event;
               begin
                  Deep_Copy (From => Event, To => Ev);
                  Put (Ev);
               end;
            end loop;
            Undo_Group (Start => False);
            Keymanager_Module.Repeat_Count := 1;
         end if;

      end if;

      --  Let gtk+ handle all events even if we have already processed one or
      --  more actions, since we want to execute everything related to that
      --  key. However, do not let events through if they are in a secondary
      --  keymap, since the shortcuts is too complex for gtk+ anyway.

      --  ??? On the other hand, if we let it through this means that for
      --  instance alt-w will open the Window menu, even after some action has
      --  been executed.
      return Found_Action or Has_Secondary;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Process_Event;

   ----------
   -- Free --
   ----------

   procedure Free (Handler : in out Key_Manager_Record) is
      Filename : constant String := Get_Home_Dir (Handler.Kernel) & "keys.xml";
      File : Node_Ptr;

      procedure Save_Table (Table : in out Key_Htable.HTable; Prefix : String);
      --  Save the contents of a specific keymap

      procedure Save_Table
        (Table : in out Key_Htable.HTable; Prefix : String)
      is
         Child   : Node_Ptr;
         Iter    : Key_Htable.Iterator;
         Binding : Key_Description_List;
      begin
         Get_First (Table, Iter);
         loop
            Binding := Get_Element (Iter);
            exit when Binding = No_Key;

            while Binding /= null loop
               if Binding.Changed
                 and then Binding.Action /= null
               then
                  Child := new Node;
                  Child.Tag := new String'("key");
                  Set_Attribute (Child, "action", Binding.Action.all);

                  --  Key will be 0 if we have voluntarily saved an invalid
                  --  binding to indicate the binding should be disabled on the
                  --  next startup.
                  if Get_Key (Iter).Key /= 0 then
                     Child.Value := new String'
                       (Prefix
                        & Image (Get_Key (Iter).Key, Get_Key (Iter).Modifier));
                  end if;

                  Add_Child (File, Child);

               elsif Binding.Action = null then
                  if Binding.Keymap /= null then
                     Save_Table (Binding.Keymap.Table,
                       Prefix
                       & Image (Get_Key (Iter).Key,
                         Get_Key (Iter).Modifier)
                       & ' ');
                  end if;
               end if;

               Binding := Binding.Next;
            end loop;

            Get_Next (Table, Iter);
         end loop;
      end Save_Table;

   begin
      File     := new Node;
      File.Tag := new String'("Keys");

      Save_Table (Handler.Table.all, "");

      Trace (Me, "Saving " & Filename);
      Print (File, Filename);
      Free (File);

      Reset (Handler.Table.all);
      Unchecked_Free (Handler.Table);
   end Free;

   ----------------------
   -- Load_Custom_Keys --
   ----------------------

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class)
   is
      Filename : constant String := Get_Home_Dir (Kernel) & "keys.xml";
      File, Child : Node_Ptr;
      Err : String_Access;
      Prev : Boolean;
   begin
      Keymanager_Module.Key_Manager.Custom_Keys_Loaded := True;

      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);
         else
            Child := File.Child;

            Prev := Keymanager_Module.Menus_Created;
            Keymanager_Module.Menus_Created := True;

            Handler_Block (Gtk.Accel_Map.Get, Keymanager_Module.Accel_Map_Id);

            while Child /= null loop
               --  Remove all other bindings previously defined, so that only
               --  the last definition is taken into account
               Bind_Default_Key_Internal
                 (Kernel           => Kernel,
                  Table            => Keymanager_Module.Key_Manager.Table.all,
                  Action           => Get_Attribute (Child, "action"),
                  Key              => Child.Value.all,
                  Save_In_Keys_XML => True,
                  Remove_Existing_Shortcuts_For_Action => True,
                  Remove_Existing_Actions_For_Shortcut => True,
                  Update_Menus     => True);
               Child := Child.Next;
            end loop;

            Handler_Unblock
              (Gtk.Accel_Map.Get, Keymanager_Module.Accel_Map_Id);
            Keymanager_Module.Menus_Created := Prev;

            Free (File);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Insert (Kernel, -"Could not parse " & Filename, Mode => Error);
   end Load_Custom_Keys;

   ---------------------------
   -- Macro_Command_Handler --
   ---------------------------

   procedure Macro_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = "macro_load" then
         Name_Parameters (Data, Load_Macro_Cmd_Parameters);

         declare
            File    : constant String := Nth_Arg (Data, 1);
            Success : Boolean;
            Macro   : constant String := '/' & (-"Tools/Macro") & '/';

         begin
            Load_Macro (Create (File), Success);

            if Success then
               Set_Sensitive
                 (Find_Menu_Item (Get_Kernel (Data), Macro & (-"Play")), True);
            else
               Set_Error_Msg
                 (Data, Command & ": " & (-"error while reading file"));
            end if;
         end;

      elsif Command = "macro_play" then
         Name_Parameters (Data, Play_Macro_Cmd_Parameters);

         declare
            Speed : constant String := Nth_Arg (Data, 1, Default => "1.0");
         begin
            Play_Macro (Get_Kernel (Data), Duration'Value (Speed));
         exception
            when Constraint_Error =>
               Set_Error_Msg (Data, Command & ": " & (-"invalid speed value"));
         end;

      elsif Command = "macro_record" then
         Record_Macro (Get_Kernel (Data));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Macro_Command_Handler;

   -----------------
   -- Find_Parent --
   -----------------

   function Find_Parent
     (Model  : Gtk_Tree_Store;
      Action : Action_Record_Access) return Gtk_Tree_Iter
   is
      Parent : Gtk_Tree_Iter;
   begin
      if Action = null or else Action.Category = null then
         return Null_Iter;

      else
         Parent := Find_Node (Model, Action.Category.all, Action_Column);
         if Parent = Null_Iter then
            Parent := Set (Model, Null_Iter,
                           Descr => Action.Category.all);
         end if;
      end if;

      return Parent;
   end Find_Parent;

   ---------
   -- Set --
   ---------

   function Set
     (Model      : Gtk_Tree_Store;
      Parent     : Gtk_Tree_Iter;
      Descr      : String;
      Key        : String := "") return Gtk_Tree_Iter
   is
      procedure Set
        (Tree, Iter : System.Address;
         Col1       : Gint; Value1 : String;
         Col2       : Gint; Value2 : String);
      pragma Import (C, Set, "ada_gtk_tree_store_set_ptr_ptr");

      Iter : Gtk_Tree_Iter;

   begin
      Append (Model, Iter, Parent);
      Set
        (Get_Object (Model), Iter'Address,
         Col1 => Action_Column,     Value1 => Descr & ASCII.NUL,
         Col2 => Key_Column,        Value2 => Key & ASCII.NUL);
      return Iter;
   end Set;

   ----------------------------
   -- Lookup_Key_From_Action --
   ----------------------------

   function Lookup_Key_From_Action
     (Table             : HTable_Access;
      Action            : String;
      Default           : String := "none";
      Use_Markup        : Boolean := True;
      Return_Multiple   : Boolean := True;
      Default_On_Gtk    : Boolean := True;
      Is_User_Changed   : access Boolean) return String
   is
      use Ada.Strings.Unbounded;
      Result : Ada.Strings.Unbounded.Unbounded_String;

      procedure Process_Table (Table : Key_Htable.HTable; Prefix : String);
      --  Process a specific binding table

      -------------------
      -- Process_Table --
      -------------------

      procedure Process_Table (Table : Key_Htable.HTable; Prefix : String) is
         Iter    : Key_Htable.Iterator;
         Binding : Key_Description_List;
      begin
         Get_First (Table, Iter);
         loop
            Binding := Get_Element (Iter);
            exit when Binding = No_Key;

            --  If we have voluntarily assigned an invalid binding to indicate
            --  a key should be removed, ignore this here as well.
            if Get_Key (Iter).Key = 0 then
               Binding := null;
            end if;

            while Binding /= null loop
               if Binding.Action = null then
                  if Binding.Keymap /= null then
                     Process_Table
                       (Binding.Keymap.Table,
                        Prefix
                        & Image (Get_Key (Iter).Key, Get_Key (Iter).Modifier)
                        & ' ');
                  end if;

               elsif Equal (Binding.Action.all, Action,
                            Case_Sensitive => False)
                 and then Get_Key (Iter).Key /= 0
               then
                  if Return_Multiple
                    or else Result = Null_Unbounded_String
                  then
                     if Result /= Null_Unbounded_String then
                        if Use_Markup then
                           Result := Result & " <b>or</b> ";
                        else
                           Result := Result & " or ";
                        end if;
                     end if;

                     Is_User_Changed.all :=
                       Is_User_Changed.all or Binding.Changed;
                     Result := Result
                       & Prefix
                       & Image (Get_Key (Iter).Key, Get_Key (Iter).Modifier);

                  elsif not Return_Multiple
                    and then Prefix = ""
                  then
                     --  When returning a single key binding, give priority to
                     --  the ones with a single key, so that we can display
                     --  them in menu shortcuts
                     Is_User_Changed.all :=
                       Is_User_Changed.all or Binding.Changed;
                     Result := To_Unbounded_String
                       (Image (Get_Key (Iter).Key, Get_Key (Iter).Modifier));
                  end if;
               end if;

               Binding := Binding.Next;
            end loop;

            Get_Next (Table, Iter);
         end loop;
      end Process_Table;

      Key   : Gtk_Accel_Key;
      Found : Boolean;
   begin
      Is_User_Changed.all := False;

      --  The table also includes the menu accelerators set by gtk+, so by
      --  traversing the table we get access to everything.
      --  ??? This is not true for stock accelerators.
      Process_Table (Table.all, "");

      --  If we haven't found an action, fallback on the default gtk+
      --  mechanism.

      if Default_On_Gtk
        and then Action (Action'First) = '/'
        and then Result = Null_Unbounded_String
      then
         Lookup_Entry ("<gps>" & Action, Key, Found);

         if Found then
            declare
               Bind : Key_Description_List;
            begin
               Bind := new Key_Description'
                 (Action         => new String'(Action),
                  Changed        => False,
                  Keymap         => null,
                  Next           => null);

               Set
                 (Table.all,
                  Key_Binding'(Key.Accel_Key, Key.Accel_Mods), Bind);
            end;

            Result := Result & Image (Key.Accel_Key, Key.Accel_Mods);
         end if;
      end if;

      if Result = Null_Unbounded_String then
         return Default;
      else
         return To_String (Result);
      end if;
   end Lookup_Key_From_Action;

   --------------------
   -- Refresh_Editor --
   --------------------

   procedure Refresh_Editor (Editor : access Keys_Editor_Record'Class) is
      procedure Refresh_Iter (Iter : Gtk_Tree_Iter);
      --  Refresh for Iter and its sibling

      procedure Refresh_Iter (Iter : Gtk_Tree_Iter) is
         It : Gtk_Tree_Iter;
         User_Changed : aliased Boolean;
      begin
         Iter_Copy (Source => Iter, Dest => It);
         while It /= Null_Iter loop
            if Children (Editor.Model, It) /= Null_Iter then
               Refresh_Iter (Children (Editor.Model, It));
            else
               Set
                 (Editor.Model, It, Key_Column,
                  Lookup_Key_From_Action
                    (Editor.Bindings,
                     Action => Get_String (Editor.Model, It, Action_Column),
                     Default => "",
                     Default_On_Gtk => False,
                     Is_User_Changed => User_Changed'Unchecked_Access));
            end if;

            Next (Editor.Model, It);
         end loop;
      end Refresh_Iter;

   begin
      Refresh_Iter (Get_Iter_First (Editor.Model));
   end Refresh_Editor;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class) is
      Menu_Iter : Gtk_Tree_Iter := Null_Iter;
      Flat_List : constant Boolean := Get_Active (Editor.Flat_List);

      procedure Process_Menu_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean);
      --  Called for each known accel path.

      --------------------------
      -- Process_Menu_Binding --
      --------------------------

      procedure Process_Menu_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean)
      is
         Iter : Gtk_Tree_Iter;
         pragma Unreferenced (Data, Changed, Iter, Accel_Key, Accel_Mods);
         First : Natural := Accel_Path'First + 1;
         User_Changed : aliased Boolean;
      begin
         while First <= Accel_Path'Last
           and then Accel_Path (First - 1) /= '>'
         loop
            First := First + 1;
         end loop;

         if Accel_Path (First) = '/'
         --  Only add menu accelerators through this mechanism.
         --  Actions are handled by a separate loop, after the call to
         --  Foreach_Unfiltered.
           and then Accel_Path (First .. Accel_Path'Last) /= ""
         then
            Iter := Set
              (Model      => Editor.Model,
               Parent     => Menu_Iter,
               Descr      => Accel_Path (First .. Accel_Path'Last),
               Key        => Lookup_Key_From_Action
                 (Editor.Bindings,
                  Action            => Accel_Path (First .. Accel_Path'Last),
                  Is_User_Changed   => User_Changed'Unchecked_Access,
                  Default           => "",
                  Default_On_Gtk    => True));
         end if;
      end Process_Menu_Binding;

      Parent      : Gtk_Tree_Iter;
      Action      : Action_Record_Access;
      Action_Iter : Action_Iterator := Start (Editor.Kernel);
      User_Changed : aliased Boolean;
   begin
      --  Disable tree filtering while refreshing the contents of the tree.
      --  This works around a bug in gtk+.
      Editor.Disable_Filtering := True;

      Clear (Editor.Model);

      if not Flat_List then
         Menu_Iter := Set (Editor.Model, Null_Iter, -Menu_Context_Name);
      end if;

      Gtk.Accel_Map.Foreach_Unfiltered
        (System.Null_Address, Process_Menu_Binding'Unrestricted_Access);

      --  Add all known actions in the table. This doesn't include menus
      --  in general
      loop
         Action := Get (Action_Iter);
         exit when Action = null;

         if not Flat_List then
            declare
               Title : constant String := Get (Action_Iter);
            begin
               if Title (Title'First) = '/' then
                  Parent := Menu_Iter;
               else
                  Parent := Find_Parent (Editor.Model, Action);
               end if;
            end;
         else
            Parent := Null_Iter;
         end if;

         if Action.Category /= null
           and then (Flat_List or else Parent /= Null_Iter)
         then
            declare
               Name : String := Get (Action_Iter);
            begin
               Mixed_Case (Name);
               Parent := Set
                 (Model   => Editor.Model,
                  Parent  => Parent,
                  Descr   => Name,
                  Key     => Lookup_Key_From_Action
                    (Editor.Bindings,
                     Name,
                     Is_User_Changed => User_Changed'Unchecked_Access,
                     Default => -Disabled_String));
            end;
         end if;

         Next (Editor.Kernel, Action_Iter);
      end loop;

      Editor.Disable_Filtering := False;

      Refilter (Editor.Filter);
   end Fill_Editor;

   -----------------
   -- Save_Editor --
   -----------------

   procedure Save_Editor (Editor : access Keys_Editor_Record'Class) is
      Handler   : constant Key_Manager_Access := Keymanager_Module.Key_Manager;

      procedure Process_Menu_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean);
      --  Called for each known accel path.

      --------------------------
      -- Process_Menu_Binding --
      --------------------------

      procedure Process_Menu_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean)
      is
         First   : Natural := Accel_Path'First + 1;
         Iter    : Key_Htable.Iterator;
         Binding : Key_Description_List;
         Found   : Boolean := False;
         Success : Boolean;
         pragma Unreferenced (Data, Changed, Accel_Key, Accel_Mods, Success);

      begin
         while First <= Accel_Path'Last
           and then Accel_Path (First - 1) /= '>'
         loop
            First := First + 1;
         end loop;

         --  If the menu is associated with at least one short key binding (ie
         --  from the toplevel keymap), we change it so that it shows up in the
         --  menu as well).
         Get_First (Handler.Table.all, Iter);
         Foreach_Binding :
         loop
            Binding := Get_Element (Iter);
            exit Foreach_Binding when Binding = No_Key;

            if Get_Key (Iter).Key = 0 then
               --  An invalid key, here just to indicate the key should be
               --  disabled during the next startup.
               Binding := null;
            end if;

            while Binding /= null loop
               if Binding.Action /= null
                 and then Equal
                   (Binding.Action.all,
                    Accel_Path (First .. Accel_Path'Last),
                    Case_Sensitive => False)
               then
                  Found := True;

                  --  The following call will fail in general, since the
                  --  shortcut is already associated with the same Accel_Path.
                  --  Unfortunately, gtk+ doesn't detect that we are just
                  --  trying to set the same binding again, and will always
                  --  report a failure. We should not therefore fallback on
                  --  clearing the binding in case of failure. F721-013
                  Success := Change_Entry
                    (Accel_Path => Accel_Path,
                     Accel_Key  => Get_Key (Iter).Key,
                     Accel_Mods => Get_Key (Iter).Modifier,
                     Replace    => True);
                  exit Foreach_Binding;
               end if;

               Binding := Binding.Next;
            end loop;

            Get_Next (Handler.Table.all, Iter);
         end loop Foreach_Binding;

         if not Found then
            Success := Change_Entry
              (Accel_Path => Accel_Path,
               Accel_Key  => 0,
               Accel_Mods => 0,
               Replace    => True);
         end if;
      end Process_Menu_Binding;

   begin
      Clone (From => Editor.Bindings.all, To => Handler.Table.all);

      --  Update the gtk+ accelerators for the menus to reflect the keybindings
      Handler_Block (Gtk.Accel_Map.Get, Keymanager_Module.Accel_Map_Id);
      Gtk.Accel_Map.Foreach_Unfiltered
        (System.Null_Address, Process_Menu_Binding'Unrestricted_Access);
      Handler_Unblock (Gtk.Accel_Map.Get, Keymanager_Module.Accel_Map_Id);
   end Save_Editor;

   -----------------
   -- Cancel_Grab --
   -----------------

   function Cancel_Grab return Boolean is
   begin
      --  If there is a grab pending

      if Main_Level > 1 then
         Main_Quit;
      end if;

      return False;
   end Cancel_Grab;

   -----------------------
   -- Grab_Multiple_Key --
   -----------------------

   function Grab_Multiple_Key
     (Widget : access Gtk_Widget_Record'Class;
      Allow_Multiple : Boolean)
      return String
   is
      Grabbed, Tmp : String_Access;
      Key   : Gdk_Key_Type;
      Modif : Gdk_Modifier_Type;
      Id    : Timeout_Handler_Id;

   begin
      Keymanager_Module.Key_Manager.Active := False;

      Key_Grab (Widget, Key, Modif);

      if Key /= GDK_Escape or else Modif /= 0 then
         Grabbed := new String'(Image (Key, Modif));
      else
         return "";
      end if;

      --  Are we grabbing multiple keymaps ?

      if Allow_Multiple then
         loop
            Id := Timeout_Add (500, Cancel_Grab'Access);
            Key_Grab (Widget, Key, Modif);
            Timeout_Remove (Id);

            exit when Key = 0 and then Modif = 0;

            if Key = GDK_Escape and then Modif = 0 then
               Free (Grabbed);
               return "";
            end if;

            Tmp := Grabbed;
            Grabbed := new String'(Grabbed.all & ' ' & Image (Key, Modif));
            Free (Tmp);
         end loop;
      end if;

      Keymanager_Module.Key_Manager.Active := True;

      declare
         K : constant String := Grabbed.all;
      begin
         Free (Grabbed);
         return K;
      end;

   exception
      when others =>
         Keymanager_Module.Key_Manager.Active := True;
         raise;
   end Grab_Multiple_Key;

   -----------------
   -- On_Grab_Key --
   -----------------

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Keys_Editor := Keys_Editor (Editor);
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Sort_Model : Gtk_Tree_Model;
      Sort_Iter, Filter_Iter, Iter : Gtk_Tree_Iter;
--        Old_Action : Action_Record_Access;
   begin
      Get_Selected (Selection, Sort_Model, Sort_Iter);
      Convert_Iter_To_Child_Iter (Ed.Sort, Filter_Iter, Sort_Iter);
      Convert_Iter_To_Child_Iter (Ed.Filter, Iter, Filter_Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)

      if Iter /= Null_Iter
        and then Children (Ed.Model, Iter) = Null_Iter
      then
         declare
            Key     : constant String := Grab_Multiple_Key
              (Ed.View, Allow_Multiple => True);
         begin
            if Key /= "" then
               Bind_Default_Key_Internal
                 (Kernel         => Ed.Kernel,
                  Table          => Ed.Bindings.all,
                  Action         => Get_String (Ed.Model, Iter, Action_Column),
                  Key              => Key,
                  Save_In_Keys_XML => True,
                  Remove_Existing_Actions_For_Shortcut => True,
                  Remove_Existing_Shortcuts_For_Action => True,
                  Update_Menus     => False);
               Refresh_Editor (Ed);

               --  ??? Waiting for F613-014
               --  Do we already have an action with such a binding ?
--                 Old_Action := Lookup_Action_From_Key (Key);
--                 if Old_Action /= null then
--                    if Message_Dialog
--                   (Msg => -"An action is already attached to this shortcut:"
--                       & ASCII.LF
--                       & Old_Action.Name.all & ASCII.LF
--                       & (-"Do you want to override it ?"),
--                       Dialog_Type => Confirmation,
--                       Buttons => Button_OK or Button_Cancel,
--                       Title   => -"Key shortcuts already exists",
--                       Parent  => Get_Window (Ed.Kernel)) = Button_OK
--                    then
--                       Old_Action := null;
--                    end if;
--                 end if;

--                 if Old_Action = null then
--                    Trace (Me, "Binding changed to " & Key);
--                    Set (Ed.Model, Iter, Key_Column, Key);
--                    Set (Ed.Model, Iter, Changed_Column, True);
--                 end if;
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Grab_Key;

   -------------------
   -- On_Remove_Key --
   -------------------

   procedure On_Remove_Key (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Keys_Editor := Keys_Editor (Editor);
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Sort_Model     : Gtk_Tree_Model;
      Iter, Filter_Iter, Sort_Iter  : Gtk_Tree_Iter;
   begin
      Get_Selected (Selection, Sort_Model, Sort_Iter);
      Convert_Iter_To_Child_Iter (Ed.Sort, Filter_Iter, Sort_Iter);
      Convert_Iter_To_Child_Iter (Ed.Filter, Iter, Filter_Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)

      if Iter /= Null_Iter
        and then Children (Ed.Model, Iter) = Null_Iter
      then
         Bind_Default_Key_Internal
           (Table             => Ed.Bindings.all,
            Kernel            => Ed.Kernel,
            Action            => Get_String (Ed.Model, Iter, Action_Column),
            Key               => "",
            Save_In_Keys_XML  => True,
            Remove_Existing_Shortcuts_For_Action => True,
            Remove_Existing_Actions_For_Shortcut => True,
            Update_Menus      => False);

         Refresh_Editor (Ed);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Remove_Key;

   ---------------------------
   -- Add_Selection_Changed --
   ---------------------------

   procedure Add_Selection_Changed (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Keys_Editor := Keys_Editor (Editor);
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Text_Iter : Gtk_Text_Iter;
      Action    : Action_Record_Access;
      Comp_Iter : Component_Iterator;
      Bold      : Gtk_Text_Tag;

      procedure Insert_Details
        (Comp_Iter : in out Component_Iterator; Prefix : String);
      --  Insert the detail for the components of the action

      --------------------
      -- Insert_Details --
      --------------------

      procedure Insert_Details
        (Comp_Iter : in out Component_Iterator; Prefix : String)
      is
         Comp    : Command_Component;
         Failure : Component_Iterator;
      begin
         loop
            Comp := Get (Comp_Iter);
            exit when Comp = null;

            Insert
              (Ed.Help, Text_Iter, Prefix & Get_Name (Comp) & ASCII.LF);

            Failure := On_Failure (Comp_Iter);
            if Failure /= null then
               Insert (Ed.Help, Text_Iter, Prefix & "on-failure:" & ASCII.LF);
               Insert_Details (Failure, Prefix & "   ");
            end if;

            Next (Comp_Iter);
         end loop;
      end Insert_Details;

      User_Changed : aliased Boolean;
   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)
      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Set_Sensitive (Ed.Remove_Button, True);
         Set_Sensitive (Ed.Grab_Button, True);

         Action := Lookup_Action (Ed.Kernel, Get_String (Model, Iter, 0));

         if Action /= null and then Action.Description /= null then
            Set_Text (Ed.Help, Action.Description.all);
         else
            Set_Text (Ed.Help, "");
         end if;

         --  Action could be null if we chose to display only lines with
         --  shortcuts and the user clicks on a line for a category
         if Action /= null then
            Get_End_Iter (Ed.Help, Text_Iter);

            Bold := Create_Tag (Ed.Help);
            Set_Property (Bold, Gtk.Text_Tag.Weight_Property,
                          Pango_Weight_Bold);

            Insert_With_Tags
              (Ed.Help, Text_Iter, ASCII.LF & ASCII.LF & (-"Key shortcuts: "),
               Bold);
            Insert
              (Ed.Help, Text_Iter,
               Lookup_Key_From_Action
                 (Ed.Bindings,
                  Action            => Get_String (Model, Iter, Action_Column),
                  Default           => -"none",
                  Is_User_Changed => User_Changed'Unchecked_Access,
                  Default_On_Gtk    => False,
                  Use_Markup        => False));

            Insert_With_Tags
              (Ed.Help, Text_Iter, ASCII.LF & (-"Declared in: "),
               Bold);
            if Action.Defined_In /= VFS.No_File then
               Insert (Ed.Help, Text_Iter, Full_Name (Action.Defined_In).all);

               Comp_Iter := Start (Action.Command);
               if Get (Comp_Iter) /= null then
                  Insert_With_Tags
                    (Ed.Help, Text_Iter,
                     ASCII.LF & (-"Implementation details:") & ASCII.LF,
                     Bold);
                  Insert_Details (Comp_Iter, "  ");
               end if;

            else
               Insert (Ed.Help, Text_Iter, -"built-in");
            end if;

            Set_Text (Ed.Action_Name, Get_String (Model, Iter, 0));
         end if;
      else
         Set_Sensitive (Ed.Remove_Button, False);
         Set_Sensitive (Ed.Grab_Button, False);
         Set_Text (Ed.Help, "");
         Set_Text (Ed.Action_Name, "");
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Add_Selection_Changed;

   -------------------------
   -- On_Toggle_Flat_List --
   -------------------------

   procedure On_Toggle_Flat_List (Editor : access Gtk_Widget_Record'Class) is
   begin
      Fill_Editor (Keys_Editor (Editor));
   end On_Toggle_Flat_List;

   ------------------------------
   -- On_Toggle_Shortcuts_Only --
   ------------------------------

   procedure On_Toggle_Shortcuts_Only
     (Editor : access Gtk_Widget_Record'Class) is
   begin
      Refilter (Keys_Editor (Editor).Filter);
   end On_Toggle_Shortcuts_Only;

   -----------------------
   -- Action_Is_Visible --
   -----------------------

   function Action_Is_Visible
     (Model : access Gtk_Tree_Model_Record'Class;
      Iter  : Gtk_Tree_Iter;
      Data  : Keys_Editor) return Boolean
   is
   begin
      return Data.Disable_Filtering
        or else not Get_Active (Data.With_Shortcut_Only)
        or else Get_String (Model, Iter, 1) /= ""
        or else N_Children (Model, Iter) > 0;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return True;
   end Action_Is_Visible;

   ------------------
   -- On_Edit_Keys --
   ------------------

   procedure On_Edit_Keys
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Editor    : Keys_Editor;
      Scrolled  : Gtk_Scrolled_Window;
      Bbox      : Gtk_Hbutton_Box;
      Hbox, Vbox, Filter_Box : Gtk_Box;
--        Button    : Gtk_Button;
      Col       : Gtk_Tree_View_Column;
      Render    : Gtk_Cell_Renderer_Text;
      Num       : Gint;
      Frame     : Gtk_Frame;
      Pane      : Gtk_Paned;
      Sep       : Gtk_Separator;
      Event     : Gtk_Event_Box;
      Text      : Gtk_Text_View;
      Action    : Gtk_Widget;
      pragma Unreferenced (Widget, Num, Action);

   begin
      Editor := new Keys_Editor_Record;
      Editor.Bindings := new Key_Htable.HTable;

      Initialize
        (Editor,
         Title  => -"Key shortcuts",
         Parent => Get_Current_Window (Kernel),
         Flags  => Destroy_With_Parent or Modal);
      Set_Name (Editor, "Key shortcuts");  --  for testsuite
      Set_Default_Size (Editor, 900, 700);
      Editor.Kernel  := Kernel;

      Clone
        (From => Keymanager_Module.Key_Manager.Table.all,
         To   => Editor.Bindings.all);

      Gtk_New_Vbox (Vbox, Homogeneous => False);
      Pack_Start (Get_Vbox (Editor), Vbox, Expand => True, Fill => True);

      Gtk_New_Hbox (Filter_Box, Homogeneous => False);
      Pack_Start (Vbox, Filter_Box, Expand => False);

      Gtk_New (Editor.With_Shortcut_Only, -"Shortcuts only");
      Set_Tip
        (Get_Tooltips (Editor.Kernel), Editor.With_Shortcut_Only,
         -("Show only actions that are associated with a key shortcut"));
      Set_Active (Editor.With_Shortcut_Only, False);
      Pack_Start (Filter_Box, Editor.With_Shortcut_Only, Expand => False);
      Widget_Callback.Object_Connect
        (Editor.With_Shortcut_Only, "toggled", On_Toggle_Shortcuts_Only'Access,
         Editor);

      Gtk_New (Editor.Flat_List, -"Flat list");
      Set_Tip
        (Get_Tooltips (Editor.Kernel), Editor.Flat_List,
         -("If selected, actions are not grouped into categories, but"
           & " displayed as a single long list. This might help to find some"
           & " specific actions"));
      Set_Active (Editor.Flat_List, False);
      Pack_Start (Filter_Box, Editor.Flat_List, Expand => False);
      Widget_Callback.Object_Connect
        (Editor.Flat_List, "toggled", On_Toggle_Flat_List'Access, Editor);

      --  ??? Will be implemented shortly
--        Gtk_New_From_Stock (Button, Stock_Find);
--        Pack_Start (Filter_Box, Button, Expand => False);

      Gtk_New_Vpaned (Pane);
      Pack_Start (Vbox, Pane, Expand => True, Fill => True);

      --  List of macros

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack1 (Pane, Scrolled, True, True);

      --  The model we will modify
      Gtk_New
        (Editor.Model,
         (Action_Column     => GType_String,
          Key_Column        => GType_String));

      --  A filter model on top of it, so that we can filter out some rows
      Gtk_New (Editor.Filter, Editor.Model);
      Keys_Editor_Visible_Funcs.Set_Visible_Func
        (Editor.Filter, Action_Is_Visible'Access, Editor);

      --  A sort model on top of the filter, so that rows can be sorted.
      Gtk_New_With_Model (Editor.Sort, Editor.Filter);

      Gtk_New (Editor.View, Editor.Sort);
      Set_Name (Editor.View, "Key shortcuts tree"); --  for testsuite
      Add (Scrolled, Editor.View);

      --  Bottom area
      Gtk_New (Frame);
      Pack2 (Pane, Frame, False, True);
      Set_Size_Request (Frame, -1, 200);

      Gtk_New_Vbox (Hbox, Homogeneous => False);
      Add (Frame, Hbox);

      --  Name of current action

      Create_Blue_Label (Editor.Action_Name, Event);
      Pack_Start (Hbox,  Event, Expand => False);

      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Start);
      Pack_Start (Hbox, Bbox, Expand => False);

      Gtk_New_From_Stock (Editor.Remove_Button, Stock_Remove);
      Set_Sensitive (Editor.Remove_Button, False);
      Pack_Start (Bbox, Editor.Remove_Button);
      Widget_Callback.Object_Connect
        (Editor.Remove_Button, "clicked", On_Remove_Key'Access, Editor);

      Gtk_New (Editor.Grab_Button, -"Grab");
      Set_Sensitive (Editor.Grab_Button, False);
      Pack_Start (Bbox, Editor.Grab_Button);
      Widget_Callback.Object_Connect
        (Editor.Grab_Button, "clicked", On_Grab_Key'Access, Editor);

      Widget_Callback.Object_Connect
        (Get_Selection (Editor.View), "changed",
         Add_Selection_Changed'Access, Editor);

      Gtk_New_Hseparator (Sep);
      Pack_Start (Hbox, Sep, Expand => False);

      --  Help on current action

      Gtk_New (Editor.Help);
      Gtk_New (Scrolled);
      Pack_Start (Hbox, Scrolled, Expand => True, Fill => True);

      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Gtk_New (Text, Editor.Help);
      Set_Wrap_Mode (Text, Wrap_Word);
      Set_Editable (Text, False);
      Add (Scrolled, Text);

      --  The tree

      Gtk_New (Render);

      Gtk_New (Col);
      Num := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Action");
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", Action_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Action_Column);

      Clicked (Col);

      Gtk_New (Col);
      Num := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Shortcut");
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "markup", Key_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Key_Column);

      Fill_Editor (Editor);

      Action := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Action := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Editor);

      if Run (Editor) = Gtk_Response_OK then
         Save_Editor (Editor);
      end if;

      Reset (Editor.Bindings.all);
      Unchecked_Free (Editor.Bindings);
      Destroy (Editor);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Edit_Keys;

   ------------------------
   -- On_Start_Recording --
   ------------------------

   procedure On_Start_Recording
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Record_Macro (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Start_Recording;

   ------------------
   -- Record_Macro --
   ------------------

   procedure Record_Macro (Kernel : Kernel_Handle) is
      Macro : constant String := '/' & (-"Tools/Macro") & '/';
   begin
      Set_Follow_Events (True);

      --  ??? There's no way to remove Pointer_Motion_Mask afterwards
      Add_Events (Get_Main_Window (Kernel), Pointer_Motion_Mask);

      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Start Recording")), False);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Stop Recording")), True);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Play")), False);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Save As...")), False);

      Free_List (Keymanager_Module.Key_Manager.Events.Events);
      Keymanager_Module.Key_Manager.Events.Last_Event := null;
      Keymanager_Module.Key_Manager.Events.Prev_Time := 0;
      Keymanager_Module.Key_Manager.Recording := True;
   end Record_Macro;

   -----------------------
   -- On_Stop_Recording --
   -----------------------

   procedure On_Stop_Recording
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Macro : constant String := '/' & (-"Tools/Macro") & '/';
   begin
      Set_Follow_Events (False);
      Keymanager_Module.Key_Manager.Recording := False;
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Start Recording")), True);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Stop Recording")), False);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Play")), True);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Save As...")), True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Stop_Recording;

   ----------------------
   -- Play_Macro_Timer --
   ----------------------

   function Play_Macro_Timer return Boolean is
      Macro         : constant String := '/' & (-"Tools/Macro") & '/';
      Current_Event : Macro_Item_Access renames
        Keymanager_Module.Key_Manager.Events.Current_Event;
      Timeout       : Guint32;
      Wait          : Duration;
      Success       : Boolean;
      Id            : Timeout_Handler_Id;
      pragma Unreferenced (Id, Success);

   begin
      if Current_Event /= null then
         Success := Play_Event
           (Current_Event.all,
            Gtk_Widget
              (Get_Main_Window (Keymanager_Module.Key_Manager.Kernel)));
         Current_Event := Current_Event.Next;
      end if;

      if Current_Event = null then
         Set_Sensitive
           (Find_Menu_Item
              (Keymanager_Module.Key_Manager.Kernel, Macro & (-"Play")), True);
      else
         --  Compute proper timeout value, taking into account the time
         --  spent to handle each event manually.

         Keymanager_Module.Key_Manager.Events.Time_Spent :=
           Keymanager_Module.Key_Manager.Events.Time_Spent +
             Current_Event.Time;
         Wait := Keymanager_Module.Key_Manager.Events.Start_Clock - Clock +
           Duration (Keymanager_Module.Key_Manager.Events.Time_Spent) /
             Duration (Keymanager_Module.Key_Manager.Events.Speed * 1000.0);

         if Wait > 0.0 then
            Timeout := Guint32 (Wait * 1000.0);
         else
            Timeout := 0;
         end if;

         Id := Gtk.Main.Timeout_Add (Timeout, Play_Macro_Timer'Access);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return False;
   end Play_Macro_Timer;

   ----------------
   -- Play_Macro --
   ----------------

   procedure Play_Macro (Kernel : Kernel_Handle; Speed : Duration := 1.0) is
      Macro         : constant String := '/' & (-"Tools/Macro") & '/';
      Current_Event : Macro_Item_Access renames
        Keymanager_Module.Key_Manager.Events.Current_Event;
      Id            : Timeout_Handler_Id;
      pragma Unreferenced (Id);

   begin
      Current_Event := Keymanager_Module.Key_Manager.Events.Events;

      if Current_Event /= null then
         Set_Sensitive
           (Find_Menu_Item (Kernel, Macro & (-"Play")), False);
         Keymanager_Module.Key_Manager.Events.Start_Clock := Clock;
         Keymanager_Module.Key_Manager.Events.Time_Spent  := 0;
         Keymanager_Module.Key_Manager.Events.Speed       := Speed;
         Id := Gtk.Main.Timeout_Add (0, Play_Macro_Timer'Access);
      end if;
   end Play_Macro;

   -------------------
   -- On_Play_Macro --
   -------------------

   procedure On_Play_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Play_Macro (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Play_Macro;

   -------------------
   -- On_Load_Macro --
   -------------------

   procedure On_Load_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Macro : constant String := '/' & (-"Tools/Macro") & '/';
      pragma Unreferenced (Widget);
   begin
      declare
         Success : Boolean := False;
         Name    : constant Virtual_File :=
           Select_File
             (Title             => -"Load Macro",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));

      begin
         if Name = VFS.No_File then
            return;
         end if;

         Load_Macro (Name, Success);

         if Success then
            Set_Sensitive (Find_Menu_Item (Kernel, Macro & (-"Play")), True);
         else
            Insert (Kernel, -"Error while loading macro", Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Load_Macro;

   -------------------
   -- On_Save_Macro --
   -------------------

   procedure On_Save_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Events : constant Macro_Item_Access :=
        Keymanager_Module.Key_Manager.Events.Events;
   begin
      if Events = null then
         return;
      end if;

      declare
         Success : Boolean;
         Name    : constant Virtual_File :=
           Select_File
             (Title             => -"Save Macro As",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Save_File,
              History           => Get_History (Kernel));

      begin
         if Name = VFS.No_File then
            return;
         end if;

         Success :=
           Save_List (Locale_From_UTF8 (Full_Name (Name).all), Events);

         if not Success then
            Insert (Kernel, -"Error while saving macro", Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Save_Macro;

   ----------------
   -- Load_Macro --
   ----------------

   procedure Load_Macro (File : Virtual_File; Success : out Boolean) is
      Buffer  : String_Access;
   begin
      Success := False;
      Buffer  := Read_File (File);

      if Buffer /= null then
         Free_List (Keymanager_Module.Key_Manager.Events.Events);
         Load_List
           (Buffer.all, Keymanager_Module.Key_Manager.Events.Events, Success);
         Free (Buffer);
      end if;
   end Load_Macro;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Keymanager_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level, File);
   begin
      if Node.Tag.all = "key" then
         declare
            Action : constant String := Get_Attribute (Node, "action");
         begin
            if Action = "" then
               Insert (Get_Kernel (Module.all),
                       -"<key> nodes must have an action attribute",
                       Mode => Error);
               raise Assert_Failure;
            end if;

            if Node.Value = null then
               Insert (Get_Kernel (Module.all),
                       -"Invalid key binding for action " & Action,
                       Mode => Error);
               raise Assert_Failure;
            end if;

            if Node.Child /= null then
               Insert
                 (Get_Kernel (Module.all),
                  -"Invalid child node for <key> tag", Mode => Error);
               raise Assert_Failure;
            end if;

            --  We want to allow several XML file to set different key bindings
            --  for the same action, so we do not remove existing shortcuts
            --  here.
            Handler_Block (Gtk.Accel_Map.Get, Keymanager_Module.Accel_Map_Id);
            Bind_Default_Key_Internal
              (Table             => Keymanager_Module.Key_Manager.Table.all,
               Kernel            => Get_Kernel (Module.all),
               Action            => Action,
               Remove_Existing_Shortcuts_For_Action => False,
               Remove_Existing_Actions_For_Shortcut => True,
               Save_In_Keys_XML  => False,
               Key               => Node.Value.all,
               Update_Menus      => True);
            Handler_Unblock
              (Gtk.Accel_Map.Get, Keymanager_Module.Accel_Map_Id);
         end;
      end if;
   end Customize;

   -------------------------
   -- Block_Key_Shortcuts --
   -------------------------

   procedure Block_Key_Shortcuts
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Keymanager_Module.Key_Manager.Active := False;
   end Block_Key_Shortcuts;

   ---------------------------
   -- Unblock_Key_Shortcuts --
   ---------------------------

   procedure Unblock_Key_Shortcuts
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Keymanager_Module.Key_Manager.Active := True;
   end Unblock_Key_Shortcuts;

   --------------------------------
   -- Keymanager_Command_Handler --
   --------------------------------

   procedure Keymanager_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
   begin
      if Command = "last_command" then
         if Keymanager_Module.Last_User_Command /= null then
            Set_Return_Value (Data, Keymanager_Module.Last_User_Command.all);
         elsif Keymanager_Module.Last_Command /= null then
            Set_Return_Value (Data, Keymanager_Module.Last_Command.all);
         else
            Set_Return_Value (Data, "");
         end if;

      elsif Command = "set_last_command" then
         Name_Parameters (Data, (1 => Command_Cst'Access));
         Free (Keymanager_Module.Last_User_Command);
         Keymanager_Module.Last_User_Command :=
           new String'(Nth_Arg (Data, 1));

      elsif Command = "repeat_next" then
         Name_Parameters (Data, (1 => Count_Cst'Access));
         Keymanager_Module.Repeat_Count := Nth_Arg (Data, 1, 1);

      elsif Command = "lookup_actions_from_key" then
         Name_Parameters (Data, (1 => Key_Cst'Access));
         declare
            Key         : constant String := Nth_Arg (Data, 1);
            Binding     : Key_Description_List;
            Keymap      : Keymap_Access := null;
            First, Last : Integer;
            Partial_Key : Gdk_Key_Type;
            Modif       : Gdk_Modifier_Type;
         begin
            Set_Return_Value_As_List (Data);

            First := Key'First;
            while First <= Key'Last loop
               Last := First + 1;
               while Last <= Key'Last and then Key (Last) /= ' ' loop
                  Last := Last + 1;
               end loop;

               Value (Key (First .. Last - 1), Partial_Key, Modif);

               if Last > Key'Last then
                  if Keymap = null then
                     Binding := Get (Keymanager_Module.Key_Manager.Table.all,
                                     (Partial_Key, Modif));
                  else
                     Binding := Get (Keymap.Table, (Partial_Key, Modif));
                  end if;

                  while Binding /= No_Key loop
                     if Binding.Action /= null then
                        Set_Return_Value (Data, To_Lower (Binding.Action.all));
                     elsif Binding.Keymap /= null then
                        Set_Return_Value (Data, "");
                     end if;
                     Binding := Binding.Next;
                  end loop;

               else
                  if Keymap = null then
                     Get_Secondary_Keymap
                       (Keymanager_Module.Key_Manager.Table.all,
                        Partial_Key, Modif, Keymap);
                  else
                     Get_Secondary_Keymap
                       (Keymap.Table, Partial_Key, Modif, Keymap);
                     exit when Keymap = null;
                  end if;
               end if;

               First := Last + 1;
            end loop;
         end;

      elsif Command = "lookup_actions" then
         declare
            Iter : Action_Iterator := Start (Get_Kernel (Data));
         begin
            Set_Return_Value_As_List (Data);
            while Get (Iter) /= null loop
               Set_Return_Value (Data, To_Lower (Get (Iter)));
               Next (Get_Kernel (Data), Iter);
            end loop;
         end;
      end if;
   end Keymanager_Command_Handler;

   --------------------
   -- Is_Numeric_Key --
   --------------------

   function Is_Numeric_Key
     (Key      : Gdk_Key_Type;
      Modifier : Gdk_Modifier_Type) return Boolean is
   begin
      return (Modifier = 0 or else Modifier = Shift_Mask)
        and then (Key in GDK_KP_0 .. GDK_KP_9 or else Key in GDK_0 .. GDK_9);
   end Is_Numeric_Key;

   --------------------------
   -- Read_Action_Argument --
   --------------------------

   procedure Read_Action_Argument
     (Validator : Argument_Key_Validator;
      Callback  : Argument_Read_Callback;
      Command   : access Interactive_Command'Class)
   is
   begin
      Free (Keymanager_Module.Argument_Current);
      Keymanager_Module.Argument_Current   := new String'("");
      Keymanager_Module.Argument_Validator := Validator;
      Keymanager_Module.Argument_Callback  := Callback;
      Keymanager_Module.Argument_Data      :=
        Interactive_Command_Access (Command);
   end Read_Action_Argument;

   ----------------------------------
   -- On_Repeat_Next_Argument_Read --
   ----------------------------------

   procedure On_Repeat_Next_Argument_Read
     (Command  : Interactive_Command'Class;
      Argument : String)
   is
      pragma Unreferenced (Command);
   begin
      Keymanager_Module.Repeat_Count := Integer'Value (Argument);
   exception
      when Constraint_Error =>
         Keymanager_Module.Repeat_Count := 1;
   end On_Repeat_Next_Argument_Read;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Repeat_Next_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Context);
   begin
      --  Read a numeric argument
      Read_Action_Argument
        (Is_Numeric_Key'Access,
         On_Repeat_Next_Argument_Read'Access,
         Command);
      return Commands.Success;
   end Execute;

   --------------------------
   -- On_Accel_Map_Changed --
   --------------------------

   procedure On_Accel_Map_Changed
     (Map    : access GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : Kernel_Handle)
   is
      Accel_Path : constant String := To_String (Args, 1);
      Accel_Key  : constant Gdk_Key_Type := Gdk_Key_Type (To_Guint (Args, 2));
      Accel_Mods : constant Gdk_Modifier_Type :=
        Gdk_Modifier_Type (Get_Flags (Nth (Args, 3)));
      pragma Unreferenced (Map);
      First : Natural := Accel_Path'First + 1;
   begin
      while First <= Accel_Path'Last
        and then Accel_Path (First - 1) /= '>'
      loop
         First := First + 1;
      end loop;

      --  Special handling when Key is 0 => either the accel_path has just
      --  been created, in which case we should preserve any key binding that
      --  was there before, or the user has cancelled dynamically the key
      --  binding through gtk+, in which case we need to obey the order.
      --
      --  Unfortunately, there is no way currently in gtk+ to distinguish
      --  between the two apparently, so the least inconvenient is to do
      --  nothing, and thus ignore cases where the user has pressed Backspace
      --  in a menu item to delete the shortcut. In fact, we even reset the
      --  old binding so that the menu still shows the old binding, for
      --  consistency.

      if Accel_Key = 0 then
         declare
            User_Changed : aliased Boolean;
            Old : constant String := Lookup_Key_From_Action
              (Table   => Keymanager_Module.Key_Manager.Table,
               Action  => Accel_Path (First .. Accel_Path'Last),
               Default => "",
               Use_Markup => False,
               Is_User_Changed => User_Changed'Unchecked_Access,
               Return_Multiple => False,
               Default_On_Gtk => False);
         begin
            if Old /= "" then
               --  Prevent recursive call to On_Accel_Map_Changed, since we
               --  are asking to modify the menus here.
               Handler_Block
                 (Gtk.Accel_Map.Get, Keymanager_Module.Accel_Map_Id);

               Bind_Default_Key_Internal
                 (Table  => Keymanager_Module.Key_Manager.Table.all,
                  Kernel => Kernel,
                  Action => Accel_Path (First .. Accel_Path'Last),
                  Key    => Old,
                  Save_In_Keys_XML  => User_Changed,
                  Remove_Existing_Shortcuts_For_Action => True,
                  Remove_Existing_Actions_For_Shortcut => True,
                  Update_Menus                         => True);
               Handler_Unblock
                 (Gtk.Accel_Map.Get, Keymanager_Module.Accel_Map_Id);
            end if;
         end;
      else
         --  Remove any other keybinding associated with that action, as well
         --  as any action associated with that key.
         Bind_Default_Key_Internal
           (Table  => Keymanager_Module.Key_Manager.Table.all,
            Kernel => Kernel,
            Action => Accel_Path (First .. Accel_Path'Last),
            Key    => Image (Accel_Key, Accel_Mods),
            Save_In_Keys_XML  =>
              (Keymanager_Module.Key_Manager.Custom_Keys_Loaded
               and then Keymanager_Module.Menus_Created),
            Remove_Existing_Shortcuts_For_Action => True,
            Remove_Existing_Actions_For_Shortcut => True,
            Update_Menus                         => False);
      end if;
   end On_Accel_Map_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Manager    : constant Key_Manager_Access := new Key_Manager_Record;
      Macro_Menu : constant String := "/" & (-"Tools/Macro");
      Key : constant String := Get_Home_Dir (Kernel) & "custom_key";
      Command    : Interactive_Command_Access;

   begin
      Manager.Kernel := Kernel_Handle (Kernel);

      Keymanager_Module := new Keymanager_Module_Record;
      Keymanager_Module.Key_Manager := Manager;
      Keymanager_Module.Key_Manager.Table := new Key_Htable.HTable;

      Register_Module
        (Keymanager_Module, Kernel, "keymanager");

      Event_Handler_Set
        (General_Event_Handler'Access,
         Convert (Kernel_Handle (Kernel)));

      Keymanager_Module.Accel_Map_Id := Kernel_Callback.Connect
        (Gtk.Accel_Map.Get, Gtk.Accel_Map.Signal_Changed,
         On_Accel_Map_Changed'Access, Kernel_Handle (Kernel));

      --  For backward compatibility with GPS 3.1.3, we load the accel map
      --  custom keys as well. These will be overloaded when we load keys.xml
      --  anyway. The file custom_key will never be overwritten by GPS from now
      --  on.

      if Is_Regular_File (Key) then
         Trace (Me, "Loading key bindings from " & Key);
         Gtk.Accel_Map.Load (Key);
      end if;

      if Active (Use_Macro) then
         Register_Menu
           (Kernel, Macro_Menu, -"_Start Recording",
            Callback => On_Start_Recording'Access);
         Register_Menu
           (Kernel, Macro_Menu, -"S_top Recording",
            Callback   => On_Stop_Recording'Access,
            Accel_Key  => GDK_Escape,
            Accel_Mods => Control_Mask,
            Sensitive  => False);
         Register_Menu
           (Kernel, Macro_Menu, -"_Play",
            Callback  => On_Play_Macro'Access,
            Sensitive => False);
         Register_Menu
           (Kernel, Macro_Menu, -"Load...",
            Callback  => On_Load_Macro'Access);
         Register_Menu
           (Kernel, Macro_Menu, -"_Save As...",
            Callback  => On_Save_Macro'Access,
            Sensitive => False);
         Register_Command
           (Kernel, "macro_play",
            Maximum_Args => 1,
            Handler      => Macro_Command_Handler'Access);
         Register_Command
           (Kernel, "macro_record",
            Handler      => Macro_Command_Handler'Access);
         Register_Command
           (Kernel, "macro_load",
            Minimum_Args => 1,
            Maximum_Args => 1,
            Handler      => Macro_Command_Handler'Access);
      end if;

      Register_Command
        (Kernel, "last_command", 0, 0, Keymanager_Command_Handler'Access);
      Register_Command
        (Kernel, "set_last_command", 1, 1, Keymanager_Command_Handler'Access);
      Register_Command
        (Kernel, "repeat_next", 1, 1, Keymanager_Command_Handler'Access);
      Register_Command
        (Kernel, "lookup_actions_from_key", 1, 1,
           Keymanager_Command_Handler'Access);
      Register_Command
        (Kernel, "lookup_actions", 0, 0, Keymanager_Command_Handler'Access);

      Command := new Repeat_Next_Command;
      Repeat_Next_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Action
        (Kernel, "Repeat Next", Command,
         -("Repeat the next action a number of times. Executing this action"
           & " takes a numeric argument (read from the keyboard). For"
           & " instance, if this is associated with ctrl-u, you can type"
           & " ""ctrl-u 30 t"" to instead the character t 30 times"),
         Category => -"General");
      Bind_Default_Key (Kernel, "Repeat Next", "control-u");

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "key_manager.preferences_changed");
   end Register_Module;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Keymanager_Module.Menus_Created := True;
   end Preferences_Changed;

   -----------------------
   -- Register_Key_Menu --
   -----------------------

   procedure Register_Key_Menu
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Register_Menu
        (Kernel, '/' & (-"Edit"),
         -"_Key Shortcuts",
         Callback => On_Edit_Keys'Access,
         Ref_Item => -"Preferences");
   end Register_Key_Menu;

end KeyManager_Module;
