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
with Traces;                  use Traces;
with VFS;                     use VFS;
with XML_Parsers;

package body KeyManager_Module is

   Me : constant Debug_Handle := Create ("Keymanager");

   Use_Macro : constant Debug_Handle := Create ("Keymanager.Macro", Off);
   --  ??? For now disable by default since this is a work in progress

   File_Cst                  : aliased constant String := "file";
   Speed_Cst                 : aliased constant String := "speed";
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

   function Next (Key : Key_Description_List) return Key_Description_List;
   --  Return the next element in the list

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

   type Keymanager_Module_Record is new Module_ID_Record with record
      Key_Manager   : Key_Manager_Access;
      Accel_Map_Id  : Handler_Id;
      Menus_Created : Boolean := False;
      --  Indicates whether the initial set of menus has been created.
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
     (Table                                : in out Key_Htable.HTable;
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

   function Lookup_Key_From_Action
     (Table             : HTable_Access;
      Action            : String;
      Default           : String := "none";
      Use_Markup        : Boolean := True) return String;
   --  Return the list of key bindings set for a specific action. The returned
   --  string can be displayed as is to the user, but is not suitable for
   --  parsing as a keybinding. The list of keybindings includes the
   --  accelerators set by gtk+ for its menus (in which case Accel_Path_Prefix
   --  needs to be defined)
   --  If Use_Markup is true, then the "or" that separates several shortcuts
   --  is displayed with a different font.
   --  If the action is not found in the table but corresponds to a menu, look
   --  it up using standard gtk+ mechanisms and insert the corresponding entry
   --  in Table to speed up further lookups.

   function Invalid_Key return Gdk_Key_Type;
   --  Return an unique invalid key.

   Action_Column     : constant := 0;
   Key_Column        : constant := 1;

   -----------------
   -- Invalid_Key --
   -----------------

   Current_Invalid_Key : Gdk_Key_Type := 16#10000#;

   function Invalid_Key return Gdk_Key_Type is
   begin
      if Current_Invalid_Key > 16#FFFFFE# then
         Current_Invalid_Key := 16#10000#;
      else
         Current_Invalid_Key := Current_Invalid_Key + 1;
      end if;

      return Current_Invalid_Key;
   end Invalid_Key;

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
      end if;

      --  Dispatch the event in the standard gtk+ main loop
      Gtk.Main.Do_Event (Event);
   end General_Event_Handler;

   ----------
   -- Next --
   ----------

   function Next (Key : Key_Description_List) return Key_Description_List is
   begin
      return Key.Next;
   end Next;

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
         N := Next (Current);
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
         Tmp := Next (Tmp);
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
     (Table                                : in out Key_Htable.HTable;
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
      begin
         if not Remove_Existing_Actions_For_Shortcut then
            Binding3 := Get (Table, Key_Binding'(Default_Key, Default_Mod));

            --  Check whether the same action is already attached to this key.
            --  ??? When we have a menu, we should check the underlying action
            --  if there is any.
            Tmp := Binding3;
            while Tmp /= null loop
               if Tmp.Action /= null and then Tmp.Action.all = Action then
                  return;
               end if;
               Tmp := Next (Tmp);
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
               if List.Action = null then
                  if List.Keymap /= null then
                     Remove_In_Keymap (List.Keymap.Table);
                  end if;

                  Previous := List;
                  List := Next (List);

               elsif List.Action.all = Action then
                  if Previous = null then
                     if Next (List) /= null then
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
                        List.Next := null;
                     end if;

                  else
                     Previous.Next := Next (List);
                     Free_Non_Recursive (List);
                     List := Next (Previous);
                  end if;

               else
                  Previous := List;
                  List := Next (List);
               end if;
            end loop;

            Get_Next (Table, Iter);
         end loop;
      end Remove_In_Keymap;

      Partial_Key : Gdk_Key_Type;
      Modif : Gdk_Modifier_Type;
      First, Last : Integer;
      Keymap  : Keymap_Access;
   begin
      --  Are we trying to cancel all bindings to Action ?
      if Remove_Existing_Shortcuts_For_Action
        or else Key = ""
      then
         Remove_In_Keymap (Table);

         if Update_Menus and then Action (Action'First) = '/' then
            --  Guess the accel path from the menu
            Change_Entry
              ("<gps>" & Action,
               Accel_Key  => 0,
               Accel_Mods => 0,
               Replace    => True);
         end if;

      end if;

      if Key = "" or else Key = -Disabled_String then
         --  Bind to an invalid key, so that when saving we know this should be
         --  removed
         Bind_Internal (Table, Invalid_Key, 0);
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
            if Keymap = null then
               Bind_Internal (Table, Partial_Key, Modif);

               if Update_Menus and then Action (Action'First) = '/' then
                  --  Guess the accel path from the menu
                  Change_Entry
                    ("<gps>" & Action,
                     Accel_Key  => Partial_Key,
                     Accel_Mods => Modif,
                     Replace    => True);
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
           and then Binding2.Action /= null
         loop
            Binding  := Binding2;  --  Last value where Next /= null
            Binding2 := Next (Binding2);
         end loop;

         --  If there is no secondary keymap yet, create one
         if Binding2 = null then
            Keymap   := new Keymap_Record;
            Binding2 := new Key_Description'
              (Action  => null,
               Changed => False,
               Keymap  => Keymap,
               Next    => null);
            Binding.Next := Binding2;
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
      Key     : constant Gdk_Key_Type := Get_Key_Val (Event);
      Modif   : Gdk_Modifier_Type := Get_State (Event);
      Binding : Key_Description_List;
      Command : Action_Record_Access;
      Has_Secondary : constant Boolean := Handler.Secondary_Keymap /= null;
      Context : Selection_Context;
      Context_Computed : Boolean := False;
      Found_Action : Boolean := False;
   begin
      --  We could test Modif /= 0 if we allowed only key shortcuts with a
      --  modifier (control, alt, ...). However, this would prevent assigning
      --  key shortcuts to F1, F2, Home, PageUp,.. so is not desirable.

      if Handler.Active
        and then Get_Event_Type (Event) = Key_Press
      then
         --  Remove any num-lock and caps-lock modifiers.

         Modif := Modif and not (Lock_Mask or Mod2_Mask);

         if Handler.Secondary_Keymap = null then
            Binding := Get (Handler.Table.all, (Key, Modif));
         else
            Binding := Get (Handler.Secondary_Keymap.Table, (Key, Modif));
         end if;

         Handler.Secondary_Keymap := null;

         --  Execute all commands bound to this key. The order is somewhat
         --  random, since it depends in what order the key shortcuts were
         --  defined.
         while Binding /= No_Key loop
            if Binding.Action = null then
               Handler.Secondary_Keymap := Binding.Keymap;
               Found_Action := True;

            else
               --  First try to activate the key shortcut using the standard
               --  Gtk+ mechanism.
               --  Do this lookup only if we are not currently processing a
               --  secondary key.
               if not Has_Secondary
                 and then Accel_Groups_Activate
                   (Get_Main_Window (Kernel), Key, Modif)
               then
                  Found_Action := True;
                  exit;
               end if;

               --  If we have not found the accelerator using the Gtk+
               --  mechanism, fallback on the standard mechanism to lookup the
               --  action.
               Command := Lookup_Action (Kernel, Binding.Action.all);

               if Command = null then
                  Insert
                    (Kernel, -"Action not defined: " & Binding.Action.all);

               elsif Command.Command /= null then
                  if not Context_Computed then
                     Context := Get_Current_Context (Kernel);
                     Context_Computed := True;
                  end if;

                  if Command.Filter = null
                    or else (Context /= No_Context
                             and then Filter_Matches (Command.Filter, Context))
                  then
                     Trace (Me, "Executing action " & Binding.Action.all);

                     Found_Action := True;
                     Launch_Background_Command
                       (Kernel,
                        Create_Proxy
                          (Command.Command,
                           (Event,
                            Context,
                            False,
                            null,
                            null,
                            new String'(Binding.Action.all))),
                        Destroy_On_Exit => False,
                        Active          => True,
                        Show_Bar        => False,
                        Queue_Id        => "");
                  end if;
               end if;
            end if;

            Binding := Next (Binding);
         end loop;
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
                  Child.Value := new String'
                    (Prefix
                     & Image (Get_Key (Iter).Key, Get_Key (Iter).Modifier));

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

               Binding := Next (Binding);
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

            while Child /= null loop
               --  Remove all other bindings previously defined, so that only
               --  the last definition is taken into account
               Bind_Default_Key_Internal
                 (Keymanager_Module.Key_Manager.Table.all,
                  Action           => Get_Attribute (Child, "action"),
                  Key              => Child.Value.all,
                  Save_In_Keys_XML => True,
                  Remove_Existing_Shortcuts_For_Action => True,
                  Remove_Existing_Actions_For_Shortcut => True,
                  Update_Menus     => True);
               Child := Child.Next;
            end loop;

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
      Use_Markup        : Boolean := True) return String
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

            while Binding /= null loop
               if Binding.Action = null then
                  if Binding.Keymap /= null then
                     Process_Table
                       (Binding.Keymap.Table,
                        Prefix
                        & Image (Get_Key (Iter).Key, Get_Key (Iter).Modifier)
                        & ' ');
                  end if;

               elsif Binding.Action.all = Action
                 and then Get_Key (Iter).Key /= 0
               then
                  if Result /= Null_Unbounded_String then
                     if Use_Markup then
                        Result := Result & " <b>or</b> ";
                     else
                        Result := Result & " or ";
                     end if;
                  end if;

                  Result := Result
                    & Prefix
                    & Image (Get_Key (Iter).Key, Get_Key (Iter).Modifier);
               end if;

               Binding := Next (Binding);
            end loop;

            Get_Next (Table, Iter);
         end loop;
      end Process_Table;

      Key   : Gtk_Accel_Key;
      Found : Boolean;
   begin
      --  The table also includes the menu accelerators set by gtk+, so by
      --  traversing the table we get access to everything.
      --  ??? This is not true for stock accelerators.
      Process_Table (Table.all, "");

      --  If we haven't found an action, fallback on the default gtk+
      --  mechanism.

      if To_String (Result) = "" and then Action (Action'First) = '/' then
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

            return Image (Key.Accel_Key, Key.Accel_Mods);
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
                     Default => ""));
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
                  Default           => ""));
         end if;
      end Process_Menu_Binding;

      Parent      : Gtk_Tree_Iter;
      Action      : Action_Record_Access;
      Action_Iter : Action_Iterator := Start (Editor.Kernel);
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
            Parent := Set
              (Model   => Editor.Model,
               Parent  => Parent,
               Descr   => Get (Action_Iter),
               Key     => Lookup_Key_From_Action
                 (Editor.Bindings,
                  Get (Action_Iter),
                  Default => -Disabled_String));
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
         pragma Unreferenced (Data, Changed, Accel_Key, Accel_Mods);
         First   : Natural := Accel_Path'First + 1;
         Iter    : Key_Htable.Iterator;
         Binding : Key_Description_List;
         Found   : Boolean := False;

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

            while Binding /= null loop
               if Binding.Action /= null
                 and then Binding.Changed
                 and then Binding.Action.all =
                   Accel_Path (First .. Accel_Path'Last)
               then
                  Found := True;
                  Change_Entry
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
            Change_Entry
              (Accel_Path => Accel_Path,
               Accel_Key  => 0,
               Accel_Mods => 0,
               Replace    => True);
         end if;
      end Process_Menu_Binding;

   begin
      Reset (Handler.Table.all);
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
                 (Ed.Bindings.all,
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
           (Ed.Bindings.all,
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

   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)
      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Set_Sensitive (Ed.Remove_Button, True);
         Set_Sensitive (Ed.Grab_Button, True);

         Action := Lookup_Action (Ed.Kernel, Get_String (Model, Iter, 0));

         if Action.Description /= null then
            Set_Text (Ed.Help, Action.Description.all);
         else
            Set_Text (Ed.Help, "");
         end if;

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

            Bind_Default_Key_Internal
              (Keymanager_Module.Key_Manager.Table.all,
               Action            => Action,
               Remove_Existing_Shortcuts_For_Action => False,
               Remove_Existing_Actions_For_Shortcut => True,
               Save_In_Keys_XML  => False,
               Key               => Node.Value.all,
               Update_Menus      => True);
         end;
      end if;
   end Customize;

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
      pragma Unreferenced (Map, Kernel);
      First : Natural := Accel_Path'First + 1;
   begin
      while First <= Accel_Path'Last
        and then Accel_Path (First - 1) /= '>'
      loop
         First := First + 1;
      end loop;

      --  Remove any other keybinding associated with that action, as well as
      --  any action associated with that key.
      Bind_Default_Key_Internal
        (Table  => Keymanager_Module.Key_Manager.Table.all,
         Action => Accel_Path (First .. Accel_Path'Last),
         Key                                  => Image (Accel_Key, Accel_Mods),
         Save_In_Keys_XML  =>
           (Keymanager_Module.Key_Manager.Custom_Keys_Loaded
            and then Keymanager_Module.Menus_Created),
         Remove_Existing_Shortcuts_For_Action => True,
         Remove_Existing_Actions_For_Shortcut => True,
         Update_Menus                         => False);
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
         -"_Key shortcuts",
         Callback => On_Edit_Keys'Access,
         Ref_Item => -"Preferences");
   end Register_Key_Menu;

end KeyManager_Module;
