-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2004                       --
--                            ACT-Europe                             --
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

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Actions;      use Glide_Kernel.Actions;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib.Convert;              use Glib.Convert;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Commands.Interactive;      use Commands, Commands.Interactive;
with HTables;                   use HTables;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GUI_Utils;                 use GUI_Utils;
with System;                    use System;
with System.Assertions;         use System.Assertions;
with Ada.Exceptions;            use Ada.Exceptions;
with Gdk.Color;                 use Gdk.Color;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Box;                   use Gtk.Box;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gtk.Vbutton_Box;           use Gtk.Vbutton_Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Glide_Intl;                use Glide_Intl;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Enums;                 use Gtk.Enums;
with System;                    use System;
with Gtk.Accel_Map;             use Gtk.Accel_Map;
with Gtk.Window;                use Gtk.Window;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Style;                 use Gtk.Style;
with Gtk.Separator;             use Gtk.Separator;
with Gtkada.Macro;              use Gtkada.Macro;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Traces;                    use Traces;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with VFS;                       use VFS;
with Ada.Calendar;              use Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body KeyManager_Module is

   Me : constant Debug_Handle := Create ("Keymanager");

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
   No_Binding : constant Key_Binding := (100000, 1000000);

   type Keymap_Record;
   type Keymap_Access is access Keymap_Record;

   type Key_Description;
   type Key_Description_List is access Key_Description;
   type Key_Description is record
      Action  : String_Access;
      Changed : Boolean := False;
      Next    : Key_Description_List;
   end record;
   No_Key : constant Key_Description_List := null;
   --  Changed is set to True when the key was customized from within GPS
   --  itself, and should therefore be saved on exit. It is false for values
   --  read from the custom files.
   --
   --  To save memory, the following encoding is used: if Action is null,
   --  this key binding is associated with a secondary keymap (for instance
   --  as in "control-x control-k". In that case, Next is of type
   --  Keymap_Access.

   function Next (Key : Key_Description_List) return Key_Description_List;
   --  Return the next element in the list

   function Get_Keymap (Key : Key_Description_List) return Keymap_Access;
   --  Return the secondary keymap associated with Key.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Key_Description, Key_Description_List);

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

   type Keymap_Record is record
      Table : Key_Htable.HTable;
   end record;
   for Keymap_Record'Alignment use Integer'Min (8, Standard'Maximum_Alignment);

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
      Table            : Key_Htable.HTable;

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
      Key_Manager : Key_Manager_Access;
   end record;
   type Keymanager_Module_ID is access all Keymanager_Module_Record'Class;

   procedure Destroy (Module : in out Keymanager_Module_Record);
   --  See doc for inherited subprogram

   --  ??? Global variable, could be queries from the kernel
   Keymanager_Module : Keymanager_Module_ID;

   procedure Bind_Default_Key
     (Handler     : access Key_Manager_Record;
      Action      : String;
      Default_Key : String);
   --  Bind Default_Key to Action, see doc in Glide_Kernel.Bind_Default_Key

   function Process_Event
     (Handler : access Key_Manager_Record;
      Kernel  : access Kernel_Handle_Record'Class;
      Event   : Gdk_Event) return Boolean;
   --  Process the event and call the appropriate actions if needed

   procedure Free (Handler : in out Key_Manager_Record);
   --  Free the memoru occupied by the key manager

   procedure Bind_Default_Key_Internal
     (Table       : in out Key_Htable.HTable;
      Action      : String;
      Default_Key : Gdk.Types.Gdk_Key_Type;
      Default_Mod : Gdk.Types.Gdk_Modifier_Type;
      Changed     : Boolean := False);
   --  Internal version that allows setting the Changed attribute.

   procedure Bind_Default_Key_Internal
     (Handler     : access Key_Manager_Record;
      Action      : String;
      Default_Key : String;
      Changed     : Boolean := False);
   --  Same as above, except Default_Key can also include secondary keymaps
   --  as in "control-c control-k"

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class;
      Manager : access Key_Manager_Record);
   --  Load the customized key bindings

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
      Kernel      : Kernel_Handle;
      View        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
      Help        : Gtk_Text_Buffer;
      Action_Name : Gtk_Label;
   end record;
   type Keys_Editor is access all Keys_Editor_Record'Class;

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class);
   --  Fill the contents of the editor

   procedure Save_Editor (Editor : access Keys_Editor_Record'Class);
   --  Save the contents of the editor

   function Set
     (Model   : Gtk_Tree_Store;
      Parent  : Gtk_Tree_Iter;
      Descr   : String;
      Changed : Boolean := False;
      Key     : String := "") return Gtk_Tree_Iter;
   --  Add a new line into the model

   procedure Lookup_Command_By_Name
     (Handler : access Key_Manager_Record;
      Action  : String;
      Keymap  : out Keymap_Access;
      Key     : out Key_Binding;
      Binding : out Key_Description_List);
   --  Search the description of a command in the table

   procedure Add_Selection_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the selection has changed

   procedure Get_Secondary_Keymap
     (Table  : in out Key_Htable.HTable;
      Key    : Gdk_Key_Type;
      Modif  : Gdk_Modifier_Type;
      Keymap : out Keymap_Access);
   --  Get or create a secondary keymap in Table.

   function Find_Parent
     (Model : Gtk_Tree_Store; Filter : Action_Filter) return Gtk_Tree_Iter;
   function Find_Parent
     (Model : Gtk_Tree_Store; Context : String) return Gtk_Tree_Iter;
   --  Find the parent node for Context.
   --  Returns null if there is no such node

   function Convert is new Ada.Unchecked_Conversion
     (Kernel_Handle, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Kernel_Handle);

   procedure General_Event_Handler
     (Event : Gdk_Event; Kernel : System.Address);
   --  Event handler called before even gtk can do its dispatching. This
   --  intercepts all events going through the application

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Called when new customization files are parsed

   Action_Column  : constant := 0;
   Key_Column     : constant := 1;
   Changed_Column : constant := 2;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Keymanager_Module_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Key_Manager_Record, Key_Manager_Access);
   begin
      Free (Module.Key_Manager.all);
      Unchecked_Free (Module.Key_Manager);
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
      Prev_Time   : Guint32 renames
        Keymanager_Module.Key_Manager.Events.Prev_Time;

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

            Prev_Time := Get_Time (Event);
         end if;
      end Save_Item;

   begin
      --  We do not put a global exception handler in this procedure since
      --  it is called very often, so when using setjmp/longjmp, the cost
      --  may not be negligible.

      if Keymanager_Module.Key_Manager.Recording then
         begin
            case Event_Type is
               when Key_Press | Key_Release =>
                  Key_Item := Create_Item (Event, Prev_Time);
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
                  Button_Item := Create_Item (Event, Prev_Time);
                  Save_Item (Macro_Item_Access (Button_Item));

               when Motion_Notify =>
                  Motion_Item := Create_Item (Event, Prev_Time);
                  Save_Item (Macro_Item_Access (Motion_Item));

               when Scroll =>
                  Scroll_Item := Create_Item (Event, Prev_Time);
                  Save_Item (Macro_Item_Access (Scroll_Item));

               --  Other events should not be needed: they will be generated as
               --  part the basic events handled above (keyboard/mouse events).

               when others =>
                  null;
            end case;

         exception
            when E : others =>
               Trace
                 (Me, "Unexpected exception: " & Exception_Information (E));
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
      if Key.Action = null then
         --  A secondary keymap in fact
         return null;
      else
         return Key.Next;
      end if;
   end Next;

   ----------------
   -- Get_Keymap --
   ----------------

   function Get_Keymap (Key : Key_Description_List) return Keymap_Access is
      function Convert is new Ada.Unchecked_Conversion
        (Key_Description_List, Keymap_Access);
   begin
      if Key.Action = null then
         return Convert (Key.Next);
      else
         return null;
      end if;
   end Get_Keymap;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Key_Binding) return Keys_Header_Num is
   begin
      return Keys_Header_Num
        ((Integer (Key.Key) + Integer (Key.Modifier) * 16#FFFF#)
          mod Integer (Keys_Header_Num'Last + 1));
   end Hash;

   ----------
   -- Free --
   ----------

   procedure Free (Element : in out Key_Description_List) is
      Current : Key_Description_List := Element;
      N       : Key_Description_List;
      Keymap  : Keymap_Access;
   begin
      while Current /= null loop
         N := Next (Current);

         if Current.Action = null then
            Keymap := Get_Keymap (Current);
            Reset (Keymap.Table);
            Unchecked_Free (Keymap);
         else
            Free (Current.Action);
         end if;
         Unchecked_Free (Current);
         Current := N;
      end loop;
   end Free;

   ----------------------------
   -- Lookup_Command_By_Name --
   ----------------------------

   procedure Lookup_Command_By_Name
     (Handler : access Key_Manager_Record;
      Action  : String;
      Keymap  : out Keymap_Access;
      Key     : out Key_Binding;
      Binding : out Key_Description_List)
   is
      procedure Process_Table
        (Table : in out Key_Htable.HTable; Found : out Boolean);
      --  Process a keymap..

      procedure Process_Table
        (Table : in out Key_Htable.HTable; Found : out Boolean)
      is
         Iter : Key_Htable.Iterator;
         Bind : Key_Description_List;
      begin
         Get_First (Table, Iter);
         loop
            Bind := Get_Element (Iter);
            exit when Bind = No_Key;

            while Bind /= null loop
               if Bind.Action /= null then
                  if Bind.Action.all = Action then
                     Key     := Get_Key (Iter);
                     Binding := Bind;
                     Found   := True;
                     return;
                  end if;
               else
                  Process_Table (Get_Keymap (Bind).Table, Found);
                  if Found then
                     return;
                  end if;
               end if;
               Bind := Next (Bind);
            end loop;

            Get_Next (Table, Iter);
         end loop;
         Found := False;
      end Process_Table;

      Found : Boolean;
   begin
      --  We do not use the most efficient method, since we simply
      --  traverse a list, but there aren't hundreds of keybindings...

      Process_Table (Handler.Table, Found);

      if not Found then
         Keymap  := null;
         Key     := No_Binding;
         Binding := null;
      end if;
   end Lookup_Command_By_Name;

   ----------------------
   -- Bind_Default_Key --
   ----------------------

   procedure Bind_Default_Key
     (Handler        : access Key_Manager_Record;
      Action         : String;
      Default_Key    : String) is
   begin
      Bind_Default_Key_Internal
        (Handler, Action, Default_Key, Changed => False);
   end Bind_Default_Key;

   -------------------------------
   -- Bind_Default_Key_Internal --
   -------------------------------

   procedure Bind_Default_Key_Internal
     (Table          : in out Key_Htable.HTable;
      Action         : String;
      Default_Key    : Gdk.Types.Gdk_Key_Type;
      Default_Mod    : Gdk.Types.Gdk_Modifier_Type;
      Changed        : Boolean := False)
   is
      Binding, Binding2 : Key_Description_List;
   begin
      Binding2 := new Key_Description'
        (Action         => new String'(Action),
         Changed        => Changed,
         Next           => null);
      Binding := Get (Table, Key_Binding'(Default_Key, Default_Mod));

      if Binding /= null then
         Binding2.Next := Binding.Next;
         Binding.Next  := Binding2;
      else
         Set (Table, Key_Binding'(Default_Key, Default_Mod), Binding2);
      end if;
   end Bind_Default_Key_Internal;

   -------------------------------
   -- Bind_Default_Key_Internal --
   -------------------------------

   procedure Bind_Default_Key_Internal
     (Handler        : access Key_Manager_Record;
      Action         : String;
      Default_Key    : String;
      Changed        : Boolean := False)
   is
      Key   : Gdk_Key_Type;
      Modif : Gdk_Modifier_Type;
      First, Last : Integer;
      Keymap  : Keymap_Access;
      Binding : Key_Description_List;
      Bind    : Key_Binding;
   begin
      Lookup_Command_By_Name (Handler, Action, Keymap, Bind, Binding);
      if Binding /= null then
         --  Keep the current key binding, since it was probably
         --  customized by the user
         Binding.Changed := Binding.Changed or else Changed;
         return;
      end if;

      if Default_Key = "" or else Default_Key = -Disabled_String then
         Bind_Default_Key_Internal
           (Handler.Table, Action, 0, 0, Changed);
         return;
      end if;

      First := Default_Key'First;
      while First <= Default_Key'Last loop
         Last := First + 1;
         while Last <= Default_Key'Last and then Default_Key (Last) /= ' ' loop
            Last := Last + 1;
         end loop;

         Value (Default_Key (First .. Last - 1), Key, Modif);

         if Last > Default_Key'Last then
            if Keymap = null then
               Bind_Default_Key_Internal
                 (Handler.Table, Action, Key, Modif, Changed);
            else
               Bind_Default_Key_Internal
                 (Keymap.Table, Action, Key, Modif, Changed);
            end if;

         else
            if Keymap = null then
               Get_Secondary_Keymap (Handler.Table, Key, Modif, Keymap);
            else
               Get_Secondary_Keymap (Keymap.Table, Key, Modif, Keymap);
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
      function Convert is new Ada.Unchecked_Conversion
        (Keymap_Access, Key_Description_List);
      Binding  : Key_Description_List := Get (Table, (Key, Modif));
      Binding2 : Key_Description_List;
   begin
      if Binding = null then
         Keymap := new Keymap_Record;
         Binding := new Key_Description'
           (Action  => null,
            Changed => False,
            Next    => Convert (Keymap));
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
               Next    => Convert (Keymap));
            Binding.Next := Binding2;
         else
            Keymap := Get_Keymap (Binding2);
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
      Command : Action_Record;
      Any_Context_Command : Action_Record := No_Action;
      Has_Secondary : constant Boolean := Handler.Secondary_Keymap /= null;
      Context : Selection_Context_Access;

   begin
      if Handler.Active
        and then Get_Event_Type (Event) = Key_Press
      then
         --  Remove any num-lock and caps-lock modifiers.
         Modif := Modif and not (Lock_Mask or Mod2_Mask);

         if Handler.Secondary_Keymap = null then
            Binding := Get (Handler.Table, (Key, Modif));
         else
            Binding := Get (Handler.Secondary_Keymap.Table, (Key, Modif));
         end if;

         Handler.Secondary_Keymap := null;

         Context := Get_Current_Context (Kernel);
         Ref (Context);

         while Binding /= No_Key loop
            if Binding.Action = null then
               Handler.Secondary_Keymap := Get_Keymap (Binding);
               return True;

            else
               Command := Lookup_Action (Kernel, Binding.Action.all);

               if Command.Command /= null then
                  --  We'll have to test last the commands that apply anywhere,
                  --  to give a chance to more specialized commands to get
                  --  called first.
                  if Command.Filter = null then
                     Any_Context_Command := Command;
                     Trace (Me, "Candidate action in any context: "
                            & Binding.Action.all);

                  elsif Filter_Matches (Command.Filter, Context, Kernel) then
                     Trace (Me, "Executing action " & Binding.Action.all);
                     Launch_Background_Command
                       (Kernel,
                        Create_Proxy
                          (Command.Command, (Event, Context, null, null)),
                        Destroy_On_Exit => False,
                        Active          => False,
                        Show_Bar        => False,
                        Queue_Id        => "");
                     return True;
                  end if;
               end if;
            end if;

            Binding := Next (Binding);
         end loop;

         if Any_Context_Command /= No_Action then
            Trace (Me, "Executing any context action");
            Launch_Background_Command
              (Kernel,
               Create_Proxy
                 (Any_Context_Command.Command, (Event, Context, null, null)),
               Destroy_On_Exit => False,
               Active          => False,
               Show_Bar        => False,
               Queue_Id        => "");
            return True;
         end if;

         Unref (Context);
      end if;

      --  Never pass through an event from a secondary keymap
      return Has_Secondary;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
                  Save_Table (Get_Keymap (Binding).Table,
                              Prefix
                              & Image (Get_Key (Iter).Key,
                                       Get_Key (Iter).Modifier)
                              & ' ');
               end if;

               Binding := Next (Binding);
            end loop;

            Get_Next (Table, Iter);
         end loop;
      end Save_Table;

   begin
      File     := new Node;
      File.Tag := new String'("Keys");

      Save_Table (Handler.Table, "");

      Trace (Me, "Saving " & Filename);
      Print (File, Filename);
      Free (File);

      Reset (Handler.Table);
   end Free;

   ----------------------
   -- Load_Custom_Keys --
   ----------------------

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class;
      Manager : access Key_Manager_Record)
   is
      Filename : constant String := Get_Home_Dir (Kernel) & "keys.xml";
      File, Child : Node_Ptr;
   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);
         File := Parse (Filename);
         Child := File.Child;

         while Child /= null loop
            Bind_Default_Key_Internal
              (Manager,
               Action      => Get_Attribute (Child, "action"),
               Default_Key => Child.Value.all,
               Changed     => True);
            Child := Child.Next;
         end loop;

         Free (File);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Macro_Command_Handler;

   -----------------
   -- Find_Parent --
   -----------------

   function Find_Parent
     (Model : Gtk_Tree_Store; Context : String) return Gtk_Tree_Iter
   is
      Parent : Gtk_Tree_Iter := Get_Iter_First (Model);
   begin
      while Parent /= Null_Iter loop
         if Get_String (Model, Parent, Action_Column) = Context then
            return Parent;
         end if;
         Next (Model, Parent);
      end loop;

      return Null_Iter;
   end Find_Parent;

   -----------------
   -- Find_Parent --
   -----------------

   function Find_Parent
     (Model : Gtk_Tree_Store; Filter : Action_Filter) return Gtk_Tree_Iter is
   begin
      if Filter = null or else Get_Name (Filter) = "" then
         return Find_Parent (Model, -"General");
      else
         return Find_Parent (Model, Get_Name (Filter));
      end if;
   end Find_Parent;

   ---------
   -- Set --
   ---------

   function Set
     (Model   : Gtk_Tree_Store;
      Parent  : Gtk_Tree_Iter;
      Descr   : String;
      Changed : Boolean := False;
      Key     : String := "") return Gtk_Tree_Iter
   is
      procedure Internal
        (Tree, Iter : System.Address;
         Col1  : Gint; Value1 : String;
         Col2  : Gint; Value2 : String;
         Col3  : Gint; Value3 : Gboolean;
         Final : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

      Iter : Gtk_Tree_Iter;
   begin
      Append (Model, Iter, Parent);
      Internal
        (Get_Object (Model), Iter'Address,
         Col1 => Action_Column,  Value1 => Descr & ASCII.NUL,
         Col2 => Key_Column,     Value2 => Key & ASCII.NUL,
         Col3 => Changed_Column, Value3 => Boolean'Pos (Changed));
      return Iter;
   end Set;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class) is
      Menu_Iter : Gtk_Tree_Iter;
      Handler   : constant Key_Manager_Access := Keymanager_Module.Key_Manager;

      procedure Process_Menu_Binding
        (Data       : System.Address;
         Accel_Path : String;
         Accel_Key  : Gdk.Types.Gdk_Key_Type;
         Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
         Changed    : Boolean);
      --  Called for each key binding associated with menus

      procedure Process_Table
        (Table : in out Key_Htable.HTable; Prefix : String);
      --  Process the contents of a specific keymap. This sets the keybindings
      --  for all associated actions

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
         pragma Unreferenced (Data, Changed, Iter);
         First : constant Natural := Accel_Path'First;
      begin
--  Temporarily commented out, we'll show the leading <gps> prefix anyway.
--  We need it to update the menus dynamically on exit, and GPS uses both
--  <gps> and <gtkada>. An extra column should be added to store this prefix
--           while First <= Accel_Path'Last
--             and then Accel_Path (First) /= '/'
--           loop
--              First := First + 1;
--           end loop;

         if Accel_Key /= 0 then
            Iter := Set (Model  => Editor.Model,
                         Parent => Menu_Iter,
                         Descr  => Accel_Path (First .. Accel_Path'Last),
                         Changed => False,
                         Key    => Image (Accel_Key, Accel_Mods));
         else
            Iter := Set (Model  => Editor.Model,
                         Parent => Menu_Iter,
                         Descr  => Accel_Path (First .. Accel_Path'Last),
                         Changed => False,
                         Key    => -Disabled_String);
         end if;
      end Process_Menu_Binding;

      -------------------
      -- Process_Table --
      -------------------

      procedure Process_Table
        (Table : in out Key_Htable.HTable; Prefix : String)
      is
         Iter    : Key_Htable.Iterator;
         Binding : Key_Description_List;
         Parent  : Gtk_Tree_Iter;
         Action  : Action_Record;
      begin
         Get_First (Table, Iter);
         loop
            Binding := Get_Element (Iter);
            exit when Binding = No_Key;

            while Binding /= null loop
               if Binding.Action = null then
                  Process_Table
                    (Get_Keymap (Binding).Table,
                     Prefix
                     & Image (Get_Key (Iter).Key,
                              Get_Key (Iter).Modifier) & ' ');

               else
                  Action := Lookup_Action (Handler.Kernel, Binding.Action.all);
                  if Action /= No_Action then
                     Parent := Find_Parent (Editor.Model, Action.Filter);
                     if Parent /= Null_Iter then
                        Parent := Children (Editor.Model, Parent);
                        while Parent /= Null_Iter loop
                           if Get_String (Editor.Model, Parent, Action_Column)
                             = Binding.Action.all
                           then
                              Set
                                (Editor.Model,
                                 Parent,
                                 Key_Column,
                                 Prefix
                                 & Image (Get_Key (Iter).Key,
                                          Get_Key (Iter).Modifier));
                              Set
                                (Editor.Model,
                                 Parent,
                                 Changed_Column,
                                 Binding.Changed);
                              exit;
                           end if;
                           Next (Editor.Model, Parent);
                        end loop;
                     end if;
                  end if;
               end if;
               Binding := Next (Binding);
            end loop;

            Get_Next (Table, Iter);
         end loop;
      end Process_Table;

      Sort_Id     : constant Gint := Freeze_Sort (Editor.Model);
      Parent      : Gtk_Tree_Iter;
      Action      : Action_Record;
      Action_Iter : Action_Iterator := Start (Editor.Kernel);
   begin
      Clear (Editor.Model);

      Menu_Iter := Set (Editor.Model, Null_Iter, -Menu_Context_Name);

      Gtk.Accel_Map.Foreach
        (System.Null_Address, Process_Menu_Binding'Unrestricted_Access);

      --  Add all actions in the table
      loop
         Action := Get (Action_Iter);
         exit when Action = No_Action;

         Parent := Find_Parent (Editor.Model, Action.Filter);
         if Parent = Null_Iter then
            if Action.Filter = null or else Get_Name (Action.Filter) = "" then
               Parent := Set (Editor.Model, Null_Iter, -"General");
            else
               Parent := Set
                 (Editor.Model, Null_Iter,
                  Get_Name (Action.Filter));
            end if;
         end if;

         Parent := Set
           (Model   => Editor.Model,
            Parent  => Parent,
            Descr   => Get (Action_Iter),
            Changed => False,
            Key     => -Disabled_String);
         Next (Editor.Kernel, Action_Iter);
      end loop;

      --  Add the key bindings definition
      Process_Table (Handler.Table, "");

      Thaw_Sort (Editor.Model, Sort_Id);
   end Fill_Editor;

   -----------------
   -- Save_Editor --
   -----------------

   procedure Save_Editor (Editor : access Keys_Editor_Record'Class) is
      Handler   : constant Key_Manager_Access := Keymanager_Module.Key_Manager;
      Context_Iter : Gtk_Tree_Iter := Get_Iter_First (Editor.Model);
      Child        : Gtk_Tree_Iter;
      Key          : Gdk_Key_Type;
      Modif        : Gdk_Modifier_Type;
   begin
      Reset (Handler.Table);

      while Context_Iter /= Null_Iter loop
         --  Special handling for menus

         if Get_String (Editor.Model, Context_Iter, Action_Column) =
           -Menu_Context_Name
         then
            Child := Children (Editor.Model, Context_Iter);
            while Child /= Null_Iter loop
               declare
                  Str : constant String :=
                    Get_String (Editor.Model, Child, Key_Column);
               begin
                  if Str /= -Disabled_String then
                     Value (Str, Key, Modif);
                     Change_Entry
                       (Accel_Path =>
                          Get_String (Editor.Model, Child, Action_Column),
                        Accel_Key  => Key,
                        Accel_Mods => Modif,
                        Replace    => True);
                  else
                     Change_Entry
                       (Accel_Path =>
                          Get_String (Editor.Model, Child, Action_Column),
                        Accel_Key  => 0,
                        Accel_Mods => 0,
                        Replace    => True);
                  end if;
               end;
               Next (Editor.Model, Child);
            end loop;

         --  Standard key bindings
         else
            Child := Children (Editor.Model, Context_Iter);
            while Child /= Null_Iter loop
               declare
                  Str : constant String :=
                    Get_String (Editor.Model, Child, Key_Column);
                  Changed : constant Boolean := Get_Boolean
                    (Editor.Model, Child, Changed_Column);
               begin
                  if Str /= -Disabled_String or else Changed then
                     Bind_Default_Key_Internal
                       (Handler,
                        Action       =>
                          Get_String (Editor.Model, Child, Action_Column),
                        Default_Key  => Str,
                        Changed      => Changed);
                  end if;
               end;
               Next (Editor.Model, Child);
            end loop;
         end if;

         Next (Editor.Model, Context_Iter);
      end loop;
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

      declare
         K : constant String := Grabbed.all;
      begin
         Free (Grabbed);
         return K;
      end;
   end Grab_Multiple_Key;

   -----------------
   -- On_Grab_Key --
   -----------------

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Keys_Editor := Keys_Editor (Editor);
      Handler   : constant Key_Manager_Access := Keymanager_Module.Key_Manager;
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)

      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Handler.Active := False;

         declare
            Is_Menu : constant Boolean := Get_String
              (Model, Parent (Model, Iter), Action_Column) =
              -Menu_Context_Name;
            Key     : constant String := Grab_Multiple_Key
              (Ed.View, Allow_Multiple => not Is_Menu);

         begin
            if Key /= "" then
               Set (Ed.Model, Iter, Key_Column, Key);
               Set (Ed.Model, Iter, Changed_Column, True);
            end if;
         end;

         Handler.Active := True;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end On_Grab_Key;

   -------------------
   -- On_Remove_Key --
   -------------------

   procedure On_Remove_Key (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Keys_Editor := Keys_Editor (Editor);
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)

      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Set (Ed.Model, Iter, Key_Column, -Disabled_String);
         Set (Ed.Model, Iter, Changed_Column, True);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end On_Remove_Key;

   ---------------------------
   -- Add_Selection_Changed --
   ---------------------------

   procedure Add_Selection_Changed (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Keys_Editor := Keys_Editor (Editor);
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Action    : Action_Record;

   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)
      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Action := Lookup_Action (Ed.Kernel, Get_String (Model, Iter, 0));

         if Action.Description /= null then
            Set_Text (Ed.Help, Action.Description.all);
         else
            Set_Text (Ed.Help, "");
         end if;

         Set_Text (Ed.Action_Name, Get_String (Model, Iter, 0));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Add_Selection_Changed;

   ------------------
   -- On_Edit_Keys --
   ------------------

   procedure On_Edit_Keys
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Editor   : Keys_Editor;
      Scrolled : Gtk_Scrolled_Window;
      Bbox     : Gtk_Vbutton_Box;
      Box, Hbox : Gtk_Box;
      Button   : Gtk_Button;
      Col      : Gtk_Tree_View_Column;
      Render   : Gtk_Cell_Renderer_Text;
      Num      : Gint;
      Frame    : Gtk_Frame;
      Pane     : Gtk_Paned;
      Sep      : Gtk_Separator;
      Event    : Gtk_Event_Box;
      Color    : Gdk_Color;
      Text     : Gtk_Text_View;
      Action   : Gtk_Widget;
      pragma Unreferenced (Widget, Num, Action);

   begin
      Editor := new Keys_Editor_Record;
      Initialize
        (Editor,
         Title  => -"Key shortcuts",
         Parent => Get_Main_Window (Kernel),
         Flags  => Destroy_With_Parent or Modal);
      Set_Default_Size (Editor, 640, 480);
      Editor.Kernel  := Kernel;

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Editor), Box);

      Gtk_New_Vpaned (Pane);
      Pack_Start (Box, Pane);

      --  List of macros

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack1 (Pane, Scrolled, True, True);

      Gtk_New
        (Editor.Model,
         (Action_Column  => GType_String,
          Key_Column     => GType_String,
          Changed_Column => GType_Boolean));
      Gtk_New (Editor.View, Editor.Model);
      Add (Scrolled, Editor.View);

      --  Bottom area
      Gtk_New (Frame);
      Pack2 (Pane, Frame, False, False);

      Gtk_New_Vbox (Hbox, Homogeneous => False);
      Add (Frame, Hbox);

      --  Name of current action

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Color := Parse ("#0e79bd");
      --  ??? Should be shared with the preferences dialog and wizard
      Alloc (Get_Default_Colormap, Color);
      Set_Style (Event, Copy (Get_Style (Event)));
      Set_Background (Get_Style (Event), State_Normal, Color);

      Gtk_New (Editor.Action_Name, "Current action");
      Set_Alignment (Editor.Action_Name, 0.1, 0.5);
      Add (Event, Editor.Action_Name);

      Gtk_New_Hseparator (Sep);
      Pack_Start (Hbox, Sep, Expand => False);

      --  Help on current action

      Gtk_New (Editor.Help);
      Gtk_New (Scrolled);
      Pack_Start (Hbox, Scrolled, Expand => True, Fill => True);

      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Gtk_New (Text, Editor.Help);
      Set_Wrap_Mode (Text, Wrap_Word);
      Add (Scrolled, Text);

      --  Buttons area

      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Start);
      Pack_Start (Box, Bbox, Expand => False);

      Gtk_New_From_Stock (Button, Stock_Remove);
      Pack_Start (Bbox, Button);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_Remove_Key'Access),
         Editor);

      Gtk_New (Button, -"Grab");
      Pack_Start (Bbox, Button);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_Grab_Key'Access),
         Editor);

      Widget_Callback.Object_Connect
        (Get_Selection (Editor.View), "changed",
         Widget_Callback.To_Marshaller (Add_Selection_Changed'Access),
         Editor);

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
      Add_Attribute (Col, Render, "text", Key_Column);
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

      Destroy (Editor);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level, File);
      N : Node_Ptr := Node;
   begin
      while N /= null loop
         if N.Tag.all = "key" then
            declare
               Action : constant String := Get_Attribute (N, "action");
            begin
               if Action = "" then
                  Insert (Kernel, -"<key> nodes must have an action attribute",
                          Mode => Error);
                  raise Assert_Failure;
               end if;

               if N.Value = null then
                  Insert (Kernel,
                            -"Invalid key binding for action " & Action,
                          Mode => Error);
                  raise Assert_Failure;
               end if;

               if N.Child /= null then
                  Insert
                    (Kernel,
                     -"Invalid child node for <key> tag", Mode => Error);
                  raise Assert_Failure;
               end if;

               Bind_Default_Key
                 (Keymanager_Module.Key_Manager,
                  Action      => Action,
                  Default_Key => N.Value.all);
            end;
         end if;

         N := N.Next;
      end loop;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Manager    : constant Key_Manager_Access := new Key_Manager_Record;
      Macro_Menu : constant String := "/" & (-"Tools/Macro");
      Edit_Menu  : constant String := "/" & (-"Edit");

   begin
      Manager.Kernel := Kernel_Handle (Kernel);
      Load_Custom_Keys (Kernel, Manager);

      Keymanager_Module := new Keymanager_Module_Record;
      Keymanager_Module.Key_Manager := Manager;
      Register_Module
        (Module_ID (Keymanager_Module), Kernel, "keymanager",
         Customization_Handler => Customize'Access);

      Event_Handler_Set
        (General_Event_Handler'Access,
         Convert (Kernel_Handle (Kernel)));

      Register_Menu
        (Kernel, Edit_Menu, -"_Key shortcuts",
         Callback => On_Edit_Keys'Access);

      --  ??? For now disable these menus since this is a work in progress

      if False then
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
      end if;

      Register_Command
        (Kernel,
         Command      => "macro_play",
         Params       => Parameter_Names_To_Usage (Play_Macro_Cmd_Parameters),
         Description  => -"Play current set of events.",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => Macro_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "macro_record",
         Description  => -"Start recording set of events.",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Macro_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "macro_load",
         Params       =>
           Parameter_Names_To_Usage (Load_Macro_Cmd_Parameters, 1),
         Description  => -"Load file containing a set of recorded events.",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Macro_Command_Handler'Access);
   end Register_Module;

end KeyManager_Module;
