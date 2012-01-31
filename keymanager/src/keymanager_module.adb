------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with System.Assertions;       use System.Assertions;

with Gdk.Event;               use Gdk.Event;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Gdk.Types;               use Gdk.Types;
with Gdk.Window;              use Gdk.Window;

with Glib.Object;             use Glib.Object;
with Glib;                    use Glib;

with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Accel_Map;           use Gtk.Accel_Map;
with Gtk.Main;                use Gtk.Main;
with Gtk.Window;              use Gtk.Window;
with Gtk.Widget;              use Gtk.Widget;

with Gtkada.MDI;              use Gtkada.MDI;
with Gtkada.Dialogs;          use Gtkada.Dialogs;

with Config;                  use Config;
with Commands.Interactive;    use Commands, Commands.Interactive;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Kernel;              use GPS.Kernel;
with GUI_Utils;               use GUI_Utils;
with HTables;                 use HTables;
with KeyManager_Module.GUI;
with Traces;

with XML_Utils;               use XML_Utils;
with XML_Parsers;

package body KeyManager_Module is

   Me : constant Trace_Handle := Create ("Keymanager", GNATCOLL.Traces.Off);
   Event_Debug_Trace : constant Trace_Handle := Create
     ("Event_Debug", GNATCOLL.Traces.Off);

   use Key_Htable;

   Command_Cst : aliased constant String := "command";
   Key_Cst     : aliased constant String := "key";
   Count_Cst   : aliased constant String := "count";

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Key_Description, Key_Description_List);

   procedure Free_Non_Recursive (Element : in out Key_Description_List);
   --  Free Element, but not its sibling.
   --  Warning: this breaks the list in which Element was, since the previous
   --  element will still point to Element.

   procedure Clone
     (From : Key_Description_List; To : out Key_Description_List);
   --  Deep-copy of From

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Keymap_Record, Keymap_Access);

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

   type Event_Handler_Record;
   type Event_Handler_Access is access all Event_Handler_Record;
   type Event_Handler_Record is record
      Handler : General_Event_Handler_Callback;
      Next    : Event_Handler_Access;
   end record;

   procedure General_Event_Handler
     (Event : Gdk_Event; Kernel : System.Address);
   --  General event handler for GPS
   pragma Convention (C, General_Event_Handler);

   procedure Debug_Event_Handler
     (Event : Gdk_Event; Kernel : System.Address);
   --  General event handler used for event-level debugging
   pragma Convention (C, Debug_Event_Handler);

   type Keymanager_Module_Record is new Module_ID_Record with record
      Handlers         : Event_Handler_Access;

      Table            : HTable_Access;

      Custom_Keys_Loaded : Boolean := False;
      --  Whether the user's custom keys have been loaded

      Secondary_Keymap : Keymap_Access := null;
      --  The secondary keymap currently in use, or null if using the primary

      Active           : Boolean := True;
      --  Whether the key manager should process the key events. This is only
      --  deactivated while editing the key bindings through the GUI.

      Menus_Created : Boolean := False;
      --  Indicates whether the initial set of menus has been created

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

      GUI_Running : Boolean := False;
      --  Whether the GUI is currently running. This affect the locations
      --  where messages are displayed.
   end record;
   type Keymanager_Module_ID is access all Keymanager_Module_Record'Class;

   overriding procedure Customize
     (Module : access Keymanager_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   overriding procedure Destroy (Module : in out Keymanager_Module_Record);
   --  See doc for inherited subprogram

   --  ??? Global variable, could be queries from the kernel
   Keymanager_Module : Keymanager_Module_ID;

   function Process_Event
     (Kernel  : access Kernel_Handle_Record'Class;
      Event   : Gdk_Event) return Boolean;
   --  Process the event and call the appropriate actions if needed

   procedure Get_Secondary_Keymap
     (Table  : in out Key_Htable.Instance;
      Key    : Gdk_Key_Type;
      Modif  : Gdk_Modifier_Type;
      Keymap : out Keymap_Access);
   --  Get or create a secondary keymap in Table

   pragma Warnings (Off);
   --  These two UCs are safe aliasing-wise, so kill warning
   function Convert is new Ada.Unchecked_Conversion
     (Kernel_Handle, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Kernel_Handle);
   pragma Warnings (On);

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   procedure Keymanager_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Process shell commands associated with this module

   type Repeat_Next_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   overriding function Execute
     (Command : access Repeat_Next_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   procedure On_Repeat_Next_Argument_Read
     (Command  : Interactive_Command'Class;
      Argument : String);
   --  This command reads a numeric argument, and will then execute the next
   --  action a number of times.

   procedure Error_Message
     (Kernel  : access Kernel_Handle_Record'Class;
      Message : String);
   --  Emit a message in the context of the key manager shortcut, sending
   --  it to the key manager GUI if present, and to the Messages window if not.

   procedure Set_Default_Key
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Accel_Key  : Natural;
      Accel_Mods : Natural);
   --  If the Action doesn't already have a key binding, then bind it to
   --  Accel_Key/Accel_Mods.

   ----------------------
   -- Save_Custom_Keys --
   ----------------------

   procedure Save_Custom_Keys (Kernel : access Kernel_Handle_Record'Class) is
      Filename : constant Virtual_File :=
                   Create_From_Dir (Get_Home_Dir (Kernel), "keys.xml");
      File     : Node_Ptr;
      Success  : Boolean;

      procedure Save_Table
        (Table       : in out Key_Htable.Instance;
         Prefix      : String;
         N, Level    : Positive;
         More_Levels : in out Boolean);
      --  Save the contents of a specific keymap

      ----------------
      -- Save_Table --
      ----------------

      procedure Save_Table
        (Table       : in out Key_Htable.Instance;
         Prefix      : String;
         N, Level    : Positive;
         More_Levels : in out Boolean)
      is
         Child   : Node_Ptr;
         Iter    : Key_Htable.Cursor;
         Binding : Key_Description_List;
      begin
         Get_First (Table, Iter);
         loop
            Binding := Get_Element (Iter);
            exit when Binding = No_Key;

            Save_Binding : while Binding /= null loop
               if Binding.Changed
                 and then Binding.Action /= null
                 and then N = Level
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

                  Add_Child (File, Child, Append => True);

               elsif Binding.Action = null then
                  if Binding.Keymap /= null then
                     if N < Level then
                        Save_Table
                          (Binding.Keymap.Table,
                           Prefix & Image
                             (Get_Key (Iter).Key,
                              Get_Key (Iter).Modifier) & ' ',
                           N + 1, Level, More_Levels);
                     else
                        More_Levels := True;
                        exit Save_Binding;
                     end if;
                  end if;
               end if;

               Binding := Binding.Next;
            end loop Save_Binding;

            Get_Next (Table, Iter);
         end loop;
      end Save_Table;

   begin
      File     := new Node;
      File.Tag := new String'("Keys");

      declare
         Level       : Positive := 1;
         More_Levels : Boolean;
      begin
         loop
            More_Levels := False;
            Save_Table
              (Keymanager_Module.Table.all, "", 1, Level, More_Levels);
            exit when not More_Levels;
            Level := Level + 1;
         end loop;
      end;

      Trace (Me, "Saving " & Filename.Display_Full_Name);
      Print (File, Filename, Success);
      Free (File);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      end if;
   end Save_Custom_Keys;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Keymanager_Module_Record) is
   begin
      Reset (Module.Table.all);
      Unchecked_Free (Module.Table);
      Keymanager_Module := null;
   end Destroy;

   -----------------------
   -- Add_Event_Handler --
   -----------------------

   procedure Add_Event_Handler
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Handler : General_Event_Handler_Callback)
   is
      pragma Unreferenced (Kernel);
   begin
      Keymanager_Module.Handlers := new Event_Handler_Record'
        (Handler => Handler,
         Next    => Keymanager_Module.Handlers);
   end Add_Event_Handler;

   --------------------------
   -- Remove_Event_Handler --
   --------------------------

   procedure Remove_Event_Handler
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Handler : General_Event_Handler_Callback)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Event_Handler_Record, Event_Handler_Access);
      pragma Unreferenced (Kernel);
      Tmp : Event_Handler_Access := Keymanager_Module.Handlers;
      N   : Event_Handler_Access;
   begin
      if Tmp.Handler = Handler then
         Keymanager_Module.Handlers := Tmp.Next;
         Unchecked_Free (Tmp);

      else
         while Tmp.Next /= null loop
            if Tmp.Next.Handler = Handler then
               N := Tmp.Next;
               Tmp.Next := Tmp.Next.Next;
               Unchecked_Free (N);
               exit;
            end if;
            Tmp := Tmp.Next;
         end loop;
      end if;
   end Remove_Event_Handler;

   ---------------------------
   -- General_Event_Handler --
   ---------------------------

   procedure General_Event_Handler
     (Event : Gdk_Event; Kernel : System.Address)
   is
      Event_Type : constant Gdk_Event_Type := Get_Event_Type (Event);

   begin
      if Keymanager_Module = null then
         --  This can happen when GPS is exiting and modules have been
         --  deallocated already.
         Gtk.Main.Do_Event (Event);
         return;
      end if;

      Call_Handlers : declare
         EH : Event_Handler_Access := Keymanager_Module.Handlers;
      begin
         while EH /= null
           and then not EH.Handler (Event, Convert (Kernel))
         loop
            EH := EH.Next;
         end loop;
      end Call_Handlers;

      if Event_Type = Key_Press or else Event_Type = Key_Release then
         --  Check that the current input window is not modal.
         --  In the case that we have a modal dialog, we do not want to
         --  interpret key shortcuts. For instance, if a "Continue Search?"
         --  dialog is open, and the user hits ctrl-n, we don't want
         --  another "Continue Search?" dialog to appear.

         declare
            Current : constant Gtk_Widget := Grab_Get_Current;
         begin
            if Current = null
              or else not Get_Modal (Gtk_Window (Get_Toplevel (Current)))
            then
               if Process_Event (Convert (Kernel), Event) then
                  return;
               end if;
            end if;
         end;

      elsif Event_Type = Button_Release then
         --  The command will be executed by gtk, we don't know exactly how
         if Keymanager_Module.Last_Command /= null then
            Free (Keymanager_Module.Last_User_Command);
         end if;
         Keymanager_Module.Last_Command := null;
      end if;

      --  Dispatch the event in the standard gtk+ main loop
      Gtk.Main.Do_Event (Event);

   --  We do not put a global exception handler in this procedure since
   --  it is called very often, so when using setjmp/longjmp, the cost
   --  may not be negligible.

   end General_Event_Handler;

   -------------------------
   -- Debug_Event_Handler --
   -------------------------

   procedure Break_Me_Configure;
   procedure Break_Me_Configure is
   begin
      null;
   end Break_Me_Configure;

   procedure Break_Me_State;
   procedure Break_Me_State is
   begin
      null;
   end Break_Me_State;

   procedure Debug_Event_Handler
     (Event : Gdk_Event; Kernel : System.Address)
   is
      Event_Type : constant Gdk_Event_Type := Get_Event_Type (Event);
   begin
      case Event_Type is
         when Configure =>
            Break_Me_Configure;
         when Window_State =>
            Break_Me_State;
         when others =>
            null;
      end case;

      Trace (Event_Debug_Trace, Event_Type'Img);
      General_Event_Handler (Event, Kernel);

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end Debug_Event_Handler;

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
      N : Key_Description_List;
   begin
      while Element /= null loop
         N := Element.Next;
         Free_Non_Recursive (Element);
         Element := N;
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
     (From : Key_Htable.Instance; To : out Key_Htable.Instance)
   is
      Iter : Key_Htable.Cursor;
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
      Table                                : in out Key_Htable.Instance;
      Action                               : String;
      Key                                  : String;
      Save_In_Keys_XML                     : Boolean;
      Remove_Existing_Shortcuts_For_Action : Boolean;
      Remove_Existing_Actions_For_Shortcut : Boolean;
      Update_Menus                         : Boolean)
  is
      procedure Bind_Internal
        (Table       : in out Key_Htable.Instance;
         Default_Key : Gdk.Types.Gdk_Key_Type;
         Default_Mod : Gdk.Types.Gdk_Modifier_Type);
      --  Internal version that allows setting the Changed attribute

      procedure Remove_In_Keymap (Table : in out Key_Htable.Instance);
      --  Remove all bindings to Action in Table and its secondary keymaps

      -------------------
      -- Bind_Internal --
      -------------------

      procedure Bind_Internal
        (Table       : in out Key_Htable.Instance;
         Default_Key : Gdk.Types.Gdk_Key_Type;
         Default_Mod : Gdk.Types.Gdk_Modifier_Type)
      is
         Tmp, Binding3, Binding2 : Key_Description_List;
         Success : Boolean;
         pragma Unreferenced (Success);
      begin
         if not Remove_Existing_Actions_For_Shortcut
           or else (Default_Key = 0 and then Default_Mod = 0)
         then
            Binding3 := Get (Table, Key_Binding'(Default_Key, Default_Mod));

            --  Check whether the same action is already attached to this key.
            --  ??? When we have a menu, we should check the underlying action
            --  if there is any.
            --  ??? Not needed if we also have
            --  Remove_Existing_Shortcuts_For_Action
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
         if Action /= "" then
            if Binding3 /= null then
               Clone (From => Binding3, To => Tmp);
            else
               Tmp := null;
            end if;

            Binding2 := new Key_Description'
              (Action  => new String'(Action),
               Changed => Save_In_Keys_XML,
               Keymap  => null,
               Next    => Tmp);
            Set (Table, Key_Binding'(Default_Key, Default_Mod), Binding2);
         else
            Remove (Table, Key_Binding'(Default_Key, Default_Mod));
         end if;
      end Bind_Internal;

      ----------------------
      -- Remove_In_Keymap --
      ----------------------

      procedure Remove_In_Keymap (Table : in out Key_Htable.Instance) is
         Iter                : Key_Htable.Cursor;
         List, Previous, Tmp : Key_Description_List;
         Move_To_Next        : Boolean;
      begin
         Get_First (Table, Iter);
         while Get_Element (Iter) /= null loop
            List := Get_Element (Iter);
            Move_To_Next := True;

            Previous := null;
            while List /= null loop
               if List.Keymap /= null then
                  Remove_In_Keymap (List.Keymap.Table);
                  Previous := List;
                  List := List.Next;

               elsif List.Action /= null
                 and then Equal
                   (List.Action.all, Action, Case_Sensitive => False)
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
                        --  There was a single element with this key binding.
                        --  We need to remove it from the table (otherwise that
                        --  keybinding will remain unusable for the rest of the
                        --  session, since GPS would believe it is associated
                        --  with a secondary keymap), but we cannot do that
                        --  directly or that would invalidate the iterator.
                        Free (List.Action);
                        Remove_And_Get_Next (Table, Iter);
                        Move_To_Next := False;
                        List := null;
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

            if Move_To_Next then
               Get_Next (Table, Iter);
            end if;
         end loop;
      end Remove_In_Keymap;

      Partial_Key           : Gdk_Key_Type;
      Modif, Mnemonic_Modif : Gdk_Modifier_Type;
      First, Last           : Integer;
      Keymap                : Keymap_Access;
      Success               : Boolean;
      pragma Unreferenced (Success);

   begin
      --  Are we trying to cancel all bindings to Action ?
      if Remove_Existing_Shortcuts_For_Action
        or else Key = "" or else Key = -Disabled_String
      then
         Remove_In_Keymap (Table);
      end if;

      --  Systematically remove the accel binding when updating menus: the
      --  binding will be set again in the call to Change_Entry further below.
      --  Not doing that may cause the call to Change_Entry to return False if
      --  the keys were already set, in which case we will fall back on
      --  removing the shortcut altogether.
      if Update_Menus
        and then Action /= ""
        and then Action (Action'First) = '/'
      then
         --  Guess the accel path from the menu
         Success := Change_Entry
           ("<gps>" & Action,
            Accel_Key  => 0,
            Accel_Mods => 0,
            Replace    => True);
      end if;

      --  On Windows binding contol-c to non default copy action can result in
      --  unexpected behavior.

      if Config.Host = Windows
        and then Key = "control-c"
        and then Action /= ""
        and then Action /= "/Edit/Copy"
        and then Action /= "Copy to Clipboard"
      then
         Console.Insert
           (Kernel, -("Warning: binding Ctrl-C is unreliable on Windows,"
            & " external actions can have unexpected results."));
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
            if Keymap = null then
               Bind_Internal (Table, Partial_Key, Modif);

               if Update_Menus
                 and then Action /= ""
                 and then Action (Action'First) = '/'
               then
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
                     --  If we couldn't change the entry, at very least disable
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
            --  We are defining a multiple-stroke key shortcut (for instance
            --  "control-x a".
            --  Check that the prefix (in the example, "control-x") is not
            --  already bound to an action. If it is, display an error
            --  message and return.

            declare
               B : Key_Description_List;
            begin
               if Keymap = null then
                  B := Get (Table, Key_Binding'(Partial_Key, Modif));
                  Get_Secondary_Keymap (Table, Partial_Key, Modif, Keymap);
               else
                  B := Get (Keymap.Table, Key_Binding'(Partial_Key, Modif));
                  Get_Secondary_Keymap
                    (Keymap.Table, Partial_Key, Modif, Keymap);
               end if;

               while B /= null loop
                  if B.Action /= null then
                     if Remove_Existing_Actions_For_Shortcut then
                        --  This prefix is already used but we asked for
                        --  removing existing actions.

                        Free (B.Action);

                     else
                        --  Cannot associate action as key shortcut already
                        --  used.
                        Error_Message
                          (Kernel,
                           (-"Cannot use key shortcut <") & Key
                           & (-"> for action """) & Action & """:" & ASCII.LF
                           & (-"prefixing key shortcut <")
                           & Key (Key'First .. Last - 1)
                           & (-"> is already bound to action """)
                           & B.Action.all & """.");
                        return;
                     end if;
                  end if;

                  B := B.Next;
               end loop;
            end;
         end if;

         First := Last + 1;
      end loop;
   end Bind_Default_Key_Internal;

   --------------------------
   -- Get_Secondary_Keymap --
   --------------------------

   procedure Get_Secondary_Keymap
     (Table  : in out Key_Htable.Instance;
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

         while Binding2 /= null and then Binding2.Keymap = null loop
            Binding  := Binding2;  --  Last value where Next /= null
            Binding2 := Binding2.Next;
         end loop;

         --  If there is no secondary keymap yet, create one
         if Binding2 = null then
            Keymap := new Keymap_Record;
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
     (Kernel   : access Kernel_Handle_Record'Class;
      Event    : Gdk.Event.Gdk_Event) return Boolean
   is
      Key              : Gdk_Key_Type;
      Modif            : Gdk_Modifier_Type;
      Binding          : Key_Description_List;
      Command          : Action_Record_Access;
      Has_Secondary    : constant Boolean :=
                           Keymanager_Module.Secondary_Keymap /= null;
      Context          : Selection_Context;
      Context_Computed : Boolean := False;
      Found_Action     : Boolean := False;
      Child            : GPS_MDI_Child;

      procedure Compute_Context;
      --  Compute the current context if not done already

      procedure Compute_Child;
      --  Compute the child that currently has the focus. If no such child, or
      --  this isn't a GPS_MDI_Child, null is set.

      procedure Undo_Group (Start : Boolean);
      --  Start or end an undo group

      ---------------------
      -- Compute_Context --
      ---------------------

      procedure Compute_Context is
      begin
         if not Context_Computed then
            Context := Get_Current_Context (Kernel);
            Context_Computed := True;
         end if;
      end Compute_Context;

      -------------------
      -- Compute_Child --
      -------------------

      procedure Compute_Child is
         C : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
      begin
         if C /= null and then C.all in GPS_MDI_Child_Record'Class then
            Child := GPS_MDI_Child (C);
         end if;
      end Compute_Child;

      ----------------
      -- Undo_Group --
      ----------------

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

      if Keymanager_Module.Active
        and then Get_Event_Type (Event) = Key_Press
      then
         --  Remove any num-lock and caps-lock modifiers
         Modif := Get_State (Event) and Get_Default_Mod_Mask;
         Key   := Get_Key_Val (Event);

         Trace (Me, "Key=" & Key'Img & " Modif=" & Modif'Img);

         --  If we are pressing down CTRL, enter Hyper Mode

         if Key = GDK_Control_L
           or Key = GDK_Control_R
         then
            Enter_Hyper_Mode (Kernel);
         end if;

         --  Are we reading arguments for a command ?

         if Keymanager_Module.Argument_Validator /= null then
            if (Modif /= 0 or else Key /= GDK_Escape)
              and then Keymanager_Module.Argument_Validator (Key, Modif)
            then
               declare
                  Tmp : String_Access := Keymanager_Module.Argument_Current;
               begin
                  --  First a simple test: it is possible that Get_String
                  --  returns an empty string although the keyval is really
                  --  printable (this is the case from the automatic testsuite
                  --  for instance).
                  if Key >= 32 and then Key <= 128 then
                     Keymanager_Module.Argument_Current :=
                       new String'(Tmp.all & Character'Val (Key));
                  else
                     Keymanager_Module.Argument_Current :=
                       new String'(Tmp.all & Get_String (Event));
                  end if;
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
            Trace (Me, "Key is just a modifier, ignored");
            return False;
         end if;

         if Keymanager_Module.Secondary_Keymap = null then
            Binding := Get (Keymanager_Module.Table.all, (Key, Modif));
         else
            Binding := Get
              (Keymanager_Module.Secondary_Keymap.Table, (Key, Modif));
         end if;

         Keymanager_Module.Secondary_Keymap := null;

         --  Execute all commands bound to this key. The order is somewhat
         --  random, since it depends in what order the key shortcuts were
         --  defined.
         while Binding /= No_Key loop
            if Binding.Action = null then
               Trace (Me, "Checking secondary keymap");
               Keymanager_Module.Secondary_Keymap := Binding.Keymap;
               Found_Action := True;

            else
               Trace (Me, "Checking action: " & Binding.Action.all);
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
                     if Active (Me) then
                        if Keymanager_Module.Repeat_Count > 1 then
                           Trace (Me, "Executing action "
                                  & Binding.Action.all
                                  & Keymanager_Module.Repeat_Count'Img
                                  & " times");
                        else
                           Trace (Me, "Executing action "
                                  & Binding.Action.all);
                        end if;
                     end if;

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
                              (Event            => Event,
                               Context          => Context,
                               Synchronous      => False,
                               Dir              => No_File,
                               Args             => null,
                               Label            => new String'
                                 (Binding.Action.all),
                               Repeat_Count     => R,
                               Remaining_Repeat =>
                                 Keymanager_Module.Repeat_Count - R)),
                           Destroy_On_Exit => True,
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

         if not Found_Action then
            Trace (Me, "No action was executed, falling though gtk+");
            --  The command will be executed by gtk, we don't know exactly how
            if Keymanager_Module.Last_Command /= null then
               Free (Keymanager_Module.Last_User_Command);
            end if;
            Keymanager_Module.Last_Command := null;

            --  To repeat this one, we need to requeue the event...
            --  No need to create an undo group, since these events are
            --  processed asynchronously anyway. The editor will properly merge
            --  editing actions into a single undo command anyway.
            for R in 2 .. Keymanager_Module.Repeat_Count loop
               declare
                  Ev : Gdk_Event;
               begin
                  Deep_Copy (From => Event, To => Ev);
                  Put (Ev);
               end;
            end loop;
            Keymanager_Module.Repeat_Count := 1;
         end if;

      elsif Get_Event_Type (Event) = Key_Release then
         Key   := Get_Key_Val (Event);

         --  If we are releasing CTRL, enter Hyper Mode

         if Key = GDK_Control_L or else Key = GDK_Control_R then
            Leave_Hyper_Mode (Kernel);
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
         Trace (Traces.Exception_Handle, E);
         return False;
   end Process_Event;

   ----------------------
   -- Load_Custom_Keys --
   ----------------------

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class)
   is
      Filename    : constant Virtual_File :=
                      Create_From_Dir (Get_Home_Dir (Kernel), "keys.xml");
      File, Child : Node_Ptr;
      Err         : String_Access;
      Prev        : Boolean;
   begin
      Keymanager_Module.Custom_Keys_Loaded := True;

      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename.Display_Full_Name);
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
                 (Kernel           => Kernel,
                  Table            => Keymanager_Module.Table.all,
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
         Trace (Traces.Exception_Handle, E);
         Insert (Kernel, -"Could not parse " &
                 Filename.Display_Full_Name, Mode => Error);
   end Load_Custom_Keys;

   ----------------------------
   -- Lookup_Key_From_Action --
   ----------------------------

   function Lookup_Key_From_Action
     (Table             : HTable_Access;
      Action            : String;
      Default           : String := "none";
      Use_Markup        : Boolean := True;
      Return_Multiple   : Boolean := True;
      Is_User_Changed   : access Boolean) return String
   is
      use Ada.Strings.Unbounded;
      Result : Ada.Strings.Unbounded.Unbounded_String;

      procedure Process_Table (Table : Key_Htable.Instance; Prefix : String);
      --  Process a specific binding table

      -------------------
      -- Process_Table --
      -------------------

      procedure Process_Table (Table : Key_Htable.Instance; Prefix : String) is
         Iter    : Key_Htable.Cursor;
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

                  elsif Prefix = "" then
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
   begin
      Is_User_Changed.all := False;
      Process_Table (Table.all, "");

      if Result = Null_Unbounded_String then
         return Default;
      else
         return To_String (Result);
      end if;
   end Lookup_Key_From_Action;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Keymanager_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
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
               if Node.Value /= null and then Node.Value.all /= "" then
                  Bind_Default_Key_Internal
                    (Kernel => Get_Kernel (Module.all),
                     Table  => Keymanager_Module.Table.all,
                     Action => "",
                     Key    => Node.Value.all,
                     Save_In_Keys_XML                     => False,
                     Remove_Existing_Shortcuts_For_Action => False,
                     Remove_Existing_Actions_For_Shortcut => True,
                     Update_Menus                         => True);
               end if;
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
            Bind_Default_Key_Internal
              (Table             => Keymanager_Module.Table.all,
               Kernel            => Get_Kernel (Module.all),
               Action            => Action,
               Remove_Existing_Shortcuts_For_Action => False,
               Remove_Existing_Actions_For_Shortcut => True,
               Save_In_Keys_XML  => False,
               Key               => Node.Value.all,
               Update_Menus      => True);
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
      Keymanager_Module.Active := False;
   end Block_Key_Shortcuts;

   ---------------------------
   -- Unblock_Key_Shortcuts --
   ---------------------------

   procedure Unblock_Key_Shortcuts
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Keymanager_Module.Active := True;
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
            Set_Return_Value (Data, String'(""));
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
                     Binding := Get (Keymanager_Module.Table.all,
                                     (Partial_Key, Modif));
                  else
                     Binding := Get (Keymap.Table, (Partial_Key, Modif));
                  end if;

                  while Binding /= No_Key loop
                     if Binding.Action /= null then
                        Set_Return_Value (Data, To_Lower (Binding.Action.all));
                     elsif Binding.Keymap /= null then
                        Set_Return_Value (Data, String'(""));
                     end if;
                     Binding := Binding.Next;
                  end loop;

               else
                  if Keymap = null then
                     Get_Secondary_Keymap
                       (Keymanager_Module.Table.all,
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
        and then
          (Key in GDK_KP_0 .. GDK_KP_9
           or else Key in GDK_0 .. GDK_9
           or else Key in GDK_Shift_L .. GDK_Shift_R);
   end Is_Numeric_Key;

   --------------------------
   -- Read_Action_Argument --
   --------------------------

   procedure Read_Action_Argument
     (Validator : Argument_Key_Validator;
      Callback  : Argument_Read_Callback;
      Command   : access Interactive_Command'Class) is
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

   overriding function Execute
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

   -------------------
   -- Get_Shortcuts --
   -------------------

   function Get_Shortcuts
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return HTable_Access
   is
      pragma Unreferenced (Kernel);
   begin
      return Keymanager_Module.Table;
   end Get_Shortcuts;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;

   begin
      Keymanager_Module := new Keymanager_Module_Record;
      Keymanager_Module.Table := new Key_Htable.Instance;

      Register_Module
        (Keymanager_Module, Kernel, "keymanager");

      if Active (Event_Debug_Trace) then
         Event_Handler_Set
           (Debug_Event_Handler'Access,
            Convert (Kernel_Handle (Kernel)));
      else
         Event_Handler_Set
           (General_Event_Handler'Access,
            Convert (Kernel_Handle (Kernel)));
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
           & " ""ctrl-u 30 t"" to  the character t 30 times"),
         Category => -"General");
      Bind_Default_Key (Kernel, "Repeat Next", "control-u");

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "key_manager.preferences_changed");

      Set_Key_Setter (Kernel, Set_Default_Key'Access);
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
      renames Standard.KeyManager_Module.GUI.Register_Key_Menu;

   -------------------
   -- Error_Message --
   -------------------

   procedure Error_Message
     (Kernel  : access Kernel_Handle_Record'Class;
      Message : String)
   is
      Dummy : Message_Dialog_Buttons;
      pragma Unreferenced (Dummy);
   begin
      if Keymanager_Module.GUI_Running then
         Dummy := Message_Dialog
           (Msg            => Message,
            Dialog_Type    => Error,
            Buttons        => Button_OK,
            Default_Button => Button_OK,
            Title          => -"Key binding error");
      else
         Insert (Kernel, Message, Mode => Error);
      end if;
   end Error_Message;

   ---------------------
   -- Set_GUI_Running --
   ---------------------

   procedure Set_GUI_Running (Running : Boolean) is
   begin
      Keymanager_Module.GUI_Running := Running;
   end Set_GUI_Running;

   ----------------------------
   -- Lookup_Action_From_Key --
   ----------------------------

   function Lookup_Action_From_Key
     (Key      : String;
      Bindings : HTable_Access) return String
   is
      Partial_Key : Gdk_Key_Type;
      Modif       : Gdk_Modifier_Type;
      First, Last : Integer;
      Keymap      : Keymap_Access;
      List        : Key_Description_List;

   begin
      First := Key'First;
      while First <= Key'Last loop
         Last := First + 1;
         while Last <= Key'Last and then Key (Last) /= ' ' loop
            Last := Last + 1;
         end loop;

         Value (Key (First .. Last - 1), Partial_Key, Modif);

         if Keymap = null then
            List := Get (Bindings.all, Key_Binding'(Partial_Key, Modif));
            Get_Secondary_Keymap (Bindings.all, Partial_Key, Modif, Keymap);
         else
            List := Get (Keymap.Table, Key_Binding'(Partial_Key, Modif));
            Get_Secondary_Keymap (Keymap.Table, Partial_Key, Modif, Keymap);
         end if;

         while List /= null loop
            if List.Action /= null then
               return List.Action.all;
            end if;

            List := List.Next;
         end loop;

         if Keymap = null then
            --  No secondary keymap, and no action => the key is not bound yet
            return "";
         end if;

         First := Last + 1;
      end loop;

      return "";
   end Lookup_Action_From_Key;

   -----------------------------
   -- Actions_With_Key_Prefix --
   -----------------------------

   function Actions_With_Key_Prefix
     (Key       : String;
      Bindings  : HTable_Access;
      Separator : Character := ASCII.LF) return String
   is
      use Ada.Strings.Unbounded;
      Partial_Key : Gdk_Key_Type;
      Modif       : Gdk_Modifier_Type;
      First, Last : Integer;
      Keymap      : Keymap_Access;
      List        : Key_Description_List;
      Result      : Unbounded_String;

      procedure Dump_Actions (Bindings : Key_Htable.Instance; Prefix : String);
      --  Dump all actions whose table is designated by Bindings

      ------------------
      -- Dump_Actions --
      ------------------

      procedure Dump_Actions
        (Bindings : Key_Htable.Instance; Prefix : String)
      is
         List : Key_Description_List;
         Pos  : Cursor;
         Key  : Key_Binding;
      begin
         Get_First (Bindings, Pos);
         List := Get_Element (Pos);

         while List /= null loop
            Key  := Get_Key (Pos);
            while List /= null loop
               if List.Action /= null then
                  Append
                    (Result,
                     List.Action.all
                     & " (" & Prefix & ' '
                     & Image (Key.Key, Key.Modifier) & ')' & Separator);
                  if List.Keymap /= null then
                     Dump_Actions
                       (List.Keymap.Table,
                        Prefix & ' ' & Image (Key.Key, Key.Modifier));
                  end if;
               end if;
               List := List.Next;
            end loop;

            Get_Next (Bindings, Pos);
            List := Get_Element (Pos);
         end loop;
      end Dump_Actions;

   begin
      First := Key'First;
      while First <= Key'Last loop
         Last := First + 1;
         while Last <= Key'Last and then Key (Last) /= ' ' loop
            Last := Last + 1;
         end loop;

         Value (Key (First .. Last - 1), Partial_Key, Modif);

         if Keymap = null then
            List := Get (Bindings.all, Key_Binding'(Partial_Key, Modif));
            Get_Secondary_Keymap (Bindings.all, Partial_Key, Modif, Keymap);
         else
            List := Get (Keymap.Table, Key_Binding'(Partial_Key, Modif));
            Get_Secondary_Keymap (Keymap.Table, Partial_Key, Modif, Keymap);
         end if;

            while List /= null loop
               if List.Action /= null then
                  Append
                    (Result,
                     List.Action.all
                     & " (" & Key (Key'First .. Last - 1) & ")" & Separator);
               end if;

               List := List.Next;
            end loop;

         if Last = Key'Last + 1 then
            if Keymap /= null then
               Dump_Actions (Keymap.Table, Key (First .. Last - 1));
               exit;
            end if;
         end if;

         First := Last + 1;
      end loop;

      return To_String (Result);
   end Actions_With_Key_Prefix;

   ---------------------
   -- Set_Default_Key --
   ---------------------

   procedure Set_Default_Key
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Accel_Key  : Natural;
      Accel_Mods : Natural)
   is
      pragma Unreferenced (Kernel);
      Keys  : Key_Description_List;
      Dummy : aliased Boolean;
   begin
      if Lookup_Key_From_Action
        (Table           => Keymanager_Module.Table,
         Action          => Action,
         Default         => "",
         Use_Markup      => False,
         Return_Multiple => True,
         Is_User_Changed => Dummy'Access) = ""
      then
         --  No key is already defined for this action: set it now.
         Keys := new Key_Description'
           (Action  => new String'(Action),
            Changed => False,
            Keymap  => null,
            Next    => null);
         Set
           (Keymanager_Module.Table.all,
            Key_Binding'
              (Gdk.Types.Gdk_Key_Type (Accel_Key),
               Gdk.Types.Gdk_Modifier_Type (Accel_Mods)),
            Keys);
      end if;
   end Set_Default_Key;

end KeyManager_Module;
