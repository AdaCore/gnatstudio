------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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
with Ada.Unchecked_Conversion;
with Commands.Interactive;    use Commands, Commands.Interactive;
with Config;                  use Config;
with Default_Preferences;     use Default_Preferences;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNATCOLL.Scripts.Python.Gtkada; use GNATCOLL.Scripts.Python.Gtkada;
with GNATCOLL.Scripts;         use GNATCOLL.Scripts;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GPS.Customizable_Modules; use GPS.Customizable_Modules;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;       use GPS.Kernel.Scripts;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Main_Window;          use GPS.Main_Window;
with GUI_Utils;                use GUI_Utils;
with Gdk.Device;               use Gdk.Device;
with Gdk.Event;                use Gdk.Event;
with Gdk.Types.Keysyms;        use Gdk.Types.Keysyms;
with Gdk.Types;                use Gdk.Types;
with Gdk.Window;               use Gdk.Window;
with Glib.Convert;             use Glib.Convert;
with Glib.Object;              use Glib.Object;
with Glib;                     use Glib;
with Gtk.Accel_Group;          use Gtk.Accel_Group;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.Style;             use Gtkada.Style;
with HTables;                  use HTables;
with Histories;                use Histories;
with Interfaces.C.Strings;
with KeyManager_Module.GUI;
with System.Assertions;        use System.Assertions;
with XML_Parsers;
with XML_Utils;                use XML_Utils;

package body KeyManager_Module is

   Me : constant Trace_Handle := Create ("Keymanager", GNATCOLL.Traces.Off);
   Debug : constant Trace_Handle := Create ("KM");
   Event_Debug_Trace : constant Trace_Handle := Create
     ("Event_Debug", GNATCOLL.Traces.Off);

   use Key_Htable;

   Command_Cst : aliased constant String := "command";
   Key_Cst     : aliased constant String := "key";
   Count_Cst   : aliased constant String := "count";

   Hist_Key_Theme : constant History_Key := "key-theme";

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

   procedure Get_Normalized_Key
     (Event    : Gdk.Event.Gdk_Event;
      Msg      : String;
      Key      : out Gdk_Key_Type;
      Modifier : out Gdk_Modifier_Type);
   --  Read the key typed by the user. This procedure takes care of normalizing
   --  the shortcut, for instance when Caps Lock was pressed, or taking
   --  keyboard-specific shortcuts into account.

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

   procedure Dump_Shortcuts (Prefix : String);
   pragma Unreferenced (Dump_Shortcuts);
   --  Debug: dump the existing shortcuts to the traces

   type Event_Handler_Record;
   type Event_Handler_Access is access all Event_Handler_Record;
   type Event_Handler_Record is record
      Handler : General_Event_Handler_Callback;
      Next    : Event_Handler_Access;
   end record;

   package Event_Handler_Kernel is new Handler_Set_User_Data
     (Kernel_Handle);

   procedure General_Event_Handler
     (Event : Gdk_Event; Kernel : Kernel_Handle);
   --  General event handler for GPS

   procedure Debug_Event_Handler
     (Event : Gdk_Event; Kernel : Kernel_Handle);
   --  General event handler used for event-level debugging

   procedure Load_XML_Keys
     (Kernel       : access Kernel_Handle_Record'Class;
      Filename     : Virtual_File;
      User_Defined : Boolean := False);
   --  Load an XML file that contains key definitions

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

      Current_Command : Cst_String_Access;
      --  Stores the command just after it has been launched. Used as a
      --  temporary storing field for Last_Command to avoid setting
      --  Last_Command before the end of the command execution

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

   function Process_Key_Event
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

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   procedure Keymanager_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Process shell commands associated with this module

   type Repeat_Next_Command is new Interactive_Command with null record;
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
      Accel_Key  : Gdk_Key_Type;
      Accel_Mods : Gdk_Modifier_Type);
   --  If the Action doesn't already have a key binding, then bind it to
   --  Accel_Key/Accel_Mods.

   function Get_Shortcut
     (Kernel          : access Kernel_Handle_Record'Class;
      Action          : String;
      Use_Markup      : Boolean := True;
      Return_Multiple : Boolean := True) return String;
   procedure Get_Shortcut_Simple
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Key        : out Gdk.Types.Gdk_Key_Type;
      Mods       : out Gdk.Types.Gdk_Modifier_Type);
   --  Implement the kernel interface for retrieving key shortcuts

   ------------------------------
   -- User_Key_Theme_Directory --
   ------------------------------

   function User_Key_Theme_Directory
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return GNATCOLL.VFS.Virtual_File
   is
   begin
      return Create_From_Dir (Kernel.Get_Home_Dir, "key_themes/");
   end User_Key_Theme_Directory;

   ----------------------
   -- Save_Custom_Keys --
   ----------------------

   procedure Save_Custom_Keys (Kernel : access Kernel_Handle_Record'Class) is
      Filename : constant Virtual_File :=
        Create_From_Dir (Get_Home_Dir (Kernel), "keys6.xml");
   begin
      Save_Keys (Kernel, Save_All => False, Filename => Filename);
   end Save_Custom_Keys;

   ---------------
   -- Save_Keys --
   ---------------

   procedure Save_Keys
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Save_All : Boolean;
      Filename : GNATCOLL.VFS.Virtual_File)
   is
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
               if (Save_All or else Binding.User_Defined)
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

      Make_Dir (Create (Filename.Dir_Name), Recursive => True);

      Trace (Debug, "Saving " & Filename.Display_Full_Name);
      Print (File, Filename, Success);
      Free (File);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      end if;
   end Save_Keys;

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
      if Tmp = null then
         null;

      elsif Tmp.Handler = Handler then
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
     (Event : Gdk_Event; Kernel : Kernel_Handle)
   is
      Event_Type : constant Gdk_Event_Type := Get_Event_Type (Event);

   begin
      if Keymanager_Module = null then
         --  This can happen when GPS is exiting and modules have been
         --  deallocated already.
         Gtk.Main.Main_Do_Event (Event);
         return;
      end if;

      Call_Handlers : declare
         EH : Event_Handler_Access := Keymanager_Module.Handlers;
      begin
         while EH /= null
           and then not EH.Handler (Event, Kernel)
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
               if Process_Key_Event (Kernel, Event) then
                  return;
               end if;
            end if;
         end;

      elsif Event_Type = Button_Release
         and then Event.Button.Button = 4
         and then Execute_Action
            (Kernel, "backward locations history",
             Context => Kernel.Get_Current_Context,
             Event  => Event,
             Error_Msg_In_Console => True)
      then
         return;

      elsif Event_Type = Button_Release
         and then Event.Button.Button = 5
         and then Execute_Action
            (Kernel, "forward locations history",
             Context => Kernel.Get_Current_Context,
             Event  => Event,
             Error_Msg_In_Console => True)
      then
         return;

      elsif Event_Type = Button_Release then
         --  The command will be executed by gtk, we don't know exactly how
         if Keymanager_Module.Last_Command /= null then
            Free (Keymanager_Module.Last_User_Command);
         end if;
         Keymanager_Module.Last_Command := null;
      end if;

      --  Dispatch the event in the standard gtk+ main loop
      Gtk.Main.Main_Do_Event (Event);

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
     (Event : Gdk_Event; Kernel : Kernel_Handle)
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
         Trace (Me, E);
   end Debug_Event_Handler;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Key_Binding) return Keys_Header_Num is
   begin
      return Keys_Header_Num
        ((Long_Integer (Key.Key) + Long_Integer (Key.Modifier) * 16#FFFF#)
          mod Long_Integer (Keys_Header_Num'Last + 1));
   end Hash;

   ------------------------
   -- Free_Non_Recursive --
   ------------------------

   procedure Free_Non_Recursive (Element : in out Key_Description_List) is
   begin
      if Element.Action /= null then
         Free (Element.Action);
      end if;

      if Element.Keymap /= null then
         Reset (Element.Keymap.Table);
         Unchecked_Free (Element.Keymap);
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

         Tmp_To.User_Defined := Tmp.User_Defined;
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
      Remove_Existing_Actions_For_Shortcut : Boolean)
   is
      Real_Action : constant String := Action_From_Menu (Kernel, Action);

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
                   (Tmp.Action.all, Real_Action, Case_Sensitive => False)
               then
                  return;
               end if;
               Tmp := Tmp.Next;
            end loop;
         end if;

         --  Put in front of the list if possible, so that when the user
         --  just added a new binding, it is visible first (in particular
         --  useful for menus).

         --  We need to clone memory associated to Binding3, since it is
         --  freed in the call to Set below.
         if Real_Action /= "" then
            if Binding3 /= null then
               Clone (From => Binding3, To => Tmp);
            else
               Tmp := null;
            end if;

            Binding2 := new Key_Description'
              (Action  => new String'(Real_Action),
               User_Defined => Save_In_Keys_XML,
               Keymap  => null,
               Next    => Tmp);
            Set (Table, Key_Binding'(Default_Key, Default_Mod), Binding2);
            Update_Shortcuts_For_Action (Kernel, Real_Action);
         else
            Binding2 := Get (Table, Key_Binding'(Default_Key, Default_Mod));
            while Binding2 /= null loop
               if Binding2.Action /= null then
                  Update_Shortcuts_For_Action (Kernel, Binding2.Action.all);
               end if;
               Binding2 := Binding2.Next;
            end loop;

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
                   (List.Action.all, Real_Action, Case_Sensitive => False)
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
      Modif                 : Gdk_Modifier_Type;
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

      --  On Windows binding control-c to non default copy action can result in
      --  unexpected behavior.

      if Config.Host = Windows
        and then (Key = "control-c" or else Key = "primary-c")
        and then Real_Action /= ""
        and then Real_Action /= "Copy to Clipboard"
      then
         Kernel.Insert
           (-("Warning: binding Ctrl-C is unreliable on Windows,"
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
                           & (-"> for action """)
                           & Real_Action & """:" & ASCII.LF
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
            User_Defined => False,
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
               User_Defined => False,
               Keymap  => Keymap,
               Next    => null);
         else
            Keymap := Binding2.Keymap;
         end if;
      end if;
   end Get_Secondary_Keymap;

   ------------------------
   -- Get_Normalized_Key --
   ------------------------

   procedure Get_Normalized_Key
     (Event    : Gdk.Event.Gdk_Event;
      Msg      : String;
      Key      : out Gdk_Key_Type;
      Modifier : out Gdk_Modifier_Type)
   is
      State : constant Gdk_Modifier_Type := Event.Key.State;
   begin
      --  Remove any num-lock and caps-lock modifiers
      Modifier := State and Get_Default_Mod_Mask;
      Key := Event.Key.Keyval;

      --  If Caps lock in on, and the key is an upper-case character,
      --  lower-case it.

      if (State and Lock_Mask) > 0
        and then Key >= GDK_A
        and then Key <= GDK_Z
      then
         Key := Key + GDK_LC_a - GDK_A;
      end if;

      if Active (Me) then
         Trace (Me, Msg & " Key=" & Key'Img & " Modif=" & Modifier'Img
                & " Code=" & Event.Key.Hardware_Keycode'Img
                & " => " & Image (Key, Modifier)
                & " / "
                & Gtk.Accel_Group.Accelerator_Get_Label (Key, Modifier)
                & " / "
                & Gtk.Accel_Group.Accelerator_Name (Key, Modifier));
      end if;

      --  Quartz backend maps the following:
      --       alphaLock  => GDK_LOCK_MASK
      --       shiftKey   => GDK_SHIFT_MASK
      --       controlKey => GDK_CONTROL_MASK
      --       optionKey  => GDK_MOD1_MASK
      --       cmdKey     => GDK_MOD2_MASK

      --  Win2 backend maps the followin:
      --       altgr      => GDK_MOD2_MASK
      --       left-menu  => GDK_MOD1_MASK
   end Get_Normalized_Key;

   -----------------------
   -- Process_Key_Event --
   -----------------------

   function Process_Key_Event
     (Kernel   : access Kernel_Handle_Record'Class;
      Event    : Gdk.Event.Gdk_Event) return Boolean
   is
      Key              : Gdk_Key_Type;
      Modif            : Gdk_Modifier_Type;
      Binding          : Key_Description_List;
      Has_Secondary    : constant Boolean :=
                           Keymanager_Module.Secondary_Keymap /= null;
      Context          : Selection_Context;
      Context_Computed : Boolean := False;
      Found_Action     : Boolean := False;
      Count            : Natural;

      procedure Compute_Context;
      --  Compute the current context if not done already

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

   begin
      --  We could test Modif /= 0 if we allowed only key shortcuts with a
      --  modifier (control, alt, ...). However, this would prevent assigning
      --  key shortcuts to F1, F2, Home, PageUp,.. so is not desirable.

      if Keymanager_Module.Active
        and then Get_Event_Type (Event) = Key_Press
      then
         Get_Normalized_Key (Event, "Press", Key, Modif);

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
                     Keymanager_Module.Argument_Current := new String'
                       (Tmp.all
                        & Interfaces.C.Strings.Value (Event.Key.String));
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

         --  If we didn't find anything in the first attempt but the
         --  shift key was pressed, attempt to find the key in the binding
         --  using the same key but with shift not being pressed.
         --
         --  This is to solve the following: we register an action
         --  (for instance 'uncomment lines') with the key shortcut
         --     ctrl + '_'
         --  the only way to generate this on a QWERTY keyboard is
         --  to press the keys ctrl + shift + '-'.
         --  GPS tries above to find this in the keymap above, and then
         --  attempts to find ctrl + '_' below.

         if Binding = No_Key
           and then (Modif and Shift_Mask) > 0

         --  Do this only for actual graphical keys, so we let Gtk+ take
         --  care of shift + <arrow> to extend the selection, for instance.
         --  ??? This is temporary: we do need to react to shift + <arrows>
         --  ourselves to handle extending selections for multi cursors
           and then Key >= 32 and then Key <= 128

         then
            if Keymanager_Module.Secondary_Keymap = null then
               Binding := Get
                 (Keymanager_Module.Table.all,
                  (Key, Modif - Shift_Mask));
            else
               Binding := Get
                 (Keymanager_Module.Secondary_Keymap.Table,
                  (Key, Modif - Shift_Mask));
            end if;
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

               Compute_Context;

               Keymanager_Module.Last_Command       :=
                 Keymanager_Module.Current_Command;
               Keymanager_Module.Current_Command    := null;

               if not GPS_Application
                  (Kernel.Get_Application).Is_Any_Menu_Open
                 and then Execute_Action
                   (Kernel  => Kernel,
                    Action  => Binding.Action.all,
                    Context => Context,
                    Event   => Event,
                    Error_Msg_In_Console => False,
                    Repeat  => Keymanager_Module.Repeat_Count)
               then
                  if Keymanager_Module.Last_Command /=
                    Cst_String_Access (Binding.Action)
                  then
                     Free (Keymanager_Module.Last_User_Command);
                  end if;
                  Keymanager_Module.Current_Command :=
                    Cst_String_Access (Binding.Action);
                  Found_Action                      := True;
                  Keymanager_Module.Repeat_Count    := 1;
               end if;
            end if;

            Binding := Binding.Next;
         end loop;

         if not Found_Action then
            --  The command will be executed by gtk, we don't know exactly how
            if Keymanager_Module.Last_Command /= null then
               Free (Keymanager_Module.Last_User_Command);
            end if;
            Keymanager_Module.Last_Command := null;

            --  To repeat this one, we need to requeue the event...
            --  No need to create an undo group, since these events are
            --  processed asynchronously anyway. The editor will properly merge
            --  editing actions into a single undo command anyway.

            Count := Keymanager_Module.Repeat_Count;
            Keymanager_Module.Repeat_Count := 1;
            for R in 2 .. Count loop
               declare
                  Ev : constant Gdk_Event := Copy (Event);
               begin
                  --  Process the event immediately
                  General_Event_Handler (Ev, Kernel_Handle (Kernel));
                  Free (Ev);
               end;
            end loop;
         end if;

      elsif Get_Event_Type (Event) = Key_Release then
         Get_Normalized_Key (Event, "Release", Key, Modif);

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

      if not (Found_Action or Has_Secondary) then
         Trace (Me, "No action was executed, falling though gtk+");
         return False;
      end if;
      return True;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Process_Key_Event;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Theme : Key_Theme_Type) return String
   is
      (Ada.Strings.Unbounded.To_String (Theme.Name));

   ---------------------
   -- Is_User_Defined --
   ---------------------

   function Is_User_Defined (Theme : Key_Theme_Type) return Boolean
   is
      (Theme.User_Defined);

   -------------------------
   -- Get_First_Reference --
   -------------------------

   function Get_First_Reference
     (Themes_List : Key_Theme_Type_List) return Key_Theme_Type_Cursor
   is
      (Key_Theme_Type_Cursor'(C => Themes_List.First));

   ----------
   -- Next --
   ----------

   procedure Next (Theme_Cursor : in out Key_Theme_Type_Cursor) is
   begin
      Key_Theme_Type_Maps.Next (Theme_Cursor.C);
   end Next;

   -------------------
   -- Get_Key_Theme --
   -------------------

   function Get_Key_Theme
     (Theme_Cursor : Key_Theme_Type_Cursor) return Key_Theme_Type
   is
     (if Theme_Cursor /= Null_Key_Theme_Type_Cursor
      and then Key_Theme_Type_Maps.Has_Element (Theme_Cursor.C)
      then
         Key_Theme_Type_Maps.Element (Theme_Cursor.C)
      else
         Null_Key_Theme);

   -------------------
   -- Get_Key_Theme --
   -------------------

   function Get_Key_Theme
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return String
   is
   begin
      return Most_Recent (Get_History (Kernel), Hist_Key_Theme, "default");
   end Get_Key_Theme;

   ------------------
   -- Find_By_Name --
   ------------------

   function Find_By_Name
     (Themes_List : Key_Theme_Type_List;
      Name        : String) return Key_Theme_Type_Cursor
   is
     (if Themes_List.Contains (Name) then
         (C => Themes_List.Find (Name))
      else
         Null_Key_Theme_Type_Cursor);

   -------------------
   -- Set_Key_Theme --
   -------------------

   procedure Set_Key_Theme
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String)
   is
   begin
      Add_To_History (Get_History (Kernel).all, Hist_Key_Theme, Name);
   end Set_Key_Theme;

   --------------------
   -- Load_Key_Theme --
   --------------------

   procedure Load_Key_Theme
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Theme  : String := "")
   is
      T : constant String :=
        (if Theme /= "" then Theme else Get_Key_Theme (Kernel));
      User_Theme : constant Virtual_File :=
        Create_From_Dir (User_Key_Theme_Directory (Kernel), +T & ".xml");
      System_Theme : constant Virtual_File :=
        Create_From_Dir (Kernel.Get_Share_Dir, +("key_themes/" & T & ".xml"));
   begin
      if User_Theme.Is_Regular_File then
         Load_XML_Keys (Kernel, User_Theme, User_Defined => False);
      else
         Load_XML_Keys (Kernel, System_Theme, User_Defined => False);
      end if;
   end Load_Key_Theme;

   ---------------------
   -- List_Key_Themes --
   ---------------------

   function List_Key_Themes
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return Key_Theme_Type_List
   is
      User_Theme : constant Virtual_File := User_Key_Theme_Directory (Kernel);
      System_Theme : constant Virtual_File :=
        Create_From_Dir (Kernel.Get_Share_Dir, +"key_themes");

      Result : Key_Theme_Type_List;

      procedure Add_From_Dir (Dir : Virtual_File; User_Defined : Boolean);

      ------------------
      -- Add_From_Dir --
      ------------------

      procedure Add_From_Dir (Dir : Virtual_File; User_Defined : Boolean) is
         Files : File_Array_Access;

         use Ada.Strings.Unbounded;
      begin
         if Dir.Is_Directory then
            Files := Read_Dir (Dir, Files_Only);
            for F in Files'Range loop
               if Files (F).File_Extension = ".xml" then
                  declare
                     Key_Theme_Name : constant String :=
                                        Files (F).Display_Base_Name (".xml");
                  begin
                     Result.Include
                       (Key      => Key_Theme_Name,
                        New_Item => Key_Theme_Type'
                          (Name         =>
                               To_Unbounded_String (Key_Theme_Name),
                           User_Defined => User_Defined));
                  end;
               end if;
            end loop;
            Unchecked_Free (Files);
         end if;
      end Add_From_Dir;

   begin
      Add_From_Dir (User_Theme, User_Defined => True);
      Add_From_Dir (System_Theme, User_Defined => False);
      return Result;
   end List_Key_Themes;

   ----------------------
   -- Remove_Shortcuts --
   ----------------------

   procedure Remove_Shortcuts
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Mode   : Remove_Mode)
   is
      procedure Update_Menu (Binding : Key_Description_List);
      --  Delete the binding and update associated menus

      procedure Update_Menu (Binding : Key_Description_List) is
         Success : Boolean;
         pragma Unreferenced (Success);
      begin
         if Binding.Action /= null then
            declare
               A : constant String := Binding.Action.all;
            begin
               --  Need to remove action so that update_shortcut_display
               --  works.
               Free (Binding.Action);
               Update_Shortcuts_For_Action (Kernel, A);
            end;
         end if;
      end Update_Menu;

      procedure Traverse (Table : in out Key_Htable.Instance);
      procedure Traverse (Table : in out Key_Htable.Instance) is
         Iter    : Key_Htable.Cursor;
         Binding, Previous : Key_Description_List;
         Remove, Remove_Action  : Boolean;
         Move_To_Next : Boolean;

      begin
         Get_First (Table, Iter);
         loop
            Binding := Get_Element (Iter);
            exit when Binding = No_Key;

            Move_To_Next := True;
            Previous := null;

            while Binding /= null loop
               Remove_Action := False;

               case Mode is
                  when All_Shortcuts =>
                     Remove_Action := True;
                  when Standard_Shortcuts =>
                     Remove_Action := not Binding.User_Defined;
                  when User_Shortcuts =>
                     Remove_Action := Binding.User_Defined;
               end case;

               if Binding.Keymap /= null then
                  Traverse (Binding.Keymap.Table);

                  --  Never remove if we still have secondary keymaps
                  Remove := Remove_Action
                    and then Get_First (Binding.Keymap.Table) = null;
               else
                  Remove := Remove_Action;
               end if;

               if Remove then
                  if Previous = null then
                     if Binding.Next = null then
                        Move_To_Next := False;
                        Update_Menu (Binding);

                        --  ??? Can't free Binding, since it might point to
                        --  non-allocated data
                        --      Free_Non_Recursive (Binding);

                        Remove_And_Get_Next (Table, Iter);
                        exit;
                     else
                        Set_Element (Iter, Binding.Next);

                        Previous := Binding;
                        Binding := Binding.Next;

                        Update_Menu (Previous);
                        Free_Non_Recursive (Previous);
                        Previous := null;
                     end if;

                  else
                     Previous.Next := Binding.Next;
                     Update_Menu (Binding);
                     Free_Non_Recursive (Binding);
                     Binding := Previous.Next;
                  end if;

               else
                  if Remove_Action then
                     Update_Menu (Binding);
                  end if;

                  Previous := Binding;
                  Binding := Binding.Next;
               end if;
            end loop;

            if Move_To_Next then
               Get_Next (Table, Iter);
            end if;
         end loop;
      end Traverse;

   begin
      Traverse (Keymanager_Module.Table.all);
   end Remove_Shortcuts;

   --------------------
   -- Dump_Shortcuts --
   --------------------

   procedure Dump_Shortcuts (Prefix : String) is
      procedure Dump_Table
        (Table : in out Key_Htable.Instance; P : String);
      procedure Dump_Table
        (Table : in out Key_Htable.Instance; P : String)
      is
         Iter    : Key_Htable.Cursor;
         Binding : Key_Description_List;
         Key          : Key_Binding;
      begin
         Increase_Indent (Debug, P);
         Get_First (Table, Iter);
         loop
            Binding := Get_Element (Iter);
            exit when Binding = No_Key;

            Key := Get_Key (Iter);

            while Binding /= null loop
               if Binding.Action /= null then
                  Trace (Debug, P & ' ' & Image (Key.Key, Key.Modifier) & ' '
                         & Binding.Action.all
                         & " user=" & Binding.User_Defined'Img);
               end if;

               if Binding.Keymap /= null then
                  Dump_Table
                    (Binding.Keymap.Table,
                     Prefix & " " & Image (Key.Key, Key.Modifier));
               end if;

               Binding := Binding.Next;
            end loop;

            Get_Next (Table, Iter);
         end loop;
         Decrease_Indent (Debug);
      end Dump_Table;

   begin
      Dump_Table (Keymanager_Module.Table.all, Prefix);
   end Dump_Shortcuts;

   -------------------
   -- Load_XML_Keys --
   -------------------

   procedure Load_XML_Keys
     (Kernel       : access Kernel_Handle_Record'Class;
      Filename     : Virtual_File;
      User_Defined : Boolean := False)
   is
      File, Child : Node_Ptr;
      Err         : String_Access;
      Prev        : Boolean;
   begin
      if Is_Regular_File (Filename) then
         Trace (Debug, "Loading " & Filename.Display_Full_Name);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);
         else
            Child := File.Child;

            Prev := Keymanager_Module.Menus_Created;
            Keymanager_Module.Menus_Created := True;

            while Child /= null loop
               declare
                  Action : constant String := Get_Attribute (Child, "action");
                  Load   : constant String := Get_Attribute (Child, "load");
               begin
                  if Load /= "" then
                     Load_Key_Theme (Kernel, Load);
                  else
                     Bind_Default_Key_Internal
                       (Kernel           => Kernel,
                        Table            => Keymanager_Module.Table.all,
                        Action           => Action,
                        Key              => Child.Value.all,
                        Save_In_Keys_XML => User_Defined,
                        Remove_Existing_Shortcuts_For_Action => User_Defined,
                        Remove_Existing_Actions_For_Shortcut => User_Defined);
                  end if;
               end;

               Child := Child.Next;
            end loop;

            Keymanager_Module.Menus_Created := Prev;

            Free (File);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Debug, E);
         Insert (Kernel, -"Could not parse " &
                   Filename.Display_Full_Name, Mode => Error);
   end Load_XML_Keys;

   ----------------------
   -- Load_Custom_Keys --
   ----------------------

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class)
   is
      Filename    : Virtual_File :=
                      Create_From_Dir (Get_Home_Dir (Kernel), "keys6.xml");
   begin
      Keymanager_Module.Custom_Keys_Loaded := True;

      if not Is_Regular_File (Filename) then
         Filename := Create_From_Dir (Get_Home_Dir (Kernel), "keys.xml");
      end if;
      Load_XML_Keys (Kernel, Filename, User_Defined => True);
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
                        & Gtk.Accel_Group.Accelerator_Get_Label
                          (Get_Key (Iter).Key, Get_Key (Iter).Modifier)
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
                           Append (Result, " <b>or</b> ");
                        else
                           Append (Result, " or ");
                        end if;
                     end if;

                     Is_User_Changed.all :=
                       Is_User_Changed.all or Binding.User_Defined;

                     if Use_Markup then
                        Append
                          (Result,
                           Escape_Text
                             (Prefix
                              & Gtk.Accel_Group.Accelerator_Get_Label
                                (Get_Key (Iter).Key,
                                 Get_Key (Iter).Modifier)));
                     else
                        Append
                          (Result,
                           Prefix
                           & Gtk.Accel_Group.Accelerator_Get_Label
                             (Get_Key (Iter).Key, Get_Key (Iter).Modifier));
                     end if;

                  elsif Prefix = "" then
                     --  When returning a single key binding, give priority to
                     --  the ones with a single key, so that we can display
                     --  them in menu shortcuts
                     Is_User_Changed.all :=
                       Is_User_Changed.all or Binding.User_Defined;

                     if Use_Markup then
                        Result := To_Unbounded_String
                          (Escape_Text
                             (Gtk.Accel_Group.Accelerator_Get_Label
                                  (Get_Key (Iter).Key,
                                   Get_Key (Iter).Modifier)));

                     else
                        Result := To_Unbounded_String
                          (Gtk.Accel_Group.Accelerator_Get_Label
                             (Get_Key (Iter).Key, Get_Key (Iter).Modifier));
                     end if;
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

   ------------------
   -- Get_Shortcut --
   ------------------

   function Get_Shortcut
     (Kernel          : access Kernel_Handle_Record'Class;
      Action          : String;
      Use_Markup      : Boolean := True;
      Return_Multiple : Boolean := True) return String
   is
      pragma Unreferenced (Kernel);
      Is_User_Changed : aliased Boolean;
   begin
      return Lookup_Key_From_Action
        (Keymanager_Module.Table,
         Action          => Action,
         Default         => "",
         Use_Markup      => Use_Markup,
         Return_Multiple => Return_Multiple,
         Is_User_Changed => Is_User_Changed'Access);
   end Get_Shortcut;

   -------------------------
   -- Get_Shortcut_Simple --
   -------------------------

   procedure Get_Shortcut_Simple
     (Kernel     : access Kernel_Handle_Record'Class;
      Action     : String;
      Key        : out Gdk.Types.Gdk_Key_Type;
      Mods       : out Gdk.Types.Gdk_Modifier_Type)
   is
      pragma Unreferenced (Kernel);
      Iter    : Key_Htable.Cursor;
      Binding : Key_Description_List;
   begin
      Get_First (Keymanager_Module.Table.all, Iter);
      loop
         Binding := Get_Element (Iter);
         exit when Binding = No_Key;

         --  If we have voluntarily assigned an invalid binding to indicate
         --  a key should be removed, ignore this here as well.
         if Get_Key (Iter).Key = 0 then
            Binding := null;
         end if;

         while Binding /= null loop
            if Binding.Action /= null
              and then Equal
                (Binding.Action.all, Action, Case_Sensitive => False)
              and then Get_Key (Iter).Key /= 0
            then
               Key := Get_Key (Iter).Key;
               Mods := Get_Key (Iter).Modifier;
               return;
            end if;

            Binding := Binding.Next;
         end loop;

         Get_Next (Keymanager_Module.Table.all, Iter);
      end loop;

      Key := 0;
      Mods := 0;
   end Get_Shortcut_Simple;

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
            Exclusive : constant Boolean :=
               To_Lower (Get_Attribute (Node, "exclusive", "true")) = "true";
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
                     Remove_Existing_Actions_For_Shortcut => Exclusive);
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
               Remove_Existing_Actions_For_Shortcut => Exclusive,
               Save_In_Keys_XML  => False,
               Key               => Node.Value.all);
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
      function Get_Window (Arg : Positive) return Gdk.Gdk_Window;
      --  Return GdkWindow from argument Arg. Return first top level
      --  window if Arg isn't provided.

      ----------------
      -- Get_Window --
      ----------------

      function Get_Window (Arg : Positive) return Gdk.Gdk_Window is
         use Widget_List;
         use type Gdk.Gdk_Window;

         List   : Widget_List.Glist;
         Win    : Gtk_Widget;
         Window : Gdk.Gdk_Window := From_PyGtk (Data, Arg);
      begin
         if Window = null then
            List := List_Toplevels;
            Win := Get_Data (List);
            Window := Get_Window (Win);
            Free (List);
         end if;

         if Window /= null then
            Ref (Window);
         end if;

         return Window;
      end Get_Window;

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

      elsif Command = "send_button_event" then
         declare
            Window  : constant Gdk.Gdk_Window := Get_Window (Arg => 1);
            Typ     : constant Gdk_Event_Type :=
              Gdk_Event_Type'Val
                (Nth_Arg (Data, 2, Gdk_Event_Type'Pos (Button_Press) - 1) + 1);
            Button  : constant Integer := Nth_Arg (Data, 3, 1);
            X       : constant Integer := Nth_Arg (Data, 4, 1);
            Y       : constant Integer := Nth_Arg (Data, 5, 1);
            State   : constant Integer := Nth_Arg (Data, 6, 0);
            Event   : Gdk_Event;
            Device  : Gdk_Device;
         begin
            Device := Gtkada.Style.Get_First_Device
              (Widget => Get_Kernel (Data).Get_Main_Window,
               Source => Source_Mouse);

            Gdk_New (Event, Typ);
            Event.Button :=
               (The_Type    => Typ,
                Window      => Window,
                Send_Event  => 1,
                Time        => 0, --  CURRENT_TIME
                X           => Gdouble (X),
                Y           => Gdouble (Y),
                Axes        => null,
                State       => Gdk_Modifier_Type (State),
                Button      => Guint (Button),
                Device      => System.Null_Address,
                X_Root      => 0.0,
                Y_Root      => 0.0);

            Device.Ref;
            Set_Device (Event, Device);
            Device.Ref;
            Set_Source_Device (Event, Device);

            Main_Do_Event (Event);

            Gdk.Event.Free (Event);
         end;

      elsif Command = "send_crossing_event" then
         declare
            Window  : constant Gdk.Gdk_Window := Get_Window (Arg => 1);
            Typ     : constant Gdk_Event_Type :=
              Gdk_Event_Type'Val
                (Nth_Arg (Data, 2, Gdk_Event_Type'Pos (Enter_Notify) - 1)
                   + 1);
            X       : constant Integer := Nth_Arg (Data, 3, 1);
            Y       : constant Integer := Nth_Arg (Data, 4, 1);
            State   : constant Integer := Nth_Arg (Data, 5, 0);
            Event   : Gdk_Event;
            Device  : Gdk_Device;
         begin
            Gdk_New (Event, Typ);
            Event.Crossing :=
               (The_Type    => Typ,
                Window      => Window,
                Send_Event  => 1,
                Time        => 0, --  CURRENT_TIME
                X           => Gdouble (X),
                Y           => Gdouble (Y),
                State       => Gdk_Modifier_Type (State),
                X_Root      => 0.0,
                Y_Root      => 0.0,
                Subwindow   => null,
                Mode        => Crossing_Normal,
                Detail      => Notify_Ancestor,
                Focus       => False);

            Device := Gtkada.Style.Get_First_Device
              (Widget => Get_Kernel (Data).Get_Main_Window,
               Source => Source_Mouse);
            Device.Ref;
            Set_Device (Event, Device);
            Device.Ref;
            Set_Source_Device (Event, Device);

            Main_Do_Event (Event);

            Gdk.Event.Free (Event);
         end;

      elsif Command = "send_key_event" then
         declare
            use type Gdk.Gdk_Window;
            use Widget_List;

            Keyval  : constant Gdk_Key_Type :=
               Gdk_Key_Type (Integer'(Nth_Arg (Data, 1)));
            Window  : Gdk.Gdk_Window := From_PyGtk (Data, 2);
            Primary : constant Boolean := Nth_Arg (Data, 3, False);
            Alt     : constant Boolean := Nth_Arg (Data, 4, False);
            Shift   : constant Boolean := Nth_Arg (Data, 5, False);
            Control : constant Boolean := Nth_Arg (Data, 6, False);
            Event   : Gdk_Event;
            List, List2    : Widget_List.Glist;
            Win     : Gtk_Widget;
            Device  : Gdk_Device;
         begin
            if Window = null then
               List := List_Toplevels;
               List2 := List;
               while List2 /= Null_List loop
                  Win := Get_Data (List2);
                  if Win /= null
                    and then Gtk_Window (Win).Has_Toplevel_Focus
                  then
                     Window := Get_Window (Win);
                     exit;

                  elsif Win /= null
                    and then Get_Window (Win) /= null
                  then
                     Window := Get_Window (Win);
                  end if;

                  List2 := Next (List2);
               end loop;

               Free (List);
            end if;

            Gdk_New (Event, Gdk.Event.Key_Press);
            Event.Key :=
               (The_Type    => Gdk.Event.Key_Press,
                Window      => Window,
                Keyval      => Keyval,
                Send_Event  => 1,
                Time        => 0, --  CURRENT_TIME
                Is_Modifier => 0,
                Group       => 0,
                State       => 0,
                Length      => 0,
                String      => Interfaces.C.Strings.Null_Ptr,
                Hardware_Keycode => 0);
            Ref (Event.Key.Window);

            if Primary then
               Event.Key.State := Event.Key.State or Primary_Mod_Mask;
            end if;

            if Control then
               Event.Key.State := Event.Key.State or Control_Mask;
            end if;

            if Shift then
               Event.Key.State := Event.Key.State or Shift_Mask;
            end if;

            if Alt then
               Event.Key.State := Event.Key.State or Mod1_Mask;
            end if;

            if Keyval = GDK_BackSpace then
               if GNAT.OS_Lib.Directory_Separator = '\' then
                  Event.Key.Hardware_Keycode := 8;
               else
                  --  On MAC, should be 51, on Unix 59
                  Event.Key.Hardware_Keycode := 59;
               end if;
            end if;

            Device := Gtkada.Style.Get_First_Device
              (Widget => Get_Kernel (Data).Get_Main_Window,
               Source => Source_Keyboard);
            Device.Ref;
            Set_Device (Event, Device);
            Device.Ref;
            Set_Source_Device (Event, Device);

            General_Event_Handler (Event, Kernel => Get_Kernel (Data));
            Gdk.Event.Free (Event);
         end;

      elsif Command = "process_all_events" then
         declare
            Dummy : Boolean;
         begin
            while Gtk.Main.Events_Pending loop
               Dummy := Gtk.Main.Main_Iteration;
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
         Event_Handler_Kernel.Handler_Set
           (Debug_Event_Handler'Access,
            Kernel_Handle (Kernel));
      else
         Event_Handler_Kernel.Handler_Set
           (General_Event_Handler'Access,
            Kernel_Handle (Kernel));
      end if;

      Create_New_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Key_Theme, Strings);
      Set_Max_Length (Get_History (Kernel).all, 1, Hist_Key_Theme);

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

      Kernel.Scripts.Register_Command
        ("send_key_event",
         (Param ("keyval"),
          Param ("window", Optional => True),
          Param ("primary", Optional => True),
          Param ("alt", Optional => True),
          Param ("shift", Optional => True),
          Param ("control", Optional => True)),
         Keymanager_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("send_button_event",
         (Param ("window", Optional => True),
          Param ("type", Optional => True),
          Param ("button", Optional => True),
          Param ("x", Optional => True),
          Param ("y", Optional => True),
          Param ("state", Optional => True)),
         Keymanager_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("send_crossing_event",
         (Param ("window", Optional => True),
          Param ("type", Optional => True),
          Param ("x", Optional => True),
          Param ("y", Optional => True),
          Param ("state", Optional => True)),
         Keymanager_Command_Handler'Access);

      Kernel.Scripts.Register_Command
        ("process_all_events", No_Params,
         Keymanager_Command_Handler'Access);

      Command := new Repeat_Next_Command;
      Register_Action
        (Kernel, "Repeat Next", Command,
         -("Repeat the next action a number of times. Executing this action"
           & " takes a numeric argument (read from the keyboard). For"
           & " instance, if this is associated with ctrl-u, you can type"
           & " ""ctrl-u 30 t"" to  the character t 30 times"),
         Category => -"General");

      Preferences_Changed_Hook.Add (new On_Pref_Changed);

      Set_Key_Setter
        (Kernel, Set_Default_Key'Access,
         Get_Shortcut'Access, Get_Shortcut_Simple'Access);
   end Register_Module;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self, Kernel, Pref);
   begin
      Keymanager_Module.Menus_Created := True;
   end Execute;

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
            Title          => -"Key binding error",
            Parent         => Kernel.Get_Main_Window);
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
         Pos  : Key_Htable.Cursor;
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
      Accel_Key  : Gdk_Key_Type;
      Accel_Mods : Gdk_Modifier_Type)
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
            User_Defined => False,
            Keymap  => null,
            Next    => null);
         Set
           (Keymanager_Module.Table.all,
            Key_Binding'(Accel_Key, Accel_Mods),
            Keys);
      end if;
   end Set_Default_Key;

end KeyManager_Module;
