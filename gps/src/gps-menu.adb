------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

with Gdk.Types.Keysyms;      use Gdk.Types.Keysyms;
with Gdk.Types;              use Gdk.Types;

with Glib.Object;            use Glib.Object;
with Glib;                   use Glib;

with Gtk.Menu_Item;          use Gtk.Menu_Item;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Widget;             use Gtk.Widget;
with Gtk.Window;             use Gtk.Window;

with Gtkada.File_Selector;   use Gtkada.File_Selector;

with Commands.Interactive;   use Commands, Commands.Interactive;
with GNATCOLL.Projects;      use GNATCOLL.Projects;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Actions;     use GPS.Kernel.Actions;
with GPS.Kernel.Clipboard;   use GPS.Kernel.Clipboard;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Main_Window;        use GPS.Main_Window;
with GPS.Stock_Icons;        use GPS.Stock_Icons;
with Histories;              use Histories;
with Projects;               use Projects;

package body GPS.Menu is
   Me : constant Trace_Handle := Create ("MENU");

   Project_History_Key : constant Histories.History_Key := "project_files";
   --  Key to use in the kernel histories to store the most recently opened
   --  files.
   --  Synchronize with welcome.adb

   type On_Reopen is new Menu_Callback_Record with record
      Kernel : Kernel_Handle;
   end record;
   overriding procedure Activate (Callback : access On_Reopen; Item : String);

   procedure On_Project_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project has just changed

   type Clipboard_Kind is (Cut, Copy, Paste, Paste_Previous);
   type Clipboard_Command is new Interactive_Command with record
      Kernel : Kernel_Handle;
      Kind   : Clipboard_Kind;
   end record;
   overriding function Execute
     (Command : access Clipboard_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  Perform the various actions associated with the clipboard

   type Save_Desktop_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Save_Desktop_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Save Desktop menu

   type Change_Dir_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Change_Dir_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Change Directory... menu

   type Save_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Save_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Save All menu

   type Exit_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Exit_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Exit menu

   type Preference_Dialog_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Preference_Dialog_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit->Preferences menu

   type Open_Project_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Open_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Project->Open menu

   type Open_From_Host_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Open_From_Host_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Project->Open remote menu

   type Reload_Project_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Reload_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the Project->Recompute Project menu

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Exit_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      GPS.Main_Window.Quit (GPS_Window (Get_Main_Window (Kernel)));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Change_Dir_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Dir : Virtual_File;
   begin
      Dir := Select_Directory
        (-"Select a directory",
         History           => Get_History (Kernel),
         Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
         Parent            => Get_Current_Window (Kernel));

      if Dir /= No_File then
         Change_Dir (Dir);
      end if;
      return Commands.Success;

   exception
      when VFS_Directory_Error =>
         Kernel.Insert
           ("Cannot change to directory: " &
            Dir.Display_Full_Name,
            Mode => Error);
         return Commands.Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Save_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ignore : Boolean;
      pragma Unreferenced (Command, Ignore);
   begin
      Ignore := Save_MDI_Children (Kernel, Force => False);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Save_Desktop_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Save_Desktop (Kernel);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Clipboard_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      W : constant Gtk_Widget := Get_Current_Focus_Widget (Command.Kernel);
      Clipboard : constant Clipboard_Access := Get_Clipboard (Command.Kernel);
   begin
      if W /= null then
         case Command.Kind is
            when Cut            => Cut_Clipboard   (Clipboard, W);
            when Copy           => Copy_Clipboard  (Clipboard, W);
            when Paste          => Paste_Clipboard (Clipboard, W);
            when Paste_Previous => Paste_Previous_Clipboard (Clipboard, W);
         end case;
         return Commands.Success;

      else
         return Commands.Failure;
      end if;
   end Execute;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Callback : access On_Reopen; Item : String) is
   begin
      Load_Project (Callback.Kernel, Create (+Item));

   exception
      when E : others => Trace (Me, E);
   end Activate;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class) is
      Project : constant Project_Type := Get_Project (Kernel);
      Path    : constant Virtual_File := Project_Path (Project);
   begin
      if Get_Registry (Kernel).Tree.Status = From_File
        and then Path /= No_File
      then
         Add_To_History
           (Kernel, Project_History_Key,
            --  ??? What if the file is not utf8 ? The saved xml file might
            --  get corrupted ...
            String (Full_Name (Path).all));
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Project_Changed;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Reload_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Reload_Project_If_Needed (Kernel);
      Recompute_View (Kernel);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Open_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Filename : constant Virtual_File :=
        Select_File
          (-"Open Project",
           File_Pattern      => "*.gpr",
           Pattern_Name      => -"Project files",
           Parent            => Get_Current_Window (Kernel),
           Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
           Kind              => Open_File,
           History           => Get_History (Kernel));
   begin
      if Filename /= GNATCOLL.VFS.No_File then
         Load_Project (Kernel, Filename);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Open_From_Host_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Filename : constant Virtual_File :=
        Select_File
          (-"Open Project",
           File_Pattern    => "*.gpr",
           Pattern_Name    => -"Project files",
           Parent          => Get_Current_Window (Kernel),
           Remote_Browsing => True,
           Kind            => Open_File,
           History         => Get_History (Kernel));
   begin
      if Filename /= GNATCOLL.VFS.No_File then
         Load_Project (Kernel, Filename);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Preference_Dialog_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Edit_Preferences (Kernel);
      return Commands.Success;
   end Execute;

   ---------------------------
   -- Register_Common_Menus --
   ---------------------------

   procedure Register_Common_Menus
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Reopen_Menu : Gtk.Menu_Item.Gtk_Menu_Item;
      Command     : Interactive_Command_Access;
   begin
      Command := new Open_Project_Command;
      Register_Action
        (Kernel, "open project dialog", Command,
         Stock_Id => Stock_Open,
         Description => -"Open the Open Project dialog");

      Command := new Open_From_Host_Command;
      Register_Action
        (Kernel, "open remote project", Command,
         Stock_Id    => Stock_Open,
         Description => -"Open remote project");

      Reopen_Menu := Find_Menu_Item (Kernel, "/Project/Recent");
      Associate (Get_History (Kernel).all,
                 Project_History_Key,
                 Reopen_Menu,
                 new On_Reopen'(Menu_Callback_Record with
                                Kernel => Kernel_Handle (Kernel)));
      Add_Hook (Kernel, Project_Changed_Hook,
                Wrapper (On_Project_Changed'Access),
                Name => "menu.project_changed");

      Command := new Reload_Project_Command;
      Register_Action
        (Kernel, "reload project", Command,
         Description =>
           -("Recompute the list of source files for the project. This should"
           & " be used whenever you create or remove files outside of GPS"),
         Stock_Id => GPS_Refresh);

      Command := new Save_All_Command;
      Register_Action
        (Kernel, "save files and projects", Command,
         Description => -("Save all modified files and projects"));

      Command := new Save_Desktop_Command;
      Register_Action
        (Kernel, "save desktop", Command,
         Description =>
           -("Save the layout of the desktop to a file, so that it is"
           & " restored when GPS is restarted later with the same project"));

      Command := new Change_Dir_Command;
      Register_Action
        (Kernel, "change directory", Command,
         Description => -"Change the current directory");

      Command := new Exit_Command;
      Register_Action
         (Kernel, "exit", Command,
          -"Exit GPS, after confirming whether to save modified files");

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Clipboard_Command (Command.all).Kind   := Cut;
      Register_Action
        (Kernel, "Cut to Clipboard", Command,
         Description => -"Cut the current selection to the clipboard",
         Stock_Id    => Stock_Cut,
         Accel_Key   => GDK_Delete,
         Accel_Mods  => Shift_Mask);

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Clipboard_Command (Command.all).Kind   := Copy;
      Register_Action
        (Kernel, "Copy to Clipboard", Command,
         Description => -"Copy the current selection to the clipboard",
         Stock_Id    => Stock_Copy,
         Accel_Key   => GDK_Insert,
         Accel_Mods  => Primary_Mod_Mask);

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Clipboard_Command (Command.all).Kind   := Paste;
      Register_Action
        (Kernel, "Paste From Clipboard", Command,
         Description =>
           -"Paste the contents of the clipboard into the current text area",
         Stock_Id   => Stock_Paste,
         Accel_Key  => GDK_Insert,
         Accel_Mods => Shift_Mask);

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Clipboard_Command (Command.all).Kind   := Paste_Previous;
      Register_Action
        (Kernel, -"Paste Previous From Clipboard", Command,
         -("Cancel the previous Paste operation, and instead insert the text"
           & " copied before through Copy To Clipboard"),
         Stock_Id   => Stock_Paste,
         Accel_Key  => GDK_Insert,
         Accel_Mods => Primary_Mod_Mask + Shift_Mask);

      --  The menus created above are created before the keymanager_module
      --  is registered, so the default key assignations is not done through
      --  the menu registration itself.
      --  Also, we cannot register these menus afterwards, since these are
      --  used by other modules as reference for placement of other menus.
      --  So we register key bindings manually here.
      --  ??? This can be removed when we have a global mechanism for menu
      --  registering and ordering.
      Bind_Default_Key (Kernel, "/Edit/Cut", "shift-Delete");
      Bind_Default_Key (Kernel, "/Edit/Copy", "primary-Insert");
      Bind_Default_Key (Kernel, "/Edit/Paste", "shift-Insert");
      Bind_Default_Key
        (Kernel, "/Edit/Paste Previous", "primary-shift-Insert");

      --  Gtk+ provides hard-coded bindings for Cut (ctrl-x), Copy (ctrl-c)
      --  and Paste (ctrl-v). Making use of these mechanisms in GPS is not a
      --  good idea, because copying/cutting and pasting within the same buffer
      --  preserves the tags.
      --  For example, when copying and pasting "non-editable" text (such as in
      --  the debugger console), the pasted text becomes non-editable.
      --  Also, pasting a line which contains multiple tag changes (for example
      --  a line containing two keywords) confuses the syntax highlighting.
      --  To avoid this, we completely circumvent the Gtk+ mechanisms by
      --  providing default bindings to the corresponding GPS actions.

      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => -"Cut to Clipboard",
         Default_Key => "primary-x");
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => -"Copy to Clipboard",
         Default_Key => "primary-c");
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => -"Paste From Clipboard",
         Default_Key => "primary-v");

      Command := new Preference_Dialog_Command;
      Register_Action
        (Kernel, "open Preferences", Command,
         Category    => -"Views",
         Stock_Id    => Stock_Preferences,
         Description => -"Open the preferences dialog");
   end Register_Common_Menus;

end GPS.Menu;
