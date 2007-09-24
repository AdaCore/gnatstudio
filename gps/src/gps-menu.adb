-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2001-2007, AdaCore                 --
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
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Actions;     use GPS.Kernel.Actions;
with GPS.Kernel.Clipboard;   use GPS.Kernel.Clipboard;
with GPS.Kernel.Console;     use GPS.Kernel.Console;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Main_Window;        use GPS.Main_Window;
with Histories;              use Histories;
with Projects;               use Projects;
with Traces;                 use Traces;
with VFS;                    use VFS;

package body GPS.Menu is

   Project_History_Key : constant Histories.History_Key := "project_files";
   --  Key to use in the kernel histories to store the most recently opened
   --  files.
   --  Synchronize with welcome.adb

   type On_Reopen is new Menu_Callback_Record with record
      Kernel : Kernel_Handle;
   end record;
   procedure Activate (Callback : access On_Reopen; Item : String);

   procedure On_Project_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project has just changed

   type Clipboard_Kind is (Cut, Copy, Paste, Paste_Previous);
   type Clipboard_Command is new Interactive_Command with record
      Kernel : Kernel_Handle;
      Kind   : Clipboard_Kind;
   end record;
   function Execute
     (Command : access Clipboard_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  Perform the various actions associated with the clipboard

   procedure On_Save_Desktop
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save Desktop menu

   procedure On_Save_Default_Desktop
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save Default Desktop menu

   procedure On_Change_Dir
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Change Directory... menu

   procedure On_Save_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Save All menu

   procedure On_Exit
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Exit menu

   -------------
   -- On_Exit --
   -------------

   procedure On_Exit
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      GPS.Main_Window.Quit (GPS_Window (Get_Main_Window (Kernel)));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Exit;

   -------------------
   -- On_Change_Dir --
   -------------------

   procedure On_Change_Dir
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Dir    : Virtual_File;

   begin
      Dir := Select_Directory
        (-"Select a directory",
         History           => Get_History (Kernel),
         Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
         Parent            => Gtk_Window (Get_Current_Window (Kernel)));

      if Dir /= No_File then
         Change_Dir (Dir);
      end if;

   exception
      when VFS_Directory_Error =>
         GPS.Kernel.Console.Insert
           (Kernel,
            "Cannot change to directory: " &
            Full_Name (Dir).all,
            Mode => Error);
      when E : others => Trace (Exception_Handle, E);
   end On_Change_Dir;

   -----------------
   -- On_Save_All --
   -----------------

   procedure On_Save_All
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Ignore : Boolean;
      pragma Unreferenced (Widget, Ignore);

   begin
      Ignore := Save_MDI_Children (Kernel, Force => False);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Save_All;

   ---------------------
   -- On_Save_Desktop --
   ---------------------

   procedure On_Save_Desktop
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Save_Desktop (Kernel, As_Default_Desktop => False);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Save_Desktop;

   -----------------------------
   -- On_Save_Default_Desktop --
   -----------------------------

   procedure On_Save_Default_Desktop
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Save_Desktop (Kernel, As_Default_Desktop => True);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Save_Default_Desktop;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Clipboard_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
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

   procedure Activate (Callback : access On_Reopen; Item : String) is
   begin
      Load_Project (Callback.Kernel, Create (Item));

   exception
      when E : others => Trace (Exception_Handle, E);
   end Activate;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class) is
      Project : constant Project_Type := Get_Project (Kernel);
      Path    : constant Virtual_File := Project_Path (Project);
   begin
      if Status (Project) = From_File and then Path /= VFS.No_File then
         Add_To_History
           (Kernel, Project_History_Key,
            Full_Name (Path, Normalize => False).all);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Project_Changed;

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Preferences
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Edit->Preferences menu

   procedure On_Open_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Project->Open menu

   procedure On_Open_Remote_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Project->Open remote menu

   procedure On_Project_Recompute
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->Recompute Project menu

   --------------------------
   -- On_Project_Recompute --
   --------------------------

   procedure On_Project_Recompute
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Reload_Project_If_Needed (Kernel);
      Recompute_View (Kernel);
   end On_Project_Recompute;

   ---------------------
   -- On_Open_Project --
   ---------------------

   procedure On_Open_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      declare
         Filename : constant Virtual_File :=
           Select_File
             (-"Open Project",
              File_Pattern      => "*.gpr",
              Pattern_Name      => -"Project files",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));
      begin
         if Filename /= VFS.No_File then
            Load_Project (Kernel, Filename);
         end if;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Open_Project;

   ----------------------------
   -- On_Open_Remote_Project --
   ----------------------------

   procedure On_Open_Remote_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      declare
         Filename : constant Virtual_File :=
           Select_File
             (-"Open Project",
              File_Pattern      => "*.gpr",
              Pattern_Name      => -"Project files",
              Parent            => Get_Current_Window (Kernel),
              Remote_Browsing   => True,
              Kind              => Open_File,
              History           => Get_History (Kernel));
      begin
         if Filename /= VFS.No_File then
            Load_Project (Kernel, Filename);
         end if;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Open_Remote_Project;

   --------------------
   -- On_Preferences --
   --------------------

   procedure On_Preferences
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Edit_Preferences (Kernel);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Preferences;

   ---------------------------
   -- Register_Common_Menus --
   ---------------------------

   procedure Register_Common_Menus
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File        : constant String := '/' & (-"File") & '/';
      Edit        : constant String := "/_" & (-"Edit")     & '/';
      Project     : constant String := "/_" & (-"Project")  & '/';
      Save        : constant String := File & (-"Save _More") & '/';
      Reopen_Menu : Gtk.Menu_Item.Gtk_Menu_Item;
      Command     : Interactive_Command_Access;
   begin
      Register_Menu
        (Kernel, Edit, -"_Preferences",
         Stock_Preferences, On_Preferences'Access,
         Ref_Item => -"Window");

      Register_Menu
        (Kernel, Project, -"_Open...", "",
         On_Open_Project'Access,
         Ref_Item => -"Window");
      Register_Menu
        (Kernel, Project, -"Open From _Host...", "",
         On_Open_Remote_Project'Access);

      Reopen_Menu := Register_Menu
        (Kernel, Project, -"_Recent", "", null);
      Associate (Get_History (Kernel).all,
                 Project_History_Key,
                 Reopen_Menu,
                 new On_Reopen'(Menu_Callback_Record with
                                Kernel => Kernel_Handle (Kernel)));
      Add_Hook (Kernel, Project_Changed_Hook,
                Wrapper (On_Project_Changed'Access),
                Name => "menu.project_changed");

      Register_Menu
        (Kernel, Project, -"R_eload Project", "",
         On_Project_Recompute'Access);

      Register_Menu
        (Kernel, Save, -"_All", "",
         On_Save_All'Access,
         Ref_Item => -"Messages");
      Register_Menu (Kernel, Save, -"_Desktop", "", On_Save_Desktop'Access);
      Register_Menu
        (Kernel, Save, -"D_efault Desktop", "",
         On_Save_Default_Desktop'Access);

      Register_Menu
        (Kernel, File, -"Change _Directory...", "",
         On_Change_Dir'Access, Ref_Item => -"Messages");

      Register_Menu (Kernel, File, -"_Exit", "", On_Exit'Access);

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Clipboard_Command (Command.all).Kind   := Cut;
      Register_Action
        (Kernel, -"Cut to Clipboard", Command,
         -"Cut the current selection to the clipboard");
      Register_Menu (Kernel, Edit, -"_Cut",  Stock_Cut,
                     null, Command,
                     GDK_Delete, Shift_Mask,
                     Ref_Item => -"Preferences");
      Register_Button
        (Kernel, Stock_Cut, Command, -"Cut To Clipboard");

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Clipboard_Command (Command.all).Kind   := Copy;
      Register_Action
        (Kernel, -"Copy to Clipboard", Command,
         -"Copy the current selection to the clipboard");
      Register_Menu (Kernel, Edit, -"C_opy",  Stock_Copy,
                     null, Command,
                     GDK_Insert, Control_Mask,
                     Ref_Item => -"Preferences");
      Register_Button
        (Kernel, Stock_Copy, Command, -"Copy To Clipboard");

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Clipboard_Command (Command.all).Kind   := Paste;
      Register_Action
        (Kernel, -"Paste From Clipboard", Command,
         -"Paste the contents of the clipboard into the current text area");
      Register_Menu (Kernel, Edit, -"P_aste",  Stock_Paste,
                     null, Command,
                     GDK_Insert, Shift_Mask,
                     Ref_Item => -"Preferences");
      Register_Button
        (Kernel, Stock_Paste, Command,
         -"Paste From Clipboard");

      Command := new Clipboard_Command;
      Clipboard_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Clipboard_Command (Command.all).Kind   := Paste_Previous;
      Register_Action
        (Kernel, -"Paste Previous From Clipboard", Command,
         -("Cancel the previous Paste operation, and instead insert the text"
           & " copied before through Copy To Clipboard"));
      Register_Menu (Kernel, Edit, -"Pa_ste Previous",  Stock_Paste,
                     null, Command,
                     GDK_Insert, Control_Mask + Shift_Mask,
                     Ref_Item => -"Preferences");

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
         Default_Key => "control-x");
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => -"Copy to Clipboard",
         Default_Key => "control-c");
      Bind_Default_Key
        (Kernel      => Kernel,
         Action      => -"Paste From Clipboard",
         Default_Key => "control-v");
   end Register_Common_Menus;

end GPS.Menu;
