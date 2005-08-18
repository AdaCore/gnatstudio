-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
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

with Glib;                         use Glib;
with Glib.Object;                  use Glib.Object;
with Gdk.Types;                    use Gdk.Types;
with Gdk.Types.Keysyms;            use Gdk.Types.Keysyms;
with Gtk.Stock;                    use Gtk.Stock;
with Gtk.Window;                   use Gtk.Window;
with Gtkada.File_Selector;         use Gtkada.File_Selector;
with Gtk.Menu_Item;                use Gtk.Menu_Item;
with Gtk.Widget;                   use Gtk.Widget;

with GPS.Intl;                     use GPS.Intl;

with GPS.Kernel;                   use GPS.Kernel;
with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Clipboard;         use GPS.Kernel.Clipboard;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;       use GPS.Kernel.Preferences;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with Histories;                    use Histories;
with Projects;                     use Projects;
with Commands.Interactive;         use Commands, Commands.Interactive;

with GNAT.OS_Lib;                  use GNAT.OS_Lib;
with Ada.Exceptions;               use Ada.Exceptions;
with Traces;                       use Traces;
with VFS;                          use VFS;

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
      Load_Project (Callback.Kernel, Item);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Activate;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class) is
      Filename : constant String := Normalize_Pathname
        (Project_Path (Get_Project (Kernel)), Resolve_Links => False);
   begin
      if Filename /= "" then
         Add_To_History (Kernel, Project_History_Key, Filename);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
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
            Load_Project (Kernel, Full_Name (Filename).all);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Open_Project;

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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Preferences;

   ---------------------------
   -- Register_Common_Menus --
   ---------------------------

   procedure Register_Common_Menus
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Edit        : constant String := "/_" & (-"Edit")     & '/';
      Project     : constant String := "/_" & (-"Project")  & '/';
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

      Reopen_Menu := Register_Menu
        (Kernel, Project, -"_Recent", "", null);
      Associate (Get_History (Kernel).all,
                 Project_History_Key,
                 Reopen_Menu,
                 new On_Reopen'(Menu_Callback_Record with
                                Kernel => Kernel_Handle (Kernel)));
      Add_Hook (Kernel, Project_Changed_Hook, On_Project_Changed'Access);

      Register_Menu
        (Kernel, Project, -"R_ecompute Project", "",
         On_Project_Recompute'Access);

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
   end Register_Common_Menus;

end GPS.Menu;
