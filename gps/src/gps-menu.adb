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
with Gtkada.MDI;                   use Gtkada.MDI;
with Gtk.Menu_Item;                use Gtk.Menu_Item;

with GPS.Intl;                   use GPS.Intl;

with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Main_Window;            use GPS.Main_Window;
with Histories;                    use Histories;
with Projects;                     use Projects;

with GNAT.Directory_Operations;    use GNAT.Directory_Operations;
with GNAT.OS_Lib;                  use GNAT.OS_Lib;
with Factory_Data;                 use Factory_Data;
with Ada.Exceptions;               use Ada.Exceptions;
with Traces;                       use Traces;
with Commands.Interactive;         use Commands.Interactive;
with VFS;                          use VFS;

package body GPS.Menu is

   Project_History_Key : constant Histories.History_Key := "project_files";
   --  Key to use in the kernel histories to store the most recently opened
   --  files.
   --  Synchronize with welcome.adb


   type Close_Command is new Interactive_Command with record
      Kernel    : Kernel_Handle;
      Close_All : Boolean;
   end record;
   function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;
   --  Close the current window (or all windows if Close_All is True.


   type On_Reopen is new Menu_Callback_Record with record
      Kernel : Kernel_Handle;
   end record;
   procedure Activate (Callback : access On_Reopen; Item : String);

   procedure On_Project_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project has just changed

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

   procedure On_Save_Desktop
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Save Desktop menu

   procedure On_Save_Default_Desktop
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Save Default Desktop menu

   procedure On_Change_Dir
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Change Directory... menu

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Exit menu

   procedure On_Preferences
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Preferences menu

   procedure On_Open_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
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
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Kernel : constant Kernel_Handle := GPS_Window (Object).Kernel;
   begin
      declare
         Filename : constant Virtual_File :=
           Select_File
             (-"Open Project",
              File_Pattern      => "*.gpr",
              Pattern_Name      => -"Project files",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
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

   -------------------
   -- On_Change_Dir --
   -------------------

   procedure On_Change_Dir
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Kernel : constant Kernel_Handle := GPS_Window (Object).Kernel;
      Dir    : constant String := Select_Directory
        (-"Select a directory",
         History => Get_History (Kernel),
         Parent  => Gtk_Window (Get_Current_Window (Kernel)));

   begin
      if Dir /= "" then
         Change_Dir (Dir);
      end if;
   end On_Change_Dir;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type
   is
      pragma Unreferenced (Context);
      MDI   : MDI_Window;
      Child : MDI_Child;
   begin
      if Command.Close_All then
         if Save_MDI_Children (Command.Kernel) then
            Close_All_Children (Command.Kernel);
         end if;
      else
         MDI := Get_MDI (Command.Kernel);
         Child := Get_Focus_Child (MDI);
         if Child /= null then
            Close (MDI, Get_Widget (Child));
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- On_Exit --
   -------------

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);
   begin
      Quit (GPS_Window (Object));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Exit;

   ---------------------
   -- On_Save_Desktop --
   ---------------------

   procedure On_Save_Desktop
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);
   begin
      Save_Desktop (GPS_Window (Object).Kernel, As_Default_Desktop => False);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Save_Desktop;

   -----------------------------
   -- On_Save_Default_Desktop --
   -----------------------------

   procedure On_Save_Default_Desktop
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);
   begin
      Save_Desktop (GPS_Window (Object).Kernel, As_Default_Desktop => True);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Save_Default_Desktop;

   --------------------
   -- On_Preferences --
   --------------------

   procedure On_Preferences
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);
      Top : constant GPS_Window := GPS_Window (Object);
   begin
      Edit_Preferences (Top.Kernel);

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
      File        : constant String := "/_" & (-"File")     & '/';
      Project     : constant String := "/_" & (-"Project")  & '/';
      Command     : Interactive_Command_Access;
      Reopen_Menu : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Command := new Close_Command;
      Close_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Close_Command (Command.all).Close_All := False;
      Register_Action
        (Kernel, "Close current window", Command,
           -"Close the currently selected window");

      Register_Menu
        (Kernel,
         Parent_Path => File,
         Text        => -"_Close",
         Stock_Image => Stock_Close,
         Callback    => null,
         Command     => Commands.Command_Access (Command),
         Ref_Item    => -"Change Directory...",
         Accel_Key   => GDK_LC_w,
         Accel_Mods  => Control_Mask,
         Add_Before  => False);

      Command := new Close_Command;
      Close_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Close_Command (Command.all).Close_All := True;
      Register_Action
        (Kernel, "Close all windows", Command,
           -"Close all open windows, asking for confirmation when relevant");

      Reopen_Menu := Register_Menu
        (Kernel, Project, -"_Recent", "",
         null, Ref_Item => -"Open...", Add_Before => False);
      Associate (Get_History (Kernel).all,
                 Project_History_Key,
                 Reopen_Menu,
                 new On_Reopen'(Menu_Callback_Record with
                                Kernel => Kernel_Handle (Kernel)));
      Add_Hook (Kernel, Project_Changed_Hook, On_Project_Changed'Access);

      Register_Menu
        (Kernel, Project, -"R_ecompute Project", "",
         On_Project_Recompute'Access, Ref_Item => -"Recent",
         Add_Before => False);

      Register_Menu
        (Kernel,
         Parent_Path => File,
         Text        => -"Close _All",
         Callback    => null,
         Command     => Commands.Command_Access (Command),
         Ref_Item    => -"Close",
         Add_Before  => False);
   end Register_Common_Menus;

   --------------------
   -- GPS_Menu_Items --
   --------------------

   function GPS_Menu_Items return Gtk_Item_Factory_Entry_Access is
      File     : constant String := "/_" & (-"File")     & '/';
      Edit     : constant String := "/_" & (-"Edit")     & '/';
      Project  : constant String := "/_" & (-"Project")  & '/';
      Tools    : constant String := "/_" & (-"Tools");
      Window   : constant String := "/_" & (-"Window");

   begin
      return new Gtk_Item_Factory_Entry_Array'
        (Gtk_New (File & (-"Sa_ve More") & '/' & (-"Desktop"), "",
                  On_Save_Desktop'Access),
         Gtk_New (File & (-"Sa_ve More") & '/' & (-"Default Desktop"), "",
                  On_Save_Default_Desktop'Access),
         Gtk_New (File & "sep1", Item_Type => Separator),
         Gtk_New (File & (-"Change _Directory..."), "", "",
                  On_Change_Dir'Access),
         Gtk_New (File & "sep2", Item_Type => Separator),
         Gtk_New (File & (-"_Exit"), "", "", On_Exit'Access),

         Gtk_New (Edit & (-"_Preferences"), "",
                  Stock_Preferences, On_Preferences'Access),

         Gtk_New (Project & (-"_Open..."), "", "",
                  On_Open_Project'Access),
         Gtk_New (Project & "sep1", Item_Type => Separator),

         Gtk_New (Tools),
         Gtk_New (Window));
   end GPS_Menu_Items;

end GPS.Menu;
