-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;
with Glib.Object;
with Gtk.Accel_Group;  use Gtk.Accel_Group;
with Gtk.Box; use Gtk.Box;
with Gtk.Item_Factory; use Gtk.Item_Factory;
with Gtk.Main;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Window; use Gtk.Window;
with Gtkada.MDI; use Gtkada.MDI;
with Gtk.Dialog; use Gtk.Dialog;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Basic_Types;
with GVD.Types;
with GVD.Histories;
pragma Elaborate_All (GVD.Histories);
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Language_Handlers;
with Ada.Unchecked_Deallocation;

package GVD.Main_Window is

   type History_Data is record
      Mode         : GVD.Types.Command_Type;
      Debugger_Num : Natural;
      Command      : String_Access;
   end record;

   type Debugger_List_Node;
   type Debugger_List_Link is access Debugger_List_Node;

   type Debugger_List_Node is record
      Debugger : Glib.Object.GObject;
      Next     : Debugger_List_Link;
   end record;

   procedure Free is new
     Ada.Unchecked_Deallocation (Debugger_List_Node, Debugger_List_Link);

   package String_History is new GVD.Histories (History_Data);
   use String_History;

   type GVD_Main_Window_Record is new Main_Debug_Window_Record with record
      Process_Mdi         : Gtkada.MDI.MDI_Window;
      --  The main widget

      Menu_Box            : Gtk.Box.Gtk_Hbox;
      Menu_Bar            : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Memory_View         : Gtk.Window.Gtk_Window;
      History_Dialog      : Gtk.Dialog.Gtk_Dialog;
      Thread_Dialog       : Gtk.Dialog.Gtk_Dialog;
      Task_Dialog         : Gtk.Dialog.Gtk_Dialog;
      PD_Dialog           : Gtk.Dialog.Gtk_Dialog;
      Breakpoints_Editor  : Gtk.Window.Gtk_Window;
      TTY_Mode            : Boolean := False;
      Debug_Mode          : Boolean := False;
      Prefix_Directory    : String_Access;
      External_XID        : Glib.Guint32 := 0;

      File_Caches         : Basic_Types.File_Cache_List;
      --  List of data cached for each of the file of the application
      --  This field is handled in GVD.Files

      Command_History     : String_History.History_List;
      --  The history of commands for the current session.

      Sessions_Dir        : String_Access;
      --  The directory containing session files.

      Home_Dir            : String_Access;
      --  The location of the configuration (e.g ~/.gvd) directory.
      --  The preferences file is found in Home_Dir/preferences

      First_Debugger      : Debugger_List_Link;
      --  The pointer to the list of debuggers.

      Current_Debugger    : Glib.Object.GObject;
      --  The current visual debugger.

      Main_Accel_Group    : Gtk_Accel_Group;
      --  The default accelerators for the GVD window.

      Lang_Handler        : Language_Handlers.Language_Handler;
      --  The type used to convert from a filename to a language

      Program_Args        : String_Access;
      --  If non null, name of program to be debugged, and additional
      --  arguments if needed, e.g. "/path/to/hello -h"
   end record;
   type GVD_Main_Window is access all GVD_Main_Window_Record'Class;

   package Main_Window_Idle is new Gtk.Main.Idle (GVD_Main_Window);

   procedure Gtk_New
     (Main_Window : out GVD_Main_Window;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array);
   --  Create a new main window.
   --  Key is a unique string identifying main_window.
   --  Menu_Items is used to create the menu bar.

   procedure Initialize
     (Main_Window : access GVD_Main_Window_Record'Class;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array);
   --  Internal initialize function.

end GVD.Main_Window;
