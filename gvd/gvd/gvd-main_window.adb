-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                use Glib;
with Gtk;                 use Gtk;
with Gtk.Accel_Group;     use Gtk.Accel_Group;
with Gtk.Box;             use Gtk.Box;
with Gtk.Menu_Bar;        use Gtk.Menu_Bar;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Object;          use Gtk.Object;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gtkada.Types;
with Factory_Data;
with GVD.Types;           use GVD.Types;
with GVD.Dialogs;         use GVD.Dialogs;
with GVD.Preferences;     use GVD.Preferences;
with GVD.Process;         use GVD.Process;
with GVD.Memory_View;     use GVD.Memory_View;
with Debugger;            use Debugger;

with Language.Ada; use Language.Ada;
with Language.C;   use Language.C;
with Language.Cpp; use Language.Cpp;
with Language;     use Language;

with System;       use System;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body GVD.Main_Window is

   Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => New_String ("preferences_changed"));
   Class_Record : GObject_Class := Uninitialized_Class;

   -----------------------
   -- Cleanup_Debuggers --
   -----------------------

   procedure Cleanup_Debuggers
     (Window : access GVD_Main_Window_Record'Class)
   is
      Tab  : Debugger_Process_Tab;
      Page : Gtk_Widget;

   begin
      --  First switch to the last page (to prevent automatic page
      --  switching when the other pages are deleted, which would fail)
      Set_Page (Window.Process_Notebook, -1);

      loop
         Page := Get_Nth_Page (Window.Process_Notebook, 0);
         exit when Page = null;

         Tab := Process_User_Data.Get (Page);
         Tab.Exiting := True;

         begin
            Close (Tab.Debugger);
         exception
            when others =>
               --  ??? Would be nice to handle more specific errors, but
               --  since we are exiting, ignore any exception instead of
               --  generating unfriendly bug boxes
               null;
         end;

         Remove_Page (Window.Process_Notebook, 0);
      end loop;

      Free (Window.Command_History);
   end Cleanup_Debuggers;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window : out GVD_Main_Window;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array) is
   begin
      Main_Window := new GVD_Main_Window_Record;
      GVD.Main_Window.Initialize (Main_Window, Key, Menu_Items);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window : access GVD_Main_Window_Record'Class;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array)
   is
      Menu        : Gtk_Widget;

   begin
      Main_Debug_Window_Pkg.Initialize (Main_Window);
      Initialize_Class_Record
        (Main_Window, Signals, Class_Record, Type_Name => "GvdMainWindow");

      Gtk_New (Main_Window.GVD_Accel_Group);
      Add_Accel_Group (Main_Window, Main_Window.GVD_Accel_Group);
      Gtk_New (Main_Window.Factory, Gtk.Menu_Bar.Get_Type, Key, Main_Window.GVD_Accel_Group);
      Factory_Data.Create_Items
        (Main_Window.Factory, Menu_Items, Main_Window.all'Access);
      Lock (Main_Window.GVD_Accel_Group);
      Menu := Get_Widget (Main_Window.Factory, Key);
      Main_Window.Menu_Bar := Gtk_Menu_Bar (Menu);
      Pack_Start (Main_Window.Vbox, Menu, False, False, 0);
      Reorder_Child (Main_Window.Vbox, Menu, 0);

      Gtk_New (Main_Window.Task_Dialog, Gtk_Window (Main_Window));
      Gtk_New (Main_Window.Thread_Dialog, Gtk_Window (Main_Window));
      Gtk_New (Main_Window.History_Dialog, Gtk_Window (Main_Window));
      Gtk_New (Main_Window.Memory_View, Gtk_Widget (Main_Window));
      Gtk_New (Main_Window.GVD_Accel_Group);
      Lock (Main_Window.GVD_Accel_Group);
      Reset_File_Extensions;
      Add_File_Extensions (Ada_Lang, Get_Pref (Ada_Extensions));
      Add_File_Extensions (C_Lang,   Get_Pref (C_Extensions));
      Add_File_Extensions (Cpp_Lang, Get_Pref (Cpp_Extensions));
   end Initialize;

   -----------------------------
   -- Update_External_Dialogs --
   -----------------------------

   procedure Update_External_Dialogs
     (Window   : access GVD_Main_Window_Record'Class;
      Debugger : Gtk.Widget.Gtk_Widget := null)
   is
      Tab : Debugger_Process_Tab := Debugger_Process_Tab (Debugger);
   begin
      if Debugger = null then
         Tab := Get_Current_Process (Window);
      end if;

      if Tab /= null then
         Update_Call_Stack (Tab);
         Update (Window.Task_Dialog, Tab);
         Update (Window.History_Dialog, Tab);
      end if;
   end Update_External_Dialogs;

   ----------------
   -- Find_Match --
   ----------------

   procedure Find_Match
     (H   : in out History_List;
      Num : in Natural;
      D   : in Direction)
   is
      Data    : GNAT.OS_Lib.String_Access;
      Current : History_Data;
   begin
      begin
         Data := Get_Current (H).Command;
      exception
         when No_Such_Item =>
            Data := null;
      end;

      loop
         if D = Backward then
            Move_To_Previous (H);
         else
            Move_To_Next (H);
         end if;

         Current := Get_Current (H);

         exit when Current.Debugger_Num = Num
           and then Current.Mode /= Hidden
           and then (Data = null
                     or else Current.Command.all /= Data.all);
      end loop;
   end Find_Match;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Window : access GVD_Main_Window_Record'Class) is
   begin
      Widget_Callback.Emit_By_Name
        (Gtk_Widget (Window), "preferences_changed");
      Reset_File_Extensions;
      Add_File_Extensions (Ada_Lang, Get_Pref (Ada_Extensions));
      Add_File_Extensions (C_Lang,   Get_Pref (C_Extensions));
      Add_File_Extensions (Cpp_Lang, Get_Pref (Cpp_Extensions));
   end Preferences_Changed;

end GVD.Main_Window;
