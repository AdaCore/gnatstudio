-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glib.Object;          use Glib.Object;
with Glide_Main_Window;    use Glide_Main_Window;
with Glide_Page;           use Glide_Page;
with Glide_Consoles;       use Glide_Consoles;
with Glide_Intl;           use Glide_Intl;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with GVD.Process;          use GVD.Process;
with GNAT.IO;              use GNAT.IO;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.MDI;           use Gtkada.MDI;

package body Glide_Kernel.Console is

   Console_Module_Id   : Glide_Kernel.Module_ID;
   Console_Module_Name : constant String := "Glide_Kernel.Console";

   function Get_Console
     (Kernel : access Kernel_Handle_Record'Class) return Glide_Console;
   --  Return the console associated with the kernel.

   procedure Console_Destroyed
     (Console : access Glib.Object.GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Called when the console has been destroyed.

   function Console_Delete_Event
     (Console : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Prevent the destrution of the console in the MDI

   procedure On_Clear_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for File->Console->Clear menu.

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Kernel : access Kernel_Handle_Record'Class)
      return Glide_Console
   is
      Top : constant Glide_Window := Glide_Window (Kernel.Main_Window);
   begin
      if Top /= null
        and then Get_Current_Process (Top) /= null
      then
         return Glide_Page.Glide_Page (Get_Current_Process (Top)).Console;
      end if;

      return null;
   end Get_Console;

   -----------
   -- Clear --
   -----------

   procedure Clear (Kernel : access Kernel_Handle_Record'Class) is
      Console : constant Glide_Console := Get_Console (Kernel);
   begin
      if Console /= null then
         Clear (Console);
      end if;
   end Clear;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Kernel         : access Kernel_Handle_Record'Class;
      Text           : String;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True;
      Mode           : Message_Type := Info)
   is
      Console : constant Glide_Console := Get_Console (Kernel);
   begin
      if Console = null then
         Put_Line (Text);
      else
         Insert (Console, Text, Highlight_Sloc, Add_LF, Mode);
      end if;
   end Insert;

   -----------------------
   -- Console_Destroyed --
   -----------------------

   procedure Console_Destroyed
     (Console : access Glib.Object.GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Console);
      Top : constant Glide_Window := Glide_Window (Kernel.Main_Window);
   begin
      if Top /= null
        and then Get_Current_Process (Top) /= null
      then
         Glide_Page.Glide_Page (Get_Current_Process (Top)).Console := null;
      end if;
   end Console_Destroyed;

   --------------------------
   -- Console_Delete_Event --
   --------------------------

   function Console_Delete_Event
     (Console : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Console);
   begin
      return True;
   end Console_Delete_Event;

   ----------------------
   -- On_Clear_Console --
   ----------------------

   procedure On_Clear_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Clear (Kernel);
   end On_Clear_Console;

   ------------------------
   -- Initialize_Console --
   ------------------------

   procedure Initialize_Console
     (Kernel         : access Kernel_Handle_Record'Class)
   is
      Top : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Console : Glide_Console;
      Child : MDI_Child;
   begin
      if Top /= null
        and then Get_Current_Process (Top) /= null
      then
         Gtk_New (Console, Kernel);
         Child := Put (Get_MDI (Kernel), Console);
         Set_Title (Child, "GPS Console");
         Set_Dock_Side (Child, Bottom);
         Dock_Child (Child);
         Raise_Child (Child);

         Glide_Page.Glide_Page (Get_Current_Process (Top)).Console := Console;

         Kernel_Callback.Connect
           (Console, "destroy",
            Kernel_Callback.To_Marshaller (Console_Destroyed'Access),
            Kernel_Handle (Kernel));
         Return_Callback.Connect
           (Console, "delete_event",
            Return_Callback.To_Marshaller (Console_Delete_Event'Access));
      end if;
   end Initialize_Console;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      File    : constant String := '/' & (-"File");
      Console : constant String := File & '/' & (-"Console");
      Mitem   : Gtk_Menu_Item;

   begin
      Register_Module
        (Module       => Console_Module_Id,
         Kernel       => Kernel,
         Module_Name  => Console_Module_Name,
         Priority     => Default_Priority);

      Register_Menu
        (Kernel, Console, Ref_Item => -"Close");
      Register_Menu
        (Kernel, Console, -"Clear", "", On_Clear_Console'Access);
      Register_Menu
        (Kernel, Console, -"Save As...", "", null, Sensitive => False);
      --             On_Save_Console_As'Access);
      Register_Menu
        (Kernel, Console, -"Load Contents...", "", null, Sensitive => False);
      --             On_Load_To_Console'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, File, Mitem, Ref_Item => -"Close");
   end Register_Module;

end Glide_Kernel.Console;
