-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glide_Main_Window; use Glide_Main_Window;
with Glide_Page;        use Glide_Page;
with Glide_Consoles;    use Glide_Consoles;
with GVD.Process;       use GVD.Process;
with GNAT.IO;           use GNAT.IO;

package body Glide_Kernel.Console is

   function Get_Console (Kernel : access Kernel_Handle_Record'Class)
      return Glide_Console;
   --  Return the console associated with the kernel.

   procedure Console_Destroyed
     (Console : access Glib.Object.GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Called when the console has been destroyed.

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console (Kernel : access Kernel_Handle_Record'Class)
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

   ------------------------
   -- Initialize_Console --
   ------------------------

   procedure Initialize_Console
     (Kernel         : access Kernel_Handle_Record'Class)
   is
      Console : constant Glide_Console := Get_Console (Kernel);
   begin
      if Console /= null then
         Kernel_Callback.Connect
           (Console, "destroy",
            Kernel_Callback.To_Marshaller (Console_Destroyed'Access),
            Kernel_Handle (Kernel));
      end if;
   end Initialize_Console;

end Glide_Kernel.Console;
