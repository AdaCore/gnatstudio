-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
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

with Gtk.Widget;      use Gtk.Widget;
with Gtkada.MDI;      use Gtkada.MDI;
with GVD.Process;
with Glide_Kernel;    use Glide_Kernel;
with Glide_Consoles;  use Glide_Consoles;

package body Glide_Page is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Page   : out Glide_Page;
      Window : access Glide_Window_Record'Class) is
   begin
      Page := new Glide_Page_Record;
      Initialize (Page, Window);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Page   : access Glide_Page_Record'Class;
      Window : access Glide_Window_Record'Class)
   is
      Child : MDI_Child;
      Iter  : Child_Iterator;

   begin
      GVD.Process.Initialize (Page, Window);
      Set_Priorities
        (Page.Process_Mdi, (Left => 2, Right => 4, Top => 1, Bottom => 3));

      if Load_Desktop (Window.Kernel) then
         Iter := First_Child (Page.Process_Mdi);

         loop
            Child := Get (Iter);

            exit when Child = null;

            if Get_Widget (Child).all in Glide_Console_Record'Class then
               Page.Console := Glide_Console (Get_Widget (Child));
            end if;

            Next (Iter);
         end loop;

      else
         Gtk_New (Page.Console, Window.Kernel);
         Child := Put (Page.Process_Mdi, Page.Console);
         Set_Title (Child, "Glide Console");
         Set_Dock_Side (Child, Bottom);
         Dock_Child (Child);
         Raise_Child (Child);
      end if;
   end Initialize;

end Glide_Page;
