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

with Gtk.Container; use Gtk.Container;
with Gtk.Box;       use Gtk.Box;
with Gtk.Paned;     use Gtk.Paned;

package body Dock_Paned is

   ---------
   -- Add --
   ---------

   procedure Add
     (Paned : access Dock_Paned_Record;
      Child : access Gtk_Widget_Record'Class) is
   begin
      if not Paned.Is_Child1 then
         Add1 (Paned, Child);
      elsif not Paned.Is_Child2 then
         Add2 (Paned, Child);
      end if;
   end Add;

   ----------
   -- Add1 --
   ----------

   procedure Add1
     (Paned : access Dock_Paned_Record;
      Child : access Gtk_Widget_Record'Class) is
   begin
      if Paned.Is_Child1 then
         return;
      end if;

      if Paned.Is_Child2 then
         declare
            Child2 : Gtk_Widget := Get_Child (Paned, 0);
         begin
            Ref (Child2);
            Remove (Gtk_Container (Paned), Child2);

            Pack_Start (Paned, Paned.Paned);
            Unref (Paned.Paned);

            Add1 (Paned.Paned, Child);
            Add2 (Paned.Paned, Child2);
            Unref (Child2);

            Show_All (Paned.Paned);
         end;
      else
         Add (Gtk_Container_Record (Paned.all)'Access, Child);
      end if;

      Paned.Is_Child1 := True;
   end Add1;

   ----------
   -- Add2 --
   ----------

   procedure Add2
     (Paned : access Dock_Paned_Record;
      Child : access Gtk_Widget_Record'Class) is
   begin
      if Paned.Is_Child2 then
         return;
      end if;

      if Paned.Is_Child1 then
         declare
            Child1 : Gtk_Widget := Get_Child (Paned, 0);
         begin
            Ref (Child1);
            Remove (Gtk_Container (Paned), Child1);

            Pack_Start (Paned, Paned.Paned);
            Unref (Paned.Paned);

            Add1 (Paned.Paned, Child1);
            Add2 (Paned.Paned, Child);
            Unref (Child1);

            Show_All (Paned.Paned);
         end;
      else
         Add (Gtk_Container_Record (Paned.all)'Access, Child);
      end if;

      Paned.Is_Child2 := True;
   end Add2;

   ---------------
   -- Get_Paned --
   ---------------

   function Get_Paned (Paned : access Dock_Paned_Record) return Gtk_Paned is
   begin
      return Paned.Paned;
   end Get_Paned;

   --------------------
   -- Gtk_New_Hpaned --
   --------------------

   procedure Gtk_New_Hpaned (Widget : out Dock_Paned) is
   begin
      Widget := new Dock_Paned_Record;
      Initialize_Vbox (Widget, False, 0);

      Gtk_New_Hpaned (Widget.Paned);
      Initialize_Hpaned (Widget.Paned);
      Widget.Is_Child1 := False;
      Widget.Is_Child2 := False;

      Ref (Widget.Paned);
   end Gtk_New_Hpaned;

   --------------------
   -- Gtk_New_Vpaned --
   --------------------

   procedure Gtk_New_Vpaned (Widget : out Dock_Paned) is
   begin
      Widget := new Dock_Paned_Record;
      Initialize_Vbox (Widget, False, 0);

      Gtk_New_Vpaned (Widget.Paned);
      Initialize_Vpaned (Widget.Paned);
      Widget.Is_Child1 := False;
      Widget.Is_Child2 := False;

      Ref (Widget.Paned);
   end Gtk_New_Vpaned;

   -----------------------
   -- Initialize_Hpaned --
   -----------------------

   procedure Initialize_Hpaned (Widget : access Dock_Paned_Record'Class) is
   begin
      Initialize_Vbox (Widget, False, 0);
      Initialize_Hpaned (Widget.Paned);
   end Initialize_Hpaned;

   -----------------------
   -- Initialize_Vpaned --
   -----------------------

   procedure Initialize_Vpaned (Widget : access Dock_Paned_Record'Class) is
   begin
      Initialize_Vbox (Widget, False, 0);
      Initialize_Vpaned (Widget.Paned);
   end Initialize_Vpaned;

   -----------------
   -- Dock_Remove --
   -----------------

   procedure Dock_Remove
     (Paned : access Dock_Paned_Record'Class;
      Child : access Gtk_Widget_Record'Class)
   is
      Other_Child : Gtk_Widget;
   begin
      if Paned.Is_Child1
        and then Paned.Is_Child2
      then
         if Gtk_Widget (Child) = Get_Child1 (Paned.Paned) then
            Other_Child := Get_Child2 (Paned.Paned);
            Paned.Is_Child1 := False;
         elsif Gtk_Widget (Child) = Get_Child2 (Paned.Paned) then
            Other_Child := Get_Child1 (Paned.Paned);
            Paned.Is_Child2 := False;
         else
            return;
         end if;
         Remove (Paned.Paned, Child);

         Ref (Other_Child);
         Remove (Paned.Paned, Other_Child);

         Ref (Paned.Paned);
         Remove (Paned, Paned.Paned);

         Add (Gtk_Container_Record (Paned.all)'Access, Other_Child);
         Unref (Other_Child);
      else
         Remove (Gtk_Container (Paned), Child);
      end if;
   end Dock_Remove;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Paned : access Dock_Paned_Record; Position : Gint) is
   begin
      Set_Position (Paned.Paned, Position);
   end Set_Position;

   ---------------------
   -- Set_Handle_Size --
   ---------------------

   procedure Set_Handle_Size
     (Paned : access Dock_Paned_Record;
      Size  : Guint16) is
   begin
      Set_Handle_Size (Paned.Paned, Size);
   end Set_Handle_Size;

   ---------------------
   -- Get_Handle_Size --
   ---------------------

   function Get_Handle_Size
     (Paned : access Dock_Paned_Record) return Guint16 is
   begin
      return Get_Handle_Size (Paned.Paned);
   end Get_Handle_Size;

   ---------------------
   -- Set_Gutter_Size --
   ---------------------

   procedure Set_Gutter_Size
     (Paned : access Dock_Paned_Record;
      Size  : in Guint16) is
   begin
      Set_Gutter_Size (Paned.Paned, Size);
   end Set_Gutter_Size;

   ---------------------
   -- Get_Gutter_Size --
   ---------------------

   function Get_Gutter_Size
     (Paned : access Dock_Paned_Record) return Guint16 is
   begin
      return Get_Gutter_Size (Paned.Paned);
   end Get_Gutter_Size;

end Dock_Paned;

