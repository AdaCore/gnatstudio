-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
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

package body GPS.Tree_View.Locations is

   use Glib;
   use Glib.Main;
   use Gtk.Tree_Model;

   package View_Idles is
     new Glib.Main.Generic_Sources (GPS_Locations_Tree_View);

   function On_Row_Expanded_Idle
     (Self : GPS_Locations_Tree_View) return Boolean;
   --  Idle callback used to ensure that the proper path is visible

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Object : in out GPS_Locations_Tree_View) is
   begin
      Object := new GPS_Locations_Tree_View_Record;
      GPS.Tree_View.Locations.Initialize (Object);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Object : in out GPS_Locations_Tree_View;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      Object := new GPS_Locations_Tree_View_Record;
      GPS.Tree_View.Locations.Initialize (Object, Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access GPS_Locations_Tree_View_Record'Class) is
   begin
      GPS.Tree_View.Initialize (Self);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : not null access GPS_Locations_Tree_View_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      GPS.Tree_View.Initialize (Self, Model);
   end Initialize;

   -----------------------------------
   -- On_Lowerst_Model_Row_Inserted --
   -----------------------------------

   overriding procedure On_Lowerst_Model_Row_Inserted
     (Self : not null access GPS_Locations_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access)
   is
      pragma Unreferenced (Self, Iter);

   begin
      if Get_Depth (Path) = 3 then
         Node.Expanded := True;
      end if;
   end On_Lowerst_Model_Row_Inserted;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   overriding procedure On_Row_Expanded
     (Self : not null access GPS_Locations_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access)
   is
      pragma Unreferenced (Node);

   begin
      --  Expansion of one node can raise an expansion of the large number of
      --  children nodes. We do scrolling in the idle callback to be sure the
      --  only first requested node is involved in scrolling.

      if Self.On_Row_Expanded_Handler = No_Source_Id then
         Self.On_Row_Expanded_Path := Copy (Path);
         Self.On_Row_Expanded_Iter := Iter;
         Self.On_Row_Expanded_Handler :=
           View_Idles.Idle_Add
             (On_Row_Expanded_Idle'Access, GPS_Locations_Tree_View (Self));
      end if;
   end On_Row_Expanded;

   --------------------------
   -- On_Row_Expanded_Idle --
   --------------------------

   function On_Row_Expanded_Idle
     (Self : GPS_Locations_Tree_View) return Boolean
   is
      Model      : Gtk_Tree_Model renames Self.Get_Model;
      Path       : Gtk_Tree_Path renames Self.On_Row_Expanded_Path;
      Iter       : Gtk_Tree_Iter renames Self.On_Row_Expanded_Iter;
      Start_Path : Gtk_Tree_Path;
      End_Path   : Gtk_Tree_Path;
      Success    : Boolean;

   begin
      Self.Get_Visible_Range (Start_Path, End_Path, Success);

      if Success
        and then Model.Has_Child (Iter)
      then
         --  Go down till not expanded node or node leaf node is found

         loop
            Down (Path);
            Iter := Model.Children (Iter);

            exit when not Self.Row_Expanded (Path)
              or else not Model.Has_Child (Iter);
         end loop;

         if Compare (Path, End_Path) >= 0 then
            Self.Scroll_To_Cell (Path, null, True, 0.9, 0.1);
         end if;
      end if;

      Path_Free (Start_Path);
      Path_Free (End_Path);
      Path_Free (Path);

      Self.On_Row_Expanded_Path := null;
      Self.On_Row_Expanded_Iter := Null_Iter;
      Self.On_Row_Expanded_Handler := No_Source_Id;

      return False;
   end On_Row_Expanded_Idle;

end GPS.Tree_View.Locations;
