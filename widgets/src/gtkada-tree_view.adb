-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2009, AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib.Object;     use Glib.Object;
with Glib.Values;     use Glib.Values;
with Glib;            use Glib;

with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

with Gtkada.Handlers; use Gtkada.Handlers;

package body Gtkada.Tree_View is

   procedure Row_Expanded_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_expanded" signal.

   procedure Row_Collapsed_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_collapsed" signal.

   procedure Row_Inserted_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_inserted" signal.

   ----------------------------
   -- Convert_To_Filter_Iter --
   ----------------------------

   procedure Convert_To_Filter_Iter
     (Self        : access Tree_View_Record'Class;
      Filter_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Store_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      if Self.Filter /= null then
         Self.Filter.Convert_Child_Iter_To_Iter (Filter_Iter, Store_Iter);

      else
         Gtk.Tree_Model.Iter_Copy (Store_Iter, Filter_Iter);
      end if;
   end Convert_To_Filter_Iter;

   ---------------------------
   -- Convert_To_Store_Iter --
   ---------------------------

   procedure Convert_To_Store_Iter
     (Self        : access Tree_View_Record'Class;
      Store_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      if Self.Filter /= null then
         Self.Filter.Convert_Iter_To_Child_Iter (Store_Iter, Filter_Iter);

      else
         Gtk.Tree_Model.Iter_Copy (Filter_Iter, Store_Iter);
      end if;
   end Convert_To_Store_Iter;

   ------------------------------------
   -- Get_Filter_Path_For_Store_Iter --
   ------------------------------------

   function Get_Filter_Path_For_Store_Iter
     (Self       : access Tree_View_Record'Class;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if Self.Filter /= null then
         Self.Filter.Convert_Child_Iter_To_Iter (Iter, Store_Iter);
         return Self.Filter.Get_Path (Iter);

      else
         return Self.Model.Get_Path (Store_Iter);
      end if;
   end Get_Filter_Path_For_Store_Iter;

   ------------------------------------
   -- Get_Store_Iter_For_Filter_Path --
   ------------------------------------

   function Get_Store_Iter_For_Filter_Path
     (Self        : access Tree_View_Record'Class;
      Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if Self.Filter /= null then
         Iter := Self.Filter.Get_Iter (Filter_Path);
         Self.Filter.Convert_Iter_To_Child_Iter (Iter, Iter);

         return Iter;

      else
         return Self.Model.Get_Iter (Filter_Path);
      end if;
   end Get_Store_Iter_For_Filter_Path;

   ---------------------------
   -- Row_Expanded_Callback --
   ---------------------------

   procedure Row_Expanded_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues)
   is
      Tree       : constant Tree_View := Tree_View (Widget);
      Iter       : Gtk_Tree_Iter;
      Store_Iter : Gtk_Tree_Iter;
      Path       : Gtk_Tree_Path;
      Dummy      : Boolean;
      pragma Unreferenced (Dummy);

   begin
      if Tree.Lock then
         return;
      end if;

      Get_Tree_Iter (Nth (Params, 1), Iter);

      Tree.Convert_To_Store_Iter (Store_Iter, Iter);

      Set (Tree.Model, Store_Iter, Tree.Expanded_State_Column, True);

      if Tree.Filter /= null then
         Iter := Children (Tree.Filter, Iter);

      else
         Iter := Children (Tree.Model, Iter);
      end if;

      while Iter /= Null_Iter loop
         Tree.Convert_To_Store_Iter (Store_Iter, Iter);

         if Get_Boolean
           (Tree.Model, Store_Iter, Tree.Expanded_State_Column)
         then
            if Tree.Filter /= null then
               Path := Tree.Filter.Get_Path (Iter);

            else
               Path := Tree.Model.Get_Path (Iter);
            end if;

            Dummy := Expand_Row (Tree, Path, False);
            Path_Free (Path);
         end if;

         if Tree.Filter /= null then
            Next (Tree.Filter, Iter);

         else
            Next (Tree.Model, Iter);
         end if;
      end loop;
   end Row_Expanded_Callback;

   ----------------------------
   -- Row_Collapsed_Callback --
   ----------------------------

   procedure Row_Collapsed_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues)
   is
      Tree : constant Tree_View := Tree_View (Widget);
      Iter : Gtk_Tree_Iter;
   begin
      Get_Tree_Iter (Nth (Params, 1), Iter);
      Set (Tree.Model, Iter, Tree.Expanded_State_Column, False);
   end Row_Collapsed_Callback;

   ---------------------------
   -- Row_Inserted_Callback --
   ---------------------------

   procedure Row_Inserted_Callback
     (Widget : access GObject_Record'Class;
      Params : Glib.Values.GValues)
   is
      Tree : constant Tree_View := Tree_View (Widget);
      Iter : Gtk_Tree_Iter;
   begin
      Get_Tree_Iter (Nth (Params, 2), Iter);
      Set (Tree.Model, Iter, Tree.Expanded_State_Column, False);
   end Row_Inserted_Callback;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget       : out Tree_View;
      Column_Types : GType_Array;
      Filtered     : Boolean := False) is
   begin
      Widget := new Tree_View_Record;
      Initialize (Widget, Column_Types, Filtered);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget       : access Tree_View_Record'Class;
      Column_Types : GType_Array;
      Filtered     : Boolean)
   is
      Real_Column_Types : GType_Array
        (Column_Types'First .. Column_Types'Last + 1);
      --  Reserve space for expanded state

   begin
      Real_Column_Types := Column_Types & (GType_Boolean);
      Widget.Expanded_State_Column := Gint (Real_Column_Types'Last);

      if Filtered then
         Gtk_New (Widget.Model, Real_Column_Types);
         Gtk_New (Widget.Filter, Widget.Model);
         Initialize (Gtk_Tree_View (Widget), Widget.Filter);

      else
         Gtk_New
           (Widget.Model,
            Real_Column_Types
              (Real_Column_Types'First .. Real_Column_Types'Last - 1));
         Initialize (Gtk_Tree_View (Widget), Widget.Model);
      end if;

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Widget,
         Signal_Row_Expanded,
         Row_Expanded_Callback'Access,
         Widget,
         After => True);

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Widget,
         Signal_Row_Collapsed,
         Row_Collapsed_Callback'Access,
         Widget,
         After => True);

      --  Consider any newly inserted row as a collapsed row,
      --  set the flag accordingly.

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Widget.Model,
         Signal_Row_Inserted,
         Row_Inserted_Callback'Access,
         Widget,
         After => True);
   end Initialize;

   ------------------
   -- Get_Expanded --
   ------------------

   function Get_Expanded
     (Widget : access Tree_View_Record;
      Iter   : Gtk_Tree_Iter) return Boolean is
   begin
      if Iter = Null_Iter then
         return True;
      else
         return Get_Boolean (Widget.Model, Iter, Widget.Expanded_State_Column);
      end if;
   end Get_Expanded;

end Gtkada.Tree_View;
