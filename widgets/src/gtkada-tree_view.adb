------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Glib.Object;     use Glib.Object;
with Glib;            use Glib;

with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package body Gtkada.Tree_View is

   procedure Row_Expanded_Callback
     (Widget      : access Gtk_Tree_View_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path);
   --  Callback for the "row_expanded" signal.

   procedure Row_Collapsed_Callback
     (Widget      : access Gtk_Tree_View_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path);
   --  Callback for the "row_collapsed" signal.

   procedure Row_Inserted_Callback
     (Widget : access GObject_Record'Class;
      Path   : Gtk_Tree_Path;
      Iter   : Gtk_Tree_Iter);   --  relative to Widget.Model always
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
         Filter_Iter := Store_Iter;
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
         Store_Iter := Filter_Iter;
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
   -- Get_Store_Path_For_Filter_Path --
   ------------------------------------

   function Get_Store_Path_For_Filter_Path
     (Self        : access Tree_View_Record'Class;
      Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
   begin
      if Self.Filter /= null then
         return Self.Filter.Convert_Path_To_Child_Path (Filter_Path);
      else
         return Copy (Filter_Path);
      end if;
   end Get_Store_Path_For_Filter_Path;

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
     (Widget      : access Gtk_Tree_View_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      pragma Unreferenced (Filter_Path);
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

      if Filter_Iter = Null_Iter then
         return;
      end if;

      Tree.Convert_To_Store_Iter (Store_Iter, Filter_Iter);
      Set (Tree.Model, Store_Iter, Tree.Expanded_State_Column, True);

      if Tree.Filter /= null then
         Iter := Children (Tree.Filter, Filter_Iter);
      else
         Iter := Children (Tree.Model, Filter_Iter);
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
     (Widget      : access Gtk_Tree_View_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      pragma Unreferenced (Filter_Path);
      Tree       : constant Tree_View := Tree_View (Widget);
      Store_Iter : Gtk_Tree_Iter;
   begin
      Tree.Convert_To_Store_Iter (Store_Iter, Filter_Iter);
      Set (Tree.Model, Store_Iter, Tree.Expanded_State_Column, False);
   end Row_Collapsed_Callback;

   ---------------------------
   -- Row_Inserted_Callback --
   ---------------------------

   procedure Row_Inserted_Callback
     (Widget : access GObject_Record'Class;
      Path   : Gtk_Tree_Path;
      Iter   : Gtk_Tree_Iter)   --  relative to Widget.Model always
   is
      pragma Unreferenced (Path);
      Tree : constant Tree_View := Tree_View (Widget);
   begin
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

      --  Tree columns in Gtk+ are numbered starting from 0
      Widget.Expanded_State_Column := Gint (Real_Column_Types'Length - 1);

      if Filtered then
         Gtk_New (Widget.Model, Real_Column_Types);
         Gtk_New (Widget.Filter, +Widget.Model);
         Initialize (Gtk_Tree_View (Widget), +Widget.Filter);

      else
         Gtk_New (Widget.Model, Real_Column_Types);
         Initialize (Gtk_Tree_View (Widget), +Widget.Model);
      end if;

      --  We can't connect with After => True, because then the Gtk_Tree_Iter
      --  might have been modified, and in particular would no longer be
      --  relative to the filter model if we use one.

      Widget.On_Row_Expanded (Row_Expanded_Callback'Access, After => False);
      Widget.On_Row_Collapsed (Row_Collapsed_Callback'Access, After => False);

      --  Consider any newly inserted row as a collapsed row,
      --  set the flag accordingly.
      On_Row_Inserted
        (+Widget.Model, Row_Inserted_Callback'Access, Widget, After => False);
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
