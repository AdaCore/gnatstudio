-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

package body Code_Peer.Message_Categories_Criteria_Models is

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : access Messages_Filter_Model_Record) is
   begin
      Code_Peer.Message_Categories_Models.Message_Categories_Model_Record
        (Self.all).Clear;

      Self.Categories.Clear;
   end Clear;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Messages_Filter_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Active_Column =>
            return Glib.GType_Boolean;

         when Name_Column =>
            return Glib.GType_String;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Messages_Filter_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Column_Count;
   end Get_N_Columns;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Messages_Filter_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
   begin
      case Column is
         when Active_Column =>
            Glib.Values.Init (Value, Glib.GType_Boolean);
            Glib.Values.Set_Boolean
              (Value, Self.Categories.Contains (Self.Category_At (Iter)));

         when Name_Column =>
            Glib.Values.Init (Value, Glib.GType_String);
            Glib.Values.Set_String (Value, Self.Category_At (Iter).Name.all);

         when others =>
            Glib.Values.Init (Value, Glib.GType_Invalid);
      end case;
   end Get_Value;

   ----------------------------
   -- Get_Visible_Categories --
   ----------------------------

   function Get_Visible_Categories
     (Self : access Messages_Filter_Model_Record'Class)
      return Code_Peer.Message_Category_Sets.Set is
   begin
      return Self.Categories;
   end Get_Visible_Categories;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model      : in out Messages_Filter_Model;
      Categories : Code_Peer.Message_Category_Sets.Set)
   is
   begin
      Model := new Messages_Filter_Model_Record;

      Initialize (Model, Categories);
   end Gtk_New;

   ----------
   -- Hide --
   ----------

   procedure Hide
     (Self     : access Messages_Filter_Model_Record'Class;
      Category : Code_Peer.Message_Category_Access)
   is
   begin
      Self.Categories.Exclude (Category);
      Self.Row_Changed (Category);
   end Hide;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : access Messages_Filter_Model_Record'Class;
      Categories : Code_Peer.Message_Category_Sets.Set) is
   begin
      Code_Peer.Message_Categories_Models.Initialize (Self, Categories);

      Self.Categories := Categories;
   end Initialize;

   ----------
   -- Show --
   ----------

   procedure Show
     (Self     : access Messages_Filter_Model_Record'Class;
      Category : Code_Peer.Message_Category_Access)
   is
   begin
      Self.Categories.Include (Category);
      Self.Row_Changed (Category);
   end Show;

end Code_Peer.Message_Categories_Criteria_Models;
