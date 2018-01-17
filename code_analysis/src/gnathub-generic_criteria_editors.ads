------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

--  This package contains implementation of the GNATHub's message categories
--  filter criteria editor. It is used by filter views.

with Ada.Containers.Ordered_Sets;

with Glib.Values;
with Gtk.Tree_Model;
with Gtk.Widget;

with GPS.Kernel;
with GNAThub.Generic_Models;

private with Gtk.Check_Button;
private with Gtk.Scrolled_Window;
private with Gtk.Tree_Model_Filter;
private with Gtk.Tree_View;

generic
   type Item is limited private;
   type Item_Access is access all Item;

   Columns : Glib.GType_Array;
   --  Types of columns

   Tooltips : Boolean;
   --  If True - column with index Columns'Last + 1 will be used
   --  as Tooltips column

   with procedure Get_Value
     (Self   : Item_Access;
      View   : Gtk.Widget.Gtk_Widget;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);
   --  Should set column's value in to Value parameter

   with function Get_History_Name
     (Self : Item;
      View : Gtk.Widget.Gtk_Widget)
      return String;
   --  Should return name for history of given item

   with function "<" (Left : Item_Access; Right : Item_Access) return Boolean;
   with package Item_Sets is new Ada.Containers.Ordered_Sets (Item_Access);

   Is_Visible : access function
     (Item : Item_Access;
      View : Gtk.Widget.Gtk_Widget)
   return Boolean := null;
   --  Should be used for filtration if set

package GNAThub.Generic_Criteria_Editors is

   type Criteria_Editor_Record is
     new Gtk.Widget.Gtk_Widget_Record with private;

   type Criteria_Editor is access all Criteria_Editor_Record'Class;

   procedure Gtk_New
     (Editor         : in out Criteria_Editor;
      Kernel         : GPS.Kernel.Kernel_Handle;
      View           : Gtk.Widget.Gtk_Widget;
      Title          : String;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean);

   procedure Initialize
     (Self           : not null access Criteria_Editor_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      View           : Gtk.Widget.Gtk_Widget;
      Title          : String;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean);

   function Get_Visible_Items
     (Self : access Criteria_Editor_Record'Class) return Item_Sets.Set;
   --  Returns a set of selected categories

   procedure Choose
     (Self : access Criteria_Editor_Record'Class;
      Item : Item_Access);
   --  Select Item

   procedure Unselect
     (Self : access Criteria_Editor_Record'Class;
      Item : Item_Access);
   --  Unselect Item

   procedure Update (Self : access Criteria_Editor_Record'Class);

   function Item_By_Path
     (Self : access Criteria_Editor_Record'Class;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path)
      return Item_Access;
   --  Return Item by Path

   procedure Highlight
     (Self : access Criteria_Editor_Record'Class;
      Item : Item_Access);
   --  Hightlight item

   Signal_Criteria_Changed : constant Glib.Signal_Name;
   --  This signal emitted by the editor in the case of the criteria change

private

   package Criteria_Models is
     new GNAThub.Generic_Models
       (Item,
        Item_Access,
        Columns,
        Get_Value,
        Get_History_Name,
        "<",
        Item_Sets);

   type Criteria_Editor_Record is
     new Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
      record
         Parent : Gtk.Widget.Gtk_Widget;
         Model  : Criteria_Models.Criteria_Model;
         Filter : Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
         View   : Gtk.Tree_View.Gtk_Tree_View;
         Toggle : Gtk.Check_Button.Gtk_Check_Button;
      end record;

   function Filter_Visible_Func
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      View  : Gtk.Widget.Gtk_Widget)
      return Boolean;

   package Set_Visible_Func_With_Data is
     new Gtk.Tree_Model_Filter.Set_Visible_Func_User_Data
       (Gtk.Widget.Gtk_Widget);

   Visible : constant Set_Visible_Func_With_Data.
     Gtk_Tree_Model_Filter_Visible_Func := Filter_Visible_Func'Access;

   Signal_Criteria_Changed : constant Glib.Signal_Name := "criteria-changed";

end GNAThub.Generic_Criteria_Editors;
