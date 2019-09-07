------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

--  This package contains implementation of the CodePeer's message categories
--  filter criteria editor. It is used by Summary Report form.

with Glib;
with Gtk.Widget;

private with Gtk.Check_Button;
private with Gtk.Scrolled_Window;
private with Gtk.Tree_View;

with CodePeer.Generic_Criteria_Models;
with CodePeer.Generic_Ordered_Set_Models;

generic
   type Item is limited private;
   type Item_Access is access all Item;
   with function Get_Name (Self : Item) return String;
   with function Get_Tooltip (Self : Item) return String;
   with function "<" (Left : Item_Access; Right : Item_Access) return Boolean;
   with package Item_Sets is new Ada.Containers.Ordered_Sets (Item_Access);
   Enable_Tooltips : Boolean := False;

package CodePeer.Generic_Criteria_Editors is

   type Criteria_Editor_Record is
     new Gtk.Widget.Gtk_Widget_Record with private;

   type Criteria_Editor is access all Criteria_Editor_Record'Class;

   procedure Gtk_New
     (Editor         : out Criteria_Editor;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean);

   procedure Initialize
     (Self           : not null access Criteria_Editor_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean);

   function Get_Visible_Items
     (Self : access Criteria_Editor_Record'Class) return Item_Sets.Set;
   --  Returns a set of selected message categories

   Signal_Criteria_Changed : constant Glib.Signal_Name;
   --  This signal emitted by the editor in the case of the criteria change

private

   package Ordered_Set_Models is
     new CodePeer.Generic_Ordered_Set_Models
           (Item, Item_Access, "<", Item_Sets);
   package Criteria_Models is
     new CodePeer.Generic_Criteria_Models
       (Item,
        Item_Access,
        Get_Name,
        Get_Tooltip,
        "<",
        Item_Sets,
        Ordered_Set_Models);

   type Criteria_Editor_Record is
     new Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
      record
         Model  : Criteria_Models.Criteria_Model;
         View   : Gtk.Tree_View.Gtk_Tree_View;
         Toggle : Gtk.Check_Button.Gtk_Check_Button;
      end record;

   Signal_Criteria_Changed : constant Glib.Signal_Name := "criteria-changed";

end CodePeer.Generic_Criteria_Editors;
