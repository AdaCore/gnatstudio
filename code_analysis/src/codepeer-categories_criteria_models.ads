------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

private with Ada.Strings.Unbounded;

with Glib.Values;
with Gtk.Tree_Model;

with CodePeer.Message_Categories_Models;

package CodePeer.Categories_Criteria_Models is

   Active_Column : constant := 0;
   Name_Column   : constant := 1;
   Column_Count  : constant := 2;

   type Categories_Criteria_Model_Record is
     new CodePeer.Message_Categories_Models.Message_Categories_Model_Record
       with private;

   type Categories_Criteria_Model is
     access all Categories_Criteria_Model_Record'Class;

   procedure Gtk_New
     (Model          : in out Categories_Criteria_Model;
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : String;
      Categories     : CodePeer.Message_Category_Sets.Set);
   --  Creates new instance. History_Prefix is a prefix to manage persistent
   --  state of selected categories.

   procedure Initialize
     (Self           : access Categories_Criteria_Model_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : String;
      Categories     : CodePeer.Message_Category_Sets.Set);

   procedure Show
     (Self     : access Categories_Criteria_Model_Record'Class;
      Category : CodePeer.Message_Category_Access);

   procedure Hide
     (Self     : access Categories_Criteria_Model_Record'Class;
      Category : CodePeer.Message_Category_Access);

   procedure Show_All (Self : access Categories_Criteria_Model_Record'Class);

   procedure Hide_All (Self : access Categories_Criteria_Model_Record'Class);

   function Get_Visible_Categories
     (Self : access Categories_Criteria_Model_Record'Class)
      return CodePeer.Message_Category_Sets.Set;

   function Is_Empty
     (Self : access Categories_Criteria_Model_Record'Class) return Boolean;
   --  Returns True if there are no selected items in the model

   function Is_Full
     (Self : access Categories_Criteria_Model_Record'Class) return Boolean;
   --  Returns True is all items is selected

   overriding procedure Clear (Self : access Categories_Criteria_Model_Record);

private

   type Categories_Criteria_Model_Record is
     new CodePeer.Message_Categories_Models.Message_Categories_Model_Record
   with record
      Kernel              : GPS.Kernel.Kernel_Handle;
      History_Prefix      : Ada.Strings.Unbounded.Unbounded_String;
      Selected_Categories : CodePeer.Message_Category_Sets.Set;
   end record;

   overriding function Get_N_Columns
     (Self : access Categories_Criteria_Model_Record) return Glib.Gint;

   overriding function Get_Column_Type
     (Self  : access Categories_Criteria_Model_Record;
      Index : Glib.Gint) return Glib.GType;

   overriding procedure Get_Value
     (Self   : access Categories_Criteria_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

end CodePeer.Categories_Criteria_Models;
