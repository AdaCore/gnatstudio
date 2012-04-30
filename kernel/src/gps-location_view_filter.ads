------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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

private with GNAT.Expect;
private with GNAT.Strings;
private with Glib.Values;
private with Gtk.Tree_Model;
with Gtkada.Abstract_Filter_Model;

package GPS.Location_View_Filter is

   type Location_View_Filter_Model_Record is
     new Gtkada.Abstract_Filter_Model.Gtk_Abstract_Filter_Model_Record
       with private;

   type Location_View_Filter_Model is
     access all Location_View_Filter_Model_Record'Class;

   procedure Set_Pattern
     (Self         : not null access Location_View_Filter_Model_Record;
      Pattern      : String;
      Is_Regexp    : Boolean;
      Hide_Matched : Boolean);
   --  Sets pattern to be used for filtering.

   procedure Gtk_New (Model : out Location_View_Filter_Model);

   procedure Initialize
     (Self : not null access Location_View_Filter_Model_Record'Class);

private

   type Location_View_Filter_Model_Record is
     new Gtkada.Abstract_Filter_Model.Gtk_Abstract_Filter_Model_Record
   with record
      Regexp  : GNAT.Expect.Pattern_Matcher_Access;
      Text    : GNAT.Strings.String_Access;
      Is_Hide : Boolean := False;
   end record;

   overriding function Is_Visible
     (Self        : not null access Location_View_Filter_Model_Record;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean;

   overriding procedure Get_Value
     (Self   : access Location_View_Filter_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);
   --  Returns value at the specified position.

end GPS.Location_View_Filter;
