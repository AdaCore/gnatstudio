-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
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

--  <description>
--  This widget provides a text area to edit the value of variables.
--  It automatically detect references to variables, and highlights them
--  accordingly, so that it is clearer we have a reference.
--  It can also be given a specific size.
--  </description>

with Gtk.Text;
with Gdk.Color;
with Gtk.Handlers;

package Value_Editors is

   type Value_Editor_Record is new Gtk.Text.Gtk_Text_Record with private;
   type Value_Editor is access all Value_Editor_Record'Class;

   procedure Gtk_New (Editor : out Value_Editor);
   --  Create a new value editor for variables

   procedure Initialize (Editor : access Value_Editor_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Set_Visible_Lines
     (Editor : access Value_Editor_Record; Lines : Natural);
   --  Set the number of visible lines in the editor

   procedure Add_Variable_Reference
     (Editor : access Value_Editor_Record; Var_Name : String);
   --  Add a reference to a variable Var_Name at the current location in
   --  the editor.

private
   type Value_Editor_Record is new Gtk.Text.Gtk_Text_Record with record
      Grey : Gdk.Color.Gdk_Color;
      Insert_Id : Gtk.Handlers.Handler_Id;
      Delete_Id : Gtk.Handlers.Handler_Id;
   end record;
end Value_Editors;
