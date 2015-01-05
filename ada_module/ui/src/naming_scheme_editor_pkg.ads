------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Gtk.Window;          use Gtk.Window;
with Gtk.Box;             use Gtk.Box;
with Gtk.Label;           use Gtk.Label;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Button;          use Gtk.Button;
with Gtk.Tree_View;
with Gtk.Tree_Store;

package Naming_Scheme_Editor_Pkg is

   type Naming_Scheme_Editor_Record is new Gtk_Window_Record with record
      Main_Box                  : Gtk_Vbox;
      Standard_Scheme           : Gtk_Combo_Box_Text;
      Spec_Extension            : Gtk_Combo_Box_Text;
      Body_Extension            : Gtk_Combo_Box_Text;
      Separate_Extension        : Gtk_Combo_Box_Text;
      Dot_Replacement           : Gtk_Entry;
      Label_Casing              : Gtk_Label;
      Casing                    : Gtk_Combo_Box_Text;
      Exception_List            : Gtk.Tree_View.Gtk_Tree_View;
      Exception_List_Model      : Gtk.Tree_Store.Gtk_Tree_Store;
      Unit_Name_Entry           : Gtk_Entry;
      Spec_Filename_Entry       : Gtk_Entry;
      Body_Filename_Entry       : Gtk_Entry;
      Update                    : Gtk_Button;
      Label_Naming_Scheme       : Gtk_Label;
      Label_Dot_Replacement     : Gtk_Label;
      Label_Spec_Extensions     : Gtk_Label;
      Label_Body_Extensions     : Gtk_Label;
      Label_Separate_Extensions : Gtk_Label;
   end record;
   type Naming_Scheme_Editor_Access is
     access all Naming_Scheme_Editor_Record'Class;

   procedure Gtk_New (Naming_Scheme_Editor : out Naming_Scheme_Editor_Access);
   procedure Initialize
     (Naming_Scheme_Editor : access Naming_Scheme_Editor_Record'Class);

end Naming_Scheme_Editor_Pkg;
