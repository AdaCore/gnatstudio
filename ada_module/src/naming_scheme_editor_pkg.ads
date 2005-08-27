-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2005                      --
--                              AdaCore                              --
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

with Gtk.Window;          use Gtk.Window;
with Gtk.Box;             use Gtk.Box;
with Gtk.Label;           use Gtk.Label;
with Gtk.Combo;           use Gtk.Combo;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Button;          use Gtk.Button;
with Gtk.Tree_View;
with Gtk.Tree_Store;

package Naming_Scheme_Editor_Pkg is

   type Naming_Scheme_Editor_Record is new Gtk_Window_Record with record
      Main_Box                  : Gtk_Vbox;
      Standard_Scheme           : Gtk_Combo;
      Spec_Extension            : Gtk_Combo;
      Body_Extension            : Gtk_Combo;
      Separate_Extension        : Gtk_Combo;
      Dot_Replacement           : Gtk_Entry;
      Label_Casing              : Gtk_Label;
      Casing                    : Gtk_Combo;
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
