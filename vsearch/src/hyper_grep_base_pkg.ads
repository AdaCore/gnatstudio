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

with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Label; use Gtk.Label;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Button; use Gtk.Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
package Hyper_Grep_Base_Pkg is

   type Hyper_Grep_Base_Record is new Gtk_Window_Record with record
      Main_Table : Gtk_Vbox;
      Search_Frame : Gtk_Frame;
      Search_Table : Gtk_Table;
      Pattern_Combo : Gtk_Combo;
      Pattern_Entry : Gtk_Entry;
      Search_For_Label : Gtk_Label;
      Scope_Frame : Gtk_Frame;
      Scan_In_Vbox : Gtk_Vbox;
      Whole_Rbutton : Gtk_Radio_Button;
      Comm_Only_Rbutton : Gtk_Radio_Button;
      Strings_Rbutton : Gtk_Radio_Button;
      Comm_Str_Rbutton : Gtk_Radio_Button;
      All_But_Comm_Rbutton : Gtk_Radio_Button;
      Options_Frame : Gtk_Frame;
      Options_Vbox : Gtk_Vbox;
      Case_Check : Gtk_Check_Button;
      Whole_Word_Check : Gtk_Check_Button;
      Regexp_Check : Gtk_Check_Button;
      Files_Frame : Gtk_Frame;
      Files_Table : Gtk_Table;
      Browse_Button : Gtk_Button;
      Files_Label : Gtk_Label;
      Directory_Label : Gtk_Label;
      Directory_Combo : Gtk_Combo;
      Directory_Entry : Gtk_Entry;
      Only_Project_Check : Gtk_Check_Button;
      Subdirs_Check : Gtk_Check_Button;
      Files_Combo : Gtk_Combo;
      Files_Entry : Gtk_Entry;
      Hbuttonbox : Gtk_Hbutton_Box;
      Start_Button : Gtk_Button;
      Stop_Button : Gtk_Button;
      Close_Button : Gtk_Button;
   end record;
   type Hyper_Grep_Base_Access is access all Hyper_Grep_Base_Record'Class;

   procedure Gtk_New (Hyper_Grep_Base : out Hyper_Grep_Base_Access);
   procedure Initialize
     (Hyper_Grep_Base : access Hyper_Grep_Base_Record'Class);

end Hyper_Grep_Base_Pkg;
