-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Value_Editors; use Value_Editors;
package New_Variable_Editor_Pkg is

   type New_Variable_Editor_Record is new Gtk_Window_Record with record
      Vbox37 : Gtk_Vbox;
      Name_Frame : Gtk_Frame;
      Variable_Name : Gtk_Entry;
      Frame33 : Gtk_Frame;
      Vbox38 : Gtk_Vbox;
      Get_Environment : Gtk_Check_Button;
      Alignment7 : Gtk_Alignment;
      Environment_Table : Gtk_Table;
      Default_Value_Label : Gtk_Label;
      Default_Env_Variable : Gtk_Combo;
      Combo_Entry8 : Gtk_Entry;
      List_Env_Variables : Gtk_Combo;
      Combo_Entry7 : Gtk_Entry;
      Label55 : Gtk_Label;
      Env_Must_Be_Defined : Gtk_Check_Button;
      Frame34 : Gtk_Frame;
      Vbox39 : Gtk_Vbox;
      Typed_Variable : Gtk_Radio_Button;
      Alignment4 : Gtk_Alignment;
      Enumeration_Scrolled : Gtk_Scrolled_Window;
      Enumeration_Value : Value_Editor;
      Untyped_List_Variable : Gtk_Radio_Button;
      Untyped_Alignment : Gtk_Alignment;
      List_Scrolled : Gtk_Scrolled_Window;
      List_Value : Value_Editor;
      Untyped_Single_Variable : Gtk_Radio_Button;
      Single_Alignment : Gtk_Alignment;
      Single_Value : Value_Editor;
      Hseparator4 : Gtk_Hseparator;
      Hbuttonbox3 : Gtk_Hbutton_Box;
      Add_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
   end record;
   type New_Variable_Editor_Access is access all New_Variable_Editor_Record'Class;

   procedure Gtk_New (New_Variable_Editor : out New_Variable_Editor_Access);
   procedure Initialize (New_Variable_Editor : access New_Variable_Editor_Record'Class);

end New_Variable_Editor_Pkg;
