-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Table; use Gtk.Table;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
package Switches_Editor_Pkg is

   type Switches_Editor_Record is new Gtk_Window_Record with record
      Vbox2 : Gtk_Vbox;
      Notebook : Gtk_Notebook;
      Make_Switches : Gtk_Table;
      Make_Dep_Frame : Gtk_Frame;
      Vbox25 : Gtk_Vbox;
      Make_All_Files : Gtk_Check_Button;
      Make_Recompile_Switches : Gtk_Check_Button;
      Make_Minimal_Recompile : Gtk_Check_Button;
      Make_Compile_Frame : Gtk_Frame;
      Vbox26 : Gtk_Vbox;
      Hbox1 : Gtk_Hbox;
      Make_Multiprocessing : Gtk_Check_Button;
      Num_Processes : Gtk_Spin_Button;
      Make_Keep_Going : Gtk_Check_Button;
      Make_Debug : Gtk_Check_Button;
      Make_Mapping_File : Gtk_Check_Button;
      Make_Switches_Entry : Gtk_Entry;
      Label17 : Gtk_Label;
      Ada_Switches : Gtk_Table;
      Ada_Codegen_Frame : Gtk_Frame;
      Vbox19 : Gtk_Vbox;
      Ada_Optimization_Level : Gtk_Combo;
      Ada_Optimization_Level_Entry : Gtk_Entry;
      Ada_No_Inline : Gtk_Check_Button;
      Ada_Interunit_Inlining : Gtk_Check_Button;
      Ada_Unroll_Loops : Gtk_Check_Button;
      Ada_Pic : Gtk_Check_Button;
      Ada_Code_Coverage : Gtk_Check_Button;
      Ada_Instrument_Arcs : Gtk_Check_Button;
      Frame22 : Gtk_Frame;
      Vbox20 : Gtk_Vbox;
      Ada_Overflow_Checking : Gtk_Check_Button;
      Ada_Suppress_All_Checks : Gtk_Check_Button;
      Ada_Stack_Checking : Gtk_Check_Button;
      Ada_Dynamic_Elaboration : Gtk_Check_Button;
      Frame23 : Gtk_Frame;
      Vbox21 : Gtk_Vbox;
      Ada_Full_Errors : Gtk_Check_Button;
      Ada_No_Warnings : Gtk_Check_Button;
      Ada_Warning_Error : Gtk_Check_Button;
      Ada_Elab_Warning : Gtk_Check_Button;
      Ada_Unused_Warning : Gtk_Check_Button;
      Ada_Style_Checks : Gtk_Check_Button;
      Vbox22 : Gtk_Vbox;
      Frame24 : Gtk_Frame;
      Vbox23 : Gtk_Vbox;
      Ada_Debug : Gtk_Check_Button;
      Ada_Assertions : Gtk_Check_Button;
      Ada_Debug_Expanded_Code : Gtk_Check_Button;
      Frame25 : Gtk_Frame;
      Vbox24 : Gtk_Vbox;
      Ada_Language_Extensions : Gtk_Check_Button;
      Ada83_Mode : Gtk_Check_Button;
      Ada_Switches_Entry : Gtk_Entry;
      Label18 : Gtk_Label;
      C_Switches : Gtk_Table;
      Frame41 : Gtk_Frame;
      Vbox48 : Gtk_Vbox;
      C_All_Warnings : Gtk_Check_Button;
      C_No_Warnings : Gtk_Check_Button;
      C_Ansi : Gtk_Check_Button;
      C_Codegen_Frame : Gtk_Frame;
      Vbox46 : Gtk_Vbox;
      C_Optimization_Level : Gtk_Combo;
      C_Optimization_Level_Entry : Gtk_Entry;
      C_No_Inline : Gtk_Check_Button;
      C_Unroll_Loops : Gtk_Check_Button;
      C_Pic : Gtk_Check_Button;
      C_Profile : Gtk_Check_Button;
      C_Code_Coverage : Gtk_Check_Button;
      C_Instrument_Arcs : Gtk_Check_Button;
      Frame44 : Gtk_Frame;
      Vbox51 : Gtk_Vbox;
      C_Debug : Gtk_Check_Button;
      C_Switches_Entry : Gtk_Entry;
      Label56 : Gtk_Label;
      Cpp_Switches : Gtk_Table;
      Frame43 : Gtk_Frame;
      Vbox50 : Gtk_Vbox;
      Cpp_All_Warnings : Gtk_Check_Button;
      Cpp_No_Warnings : Gtk_Check_Button;
      Cpp_Overloaded_Virtual : Gtk_Check_Button;
      Cpp_Switches_Entry : Gtk_Entry;
      Cpp_Codegen_Frame : Gtk_Frame;
      Vbox49 : Gtk_Vbox;
      Cpp_Optimization_Level : Gtk_Combo;
      Cpp_Optimization_Level_Entry : Gtk_Entry;
      Cpp_No_Inline : Gtk_Check_Button;
      Cpp_Unroll_Loops : Gtk_Check_Button;
      Cpp_Pic : Gtk_Check_Button;
      Cpp_Profile : Gtk_Check_Button;
      Cpp_Code_Coverage : Gtk_Check_Button;
      Cpp_Instrument_Arcs : Gtk_Check_Button;
      Cpp_Exceptions : Gtk_Check_Button;
      Cpp_Elide_Constructor : Gtk_Check_Button;
      Cpp_Conserve_Space : Gtk_Check_Button;
      Frame45 : Gtk_Frame;
      Vbox52 : Gtk_Vbox;
      Cpp_Debug : Gtk_Check_Button;
      Label57 : Gtk_Label;
      Binder_Switches : Gtk_Table;
      Binder_Switches_Entry : Gtk_Entry;
      Vbox27 : Gtk_Vbox;
      Binder_Tracebacks : Gtk_Check_Button;
      Binder_Restrictions : Gtk_Check_Button;
      Binder_Static_Gnat : Gtk_Radio_Button;
      Binder_Shared_Gnat : Gtk_Radio_Button;
      Label19 : Gtk_Label;
      Linker_Switches : Gtk_Table;
      Linker_Switches_Entry : Gtk_Entry;
      Vbox40 : Gtk_Vbox;
      Linker_Strip : Gtk_Check_Button;
      Linker_Debug : Gtk_Check_Button;
      Linker_Profile : Gtk_Check_Button;
      Label20 : Gtk_Label;
      Hbuttonbox1 : Gtk_Hbutton_Box;
   end record;
   type Switches_Editor_Access is access all Switches_Editor_Record'Class;

   procedure Gtk_New (Switches_Editor : out Switches_Editor_Access);
   procedure Initialize
     (Switches_Editor : access Switches_Editor_Record'Class);

end Switches_Editor_Pkg;
