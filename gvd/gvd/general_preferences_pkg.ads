-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                             ACT-Europe                            --
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
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Extra.Font_Combo; use Gtk.Extra.Font_Combo;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with GVD.Color_Combo;

package General_Preferences_Pkg is

   type General_Preferences_Record is new Gtk_Window_Record with record
      --  Additional fields

      Main_Window : Gtk.Window.Gtk_Window;
      --  The main window to which this preferences dialog belongs

      Vbox2 : Gtk_Vbox;
      Notebook1 : Gtk_Notebook;
      Frame1 : Gtk_Frame;
      Table1 : Gtk_Table;
      Label13 : Gtk_Label;
      Button_Hint_Popup_Check : Gtk_Check_Button;
      Button_Hint_Status_Check : Gtk_Check_Button;
      Warn_Multiple_Check : Gtk_Check_Button;
      Label16 : Gtk_Label;
      Statusbar_Timeout_Entry : Gtk_Entry;
      Label17 : Gtk_Label;
      Break_Exception_Check : Gtk_Check_Button;
      Label_General : Gtk_Label;
      Vbox18 : Gtk_Vbox;
      Frame16 : Gtk_Frame;
      Table9 : Gtk_Table;
      Display_Explorer_Check : Gtk_Check_Button;
      Label75 : Gtk_Label;
      File_Name_Bg_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Frame17 : Gtk_Frame;
      Table10 : Gtk_Table;
      Label76 : Gtk_Label;
      Show_Lines_Code_Check : Gtk_Check_Button;
      Show_Line_Numbers_Check : Gtk_Check_Button;
      Label79 : Gtk_Label;
      Label80 : Gtk_Label;
      Label81 : Gtk_Label;
      Editor_Font_Combo : Gtk_Font_Combo;
      Comment_Color_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      String_Color_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Keyword_Color_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Syntax_Highlight_Check : Gtk_Check_Button;
      Strip_Cr_Check : Gtk_Check_Button;
      Tooltips_Check : Gtk_Check_Button;
      Frame18 : Gtk_Frame;
      Hbox7 : Gtk_Hbox;
      Label82 : Gtk_Label;
      Asm_Highlight_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Label_Source : Gtk_Label;
      Frame3 : Gtk_Frame;
      Table3 : Gtk_Table;
      Label83 : Gtk_Label;
      Detect_Aliases_Check : Gtk_Check_Button;
      Align_Grid_Check : Gtk_Check_Button;
      Label84 : Gtk_Label;
      Label85 : Gtk_Label;
      Label86 : Gtk_Label;
      Label87 : Gtk_Label;
      Look_3d_Check : Gtk_Check_Button;
      Label88 : Gtk_Label;
      Label89 : Gtk_Label;
      Label90 : Gtk_Label;
      Hide_Big_Items_Check : Gtk_Check_Button;
      Label91 : Gtk_Label;
      Big_Item_Spin : Gtk_Spin_Button;
      Display_Grid_Check : Gtk_Check_Button;
      Title_Font_Combo : Gtk_Font_Combo;
      Value_Font_Combo : Gtk_Font_Combo;
      Type_Font_Combo : Gtk_Font_Combo;
      Xref_Color_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Change_Color_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Thaw_Bg_Color_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Title_Color_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Freeze_Bg_Color_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Label_Data : Gtk_Label;
      Frame13 : Gtk_Frame;
      Table8 : Gtk_Table;
      Label92 : Gtk_Label;
      Label93 : Gtk_Label;
      Debug_Font_Combo : Gtk_Font_Combo;
      Debug_Highlight_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Label_Command : Gtk_Label;
      Frame19 : Gtk_Frame;
      Table11 : Gtk_Table;
      Label206 : Gtk_Label;
      Label207 : Gtk_Label;
      Memory_Font_Combo : Gtk_Font_Combo;
      Memory_Default_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Label210 : Gtk_Label;
      Label209 : Gtk_Label;
      Label208 : Gtk_Label;
      Memory_Highlight_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Memory_Selection_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Memory_Modified_Combo : GVD.Color_Combo.Gvd_Color_Combo;
      Label_Memory : Gtk_Label;
      Frame6 : Gtk_Frame;
      Table6 : Gtk_Table;
      Edit_Source_Entry : Gtk_Entry;
      List_Processes_Entry : Gtk_Entry;
      Remote_Shell_Entry : Gtk_Entry;
      Remote_Copy_Entry : Gtk_Entry;
      Label43 : Gtk_Label;
      Label44 : Gtk_Label;
      Label45 : Gtk_Label;
      Label48 : Gtk_Label;
      Label_Helpers : Gtk_Label;
      Hbuttonbox6 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Reset_Button : Gtk_Button;
      Help_Button : Gtk_Button;
      Apply_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
   end record;
   type General_Preferences_Access is access all General_Preferences_Record'Class;

   procedure Gtk_New
     (General_Preferences : out General_Preferences_Access;
      Main_Window         : access Gtk.Window.Gtk_Window_Record'Class);
   procedure Initialize
     (General_Preferences : access General_Preferences_Record'Class;
      Main_Window         : access Gtk.Window.Gtk_Window_Record'Class);

   General_Preferences : General_Preferences_Access;

end General_Preferences_Pkg;
