-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Button; use Gtk.Button;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Scale; use Gtk.Scale;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Object; use Gtk.Object;
package Odd_Preferences_Pkg is

   type Odd_Preferences_Record is new Gtk_Window_Record with record
      Vbox2 : Gtk_Vbox;
      Notebook1 : Gtk_Notebook;
      Frame1 : Gtk_Frame;
      Table1 : Gtk_Table;
      Label14 : Gtk_Label;
      Checkbutton2 : Gtk_Check_Button;
      Checkbutton3 : Gtk_Check_Button;
      Checkbutton4 : Gtk_Check_Button;
      Checkbutton5 : Gtk_Check_Button;
      Label15 : Gtk_Label;
      Radiobutton1 : Gtk_Radio_Button;
      Radiobutton2 : Gtk_Radio_Button;
      Checkbutton6 : Gtk_Check_Button;
      Checkbutton7 : Gtk_Check_Button;
      Checkbutton8 : Gtk_Check_Button;
      Checkbutton9 : Gtk_Check_Button;
      Checkbutton10 : Gtk_Check_Button;
      Button39 : Gtk_Button;
      Label13 : Gtk_Label;
      Label16 : Gtk_Label;
      Label17 : Gtk_Label;
      Entry2 : Gtk_Entry;
      Label7 : Gtk_Label;
      Frame2 : Gtk_Frame;
      Table2 : Gtk_Table;
      Hscale1 : Gtk_Hscale;
      Hscale2 : Gtk_Hscale;
      Hscale3 : Gtk_Hscale;
      Label22 : Gtk_Label;
      Label21 : Gtk_Label;
      Radiobutton4 : Gtk_Radio_Button;
      Radiobutton6 : Gtk_Radio_Button;
      Radiobutton3 : Gtk_Radio_Button;
      Radiobutton5 : Gtk_Radio_Button;
      Label18 : Gtk_Label;
      Label19 : Gtk_Label;
      Label20 : Gtk_Label;
      Radiobutton8 : Gtk_Radio_Button;
      Radiobutton7 : Gtk_Radio_Button;
      Label23 : Gtk_Label;
      Label24 : Gtk_Label;
      Label25 : Gtk_Label;
      Checkbutton11 : Gtk_Check_Button;
      Checkbutton13 : Gtk_Check_Button;
      Checkbutton12 : Gtk_Check_Button;
      Checkbutton14 : Gtk_Check_Button;
      Checkbutton15 : Gtk_Check_Button;
      Label8 : Gtk_Label;
      Frame3 : Gtk_Frame;
      Table3 : Gtk_Table;
      Checkbutton20 : Gtk_Check_Button;
      Checkbutton19 : Gtk_Check_Button;
      Checkbutton16 : Gtk_Check_Button;
      Checkbutton17 : Gtk_Check_Button;
      Checkbutton18 : Gtk_Check_Button;
      Checkbutton21 : Gtk_Check_Button;
      Checkbutton22 : Gtk_Check_Button;
      Label26 : Gtk_Label;
      Label27 : Gtk_Label;
      Checkbutton23 : Gtk_Check_Button;
      Checkbutton24 : Gtk_Check_Button;
      Checkbutton25 : Gtk_Check_Button;
      Hscale4 : Gtk_Hscale;
      Label28 : Gtk_Label;
      Label9 : Gtk_Label;
      Frame4 : Gtk_Frame;
      Table4 : Gtk_Table;
      Label36 : Gtk_Label;
      Label34 : Gtk_Label;
      Label33 : Gtk_Label;
      Label32 : Gtk_Label;
      Label31 : Gtk_Label;
      Label30 : Gtk_Label;
      Label29 : Gtk_Label;
      Label35 : Gtk_Label;
      Checkbutton26 : Gtk_Check_Button;
      Radiobutton9 : Gtk_Radio_Button;
      Radiobutton10 : Gtk_Radio_Button;
      Radiobutton11 : Gtk_Radio_Button;
      Radiobutton12 : Gtk_Radio_Button;
      Radiobutton13 : Gtk_Radio_Button;
      Radiobutton14 : Gtk_Radio_Button;
      Checkbutton27 : Gtk_Check_Button;
      Checkbutton28 : Gtk_Check_Button;
      Checkbutton29 : Gtk_Check_Button;
      Checkbutton30 : Gtk_Check_Button;
      Checkbutton31 : Gtk_Check_Button;
      Checkbutton32 : Gtk_Check_Button;
      Checkbutton33 : Gtk_Check_Button;
      Radiobutton15 : Gtk_Radio_Button;
      Radiobutton17 : Gtk_Radio_Button;
      Radiobutton18 : Gtk_Radio_Button;
      Radiobutton16 : Gtk_Radio_Button;
      Radiobutton19 : Gtk_Radio_Button;
      Radiobutton20 : Gtk_Radio_Button;
      Radiobutton21 : Gtk_Radio_Button;
      Radiobutton22 : Gtk_Radio_Button;
      Radiobutton23 : Gtk_Radio_Button;
      Radiobutton24 : Gtk_Radio_Button;
      Label10 : Gtk_Label;
      Frame5 : Gtk_Frame;
      Table5 : Gtk_Table;
      Entry3 : Gtk_Entry;
      Label38 : Gtk_Label;
      Entry5 : Gtk_Entry;
      Label40 : Gtk_Label;
      Entry6 : Gtk_Entry;
      Entry7 : Gtk_Entry;
      Label42 : Gtk_Label;
      Entry8 : Gtk_Entry;
      Label37 : Gtk_Label;
      Label39 : Gtk_Label;
      Label41 : Gtk_Label;
      Button46 : Gtk_Button;
      Button47 : Gtk_Button;
      Button48 : Gtk_Button;
      Entry4 : Gtk_Entry;
      Label11 : Gtk_Label;
      Frame6 : Gtk_Frame;
      Table6 : Gtk_Table;
      Entry9 : Gtk_Entry;
      Entry10 : Gtk_Entry;
      Entry11 : Gtk_Entry;
      Entry12 : Gtk_Entry;
      Entry13 : Gtk_Entry;
      Entry14 : Gtk_Entry;
      Label43 : Gtk_Label;
      Label44 : Gtk_Label;
      Label45 : Gtk_Label;
      Label46 : Gtk_Label;
      Label47 : Gtk_Label;
      Label48 : Gtk_Label;
      Label12 : Gtk_Label;
      Hbuttonbox6 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Reset_Button : Gtk_Button;
      Help_Button : Gtk_Button;
   end record;
   type Odd_Preferences_Access is access all Odd_Preferences_Record'Class;

   procedure Gtk_New (Odd_Preferences : out Odd_Preferences_Access);
   procedure Initialize
     (Odd_Preferences : access Odd_Preferences_Record'Class);

end Odd_Preferences_Pkg;
