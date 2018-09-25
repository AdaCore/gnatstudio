------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Gtk;                       use Gtk;
with Gtk.Adjustment;            use Gtk.Adjustment;
with Gtk.Arrow;                 use Gtk.Arrow;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Hbutton_Box;           use Gtk.Hbutton_Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Vbutton_Box;           use Gtk.Vbutton_Box;

with GPS.Intl;                  use GPS.Intl;

package body Memory_View_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Memory_View : out Memory_View_Access) is
   begin
      Memory_View := new Memory_View_Record;
      Memory_View_Pkg.Initialize (Memory_View);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Memory_View : access Memory_View_Record'Class) is
      pragma Suppress (All_Checks);

      Label95      : Gtk_Label;
      Hbox12       : Gtk_Hbox;
      Vbuttonbox5  : Gtk_Vbutton_Box;
      Label98      : Gtk_Label;
      Vseparator7  : Gtk_Vseparator;
      Label97      : Gtk_Label;
      Vseparator10 : Gtk_Vseparator;
      Vseparator9  : Gtk_Vseparator;
      Arrow1       : Gtk_Arrow;
      Arrow2       : Gtk_Arrow;
      Hseparator2  : Gtk_Hseparator;
      Hbuttonbox11 : Gtk_Hbutton_Box;
      Adjustment   : Gtk_Adjustment;
      Table        : Gtk_Table;

   begin
      Gtk.Box.Initialize_Vbox (Memory_View, False, 0);

      --  Main controls

      Gtk_New (Table, Rows => 2, Columns => 3, Homogeneous => False);
      Pack_Start (Memory_View, Table, Expand => False);

      Gtk_New (Label95, -("Location"));
      Set_Alignment (Label95, 0.5, 0.5);
      Set_Padding (Label95, 0, 0);
      Set_Justify (Label95, Justify_Center);
      Set_Line_Wrap (Label95, False);
      Attach (Table, Label95, 0, 1, 0, 1, Xoptions => 0);

      Gtk_New (Memory_View.Address_Entry);
      Set_Editable (Memory_View.Address_Entry, True);
      Set_Max_Length (Memory_View.Address_Entry, 0);
      Set_Text (Memory_View.Address_Entry, -"");
      Set_Visibility (Memory_View.Address_Entry, True);
      Attach (Table, Memory_View.Address_Entry, 1, 2, 0, 1);
      Set_Name (Memory_View.Address_Entry, "memory view adress entry");

      Gtk_New (Memory_View.Address_View, -"View");
      Set_Relief (Memory_View.Address_View, Relief_Normal);
      Attach (Table, Memory_View.Address_View, 2, 3, 0, 1, Xoptions => 0);

      --  The toolbar

      Gtk_New_Hbox (Hbox12, False, 0);
      Pack_Start (Memory_View, Hbox12, False, False, 4);

      Gtk_New (Vbuttonbox5);
      Set_Spacing (Vbuttonbox5, 10);
      Set_Layout (Vbuttonbox5, Buttonbox_Spread);
      Pack_Start (Hbox12, Vbuttonbox5, False, False, 0);

      Gtk_New (Label98, -("Unit size: "));
      Set_Alignment (Label98, 0.5, 0.5);
      Set_Padding (Label98, 0, 0);
      Set_Justify (Label98, Justify_Center);
      Set_Line_Wrap (Label98, False);
      Pack_Start (Hbox12, Label98, True, True, 4);

      Gtk_New (Memory_View.Size);
      Memory_View.Size.Append_Text (-"Byte");
      Memory_View.Size.Append_Text (-"Halfword");
      Memory_View.Size.Append_Text (-"Word");
      Memory_View.Size.Set_Active (0);
      Pack_Start (Hbox12, Memory_View.Size, True, True, 0);
      Set_Name (Memory_View.Size, "memory view unit size");

      Gtk_New_Vseparator (Vseparator7);
      Pack_Start (Hbox12, Vseparator7, False, True, 10);

      Gtk_New (Label97, -("Format: "));
      Set_Alignment (Label97, 0.5, 0.5);
      Set_Padding (Label97, 0, 0);
      Set_Justify (Label97, Justify_Center);
      Set_Line_Wrap (Label97, False);
      Pack_Start (Hbox12, Label97, True, True, 0);

      Gtk_New (Memory_View.Format);
      Memory_View.Format.Append_Text (-"Hex");
      Memory_View.Format.Append_Text (-"Decimal");
      Memory_View.Format.Append_Text (-"Octal");
      Memory_View.Format.Append_Text (-"ASCII");
      Memory_View.Format.Set_Active (0);
      Pack_Start (Hbox12, Memory_View.Format, True, True, 7);
      Set_Name (Memory_View.Format, "memory view format");

      Gtk_New_Vseparator (Vseparator10);
      Pack_Start (Hbox12, Vseparator10, True, True, 0);

      Gtk_New (Memory_View.Show_Ascii, -"Show ASCII");
      Set_Active (Memory_View.Show_Ascii, True);
      Pack_Start (Hbox12, Memory_View.Show_Ascii, False, False, 0);
      Set_Name (Memory_View.Show_Ascii, "memory view show ascii");

      Gtk_New_Vseparator (Vseparator9);
      Pack_Start (Hbox12, Vseparator9, True, True, 0);

      Gtk_New (Memory_View.Pgup);
      Set_Relief (Memory_View.Pgup, Relief_Normal);
      Pack_Start (Hbox12, Memory_View.Pgup, True, True, 0);

      Gtk_New (Arrow1, Arrow_Up, Shadow_Out);
      Set_Alignment (Arrow1, 0.5, 0.5);
      Set_Padding (Arrow1, 0, 0);
      Add (Memory_View.Pgup, Arrow1);

      Gtk_New (Memory_View.Pgdn);
      Set_Relief (Memory_View.Pgdn, Relief_Normal);
      Pack_Start (Hbox12, Memory_View.Pgdn, True, True, 0);

      Gtk_New (Arrow2, Arrow_Down, Shadow_Out);
      Set_Alignment (Arrow2, 0.5, 0.5);
      Set_Padding (Arrow2, 0, 0);
      Add (Memory_View.Pgdn, Arrow2);

      Gtk_New
        (Adjustment, 16.0, 1.0, 30.0, 1.0, 0.0);
      Gtk_New
        (Memory_View.Lines_Spin, Adjustment, 0.0, 0);
      Pack_Start (Hbox12, Memory_View.Lines_Spin, True, True, 0);

      --  The scrolled window showing the memory

      Gtk_New (Memory_View.Scrolledwindow);
      Set_Policy
        (Memory_View.Scrolledwindow, Policy_Automatic, Policy_Automatic);
      Pack_Start (Memory_View, Memory_View.Scrolledwindow, True, True, 0);

      Gtk_New (Memory_View.View);
      Set_Editable (Memory_View.View, True);
      Memory_View.View.Get_Buffer.Set_Text ("");
      Set_Name (Memory_View.View, "memory view text");
      Add (Memory_View.Scrolledwindow, Memory_View.View);

      Gtk_New_Hseparator (Hseparator2);
      Pack_Start (Memory_View, Hseparator2, False, False, 3);

      Gtk_New (Hbuttonbox11);
      Set_Spacing (Hbuttonbox11, 30);
      Set_Layout (Hbuttonbox11, Buttonbox_Spread);
      Pack_Start (Memory_View, Hbuttonbox11, False, False, 0);

      Gtk_New (Memory_View.Reset, -"Undo changes");
      Set_Relief (Memory_View.Reset, Relief_Normal);
      Add (Hbuttonbox11, Memory_View.Reset);
      Set_Sensitive (Memory_View.Reset, False);

      Gtk_New (Memory_View.Submit, -"Submit changes");
      Set_Relief (Memory_View.Submit, Relief_Normal);
      Add (Hbuttonbox11, Memory_View.Submit);
      Set_Sensitive (Memory_View.Submit, False);

      Set_Wrap_Mode (Memory_View.View, Wrap_None);
   end Initialize;

end Memory_View_Pkg;
