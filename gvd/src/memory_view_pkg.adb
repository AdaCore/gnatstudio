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

with Gtk;                 use Gtk;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Arrow;           use Gtk.Arrow;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Flow_Box;        use Gtk.Flow_Box;
with Gtk.Hbutton_Box;     use Gtk.Hbutton_Box;
with Gtk.Label;           use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Separator;       use Gtk.Separator;

with GPS.Intl;            use GPS.Intl;

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

      Label        : Gtk_Label;
      Flow         : Gtk_Flow_Box;
      Hbox         : Gtk_Hbox;
      Hseparator   : Gtk_Hseparator;
      Arrow        : Gtk_Arrow;
      Hbuttonbox11 : Gtk_Hbutton_Box;
      Adjustment   : Gtk_Adjustment;
      Scrolled     : Gtk_Scrolled_Window;

   begin
      Gtk.Box.Initialize_Vbox (Memory_View);

      --  Flowbox containing control widgets

      Gtk_New (Flow);
      Pack_Start (Memory_View, Flow, Expand => False);
      Flow.Set_Homogeneous (False);
      Flow.Set_Orientation (Orientation_Horizontal);
      Flow.Set_Can_Focus (False);
      Flow.Set_Selection_Mode (Selection_None);

      --  Location widget

      Gtk_New_Hbox (Hbox);
      Flow.Add (Hbox);

      Gtk_New (Label, -("Location"));
      Set_Alignment (Label, 0.5, 0.5);
      Set_Padding (Label, 0, 0);
      Set_Justify (Label, Justify_Center);
      Set_Line_Wrap (Label, False);
      Hbox.Pack_Start (Label, False, False, 4);

      Gtk_New (Memory_View.Address_Entry);
      Set_Editable (Memory_View.Address_Entry, True);
      Set_Max_Length (Memory_View.Address_Entry, 0);
      Set_Text (Memory_View.Address_Entry, -"");
      Set_Visibility (Memory_View.Address_Entry, True);
      Set_Name (Memory_View.Address_Entry, "memory view adress entry");
      Hbox.Pack_Start (Memory_View.Address_Entry, False);

      Gtk_New (Memory_View.Address_View, -"View");
      Set_Relief (Memory_View.Address_View, Relief_Normal);
      Hbox.Pack_Start (Memory_View.Address_View, False, False);

      --  Unit size widget

      Gtk_New_Hbox (Hbox);
      Flow.Add (Hbox);

      Gtk_New (Label, -("Unit size: "));
      Set_Line_Wrap (Label, False);
      Pack_Start (Hbox, Label, False, False, 4);

      Gtk_New (Memory_View.Size);
      Memory_View.Size.Append_Text (-"Byte");
      Memory_View.Size.Append_Text (-"Halfword");
      Memory_View.Size.Append_Text (-"Word");
      Memory_View.Size.Set_Active (0);
      Pack_Start (Hbox, Memory_View.Size, False, False, 7);
      Set_Name (Memory_View.Size, "memory view unit size");

      --  Format widget

      Gtk_New_Hbox (Hbox);
      Flow.Add (Hbox);

      Gtk_New (Label, -("Format: "));
      Set_Line_Wrap (Label, False);
      Pack_Start (Hbox, Label, False, False, 4);

      Gtk_New (Memory_View.Format);
      Memory_View.Format.Append_Text (-"Hex");
      Memory_View.Format.Append_Text (-"Decimal");
      Memory_View.Format.Append_Text (-"Octal");
      Memory_View.Format.Append_Text (-"ASCII");
      Memory_View.Format.Set_Active (0);
      Pack_Start (Hbox, Memory_View.Format, False, False, 7);
      Set_Name (Memory_View.Format, "memory view format");

      --  ASCII check button

      Gtk_New (Memory_View.Show_Ascii, -"Show ASCII");
      Set_Active (Memory_View.Show_Ascii, True);
      Set_Name (Memory_View.Show_Ascii, "memory view show ascii");
      Flow.Add (Memory_View.Show_Ascii);

      --  Page up/down action

      Gtk_New_Hbox (Hbox);
      Flow.Add (Hbox);

      Gtk_New (Memory_View.Pgup);
      Set_Relief (Memory_View.Pgup, Relief_Normal);
      Pack_Start (Hbox, Memory_View.Pgup, False, False, 0);

      Gtk_New (Arrow, Arrow_Up, Shadow_Out);
      Set_Alignment (Arrow, 0.5, 0.5);
      Set_Padding (Arrow, 0, 0);
      Add (Memory_View.Pgup, Arrow);

      Gtk_New (Memory_View.Pgdn);
      Set_Relief (Memory_View.Pgdn, Relief_Normal);
      Pack_Start (Hbox, Memory_View.Pgdn, False, False, 0);

      Gtk_New (Arrow, Arrow_Down, Shadow_Out);
      Set_Alignment (Arrow, 0.5, 0.5);
      Set_Padding (Arrow, 0, 0);
      Add (Memory_View.Pgdn, Arrow);

      --  Lines spin

      Gtk_New (Adjustment, 16.0, 1.0, 30.0, 1.0, 0.0);
      Gtk_New (Memory_View.Lines_Spin, Adjustment, 0.0, 0);
      Pack_Start (Hbox, Memory_View.Lines_Spin, False, False, 0);

      --  Scrolled window containing the memory

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Memory_View, Scrolled, True, True, 0);

      --  Memory view

      Gtk_New (Memory_View.View);
      Set_Editable (Memory_View.View, True);
      Memory_View.View.Get_Buffer.Set_Text ("");
      Set_Name (Memory_View.View, "memory view text");
      Add (Scrolled, Memory_View.View);

      Gtk_New_Hseparator (Hseparator);
      Pack_Start (Memory_View, Hseparator, False, False, 3);

      --  Buttons to undo/submit changes

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
