-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2007                       --
--                             AdaCore                               --
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

with Gtk;                       use Gtk;
with Gtk.Adjustment;            use Gtk.Adjustment;
with Gtk.Arrow;                 use Gtk.Arrow;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Hbutton_Box;           use Gtk.Hbutton_Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toggle_Button;         use Gtk.Toggle_Button;
with Gtk.Vbutton_Box;           use Gtk.Vbutton_Box;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;

with GPS.Intl;                  use GPS.Intl;
with GVD.Callbacks;             use GVD.Callbacks;
with Memory_View_Pkg.Callbacks; use Memory_View_Pkg.Callbacks;

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

      Size_Items   : String_List.Glist;
      Format_Items : String_List.Glist;
      Vbox20       : Gtk_Vbox;
      Hbox8        : Gtk_Hbox;
      Hbox11       : Gtk_Hbox;
      Vbox23       : Gtk_Vbox;
      Label95      : Gtk_Label;
      Label96      : Gtk_Label;
      Vbox24       : Gtk_Vbox;
      Vbuttonbox6  : Gtk_Vbutton_Box;
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

   begin
      Gtk.Window.Initialize (Memory_View, Window_Toplevel);
      Set_Title (Memory_View, -"Memory View");
      Set_Policy (Memory_View, True, True, False);
      Set_Position (Memory_View, Win_Pos_None);
      Set_Modal (Memory_View, False);

      Gtk_New_Vbox (Vbox20, False, 0);
      Add (Memory_View, Vbox20);

      Gtk_New (Memory_View.Frame);
      Set_Shadow_Type (Memory_View.Frame, Shadow_Etched_In);
      Pack_Start (Vbox20, Memory_View.Frame, False, False, 0);

      Gtk_New_Hbox (Hbox8, False, 0);
      Add (Memory_View.Frame, Hbox8);

      Gtk_New_Hbox (Hbox11, False, 0);
      Pack_Start (Hbox8, Hbox11, True, True, 0);

      Gtk_New_Vbox (Vbox23, True, 0);
      Pack_Start (Hbox11, Vbox23, False, False, 7);

      Gtk_New (Label95, -("Location"));
      Set_Alignment (Label95, 0.5, 0.5);
      Set_Padding (Label95, 0, 0);
      Set_Justify (Label95, Justify_Center);
      Set_Line_Wrap (Label95, False);
      Pack_Start (Vbox23, Label95, False, False, 0);

      Gtk_New (Label96, -("Find"));
      Set_Alignment (Label96, 0.5, 0.5);
      Set_Padding (Label96, 0, 0);
      Set_Justify (Label96, Justify_Center);
      Set_Line_Wrap (Label96, False);
      Set_Sensitive (Label96, False);
      Pack_Start (Vbox23, Label96, False, False, 0);

      Gtk_New_Vbox (Vbox24, True, 0);
      Pack_Start (Hbox11, Vbox24, True, True, 0);

      Gtk_New (Memory_View.Address_Entry);
      Set_Editable (Memory_View.Address_Entry, True);
      Set_Max_Length (Memory_View.Address_Entry, 0);
      Set_Text (Memory_View.Address_Entry, -"");
      Set_Visibility (Memory_View.Address_Entry, True);
      Pack_Start
        (Vbox24, Memory_View.Address_Entry, False, False, 0);
      Entry_Callback.Connect
        (Memory_View.Address_Entry, Gtk.GEntry.Signal_Activate,
         Entry_Callback.To_Marshaller (On_Address_Entry_Activate'Access));

      Gtk_New (Memory_View.Search_Entry);
      Set_Editable (Memory_View.Search_Entry, True);
      Set_Max_Length (Memory_View.Search_Entry, 0);
      Set_Text (Memory_View.Search_Entry, -"");
      Set_Visibility (Memory_View.Search_Entry, True);
      Set_Sensitive (Memory_View.Search_Entry, False);
      Pack_Start (Vbox24, Memory_View.Search_Entry, False, False, 0);

      Gtk_New (Vbuttonbox6);
      Set_Spacing (Vbuttonbox6, 10);
      Set_Layout (Vbuttonbox6, Buttonbox_Spread);
      Set_Child_Size (Vbuttonbox6, 85, 27);
      Set_Child_Ipadding (Vbuttonbox6, 7, 0);
      Pack_Start (Hbox8, Vbuttonbox6, False, False, 0);

      Gtk_New (Memory_View.Address_View, -"View");
      Set_Relief (Memory_View.Address_View, Relief_Normal);
      Set_Flags (Memory_View.Address_View, Can_Default);
      Button_Callback.Connect
        (Memory_View.Address_View, Signal_Clicked,
         Button_Callback.To_Marshaller (On_Address_View_Clicked'Access));
      Add (Vbuttonbox6, Memory_View.Address_View);

      Gtk_New (Memory_View.Search_Button, -"Search");
      Set_Relief (Memory_View.Search_Button, Relief_Normal);
      Set_Sensitive (Memory_View.Search_Button, False);
      Set_Flags (Memory_View.Search_Button, Can_Default);
      Add (Vbuttonbox6, Memory_View.Search_Button);

      Gtk_New_Hbox (Hbox12, False, 0);
      Pack_Start (Vbox20, Hbox12, False, False, 4);

      Gtk_New (Vbuttonbox5);
      Set_Spacing (Vbuttonbox5, 10);
      Set_Layout (Vbuttonbox5, Buttonbox_Spread);
      Set_Child_Size (Vbuttonbox5, 85, 27);
      Set_Child_Ipadding (Vbuttonbox5, 7, 0);
      Pack_Start (Hbox12, Vbuttonbox5, False, False, 0);

      Gtk_New (Label98, -("Unit size: "));
      Set_Alignment (Label98, 0.5, 0.5);
      Set_Padding (Label98, 0, 0);
      Set_Justify (Label98, Justify_Center);
      Set_Line_Wrap (Label98, False);
      Pack_Start (Hbox12, Label98, True, True, 4);

      Gtk_New (Memory_View.Size);
      Set_Case_Sensitive (Memory_View.Size, False);
      Set_Use_Arrows (Memory_View.Size, True);
      Set_Use_Arrows_Always (Memory_View.Size, False);
      String_List.Append (Size_Items, -"Byte");
      String_List.Append (Size_Items, -"Halfword");
      String_List.Append (Size_Items, -"Word");
      Combo.Set_Popdown_Strings (Memory_View.Size, Size_Items);
      Free_String_List (Size_Items);
      Pack_Start (Hbox12, Memory_View.Size, True, True, 0);

      Memory_View.Size_Entry := Get_Entry (Memory_View.Size);
      Set_Editable (Memory_View.Size_Entry, False);
      Set_Max_Length (Memory_View.Size_Entry, 0);
      Set_Text (Memory_View.Size_Entry, -"Byte");
      Set_Visibility (Memory_View.Size_Entry, True);
      Entry_Callback.Connect
        (Memory_View.Size_Entry, Signal_Changed,
         Entry_Callback.To_Marshaller (On_Size_Entry_Changed'Access));

      Gtk_New_Vseparator (Vseparator7);
      Pack_Start (Hbox12, Vseparator7, False, True, 10);

      Gtk_New (Label97, -("Format: "));
      Set_Alignment (Label97, 0.5, 0.5);
      Set_Padding (Label97, 0, 0);
      Set_Justify (Label97, Justify_Center);
      Set_Line_Wrap (Label97, False);
      Pack_Start (Hbox12, Label97, True, True, 0);

      Gtk_New (Memory_View.Format);
      Set_Case_Sensitive (Memory_View.Format, False);
      Set_Use_Arrows (Memory_View.Format, True);
      Set_Use_Arrows_Always (Memory_View.Format, False);
      String_List.Append (Format_Items, -"Hex");
      String_List.Append (Format_Items, -"Decimal");
      String_List.Append (Format_Items, -"Octal");
      String_List.Append (Format_Items, -"ASCII");
      Combo.Set_Popdown_Strings (Memory_View.Format, Format_Items);
      Free_String_List (Format_Items);
      Pack_Start (Hbox12, Memory_View.Format, True, True, 7);

      Memory_View.Data_Entry := Get_Entry (Memory_View.Format);
      Set_Editable (Memory_View.Data_Entry, False);
      Set_Max_Length (Memory_View.Data_Entry, 0);
      Set_Text (Memory_View.Data_Entry, -"Hex");
      Set_Visibility (Memory_View.Data_Entry, True);
      Entry_Callback.Connect
        (Memory_View.Data_Entry, Signal_Changed,
         Entry_Callback.To_Marshaller (On_Data_Entry_Changed'Access));

      Gtk_New_Vseparator (Vseparator10);
      Pack_Start (Hbox12, Vseparator10, True, True, 0);

      Gtk_New (Memory_View.Show_Ascii, -"Show ASCII");
      Set_Active (Memory_View.Show_Ascii, True);
      Pack_Start (Hbox12, Memory_View.Show_Ascii, False, False, 0);
      Check_Button_Callback.Connect
        (Memory_View.Show_Ascii, Signal_Toggled,
         Check_Button_Callback.To_Marshaller (On_Show_Ascii_Toggled'Access));

      Gtk_New_Vseparator (Vseparator9);
      Pack_Start (Hbox12, Vseparator9, True, True, 0);

      Gtk_New (Memory_View.Pgup);
      Set_Relief (Memory_View.Pgup, Relief_Normal);
      Set_Flags (Memory_View.Pgup, Can_Default);
      Pack_Start (Hbox12, Memory_View.Pgup, True, True, 0);
      Button_Callback.Connect
        (Memory_View.Pgup, Signal_Clicked,
         Button_Callback.To_Marshaller (On_Pgup_Clicked'Access));

      Gtk_New (Arrow1, Arrow_Up, Shadow_Out);
      Set_Alignment (Arrow1, 0.5, 0.5);
      Set_Padding (Arrow1, 0, 0);
      Add (Memory_View.Pgup, Arrow1);

      Gtk_New (Memory_View.Pgdn);
      Set_Relief (Memory_View.Pgdn, Relief_Normal);
      Set_Flags (Memory_View.Pgdn, Can_Default);
      Pack_Start (Hbox12, Memory_View.Pgdn, True, True, 0);
      Button_Callback.Connect
        (Memory_View.Pgdn, Signal_Clicked,
         Button_Callback.To_Marshaller (On_Pgdn_Clicked'Access));

      Gtk_New (Arrow2, Arrow_Down, Shadow_Out);
      Set_Alignment (Arrow2, 0.5, 0.5);
      Set_Padding (Arrow2, 0, 0);
      Add (Memory_View.Pgdn, Arrow2);

      Gtk_New
        (Adjustment, 16.0, 1.0, 30.0, 1.0, 0.0, 0.0);
      Gtk_New
        (Memory_View.Lines_Spin, Adjustment, 0.0, 0);
      Entry_Return_Callback.Connect
        (Gtk_Entry (Memory_View.Lines_Spin), Signal_Button_Release_Event,
         On_Button_Release'Access);
      Pack_Start (Hbox12, Memory_View.Lines_Spin, True, True, 0);

      Gtk_New (Memory_View.Scrolledwindow);
      Set_Policy
        (Memory_View.Scrolledwindow, Policy_Automatic, Policy_Automatic);
      Pack_Start (Vbox20, Memory_View.Scrolledwindow, True, True, 0);

      Gtk_New (Memory_View.View);
      Set_Editable (Memory_View.View, True);
      Return_Callback.Connect
        (Memory_View.View, Signal_Key_Press_Event,
         On_View_Key_Press_Event'Access);
      Text_Callback.Connect
        (Memory_View.View, Gtk.Text_View.Signal_Move_Cursor,
         On_View_Move_Cursor'Access);
      Return_Callback.Connect
        (Memory_View.View, Signal_Button_Release_Event,
         On_View_Button_Release_Event'Access);
      Add (Memory_View.Scrolledwindow, Memory_View.View);

      Gtk_New_Hseparator (Hseparator2);
      Pack_Start (Vbox20, Hseparator2, False, False, 3);

      Gtk_New (Hbuttonbox11);
      Set_Spacing (Hbuttonbox11, 30);
      Set_Layout (Hbuttonbox11, Buttonbox_Spread);
      Set_Child_Size (Hbuttonbox11, 85, 27);
      Set_Child_Ipadding (Hbuttonbox11, 7, 0);
      Pack_Start (Vbox20, Hbuttonbox11, False, False, 0);

      Gtk_New (Memory_View.Reset, -"Undo changes");
      Set_Relief (Memory_View.Reset, Relief_Normal);
      Set_Flags (Memory_View.Reset, Can_Default);
      Button_Callback.Connect
        (Memory_View.Reset, Signal_Clicked,
         Button_Callback.To_Marshaller (On_Reset_Clicked'Access));
      Add (Hbuttonbox11, Memory_View.Reset);

      Gtk_New (Memory_View.Submit, -"Submit changes");
      Set_Relief (Memory_View.Submit, Relief_Normal);
      Set_Flags (Memory_View.Submit, Can_Default);
      Button_Callback.Connect
        (Memory_View.Submit, Signal_Clicked,
         Button_Callback.To_Marshaller (On_Submit_Clicked'Access));
      Add (Hbuttonbox11, Memory_View.Submit);

      Gtk_New_From_Stock (Memory_View.Close, Stock_Close);
      Set_Flags (Memory_View.Close, Can_Default);
      Button_Callback.Connect
        (Memory_View.Close, Signal_Clicked,
         Button_Callback.To_Marshaller (On_Close_Clicked'Access));
      Add (Hbuttonbox11, Memory_View.Close);
   end Initialize;

end Memory_View_Pkg;
