-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
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

with Gtk; use Gtk;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Clist;           use Gtk.Clist;
with Gtk.Box;             use Gtk.Box;
with Gtk.Label;           use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Vbutton_Box;     use Gtk.Vbutton_Box;
with Gtk.Hbutton_Box;     use Gtk.Hbutton_Box;

with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Aunit_Gui; use Callbacks_Aunit_Gui;
with Glide_Intl; use Glide_Intl;
with Make_Suite_Window_Pkg.Callbacks; use Make_Suite_Window_Pkg.Callbacks;

package body Make_Suite_Window_Pkg is
   --  "AUnit_Make_Suite" main window definition.

   procedure Gtk_New (Make_Suite_Window : out Make_Suite_Window_Access) is
   begin
      Make_Suite_Window := new Make_Suite_Window_Record;
      Make_Suite_Window_Pkg.Initialize (Make_Suite_Window);
   end Gtk_New;

   procedure Initialize
     (Make_Suite_Window : access Make_Suite_Window_Record'Class)
   is
      Hbox1 : Gtk_Hbox;
      Hbox2 : Gtk_Hbox;
      Hbox3 : Gtk_Hbox;

      Vbox1 : Gtk_Vbox;
      Vbox2 : Gtk_Vbox;
      Vbox3 : Gtk_Vbox;
      Vbox4 : Gtk_Vbox;

      Label1 : Gtk_Label;
      Label2 : Gtk_Label;
      Label3 : Gtk_Label;
      Label5 : Gtk_Label;
      Label6 : Gtk_Label;

      Vbuttonbox1 : Gtk_Vbutton_Box;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Scrolledwindow2 : Gtk_Scrolled_Window;

   begin
      Gtk.Window.Initialize (Make_Suite_Window, Window_Toplevel);
      Set_Title (Make_Suite_Window, -"Make new suite");
      Set_Policy (Make_Suite_Window, False, True, False);
      Set_Position (Make_Suite_Window, Win_Pos_None);
      Set_Modal (Make_Suite_Window, False);
      Set_Default_Size (Make_Suite_Window, 500, 300);

      Return_Callback.Connect
        (Make_Suite_Window, "delete_event",
         On_Make_Suite_Window_Delete_Event'Access);

      Gtk_New_Vbox (Vbox1, False, 0);
      Add (Make_Suite_Window, Vbox1);

      Gtk_New_Hbox (Hbox1, False, 0);
      Pack_Start (Vbox1, Hbox1, False, True, 3);

      Gtk_New_Vbox (Vbox2, True, 0);
      Pack_Start (Hbox1, Vbox2, False, False, 5);

      Gtk_New (Label1, -("Suite name :"));
      Set_Alignment (Label1, 1.0, 0.5);
      Set_Padding (Label1, 0, 0);
      Set_Justify (Label1, Justify_Center);
      Set_Line_Wrap (Label1, False);
      Pack_Start (Vbox2, Label1, False, False, 0);

      Gtk_New_Vbox (Vbox3, True, 0);
      Pack_Start (Hbox1, Vbox3, True, True, 3);

      Gtk_New (Make_Suite_Window.Name_Entry);
      Set_Editable (Make_Suite_Window.Name_Entry, True);
      Set_Max_Length (Make_Suite_Window.Name_Entry, 0);
      Set_Text (Make_Suite_Window.Name_Entry, -"New_Suite");
      Set_Visibility (Make_Suite_Window.Name_Entry, True);
      Pack_Start (Vbox3, Make_Suite_Window.Name_Entry, False, False, 1);

      Gtk_New_Hbox (Hbox3, False, 0);
      Pack_Start (Vbox1, Hbox3, True, True, 3);

      Gtk_New_Vbox (Vbox4, False, 0);
      Pack_Start (Hbox3, Vbox4, True, True, 3);

      Gtk_New (Label2,
               -("The following tests will be added to the new suite :"));
      Set_Alignment (Label2, 0.0, 0.5);
      Set_Padding (Label2, 6, 0);
      Set_Justify (Label2, Justify_Center);
      Set_Line_Wrap (Label2, False);
      Pack_Start (Vbox4, Label2, False, False, 0);

      Gtk_New_Hbox (Hbox2, False, 0);
      Pack_Start (Vbox4, Hbox2, True, True, 0);

      Gtk_New (Scrolledwindow2);
      Set_Policy (Scrolledwindow2, Policy_Automatic, Policy_Automatic);
      Pack_Start (Hbox2, Scrolledwindow2, True, True, 3);

      Gtk_New (Make_Suite_Window.Test_List, 2);
      Set_Selection_Mode (Make_Suite_Window.Test_List, Selection_Single);
      Set_Shadow_Type (Make_Suite_Window.Test_List, Shadow_In);
      Set_Show_Titles (Make_Suite_Window.Test_List, False);
      Set_Column_Width (Make_Suite_Window.Test_List, 0, 80);
      Set_Column_Width (Make_Suite_Window.Test_List, 1, 80);
      Set_Row_Height (Make_Suite_Window.Test_List, 15);
      Set_Column_Auto_Resize (Make_Suite_Window.Test_List, 0, True);
      Add (Scrolledwindow2, Make_Suite_Window.Test_List);

      Gtk_New (Label5, -("label5"));
      Set_Alignment (Label5, 0.5, 0.5);
      Set_Padding (Label5, 0, 0);
      Set_Justify (Label5, Justify_Center);
      Set_Line_Wrap (Label5, False);
      Set_Column_Widget (Make_Suite_Window.Test_List, 0, Label5);

      Gtk_New (Label6, -("label6"));
      Set_Alignment (Label6, 0.5, 0.5);
      Set_Padding (Label6, 0, 0);
      Set_Justify (Label6, Justify_Center);
      Set_Line_Wrap (Label6, False);
      Set_Column_Widget (Make_Suite_Window.Test_List, 1, Label6);

      Gtk_New (Label3, -("label7"));
      Set_Alignment (Label3, 0.5, 0.5);
      Set_Padding (Label3, 0, 0);
      Set_Justify (Label3, Justify_Center);
      Set_Line_Wrap (Label3, False);
      Set_Column_Widget (Make_Suite_Window.Test_List, 2, Label3);

      Gtk_New (Vbuttonbox1);
      Set_Spacing (Vbuttonbox1, 10);
      Set_Layout (Vbuttonbox1, Buttonbox_Spread);
      Set_Child_Size (Vbuttonbox1, 85, 27);
      Set_Child_Ipadding (Vbuttonbox1, 7, 0);
      Pack_Start (Hbox2, Vbuttonbox1, False, True, 3);

      Gtk_New (Make_Suite_Window.Add, -"Add");
      Set_Relief (Make_Suite_Window.Add, Relief_Normal);
      Set_Flags (Make_Suite_Window.Add, Can_Default);
      Button_Callback.Connect
        (Make_Suite_Window.Add, "clicked",
         Button_Callback.To_Marshaller (On_Add_Clicked'Access));
      Add (Vbuttonbox1, Make_Suite_Window.Add);

      Gtk_New (Make_Suite_Window.Remove, -"Remove");
      Set_Relief (Make_Suite_Window.Remove, Relief_Normal);
      Set_Flags (Make_Suite_Window.Remove, Can_Default);
      Button_Callback.Connect
        (Make_Suite_Window.Remove, "clicked",
         Button_Callback.To_Marshaller (On_Remove_Clicked'Access));
      Add (Vbuttonbox1, Make_Suite_Window.Remove);

      Gtk_New (Hbuttonbox1);
      Set_Spacing (Hbuttonbox1, 30);
      Set_Layout (Hbuttonbox1, Buttonbox_Spread);
      Set_Child_Size (Hbuttonbox1, 85, 27);
      Set_Child_Ipadding (Hbuttonbox1, 7, 0);
      Pack_Start (Vbox1, Hbuttonbox1, False, False, 3);

      Gtk_New (Make_Suite_Window.Ok, -"OK");
      Set_Relief (Make_Suite_Window.Ok, Relief_Normal);
      Set_Flags (Make_Suite_Window.Ok, Can_Default);
      Button_Callback.Connect
        (Make_Suite_Window.Ok, "clicked",
         Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
      Add (Hbuttonbox1, Make_Suite_Window.Ok);

      Gtk_New (Make_Suite_Window.Cancel, -"Cancel");
      Set_Relief (Make_Suite_Window.Cancel, Relief_Normal);
      Set_Flags (Make_Suite_Window.Cancel, Can_Default);
      Button_Callback.Connect
        (Make_Suite_Window.Cancel, "clicked",
         Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
      Add (Hbuttonbox1, Make_Suite_Window.Cancel);

      Gtk_New (Make_Suite_Window.Help, -"Help");
      Set_Relief (Make_Suite_Window.Help, Relief_Normal);
      Set_Flags (Make_Suite_Window.Help, Can_Default);
      Button_Callback.Connect
        (Make_Suite_Window.Help, "clicked",
         Button_Callback.To_Marshaller (On_Help_Clicked'Access));
      Add (Hbuttonbox1, Make_Suite_Window.Help);
   end Initialize;

end Make_Suite_Window_Pkg;
