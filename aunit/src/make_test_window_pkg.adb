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
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Box;         use Gtk.Box;
with Gtk.Label;       use Gtk.Label;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;

with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Aunit_Gui; use Callbacks_Aunit_Gui;
with Glide_Intl; use Glide_Intl;
with Make_Test_Window_Pkg.Callbacks; use Make_Test_Window_Pkg.Callbacks;

package body Make_Test_Window_Pkg is

   procedure Gtk_New (Make_Test_Window : out Make_Test_Window_Access) is
   begin
      Make_Test_Window := new Make_Test_Window_Record;
      Make_Test_Window_Pkg.Initialize (Make_Test_Window);
   end Gtk_New;

   procedure Initialize
     (Make_Test_Window : access Make_Test_Window_Record'Class)
   is
      pragma Suppress (All_Checks);

      Vbox0 : Gtk_Vbox;
      Vbox2 : Gtk_Vbox;

      Hbox1 : Gtk_Hbox;
      Vbox1 : Gtk_Vbox;
      Label : Gtk_Label;

      Hbuttonbox1 : Gtk_Hbutton_Box;

   begin
      Gtk.Window.Initialize (Make_Test_Window, Window_Toplevel);
      Set_Title (Make_Test_Window, -"New test unit");
      Set_Policy (Make_Test_Window, False, True, False);
      Set_Position (Make_Test_Window, Win_Pos_None);
      Set_Modal (Make_Test_Window, False);
      Return_Callback.Connect
        (Make_Test_Window, "delete_event",
         On_Make_Test_Window_Delete_Event'Access);

      Gtk_New_Vbox (Vbox0, False, 3);
      Add (Make_Test_Window, Vbox0);

      Gtk_New_Hbox (Hbox1, False, 3);
      Pack_Start (Vbox0, Hbox1, True, True, 3);

      Gtk_New_Vbox (Vbox1, True, 0);
      Pack_Start (Hbox1, Vbox1, False, False, 5);

      Gtk_New (Label, -"Unit name: ");
      Pack_Start (Vbox1, Label, False, False, 3);

      Gtk_New (Label, -"Description: ");
      Pack_Start (Vbox1, Label, False, False, 3);

      Gtk_New (Label);
      Pack_Start (Vbox1, Label, False, False, 0);

      Gtk_New (Label);
      Pack_Start (Vbox1, Label, False, False, 0);

      Gtk_New_Vbox (Vbox2, True, 0);
      Pack_Start (Hbox1, Vbox2, True, True, 3);

      Gtk_New (Make_Test_Window.Name_Entry);
      Set_Editable (Make_Test_Window.Name_Entry, True);
      Set_Max_Length (Make_Test_Window.Name_Entry, 0);
      Set_Text (Make_Test_Window.Name_Entry, -"New_Test");
      Set_Visibility (Make_Test_Window.Name_Entry, True);
      Pack_Start (Vbox2, Make_Test_Window.Name_Entry, False, False, 3);
      Entry_Callback.Connect
        (Make_Test_Window.Name_Entry, "activate",
         Entry_Callback.To_Marshaller (On_Name_Entry_Activate'Access));

      Gtk_New (Make_Test_Window.Description_Entry);
      Set_Editable (Make_Test_Window.Description_Entry, True);
      Set_Max_Length (Make_Test_Window.Description_Entry, 0);
      Set_Text (Make_Test_Window.Description_Entry, -"(no description)");
      Set_Visibility (Make_Test_Window.Description_Entry, True);
      Pack_Start (Vbox2, Make_Test_Window.Description_Entry, False, False, 3);
      Entry_Callback.Connect
        (Make_Test_Window.Description_Entry, "activate",
         Entry_Callback.To_Marshaller (On_Description_Entry_Activate'Access));

      Gtk_New (Make_Test_Window.Override_Tear_Down, -"Override Tear_Down");
      Set_Active (Make_Test_Window.Override_Tear_Down, False);
      Pack_Start (Vbox2, Make_Test_Window.Override_Tear_Down, False, False, 3);

      Gtk_New (Make_Test_Window.Override_Set_Up, -"Override Set_up");
      Set_Active (Make_Test_Window.Override_Set_Up, False);
      Pack_Start (Vbox2, Make_Test_Window.Override_Set_Up, False, False, 3);

      Gtk_New (Hbuttonbox1);
      Set_Spacing (Hbuttonbox1, 30);
      Set_Layout (Hbuttonbox1, Buttonbox_Spread);
      Set_Child_Size (Hbuttonbox1, 85, 27);
      Set_Child_Ipadding (Hbuttonbox1, 7, 0);
      Pack_Start (Vbox0, Hbuttonbox1, True, True, 3);

      Gtk_New (Make_Test_Window.Ok, -"OK");
      Set_Relief (Make_Test_Window.Ok, Relief_Normal);
      Set_Flags (Make_Test_Window.Ok, Can_Default);
      Button_Callback.Connect
        (Make_Test_Window.Ok, "clicked",
         Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
      Add (Hbuttonbox1, Make_Test_Window.Ok);

      Gtk_New (Make_Test_Window.Cancel, -"Cancel");
      Set_Relief (Make_Test_Window.Cancel, Relief_Normal);
      Set_Flags (Make_Test_Window.Cancel, Can_Default);
      Button_Callback.Connect
        (Make_Test_Window.Cancel, "clicked",
         Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
      Add (Hbuttonbox1, Make_Test_Window.Cancel);
   end Initialize;

end Make_Test_Window_Pkg;
