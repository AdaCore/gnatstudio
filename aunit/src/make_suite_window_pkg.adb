-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                              AdaCore                              --
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

with Gtk;                 use Gtk;
with Gtk.Box;             use Gtk.Box;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Stock;           use Gtk.Stock;
with Gtk.Vbutton_Box;     use Gtk.Vbutton_Box;
with Gtk.Widget;          use Gtk.Widget;

with Callbacks_Aunit_Gui; use Callbacks_Aunit_Gui;
with GPS.Intl;            use GPS.Intl;
with Histories;           use Histories;

with Aunit_Utils;         use Aunit_Utils;
with Make_Suite_Window_Pkg.Callbacks; use Make_Suite_Window_Pkg.Callbacks;

package body Make_Suite_Window_Pkg is
   --  "AUnit_Make_Suite" main window definition.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Make_Suite_Window : out Make_Suite_Window_Access;
      Handle            : GPS.Kernel.Kernel_Handle) is
   begin
      Make_Suite_Window := new Make_Suite_Window_Record;
      Make_Suite_Window_Pkg.Initialize (Make_Suite_Window, Handle);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Make_Suite_Window : access Make_Suite_Window_Record'Class;
      Handle            : GPS.Kernel.Kernel_Handle)
   is
      pragma Suppress (All_Checks);

      Hbox1 : Gtk_Hbox;
      Hbox2 : Gtk_Hbox;
      Hbox3 : Gtk_Hbox;
      Hbox4 : Gtk_Hbox;

      Vbox2 : Gtk_Vbox;
      Vbox3 : Gtk_Vbox;
      Vbox4 : Gtk_Vbox;

      Label : Gtk_Label;

      Vbuttonbox1     : Gtk_Vbutton_Box;
      Scrolledwindow2 : Gtk_Scrolled_Window;
      Button : Gtk_Button;
      pragma Unreferenced (Button);

   begin
      Make_Suite_Window.Kernel := Handle;

      Gtk.Dialog.Initialize
        (Make_Suite_Window,
         Title  => -"Make new suite",
         Parent => GPS.Kernel.Get_Main_Window (Handle),
         Flags  => 0);
      Set_Modal (Make_Suite_Window, True);
      Set_Policy (Make_Suite_Window, False, True, False);
      Set_Position (Make_Suite_Window, Win_Pos_Mouse);
      Set_Default_Size (Make_Suite_Window, 500, 300);
      Set_Has_Separator (Make_Suite_Window, False);

      Gtk_New_Hbox (Hbox1, False, 0);
      Pack_Start (Get_Vbox (Make_Suite_Window), Hbox1, False, True, 3);

      Gtk_New_Vbox (Vbox2, True, 0);
      Pack_Start (Hbox1, Vbox2, False, False, 5);

      Gtk_New (Label, -"Save in: ");
      Pack_Start (Vbox2, Label, False, False, 3);

      Gtk_New (Label, -"Suite name:");
      Pack_Start (Vbox2, Label, False, False, 0);

      Gtk_New_Vbox (Vbox3, True, 0);
      Pack_Start (Hbox1, Vbox3, True, True, 3);

      Gtk_New_Hbox (Hbox4, False, 0);
      Pack_Start (Vbox3, Hbox4, False, False, 3);

      Gtk_New (Make_Suite_Window.Directory_Entry);
      Set_Editable (Make_Suite_Window.Directory_Entry, True);
      Set_Max_Length (Make_Suite_Window.Directory_Entry, 0);
      Set_Text (Make_Suite_Window.Directory_Entry,
                Get_Context_Directory (Handle));
      Set_Visibility (Make_Suite_Window.Directory_Entry, True);
      Pack_Start (Hbox4, Make_Suite_Window.Directory_Entry, True, True, 3);
      Widget_Callback.Connect
        (Make_Suite_Window.Directory_Entry, "changed",
         Check_Validity'Access);

      Gtk_New (Make_Suite_Window.Browse_Directory, -"Browse...");
      Set_Relief (Make_Suite_Window.Browse_Directory, Relief_Normal);
      Pack_Start
        (Hbox4, Make_Suite_Window.Browse_Directory, False, False, 3);
      Button_Callback.Connect
        (Make_Suite_Window.Browse_Directory, "clicked",
         On_Browse_Directory_Clicked'Access);

      Gtk_New (Make_Suite_Window.Name_Entry);
      Set_Editable (Make_Suite_Window.Name_Entry, True);
      Set_Max_Length (Make_Suite_Window.Name_Entry, 0);
      Set_Text (Make_Suite_Window.Name_Entry, -"New_Suite");
      Set_Visibility (Make_Suite_Window.Name_Entry, True);
      Pack_Start (Vbox3, Make_Suite_Window.Name_Entry, False, False, 1);
      Widget_Callback.Connect
        (Make_Suite_Window.Name_Entry, "changed",
         Check_Validity'Access);

      Gtk_New_Hbox (Hbox3, False, 0);
      Pack_Start (Get_Vbox (Make_Suite_Window), Hbox3, True, True, 3);

      Gtk_New_Vbox (Vbox4, False, 0);
      Pack_Start (Hbox3, Vbox4, True, True, 3);

      Gtk_New (Label, -"The following tests will be added to the new suite:");
      Pack_Start (Vbox4, Label, False, False, 0);

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

      Gtk_New (Label);
      Set_Column_Widget (Make_Suite_Window.Test_List, 0, Label);

      Gtk_New (Label);
      Set_Column_Widget (Make_Suite_Window.Test_List, 1, Label);

      Gtk_New (Label);
      Set_Column_Widget (Make_Suite_Window.Test_List, 2, Label);

      Gtk_New (Vbuttonbox1);
      Set_Spacing (Vbuttonbox1, 10);
      Set_Layout (Vbuttonbox1, Buttonbox_Spread);
      Set_Child_Size (Vbuttonbox1, 85, 27);
      Set_Child_Ipadding (Vbuttonbox1, 7, 0);
      Pack_Start (Hbox2, Vbuttonbox1, False, True, 3);

      Gtk_New_From_Stock (Make_Suite_Window.Add, Stock_Add);
      Set_Relief (Make_Suite_Window.Add, Relief_Normal);
      Set_Flags (Make_Suite_Window.Add, Can_Default);
      Button_Callback.Connect
        (Make_Suite_Window.Add, "clicked",
         On_Add_Clicked'Access);
      Add (Vbuttonbox1, Make_Suite_Window.Add);

      Gtk_New_From_Stock (Make_Suite_Window.Remove, Stock_Remove);
      Set_Relief (Make_Suite_Window.Remove, Relief_Normal);
      Set_Flags (Make_Suite_Window.Remove, Can_Default);
      Button_Callback.Connect
        (Make_Suite_Window.Remove, "clicked",
         On_Remove_Clicked'Access);
      Add (Vbuttonbox1, Make_Suite_Window.Remove);

      Gtk_New (Make_Suite_Window.Aunit1_Button, -"Create AUnit1.x test suite");
      Create_New_Boolean_Key_If_Necessary
        (GPS.Kernel.Get_History (Handle).all,
         "aunit-aunit1-compatibility", False);
      Associate
        (GPS.Kernel.Get_History (Handle).all,
         "aunit-aunit1-compatibility",
         Make_Suite_Window.Aunit1_Button);
      Pack_Start (Get_Vbox (Make_Suite_Window),
                  Make_Suite_Window.Aunit1_Button);

      Gtk_New (Make_Suite_Window.Label);
      Set_Alignment (Make_Suite_Window.Label, 0.0, 0.5);
      Pack_Start (Get_Vbox (Make_Suite_Window),
                  Make_Suite_Window.Label,
                  Expand  => True,
                  Fill    => False,
                  Padding => 0);

      Button := Gtk_Button
        (Add_Button (Make_Suite_Window, Stock_Ok, Gtk_Response_OK));
      Button := Gtk_Button
        (Add_Button (Make_Suite_Window, Stock_Cancel, Gtk_Response_Cancel));

      Check_Validity (Make_Suite_Window);
   end Initialize;

end Make_Suite_Window_Pkg;
