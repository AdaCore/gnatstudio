-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
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
with Gtk.Editable;        use Gtk.Editable;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Stock;           use Gtk.Stock;

with Callbacks_Aunit_Gui; use Callbacks_Aunit_Gui;
with GPS.Intl;            use GPS.Intl;
with Histories;           use Histories;

with Aunit_Utils;         use Aunit_Utils;

with Make_Test_Window_Pkg.Callbacks; use Make_Test_Window_Pkg.Callbacks;

package body Make_Test_Window_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Make_Test_Window : out Make_Test_Window_Access;
      Handle           : GPS.Kernel.Kernel_Handle) is
   begin
      Make_Test_Window := new Make_Test_Window_Record;
      Make_Test_Window_Pkg.Initialize (Make_Test_Window, Handle);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Make_Test_Window : access Make_Test_Window_Record'Class;
      Handle           : GPS.Kernel.Kernel_Handle)
   is
      Vbox2  : Gtk_Vbox;
      Hbox1  : Gtk_Hbox;
      Hbox2  : Gtk_Hbox;
      Vbox1  : Gtk_Vbox;
      Label  : Gtk_Label;
      Button : Gtk_Button;
      pragma Unreferenced (Button);

   begin
      Make_Test_Window.Kernel := Handle;

      Gtk.Dialog.Initialize
        (Make_Test_Window,
         Title  => -"Make new test case",
         Parent => GPS.Kernel.Get_Main_Window (Handle),
         Flags  => 0);
      Set_Policy (Make_Test_Window, False, True, False);
      Set_Position (Make_Test_Window, Win_Pos_Mouse);
      Set_Modal (Make_Test_Window, True);
      Set_Has_Separator (Make_Test_Window, False);

      Gtk_New_Hbox (Hbox1, False, 3);
      Pack_Start (Get_Vbox (Make_Test_Window), Hbox1, True, True, 3);

      Gtk_New_Vbox (Vbox1, True, 0);
      Pack_Start (Hbox1, Vbox1, False, False, 5);

      Gtk_New (Label, -"Save in: ");
      Pack_Start (Vbox1, Label, False, False, 3);

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

      Gtk_New_Hbox (Hbox2, False, 0);
      Pack_Start (Vbox2, Hbox2, False, False, 3);

      Gtk_New (Make_Test_Window.Directory_Entry);
      Set_Editable (Make_Test_Window.Directory_Entry, True);
      Set_Width_Chars (Make_Test_Window.Directory_Entry, 50);
      Set_Max_Length (Make_Test_Window.Directory_Entry, 0);
      Set_Text (Make_Test_Window.Directory_Entry,
                Get_Context_Directory (Handle));
      Set_Visibility (Make_Test_Window.Directory_Entry, True);
      Pack_Start (Hbox2, Make_Test_Window.Directory_Entry, True, True, 3);

      Gtk_New (Make_Test_Window.Browse_Directory, -"Browse...");
      Set_Relief (Make_Test_Window.Browse_Directory, Relief_Normal);
      Pack_Start
        (Hbox2, Make_Test_Window.Browse_Directory, False, False, 3);
      Button_Callback.Connect
        (Make_Test_Window.Browse_Directory, Signal_Clicked,
         On_Browse_Directory_Clicked'Access);

      Gtk_New (Make_Test_Window.Name_Entry);
      Set_Editable (Make_Test_Window.Name_Entry, True);
      Set_Max_Length (Make_Test_Window.Name_Entry, 0);
      Set_Text (Make_Test_Window.Name_Entry, -"New_Test");
      Set_Visibility (Make_Test_Window.Name_Entry, True);
      Pack_Start (Vbox2, Make_Test_Window.Name_Entry, False, False, 3);
      Widget_Callback.Connect
        (Make_Test_Window.Name_Entry, Signal_Changed,
         Check_Validity'Access);

      Gtk_New (Make_Test_Window.Description_Entry);
      Set_Editable (Make_Test_Window.Description_Entry, True);
      Set_Max_Length (Make_Test_Window.Description_Entry, 0);
      Set_Text (Make_Test_Window.Description_Entry, -"(no description)");
      Set_Visibility (Make_Test_Window.Description_Entry, True);
      Pack_Start (Vbox2, Make_Test_Window.Description_Entry, False, False, 3);
      Widget_Callback.Connect
        (Make_Test_Window.Description_Entry, Signal_Changed,
         Check_Validity'Access);

      Gtk_New (Make_Test_Window.Override_Tear_Down, -"Override Tear_Down");
      Set_Active (Make_Test_Window.Override_Tear_Down, False);
      Pack_Start (Vbox2, Make_Test_Window.Override_Tear_Down, False, False, 3);

      Gtk_New (Make_Test_Window.Override_Set_Up, -"Override Set_up");
      Set_Active (Make_Test_Window.Override_Set_Up, False);
      Pack_Start (Vbox2, Make_Test_Window.Override_Set_Up, False, False, 3);

      Gtk_New (Make_Test_Window.Aunit1_Button, -"Create AUnit1.x test case");
      Create_New_Boolean_Key_If_Necessary
        (GPS.Kernel.Get_History (Handle).all,
         "aunit-aunit1-compatibility", False);
      Associate
        (GPS.Kernel.Get_History (Handle).all,
         "aunit-aunit1-compatibility",
         Make_Test_Window.Aunit1_Button);
      Pack_Start (Vbox2, Make_Test_Window.Aunit1_Button);

      Gtk_New (Make_Test_Window.Label);
      Set_Alignment (Make_Test_Window.Label, 0.0, 0.5);
      Pack_Start (Get_Vbox (Make_Test_Window),
                  Make_Test_Window.Label,
                  Expand => True,
                  Fill   => False,
                  Padding => 10);

      Button := Gtk_Button
        (Add_Button (Make_Test_Window, Stock_Ok, Gtk_Response_OK));
      Button := Gtk_Button
        (Add_Button (Make_Test_Window, Stock_Cancel, Gtk_Response_Cancel));

      Check_Validity (Make_Test_Window);
   end Initialize;

end Make_Test_Window_Pkg;
