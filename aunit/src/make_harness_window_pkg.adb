-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2001-2009, AdaCore                 --
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
with Aunit_Utils;         use Aunit_Utils;

with Make_Harness_Window_Pkg.Callbacks; use Make_Harness_Window_Pkg.Callbacks;

package body Make_Harness_Window_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Make_Harness_Window : out Make_Harness_Window_Access;
      Handle              : GPS.Kernel.Kernel_Handle) is
   begin
      Make_Harness_Window := new Make_Harness_Window_Record;
      Make_Harness_Window_Pkg.Initialize (Make_Harness_Window, Handle);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Make_Harness_Window : access Make_Harness_Window_Record'Class;
      Handle              : GPS.Kernel.Kernel_Handle)
   is
      pragma Suppress (All_Checks);

      Hbox_Main    : Gtk_Hbox;
      Vbox_Labels  : Gtk_Vbox;
      Vbox_Entries : Gtk_Vbox;

      Hbox   : Gtk_Hbox;

      Label  : Gtk_Label;
      Button, Cancel_Button : Gtk_Button;
      pragma Unreferenced (Cancel_Button);

   begin
      Make_Harness_Window.Kernel := Handle;

      Gtk.Dialog.Initialize
        (Make_Harness_Window,
         Title  => -"Make new harness",
         Parent => GPS.Kernel.Get_Main_Window (Handle),
         Flags  => 0);
      Set_Modal (Make_Harness_Window, True);
      Set_Policy (Make_Harness_Window, False, True, False);
      Set_Position (Make_Harness_Window, Win_Pos_Mouse);
      Set_Has_Separator (Make_Harness_Window, False);

      Gtk_New_Hbox (Hbox_Main, False, 0);
      Pack_Start (Get_Vbox (Make_Harness_Window), Hbox_Main, True, True, 0);

      Gtk_New_Vbox (Vbox_Labels, True, 0);
      Pack_Start (Hbox_Main, Vbox_Labels, True, True, 3);

      Gtk_New_Vbox (Vbox_Entries, True, 0);
      Pack_Start (Hbox_Main, Vbox_Entries, True, True, 3);

      Gtk_New_Hbox (Hbox, True, 0);
      Pack_Start (Vbox_Labels, Hbox, True, True, 3);
      Gtk_New (Label, -"Save in:");
      Pack_Start (Hbox, Label, False, False, 3);

      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (Vbox_Labels, Hbox, True, True, 0);
      Gtk_New (Label, -"Procedure name:");
      Pack_Start (Hbox, Label, False, False, 3);

      Gtk_New_Hbox (Hbox, True, 0);
      Pack_Start (Vbox_Labels, Hbox, True, True, 3);
      Gtk_New (Label, -"Suite file:");
      Pack_Start (Hbox, Label, False, False, 3);

      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (Vbox_Entries, Hbox, False, False, 3);

      Gtk_New (Make_Harness_Window.Directory_Entry);
      Set_Editable (Make_Harness_Window.Directory_Entry, True);
      Set_Width_Chars (Make_Harness_Window.Directory_Entry, 50);
      Set_Max_Length (Make_Harness_Window.Directory_Entry, 0);
      Set_Text (Make_Harness_Window.Directory_Entry,
                Get_Context_Directory (Handle).Display_Full_Name);
      Set_Visibility (Make_Harness_Window.Directory_Entry, True);
      Pack_Start (Hbox, Make_Harness_Window.Directory_Entry, True, True, 3);
      Widget_Callback.Connect
        (Make_Harness_Window.Directory_Entry, Signal_Changed,
         Check_Validity'Access);

      Gtk_New (Make_Harness_Window.Browse_Directory, -"Browse...");
      Set_Relief (Make_Harness_Window.Browse_Directory, Relief_Normal);
      Pack_Start
        (Hbox, Make_Harness_Window.Browse_Directory, False, False, 3);
      Button_Callback.Connect
        (Make_Harness_Window.Browse_Directory, Signal_Clicked,
         On_Browse_Directory_Clicked'Access);

      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (Vbox_Entries, Hbox, False, False, 3);

      Gtk_New (Make_Harness_Window.Procedure_Entry);
      Set_Editable (Make_Harness_Window.Procedure_Entry, True);
      Set_Max_Length (Make_Harness_Window.Procedure_Entry, 0);
      Set_Text (Make_Harness_Window.Procedure_Entry, -"Harness");
      Set_Visibility (Make_Harness_Window.Procedure_Entry, True);
      Pack_Start (Hbox, Make_Harness_Window.Procedure_Entry, True, True, 3);
      Widget_Callback.Connect
        (Make_Harness_Window.Procedure_Entry, Signal_Changed,
         Check_Validity'Access);

      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (Vbox_Entries, Hbox, True, True, 3);

      Gtk_New (Make_Harness_Window.File_Name_Entry);
      Set_Editable (Make_Harness_Window.File_Name_Entry, True);
      Set_Max_Length (Make_Harness_Window.File_Name_Entry, 0);
      Set_Text (Make_Harness_Window.File_Name_Entry, -"");
      Set_Visibility (Make_Harness_Window.File_Name_Entry, True);
      Pack_Start (Hbox, Make_Harness_Window.File_Name_Entry, True, True, 3);
      Widget_Callback.Connect
        (Make_Harness_Window.File_Name_Entry, Signal_Changed,
         Check_Validity'Access);

      Gtk_New (Make_Harness_Window.Browse, -"Browse...");
      Set_Name (Make_Harness_Window.Browse,
                "aunit_make_new_harness_browse_button");
      Set_Relief (Make_Harness_Window.Browse, Relief_Normal);
      Pack_Start (Hbox, Make_Harness_Window.Browse, False, False, 3);
      Button_Callback.Connect
        (Make_Harness_Window.Browse, Signal_Clicked,
         On_Browse_Clicked'Access);

      Gtk_New (Make_Harness_Window.Label);
      Set_Alignment (Make_Harness_Window.Label, 0.0, 0.5);
      Pack_Start (Get_Vbox (Make_Harness_Window),
                  Make_Harness_Window.Label,
                  Expand => True,
                  Fill   => False,
                  Padding => 10);

      Button := Gtk_Button
        (Add_Button (Make_Harness_Window, Stock_Ok, Gtk_Response_OK));
      Set_Name (Button,
                "aunit_make_new_harness_ok_button");
      Cancel_Button := Gtk_Button
        (Add_Button (Make_Harness_Window, Stock_Cancel, Gtk_Response_Cancel));

      Check_Validity (Make_Harness_Window);
   end Initialize;

end Make_Harness_Window_Pkg;
