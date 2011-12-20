------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Glib; use Glib;

with Gtk;                 use Gtk;
with Gtk.Box;             use Gtk.Box;
with Gtk.Editable;        use Gtk.Editable;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Stock;           use Gtk.Stock;
with Gtk.Vbutton_Box;     use Gtk.Vbutton_Box;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Callbacks_Aunit_Gui; use Callbacks_Aunit_Gui;
with GPS.Intl;            use GPS.Intl;

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
      Button, Cancel_Button : Gtk_Button;
      pragma Unreferenced (Cancel_Button);

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
                Get_Context_Directory (Handle).Display_Full_Name);
      Set_Visibility (Make_Suite_Window.Directory_Entry, True);
      Pack_Start (Hbox4, Make_Suite_Window.Directory_Entry, True, True, 3);
      Widget_Callback.Connect
        (Make_Suite_Window.Directory_Entry, Signal_Changed,
         Check_Validity'Access);

      Gtk_New (Make_Suite_Window.Browse_Directory, -"Browse...");
      Set_Relief (Make_Suite_Window.Browse_Directory, Relief_Normal);
      Pack_Start
        (Hbox4, Make_Suite_Window.Browse_Directory, False, False, 3);
      Button_Callback.Connect
        (Make_Suite_Window.Browse_Directory, Gtk.Button.Signal_Clicked,
         On_Browse_Directory_Clicked'Access);

      Gtk_New (Make_Suite_Window.Name_Entry);
      Set_Editable (Make_Suite_Window.Name_Entry, True);
      Set_Max_Length (Make_Suite_Window.Name_Entry, 0);
      Set_Text (Make_Suite_Window.Name_Entry, -"New_Suite");
      Set_Visibility (Make_Suite_Window.Name_Entry, True);
      Pack_Start (Vbox3, Make_Suite_Window.Name_Entry, False, False, 1);
      Widget_Callback.Connect
        (Make_Suite_Window.Name_Entry, Signal_Changed,
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

      Gtk_New (Make_Suite_Window.Test_Model,
        (0 => GType_String,
         1 => GType_String,
         2 => GType_String,
         3 => GType_String));
      Gtk_New (Make_Suite_Window.Test_View, Make_Suite_Window.Test_Model);

      declare
         T : Gtk_Cell_Renderer_Text;
         C : Gtk_Tree_View_Column;
         Dummy : Gint;
         pragma Unreferenced (Dummy);
      begin
         Gtk_New (C);
         Set_Title (C, "");
         Dummy := Make_Suite_Window.Test_View.Append_Column (C);

         Gtk_New (T);
         Pack_Start (C, T, True);
         Add_Attribute (C, T, "text", 1);

         Gtk_New (T);
         Pack_Start (C, T, True);
         Add_Attribute (C, T, "text", 2);
      end;

      Set_Headers_Visible (Make_Suite_Window.Test_View, False);

      Add (Scrolledwindow2, Make_Suite_Window.Test_View);

      Gtk_New (Vbuttonbox1);
      Set_Spacing (Vbuttonbox1, 10);
      Set_Layout (Vbuttonbox1, Buttonbox_Spread);
      Set_Child_Size (Vbuttonbox1, 85, 27);
      Set_Child_Ipadding (Vbuttonbox1, 7, 0);
      Pack_Start (Hbox2, Vbuttonbox1, False, True, 3);

      Gtk_New_From_Stock (Make_Suite_Window.Add, Stock_Add);
      Set_Name (Make_Suite_Window.Add,
                "aunit_make_new_test_suite_browse_button");
      Set_Relief (Make_Suite_Window.Add, Relief_Normal);
      Set_Flags (Make_Suite_Window.Add, Can_Default);
      Button_Callback.Connect
        (Make_Suite_Window.Add, Gtk.Button.Signal_Clicked,
         On_Add_Clicked'Access);
      Add (Vbuttonbox1, Make_Suite_Window.Add);

      Gtk_New_From_Stock (Make_Suite_Window.Remove, Stock_Remove);
      Set_Relief (Make_Suite_Window.Remove, Relief_Normal);
      Set_Flags (Make_Suite_Window.Remove, Can_Default);
      Button_Callback.Connect
        (Make_Suite_Window.Remove, Gtk.Button.Signal_Clicked,
         On_Remove_Clicked'Access);
      Add (Vbuttonbox1, Make_Suite_Window.Remove);

      Gtk_New (Make_Suite_Window.Label);
      Set_Alignment (Make_Suite_Window.Label, 0.0, 0.5);
      Pack_Start (Get_Vbox (Make_Suite_Window),
                  Make_Suite_Window.Label,
                  Expand  => True,
                  Fill    => False,
                  Padding => 0);

      Button := Gtk_Button
        (Add_Button (Make_Suite_Window, Stock_Ok, Gtk_Response_OK));
      Set_Name (Button,
                "aunit_make_new_test_suite_ok_button");
      Cancel_Button := Gtk_Button
        (Add_Button (Make_Suite_Window, Stock_Cancel, Gtk_Response_Cancel));

      Check_Validity (Make_Suite_Window);
   end Initialize;

end Make_Suite_Window_Pkg;
