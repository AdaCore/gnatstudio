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

with Gtk; use Gtk;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Print_Dialog_Pkg.Callbacks; use Print_Dialog_Pkg.Callbacks;

package body Print_Dialog_Pkg is

procedure Gtk_New (Print_Dialog : out Print_Dialog_Access) is
begin
   Print_Dialog := new Print_Dialog_Record;
   Print_Dialog_Pkg.Initialize (Print_Dialog);
end Gtk_New;

procedure Initialize (Print_Dialog : access Print_Dialog_Record'Class) is
begin
   Gtk.Dialog.Initialize (Print_Dialog);
   Set_Title (Print_Dialog, "Print Dialog");
   Set_Policy (Print_Dialog, True, True, False);
   Set_Position (Print_Dialog, Win_Pos_Mouse);
   Set_Modal (Print_Dialog, True);

   Print_Dialog.Dialog_Vbox1 := Get_Vbox (Print_Dialog);
   Set_Homogeneous (Print_Dialog.Dialog_Vbox1, False);
   Set_Spacing (Print_Dialog.Dialog_Vbox1, 0);

   Gtk_New_Vbox (Print_Dialog.Vbox1, False, 5);
   Pack_Start (Print_Dialog.Dialog_Vbox1, Print_Dialog.Vbox1, True, True, 0);
   Set_Border_Width (Print_Dialog.Vbox1, 10);

   Gtk_New (Print_Dialog.Label1, "Print Expression");
   Pack_Start (Print_Dialog.Vbox1, Print_Dialog.Label1, False, False, 0);
   Set_Alignment (Print_Dialog.Label1, 0.0, 0.5);
   Set_Padding (Print_Dialog.Label1, 0, 0);
   Set_Justify (Print_Dialog.Label1, Justify_Center);
   Set_Line_Wrap (Print_Dialog.Label1, False);

   Gtk_New (Print_Dialog.Combo1);
   Pack_Start (Print_Dialog.Vbox1, Print_Dialog.Combo1, False, False, 0);
   Set_Case_Sensitive (Print_Dialog.Combo1, False);
   Set_Use_Arrows (Print_Dialog.Combo1, True);
   Set_Use_Arrows_Always (Print_Dialog.Combo1, False);

   Print_Dialog.Combo_Entry1 := Get_Entry (Print_Dialog.Combo1);
   Set_Editable (Print_Dialog.Combo_Entry1, True);
   Set_Max_Length (Print_Dialog.Combo_Entry1, 0);
   Set_Text (Print_Dialog.Combo_Entry1, "");
   Set_Visibility (Print_Dialog.Combo_Entry1, True);

   Print_Dialog.Dialog_Action_Area1 := Get_Action_Area (Print_Dialog);
   Set_Border_Width (Print_Dialog.Dialog_Action_Area1, 10);
   Set_Homogeneous (Print_Dialog.Dialog_Action_Area1, True);
   Set_Spacing (Print_Dialog.Dialog_Action_Area1, 5);

   Gtk_New (Print_Dialog.Hbuttonbox1);
   Pack_Start (Print_Dialog.Dialog_Action_Area1, Print_Dialog.Hbuttonbox1, True, True, 0);
   Set_Spacing (Print_Dialog.Hbuttonbox1, 30);
   Set_Layout (Print_Dialog.Hbuttonbox1, Buttonbox_Spread);
   Set_Child_Size (Print_Dialog.Hbuttonbox1, 85, 27);
   Set_Child_Ipadding (Print_Dialog.Hbuttonbox1, 7, 0);

   Gtk_New (Print_Dialog.Print_Button, "Print");
   Widget_Callback.Object_Connect
     (Print_Dialog.Print_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Print_Button_Clicked'Access), Print_Dialog);
   Add (Print_Dialog.Hbuttonbox1, Print_Dialog.Print_Button);

   Gtk_New (Print_Dialog.Cancel_Button, "Cancel");
   Button_Callback.Connect
     (Print_Dialog.Cancel_Button, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access));
   Add (Print_Dialog.Hbuttonbox1, Print_Dialog.Cancel_Button);

   Gtk_New (Print_Dialog.Help_Button, "Help");
   Add (Print_Dialog.Hbuttonbox1, Print_Dialog.Help_Button);

   Disable_Activate (Print_Dialog.Combo1);
   Widget_Callback.Object_Connect
     (Print_Dialog.Combo_Entry1, "activate",
      Widget_Callback.To_Marshaller (On_Print_Button_Clicked'Access),
      Print_Dialog);
end Initialize;

end Print_Dialog_Pkg;
