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
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Menu;        use Gtk.Menu;
with Gtk.Menu_Item;   use Gtk.Menu_Item;
with Gtk.Main;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Open_Session_Pkg.Callbacks; use Open_Session_Pkg.Callbacks;
with System;
with Odd.Types;       use Odd.Types;

package body Open_Session_Pkg is

pragma Suppress (All_Checks);
--  Checks are expensive (in code size) in this unit, and not needed,
--  since the following code is generated automatically.

procedure Gtk_New (Open_Session : out Open_Session_Access) is
begin
   Open_Session := new Open_Session_Record;
   Open_Session_Pkg.Initialize (Open_Session);
end Gtk_New;

procedure Initialize (Open_Session : access Open_Session_Record'Class) is
   Combo12_Items : String_List.Glist;
   Launch_Menu_Menu : Gtk_Menu;
   The_Menu_Item : Gtk_Menu_Item;

begin
   Gtk.Window.Initialize (Open_Session, Window_Toplevel);
   Set_Title (Open_Session, -"Open Session");
   Set_Policy (Open_Session, False, True, False);
   Set_Position (Open_Session, Win_Pos_Center);
   Set_Modal (Open_Session, True);

   Gtk_New_Vbox (Open_Session.Vbox17, False, 0);
   Add (Open_Session, Open_Session.Vbox17);

   Gtk_New (Open_Session.Frame13);
   Pack_Start (Open_Session.Vbox17, Open_Session.Frame13, True, True, 0);
   Set_Shadow_Type (Open_Session.Frame13, Shadow_Etched_In);

   Gtk_New (Open_Session.Table8, 2, 7, True);
   Add (Open_Session.Frame13, Open_Session.Table8);
   Set_Row_Spacings (Open_Session.Table8, 3);
   Set_Col_Spacings (Open_Session.Table8, 3);

   Gtk_New (Open_Session.Combo12);
   Attach (Open_Session.Table8, Open_Session.Combo12, 1, 6, 0, 1,
     Expand or Fill, 0,
     0, 0);
   Set_Case_Sensitive (Open_Session.Combo12, False);
   Set_Use_Arrows (Open_Session.Combo12, True);
   Set_Use_Arrows_Always (Open_Session.Combo12, False);
   String_List.Append (Combo12_Items, -"");
   Combo.Set_Popdown_Strings (Open_Session.Combo12, Combo12_Items);
   Free_String_List (Combo12_Items);

   Open_Session.Program_Entry := Get_Entry (Open_Session.Combo12);
   Set_Editable (Open_Session.Program_Entry, True);
   Set_Max_Length (Open_Session.Program_Entry, 0);
   Set_Text (Open_Session.Program_Entry, -"");
   Set_Visibility (Open_Session.Program_Entry, True);

   Gtk_New (Open_Session.Open_Button, -"...");
   Attach (Open_Session.Table8, Open_Session.Open_Button, 6, 7, 0, 1,
     0, 0,
     0, 0);
   Button_Callback.Connect
     (Open_Session.Open_Button, "clicked",
      Button_Callback.To_Marshaller (On_Open_Button_Clicked'Access));

   Gtk_New (Open_Session.Launch_Menu);
   Attach (Open_Session.Table8, Open_Session.Launch_Menu, 1, 6, 1, 2,
     Fill, 0,
     0, 0);
   Menu.Gtk_New (Launch_Menu_Menu);
   Menu_Item.Gtk_New (The_Menu_Item, -"in the current debugger session");
   Menu.Append (Launch_Menu_Menu, The_Menu_Item);
   Menu_Item.Gtk_New (The_Menu_Item, -"in a new debugger session");
   Menu.Append (Launch_Menu_Menu, The_Menu_Item);
   Option_Menu.Set_Menu
     (Gtk_Option_Menu (Open_Session.Launch_Menu),
      Launch_Menu_Menu);

   Gtk_New (Open_Session.Label69, -("Session File"));
   Attach (Open_Session.Table8, Open_Session.Label69, 0, 1, 0, 1,
     Fill, Expand,
     0, 0);
   Set_Alignment (Open_Session.Label69, 7.45058e-09, 0.5);
   Set_Padding (Open_Session.Label69, 0, 0);
   Set_Justify (Open_Session.Label69, Justify_Center);
   Set_Line_Wrap (Open_Session.Label69, False);

   Gtk_New (Open_Session.Label71, -("Launch"));
   Attach (Open_Session.Table8, Open_Session.Label71, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (Open_Session.Label71, 7.45058e-09, 0.5);
   Set_Padding (Open_Session.Label71, 0, 0);
   Set_Justify (Open_Session.Label71, Justify_Center);
   Set_Line_Wrap (Open_Session.Label71, False);

   Gtk_New (Open_Session.Hbuttonbox9);
   Pack_Start (Open_Session.Vbox17, Open_Session.Hbuttonbox9, False, True, 0);
   Set_Spacing (Open_Session.Hbuttonbox9, 30);
   Set_Layout (Open_Session.Hbuttonbox9, Buttonbox_Spread);
   Set_Child_Size (Open_Session.Hbuttonbox9, 85, 27);
   Set_Child_Ipadding (Open_Session.Hbuttonbox9, 7, 0);

   Gtk_New (Open_Session.Ok_Button, -"OK");
   Set_Flags (Open_Session.Ok_Button, Can_Default);
   Button_Callback.Connect
     (Open_Session.Ok_Button, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Open_Clicked'Access));
   Add (Open_Session.Hbuttonbox9, Open_Session.Ok_Button);

   Gtk_New (Open_Session.Cancel_Button, -"Cancel");
   Set_Flags (Open_Session.Cancel_Button, Can_Default);
   Button_Callback.Connect
     (Open_Session.Cancel_Button, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Open_Clicked'Access));
   Add (Open_Session.Hbuttonbox9, Open_Session.Cancel_Button);

   Gtk_New (Open_Session.Help_Button, -"Help");
   Set_Flags (Open_Session.Help_Button, Can_Default);
   Button_Callback.Connect
     (Open_Session.Help_Button, "clicked",
      Button_Callback.To_Marshaller (On_Help_Open_Clicked'Access));
   Add (Open_Session.Hbuttonbox9, Open_Session.Help_Button);

end Initialize;

procedure Open_Session
  (Open   : in out Open_Session_Access;
   File   : out Odd.Types.String_Access;
   Launch : out Launch_Method)
is
   Menu  : System.Address;
   Menu1 : Widget_List.Glist;

   use Widget_List;
   use type System.Address;

begin
   if Open = null then
      Gtk_New (Open);
   end if;

   Show_All (Open);
   Gtk.Main.Main;

   if not Open.Valid then
      Launch := None;
      Hide (Open);
      return;
   end if;

   File := new String' (Get_Text (Open.Program_Entry));

   --  Retrieve the label associated with the selected option menu

   Menu   := Get_Object (Get_Active (Get_Menu (Open.Launch_Menu)));
   Menu1  := Children (Get_Menu (Open.Launch_Menu));
   Launch := Launch_Method'Succ (None);

   while Get_Object (Get_Data (Menu1)) /= Menu loop
      Menu1 := Next (Menu1);
      Launch := Launch_Method'Succ (Launch);
   end loop;

   Hide (Open);
end Open_Session;

end Open_Session_Pkg;
