-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Stock;       use Gtk.Stock;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Codefix_Interface; use Callbacks_Codefix_Interface;
with Glide_Intl;      use Glide_Intl;
with Codefix_Window_Pkg.Callbacks; use Codefix_Window_Pkg.Callbacks;

package body Codefix_Window_Pkg is

procedure Gtk_New (Codefix_Window : out Codefix_Window_Access) is
begin
   Codefix_Window := new Codefix_Window_Record;
   Codefix_Window_Pkg.Initialize (Codefix_Window);
end Gtk_New;

procedure Initialize (Codefix_Window : access Codefix_Window_Record'Class) is
   pragma Suppress (All_Checks);
   Fix_Caption_List_Items : String_List.Glist;
   Tooltips : Gtk_Tooltips;

begin
   Gtk.Box.Initialize_Hbox (Codefix_Window);

   Gtk_New_Vbox (Codefix_Window.Vbox6, False, 0);
   Pack_Start (Codefix_Window, Codefix_Window.Vbox6, True, True, 0);

   Gtk_New (Codefix_Window.Error_Caption);
   Set_Alignment (Codefix_Window.Error_Caption, 0.5, 0.5);
   Set_Padding (Codefix_Window.Error_Caption, 0, 0);
   Set_Justify (Codefix_Window.Error_Caption, Justify_Left);
   Set_Line_Wrap (Codefix_Window.Error_Caption, False);
   Pack_Start (Codefix_Window.Vbox6, Codefix_Window.Error_Caption, False, False, 0);

   Gtk_New (Codefix_Window.Choices_Proposed);
   Set_Scrollable (Codefix_Window.Choices_Proposed, False);
   Set_Show_Border (Codefix_Window.Choices_Proposed, True);
   Set_Show_Tabs (Codefix_Window.Choices_Proposed, False);
   Set_Tab_Hborder (Codefix_Window.Choices_Proposed, 2);
   Set_Tab_Vborder (Codefix_Window.Choices_Proposed, 2);
   Set_Tab_Pos (Codefix_Window.Choices_Proposed, Pos_Top);
   Pack_Start (Codefix_Window.Vbox6, Codefix_Window.Choices_Proposed, True, True, 0);

   Gtk_New_Vbox (Codefix_Window.Vbox4, False, 0);
   Add (Codefix_Window.Choices_Proposed, Codefix_Window.Vbox4);

   Gtk_New (Codefix_Window.Label3, -("Choice 1"));
   Set_Alignment (Codefix_Window.Label3, 0.5, 0.5);
   Set_Padding (Codefix_Window.Label3, 0, 0);
   Set_Justify (Codefix_Window.Label3, Justify_Center);
   Set_Line_Wrap (Codefix_Window.Label3, False);
   Set_Tab (Codefix_Window.Choices_Proposed, 0, Codefix_Window.Label3);

   Gtk_New (Codefix_Window.Fix_Caption_List);
   Set_Case_Sensitive (Codefix_Window.Fix_Caption_List, False);
   Set_Use_Arrows (Codefix_Window.Fix_Caption_List, True);
   Set_Use_Arrows_Always (Codefix_Window.Fix_Caption_List, False);
   String_List.Append (Fix_Caption_List_Items, -"");
   Combo.Set_Popdown_Strings (Codefix_Window.Fix_Caption_List, Fix_Caption_List_Items);
   Free_String_List (Fix_Caption_List_Items);
   Pack_Start (Codefix_Window.Vbox6, Codefix_Window.Fix_Caption_List, False, False, 0);

   Codefix_Window.Fix_Entry := Get_Entry (Codefix_Window.Fix_Caption_List);
   Set_Editable (Codefix_Window.Fix_Entry, True);
   Set_Max_Length (Codefix_Window.Fix_Entry, 0);
   Set_Text (Codefix_Window.Fix_Entry, -"");
   Set_Visibility (Codefix_Window.Fix_Entry, True);
   Widget_Callback.Object_Connect
     (Codefix_Window.Fix_Entry, "changed",
      Widget_Callback.To_Marshaller (On_Fix_Entry_Changed'Access), Codefix_Window);

   Gtk_New (Codefix_Window.Vbuttonbox1);
   Set_Spacing (Codefix_Window.Vbuttonbox1, 10);
   Set_Layout (Codefix_Window.Vbuttonbox1, Buttonbox_Start);
   Set_Child_Size (Codefix_Window.Vbuttonbox1, 85, 27);
   Set_Child_Ipadding (Codefix_Window.Vbuttonbox1, 7, 0);
   Pack_Start (Codefix_Window, Codefix_Window.Vbuttonbox1, False, True, 0);

   Gtk_New_From_Stock (Codefix_Window.Prev, Stock_Go_Back);
   Set_Relief (Codefix_Window.Prev, Relief_Normal);
   Set_Flags (Codefix_Window.Prev, Can_Default);
   Gtk_New (Tooltips);
   Set_Tip (Tooltips, Codefix_Window.Prev, -"Go to previous error");
   Widget_Callback.Object_Connect
     (Codefix_Window.Prev, "clicked",
      Widget_Callback.To_Marshaller (On_Prev_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Prev);

   Gtk_New_From_Stock (Codefix_Window.Next, Stock_Go_Forward);
   Set_Relief (Codefix_Window.Next, Relief_Normal);
   Set_Flags (Codefix_Window.Next, Can_Default);
   Set_Tip (Tooltips, Codefix_Window.Next, -"Go to next error without fixing the current one");
   Widget_Callback.Object_Connect
     (Codefix_Window.Next, "clicked",
      Widget_Callback.To_Marshaller (On_Next_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Next);

   Gtk_New_From_Stock (Codefix_Window.Accept_Correction, Stock_Apply);
   Set_Relief (Codefix_Window.Accept_Correction, Relief_Normal);
   Set_Flags (Codefix_Window.Accept_Correction, Can_Default);
   Grab_Default (Codefix_Window.Accept_Correction);
   Set_Tip (Tooltips, Codefix_Window.Accept_Correction, -"Fix this error and go to next one");
   Widget_Callback.Object_Connect
     (Codefix_Window.Accept_Correction, "clicked",
      Widget_Callback.To_Marshaller (On_Apply_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Accept_Correction);

   Gtk_New_From_Stock (Codefix_Window.Undo, Stock_Undo);
   Set_Relief (Codefix_Window.Undo, Relief_Normal);
   Set_Flags (Codefix_Window.Undo, Can_Default);
   Set_Tip (Tooltips, Codefix_Window.Undo, -"Undo previous change");
   Widget_Callback.Object_Connect
     (Codefix_Window.Undo, "clicked",
      Widget_Callback.To_Marshaller (On_Undo_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Undo);

   Gtk_New_From_Stock (Codefix_Window.Refresh, Stock_Refresh);
   Set_Relief (Codefix_Window.Refresh, Relief_Normal);
   Set_Flags (Codefix_Window.Refresh, Can_Default);
   Set_Tip (Tooltips, Codefix_Window.Refresh, -"Recompute fix with last user's changes");
   Widget_Callback.Object_Connect
     (Codefix_Window.Refresh, "clicked",
      Widget_Callback.To_Marshaller (On_Refresh_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Refresh);

end Initialize;

end Codefix_Window_Pkg;
