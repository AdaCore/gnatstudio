-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                      use Glib;
with Gtk;                       use Gtk;
with Gtk.CList;                 use Gtk.CList;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Main;                  use Gtk.Main;
with Callbacks_Odd;             use Callbacks_Odd;
with Odd_Intl;                  use Odd_Intl;
with List_Select_Pkg.Callbacks; use List_Select_Pkg.Callbacks;
with Gtkada.Types;              use Gtkada.Types;

package body List_Select_Pkg is

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (List_Select : List_Select_Access;
      Label       : String;
      Comment     : String)
   is
      Index : Gint;
      Text  : Chars_Ptr_Array := Label + Comment;
   begin
      Index := Append (List_Select.List, Text);
      Free (Text);
   end Add_Item;

   ----------------------
   -- Remove_All_Items --
   ----------------------

   procedure Remove_All_Items (List_Select : List_Select_Access) is
   begin
      Clear (List_Select.List);
   end Remove_All_Items;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (List_Select : List_Select_Access; Label : String) is
   begin
      Set_Text (List_Select.The_Entry, Label);
   end Set_Label;

   ----------
   -- Show --
   ----------

   function Show (List_Select : List_Select_Access) return String is
      Dummy : Gint;
   begin
      Dummy := Columns_Autosize (List_Select.List);
      Show_All (List_Select);
      Gtk.Main.Main;

      declare
         S : constant String := Get_Text (List_Select.The_Entry);
      begin
         Destroy (List_Select);
         return S;
      end;
   end Show;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (List_Select   : out List_Select_Access;
      Title         : String := "";
      Help_Message  : String := "";
      Item_Label    : String := "";
      Comment_Label : String := "") is
   begin
      List_Select := new List_Select_Record;
      List_Select_Pkg.Initialize
        (List_Select, Title, Help_Message, Item_Label, Comment_Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (List_Select   : access List_Select_Record'Class;
      Title         : String;
      Help_Message  : String;
      Item_Label    : String;
      Comment_Label : String) is
   begin
      Gtk.Window.Initialize (List_Select, Window_Toplevel);
      List_Select.Help_Text := new String' (Help_Message);

      Set_Policy (List_Select, False, True, False);
      Set_Modal (List_Select, True);

      Gtk_New_Vbox (List_Select.Vbox, False, 0);
      Add (List_Select, List_Select.Vbox);

      Gtk_New_Hbox (List_Select.Hbox, False, 0);
      Pack_Start (List_Select.Vbox, List_Select.Hbox, True, True, 15);

      Gtk_New (List_Select.Scrolledwindow);
      Set_Policy
        (List_Select.Scrolledwindow, Policy_Automatic, Policy_Automatic);
      Pack_Start
        (List_Select.Hbox, List_Select.Scrolledwindow, True, True, 15);
      Set_USize (List_Select.Scrolledwindow, -1, 250);

      Gtk_New (List_Select.List, 2);
      Set_Selection_Mode (List_Select.List, Selection_Single);
      Set_Show_Titles (List_Select.List, True);
      Set_Column_Width (List_Select.List, 0, 80);
      Set_Column_Width (List_Select.List, 1, 80);
      C_List_Callback.Connect
        (List_Select.List, "select_row", On_Clist_Select_Row'Access);
      Add_With_Viewport (List_Select.Scrolledwindow, List_Select.List);

      Gtk_New (List_Select.Label1);
      Set_Column_Widget (List_Select.List, 0, List_Select.Label1);

      Gtk_New (List_Select.Label2);
      Set_Column_Widget (List_Select.List, 1, List_Select.Label2);

      Gtk_New_Hbox (List_Select.Hbox2, False, 0);
      Pack_Start (List_Select.Vbox, List_Select.Hbox2, False, False, 0);

      Gtk_New (List_Select.The_Entry);
      Set_Editable (List_Select.The_Entry, True);
      Set_Max_Length (List_Select.The_Entry, 0);
      Set_Visibility (List_Select.The_Entry, True);
      Pack_Start (List_Select.Hbox2, List_Select.The_Entry, True, True, 15);
      Entry_Callback.Connect
        (List_Select.The_Entry, "activate",
         Entry_Callback.To_Marshaller (On_The_Entry_Activate'Access));

      Gtk_New (List_Select.Hbuttonbox);
      Set_Spacing (List_Select.Hbuttonbox, 30);
      Set_Layout (List_Select.Hbuttonbox, Buttonbox_Spread);
      Set_Child_Size (List_Select.Hbuttonbox, 85, 27);
      Set_Child_Ipadding (List_Select.Hbuttonbox, 7, 0);
      Pack_Start (List_Select.Vbox, List_Select.Hbuttonbox, False, True, 0);

      Gtk_New (List_Select.Ok, -"OK");
      Set_Flags (List_Select.Ok, Can_Default);
      Button_Callback.Connect
        (List_Select.Ok, "clicked",
         Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
      Add (List_Select.Hbuttonbox, List_Select.Ok);

      Gtk_New (List_Select.Cancel, -"Cancel");
      Set_Flags (List_Select.Cancel, Can_Default);
      Button_Callback.Connect
        (List_Select.Cancel, "clicked",
         Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
      Add (List_Select.Hbuttonbox, List_Select.Cancel);

      if List_Select.Help_Text.all /= "" then
         Gtk_New (List_Select.Help, -"Help");
         Set_Flags (List_Select.Help, Can_Default);
         Button_Callback.Connect
           (List_Select.Help, "clicked",
            Button_Callback.To_Marshaller (On_Help_Clicked'Access));
         Add (List_Select.Hbuttonbox, List_Select.Help);
      end if;

      if Item_Label = "" and then Comment_Label = "" then
         Column_Titles_Hide (List_Select.List);
      else
         Set_Column_Title (List_Select.List, 0, Item_Label);
         Set_Column_Title (List_Select.List, 1, Comment_Label);
      end if;

      Set_Title (List_Select, Title);
   end Initialize;
end List_Select_Pkg;
