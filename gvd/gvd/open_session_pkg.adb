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

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with Gtk;                        use Gtk;
with Gtk.Main;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.List;                   use Gtk.List;
with Gtk.List_Item;              use Gtk.List_Item;
with Gtk.Enums;                  use Gtk.Enums;
with Callbacks_Odd;              use Callbacks_Odd;
with Odd_Intl;                   use Odd_Intl;
with Open_Session_Pkg.Callbacks; use Open_Session_Pkg.Callbacks;

package body Open_Session_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Open_Session : out Open_Session_Access) is
   begin
      Open_Session := new Open_Session_Record;
      Open_Session_Pkg.Initialize (Open_Session);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Open_Session : access Open_Session_Record'Class) is
   begin
      Gtk.Window.Initialize (Open_Session, Window_Toplevel);
      Set_Title (Open_Session, -"Open Session");
      Set_Policy (Open_Session, False, True, False);
      Set_Position (Open_Session, Win_Pos_Center);
      Set_Modal (Open_Session, True);

      Gtk_New_Vbox (Open_Session.Vbox17, False, 0);
      Add (Open_Session, Open_Session.Vbox17);

      Gtk_New (Open_Session.Scrolledwindow10);
      Pack_Start (Open_Session.Vbox17,
                  Open_Session.Scrolledwindow10, True, True, 0);
      Set_Policy (Open_Session.Scrolledwindow10,
                  Policy_Automatic, Policy_Automatic);

      Gtk_New (Open_Session.Viewport1);
      Add (Open_Session.Scrolledwindow10, Open_Session.Viewport1);
      Set_Shadow_Type (Open_Session.Viewport1, Shadow_In);

      Gtk_New (Open_Session.List);
      List_Callback.Connect
        (Open_Session.List, "selection_changed",
         List_Callback.To_Marshaller (On_List_Selection_Changed'Access));
      List_Callback.Connect
        (Open_Session.List, "select_child", On_List_Select_Child'Access);
      Add (Open_Session.Viewport1, Open_Session.List);
      Set_Selection_Mode (Open_Session.List, Selection_Single);

      Gtk_New_Hbox (Open_Session.Hbox6, False, 0);
      Pack_Start (Open_Session.Vbox17, Open_Session.Hbox6, True, True, 0);

      Gtk_New (Open_Session.Label73, -("Session :"));
      Pack_Start (Open_Session.Hbox6, Open_Session.Label73, False, False, 5);
      Set_Alignment (Open_Session.Label73, 0.5, 0.5);
      Set_Padding (Open_Session.Label73, 0, 0);
      Set_Justify (Open_Session.Label73, Justify_Center);
      Set_Line_Wrap (Open_Session.Label73, False);

      Gtk_New (Open_Session.Entry1);
      Pack_Start (Open_Session.Hbox6, Open_Session.Entry1, True, True, 14);
      Set_Editable (Open_Session.Entry1, True);
      Set_Max_Length (Open_Session.Entry1, 0);
      Set_Text (Open_Session.Entry1, -"");
      Set_Visibility (Open_Session.Entry1, True);

      Gtk_New (Open_Session.Hbuttonbox9);
      Pack_Start (Open_Session.Vbox17,
                  Open_Session.Hbuttonbox9, True, True, 0);
      Set_Spacing (Open_Session.Hbuttonbox9, 30);
      Set_Layout (Open_Session.Hbuttonbox9, Buttonbox_Spread);
      Set_Child_Size (Open_Session.Hbuttonbox9, 85, 27);
      Set_Child_Ipadding (Open_Session.Hbuttonbox9, 7, 0);

      Gtk_New (Open_Session.Ok_Button, -"OK");
      Set_Flags (Open_Session.Ok_Button, Can_Default);
      Button_Callback.Connect
        (Open_Session.Ok_Button, "clicked",
         Button_Callback.To_Marshaller (On_Ok_Button_Clicked'Access));
      Add (Open_Session.Hbuttonbox9, Open_Session.Ok_Button);

      Gtk_New (Open_Session.Cancel_Button, -"Cancel");
      Set_Flags (Open_Session.Cancel_Button, Can_Default);
      Button_Callback.Connect
        (Open_Session.Cancel_Button, "clicked",
         Button_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access));
      Add (Open_Session.Hbuttonbox9, Open_Session.Cancel_Button);

      Gtk_New (Open_Session.Help_Button, -"Help");
      Set_Flags (Open_Session.Help_Button, Can_Default);
      Button_Callback.Connect
        (Open_Session.Help_Button, "clicked",
         Button_Callback.To_Marshaller (On_Help_Button_Clicked'Access));
      Add (Open_Session.Hbuttonbox9, Open_Session.Help_Button);

   end Initialize;

   ------------------
   -- Open_Session --
   ------------------

   procedure Open_Session (Open : in out Open_Session_Access;
                           File : out Odd.Types.String_Access) is
      Home : GNAT.OS_Lib.String_Access;
      Sessions_Dir : Dir_Type;
      Buffer : String (1 .. 256);
      Last : Natural;
      Item : Gtk_List_Item;

   begin
      if Open = null then
         Gtk_New (Open);
      end if;

      Show_All (Open);

      Home := Getenv ("HOME");

      Remove_Items (Open.List, Get_Children (Open.List));

      if Home /= null then

         GNAT.Directory_Operations.Open
           (Sessions_Dir, Home.all & Directory_Separator &
            ".gvd" & Directory_Separator & "sessions");
         loop
            Read (Sessions_Dir, Buffer, Last);
            exit when Last = 0;
            if Buffer (1) /= '.' then
               Gtk_New (Item, Label => Buffer (1 .. Last));
               Show (Item);
               Add (Open.List, Item);
            end if;
         end loop;
      end if;

      GNAT.Directory_Operations.Close (Sessions_Dir);
      Gtk.Main.Main;

      File := new String' (Get_Text (Open.Entry1));

   end Open_Session;

end Open_Session_Pkg;
