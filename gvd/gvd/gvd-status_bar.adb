-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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

with Glib;            use Glib;
with Gdk.Main;        use Gdk.Main;
with Gdk.Types;       use Gdk.Types;
with Gdk.Window;      use Gdk.Window;
with Gtk.Arrow;       use Gtk.Arrow;
with Gtk.Box;         use Gtk.Box;
with Gtk.Button;      use Gtk.Button;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Frame;       use Gtk.Frame;
with Gtk.Main;        use Gtk.Main;
with Gtk.Status_Bar;  use Gtk.Status_Bar;
with Gtk.Style;       use Gtk.Style;
with Gtk.Text;        use Gtk.Text;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;      use Gtk.Window;
with Gtkada.Handlers; use Gtkada.Handlers;
with Odd_Intl;        use Odd_Intl;
with GVD.Preferences; use GVD.Preferences;

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Odd.Status_Bar is

   package Status_Timeout is new Gtk.Main.Timeout (Odd_Status_Bar);

   function Hide_Callback (Status : Odd_Status_Bar) return Boolean;
   --  Function called to hide the current message.

   procedure Arrow_Cb (Widget : access Gtk_Widget_Record'Class);
   --  Called when the user pressed the arrow

   function Button_Press
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when a mouse button is pressed and the history window is
   --  displayed.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Status : out Odd_Status_Bar) is
   begin
      Status := new Odd_Status_Bar_Record;
      Initialize (Status);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Status : access Odd_Status_Bar_Record'Class) is
   begin
      Initialize_Hbox (Status, Homogeneous => False, Spacing => 4);

      Gtk_New (Status.Arrow_Button);
      Pack_Start (Status, Status.Arrow_Button, False, False, 0);

      Gtk_New (Status.Arrow, Arrow_Up, Shadow_Out);
      Add (Status.Arrow_Button, Status.Arrow);

      Gtk_New (Status.Status);
      Pack_Start (Status, Status.Status, True, True, 0);

      Status.Ids (Help)  := Get_Context_Id (Status.Status, "Help");
      Status.Ids (Error) := Get_Context_Id (Status.Status, "Error");

      Widget_Callback.Object_Connect
        (Status.Arrow_Button, "clicked",
         Widget_Callback.To_Marshaller (Arrow_Cb'Access),
         Status);

      Show_All (Status);
   end Initialize;

   -------------------
   -- Print_Message --
   -------------------

   procedure Print_Message
     (Status  : access Odd_Status_Bar_Record;
      Context : Category;
      Msg     : String)
   is
      Id : Message_Id;
   begin
      if Status.Is_Blank then
         Pop (Status.Status, Status.Ids (Help));
      end if;

      if Status.Timeout_Id /= 0 then
         Timeout_Remove (Status.Timeout_Id);
      end if;

      Id := Push (Status.Status, Status.Ids (Context), Msg);
      Status.Is_Blank := False;

      --  schedule the message to be removed
      Status.Timeout_Id := Status_Timeout.Add
        (Hide_Delay, Hide_Callback'Access, Odd_Status_Bar (Status));
   end Print_Message;

   -------------------
   -- Hide_Callback --
   -------------------

   function Hide_Callback (Status : Odd_Status_Bar) return Boolean is
      Id : Message_Id;
   begin
      if not Status.Is_Blank then
         Status.Is_Blank := True;
         Id := Push (Status.Status, Status.Ids (Help), "");
      end if;

      --  Unregister the timeout
      Status.Timeout_Id := 0;
      return False;
   end Hide_Callback;

   --------------
   -- Arrow_Cb --
   --------------

   procedure Arrow_Cb (Widget : access Gtk_Widget_Record'Class) is
      Status  : Odd_Status_Bar := Odd_Status_Bar (Widget);
      X, Y    : Gint;
      Success : Boolean;
      Text    : Gtk_Text;
      List    : Messages_List.GSlist := Get_Messages (Status.Status);
      use type Messages_List.GSlist;
      Num     : Natural := 1;
      Frame   : Gtk_Frame;

   begin
      --  If not already displayed, create it.
      if Status.Historic_Win = null then

         Destroy (Status.Arrow);
         Gtk_New (Status.Arrow, Arrow_Down, Shadow_Out);
         Add (Status.Arrow_Button, Status.Arrow);
         Show (Status.Arrow);

         Get_Origin (Get_Window (Status.Arrow_Button), X, Y, Success);

         Gtk_New (Status.Historic_Win, Window_Popup);
         Set_Policy
           (Status.Historic_Win,
            Allow_Shrink => True,
            Allow_Grow   => True,
            Auto_Shrink  => True);

         Gtk_New (Frame);
         Set_Shadow_Type (Frame, Shadow_Etched_In);
         Add (Status.Historic_Win, Frame);
         Show (Frame);

         Gtk_New (Text);
         Add (Frame, Text);

         --  Use a grey background for this window
         Set_Style (Text, Gtk.Style.Copy (Get_Style (Status.Arrow)));
         Set_Base
           (Get_Style (Text), State_Normal,
            Color => Get_Background (Get_Style (Status.Arrow), State_Normal));

         Set_Line_Wrap (Text, False);
         Set_Word_Wrap (Text, False);
         Set_Editable (Text, False);

         Freeze (Text);
         Insert (Text, Chars => -"Recent message (most recent first):"
                 & ASCII.LF & ASCII.LF);

         --  Skip one message if it is a blank string
         if Status.Is_Blank then
            List := Messages_List.Next (List);
         end if;

         while List /= Messages_List.Null_List
           and then Num < 30
         loop
            declare
               Msg : Status_Bar_Msg := Messages_List.Get_Data (List);
            begin
               Insert
                 (Text,
                  Chars => Interfaces.C.Strings.Value (Msg.Text) & ASCII.LF);
            end;
            List := Messages_List.Next (List);
            Num := Num + 1;
         end loop;

         Thaw (Text);
         Show (Text);

         --  Reserve a minimal size
         --  ??? The text widget should be able to automatically give a correct
         --  width, but it doesn't.
         Set_Default_Size
           (Status.Historic_Win,
            Gint'Min (Gint (Get_Allocation_Width (Status.Status)), 500),
            -1);

         Realize (Status.Historic_Win);
         Y := Y - Gint (Get_Allocation_Height (Status.Historic_Win));
         Set_UPosition (Status.Historic_Win, X, Y);

         Show_All (Status.Historic_Win);

         Grab_Add (Status.Historic_Win);
         Success := Pointer_Grab
           (Get_Window (Status.Historic_Win),
            Event_Mask => Button_Press_Mask or Button_Release_Mask,
            Time       => 0);

         Return_Callback.Object_Connect
           (Status.Historic_Win, "button_press_event",
            Return_Callback.To_Marshaller (Button_Press'Access),
            Status);
         Return_Callback.Object_Connect
           (Status.Historic_Win, "key_press_event",
            Return_Callback.To_Marshaller (Button_Press'Access),
            Status);

      --  Otherwise remove it
      else
         Hide_Historic (Status);
      end if;
   end Arrow_Cb;

   -------------------
   -- Hide_Historic --
   -------------------

   procedure Hide_Historic (Status : access Odd_Status_Bar_Record) is
   begin
      Pointer_Ungrab (0);
      Grab_Remove (Status.Historic_Win);
      Destroy (Status.Historic_Win);

      Destroy (Status.Arrow);
      Gtk_New (Status.Arrow, Arrow_Up, Shadow_Out);
      Add (Status.Arrow_Button, Status.Arrow);
      Show (Status.Arrow);

      Status.Historic_Win := null;
   end Hide_Historic;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      Hide_Historic (Odd_Status_Bar (Widget));
      return False;
   end Button_Press;

end Odd.Status_Bar;
