-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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

with Glib;             use Glib;
pragma Warnings (Off);
with Gdk.Event;        use Gdk.Event;
with Gdk.Types;        use Gdk.Types;
pragma Warnings (On);
with Gdk.Main;         use Gdk.Main;
with Gdk.Window;       use Gdk.Window;
with Gtk.Arrow;        use Gtk.Arrow;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Event_Box;    use Gtk.Event_Box;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Main;         use Gtk.Main;
pragma Elaborate_All (Gtk.Main);
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Status_Bar;   use Gtk.Status_Bar;
with Gtk.Style;        use Gtk.Style;
with Gtk.Text;         use Gtk.Text;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Odd_Intl;         use Odd_Intl;
with GVD.Preferences;  use GVD.Preferences;

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body GVD.Status_Bar is

   package Status_Timeout is new Gtk.Main.Timeout (GVD_Status_Bar);

   function Hide_Callback (Status : GVD_Status_Bar) return Boolean;
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

   procedure Gtk_New (Status : out GVD_Status_Bar) is
   begin
      Status := new GVD_Status_Bar_Record;
      Initialize (Status);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Status : access GVD_Status_Bar_Record'Class) is
      Event : Gtk_Event_Box;
   begin
      Initialize_Hbox (Status, Homogeneous => False, Spacing => 4);

      Gtk_New (Event);
      Gtk_New (Status.Arrow_Button);
      Add (Event, Status.Arrow_Button);
      Pack_Start (Status, Event, False, False, 0);

      Gtk_New (Status.Arrow, Arrow_Up, Shadow_Out);
      Add (Status.Arrow_Button, Status.Arrow);

      Gtk_New (Status.Status);
      Pack_Start (Status, Status.Status, True, True, 0);

      Gtk_New (Status.Progress);
      Pack_End (Status, Status.Progress, False, False);

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
     (Status  : access GVD_Status_Bar_Record;
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
      Status.Timeout_Id :=
        Status_Timeout.Add
          (Guint32 (Get_Pref (Hide_Delay)),
           Hide_Callback'Access, GVD_Status_Bar (Status));
   end Print_Message;

   -------------------
   -- Hide_Callback --
   -------------------

   function Hide_Callback (Status : GVD_Status_Bar) return Boolean is
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
      Status  : GVD_Status_Bar := GVD_Status_Bar (Widget);
      X, Y, W : Gint;
      Success : Boolean;
      Text    : Gtk_Text;
      List    : Messages_List.GSlist := Get_Messages (Status.Status);
      use type Messages_List.GSlist;
      Num     : Natural := 1;
      Frame   : Gtk_Frame;
      Requisition : Gtk_Requisition;

   begin
      --  If not already displayed, create it.
      if Status.History_Win = null then

         Destroy (Status.Arrow);
         Gtk_New (Status.Arrow, Arrow_Down, Shadow_Out);
         Add (Status.Arrow_Button, Status.Arrow);
         Show (Status.Arrow);

         Get_Origin (Get_Window (Status.Arrow_Button), X, Y, Success);

         Gtk_New (Status.History_Win, Window_Popup);
         Set_Policy
           (Status.History_Win,
            Allow_Shrink => True,
            Allow_Grow   => True,
            Auto_Shrink  => True);

         Gtk_New (Frame);
         Set_Shadow_Type (Frame, Shadow_Etched_In);
         Add (Status.History_Win, Frame);
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
         Insert
           (Text,
            Chars =>
              -"Recent messages (most recent first):" & ASCII.LF & ASCII.LF);

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
         --  ??? The text widget should be able to automatically give a
         --  correct width, but it doesn't.
         Size_Request (Status.History_Win, Requisition);
         W := Gint'Min (Gint (Get_Allocation_Width (Status.Status)), 500);
         Set_Default_Size (Status.History_Win, W, -1);

         Y := Y - Gint (Requisition.Height);
         if Y < 0 then
            Y := 0;
         end if;

         X := X + Gint (Get_Allocation_Width (Status.Arrow_Button));
         if X + W > Screen_Width then
            X := Gint'Max (0, Screen_Width - W);
         end if;

         Set_UPosition (Status.History_Win, X, Y);

         Realize (Status.History_Win);

         Show_All (Status.History_Win);

         Grab_Add (Status.History_Win);
         Success := Pointer_Grab
           (Get_Window (Status.History_Win),
            Event_Mask => Button_Press_Mask or Button_Release_Mask,
            Time       => 0);

         Return_Callback.Object_Connect
           (Status.History_Win, "button_press_event",
            Return_Callback.To_Marshaller (Button_Press'Access),
            Status);
         Return_Callback.Object_Connect
           (Status.History_Win, "key_press_event",
            Return_Callback.To_Marshaller (Button_Press'Access),
            Status);

      --  Otherwise remove it
      else
         Hide_History (Status);
      end if;
   end Arrow_Cb;

   ------------------
   -- Hide_History --
   ------------------

   procedure Hide_History (Status : access GVD_Status_Bar_Record) is
   begin
      Pointer_Ungrab (0);
      Grab_Remove (Status.History_Win);
      Destroy (Status.History_Win);

      Destroy (Status.Arrow);
      Gtk_New (Status.Arrow, Arrow_Up, Shadow_Out);
      Add (Status.Arrow_Button, Status.Arrow);
      Show (Status.Arrow);

      Status.History_Win := null;
   end Hide_History;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      Hide_History (GVD_Status_Bar (Widget));
      return False;
   end Button_Press;

   ------------------
   -- Set_Fraction --
   ------------------

   procedure Set_Fraction
     (Status   : access GVD_Status_Bar_Record;
      Fraction : Gdouble) is
   begin
      Set_Fraction (Status.Progress, Fraction);
   end Set_Fraction;

   -----------------------
   -- Set_Progress_Text --
   -----------------------

   procedure Set_Progress_Text
     (Status : access GVD_Status_Bar_Record;
      Text   : String) is
   begin
      Set_Text (Status.Progress, Text);
   end Set_Progress_Text;

end GVD.Status_Bar;
