-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib;                 use Glib;
with Gdk.Color;            use Gdk.Color;
with Gdk.Event;            use Gdk.Event;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Gtk.Text;             use Gtk.Text;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Handlers;      use Gtkada.Handlers;

with Gint_Xml;             use Gint_Xml;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Editor;  use Glide_Kernel.Editor;
with Glide_Kernel.Console; use Glide_Kernel.Console;

with GNAT.Regpat;          use GNAT.Regpat;
with GNAT.OS_Lib;          use GNAT.OS_Lib;


package body Glide_Consoles is

   Highlight_File : constant String := "#FF0000000000";
   --  <preference>

   Highlight_Error : constant String := "#FF0000000000";
   --  <preference>

   function On_Button_Release
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handler for "button_press_event" signal

   function Load_Desktop
     (Node : Gint_Xml.Node_Ptr; User : Kernel_Handle)
      return Gtk_Widget;
   --  Save the status of the console to an XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Restore the status of the console from a saved XML tree.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Console : out Glide_Console;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Console := new Glide_Console_Record;
      Initialize (Console, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Console : access Glide_Console_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Gtk.Scrolled_Window.Initialize (Console);
      Console.Kernel := Kernel_Handle (Kernel);

      Set_Policy (Console, Policy_Never, Policy_Always);
      Set_Size_Request (Console, -1, 100);

      Gtk_New (Console.Text);
      Set_Editable (Console.Text, False);
      Add (Console, Console.Text);

      Return_Callback.Object_Connect
        (Console.Text, "button_release_event",
         Return_Callback.To_Marshaller (On_Button_Release'Access), Console);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Console        : access Glide_Console_Record;
      Text           : String;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True;
      Mode           : Glide_Kernel.Console.Message_Type := Info)
   is
      New_Text  : String_Access;
      Color     : Gdk_Color;

   begin
      if Mode = Error then
         Color := Parse (Highlight_Error);
      else
         Color := Null_Color;
      end if;

      if Add_LF then
         New_Text := new String' (Text & ASCII.LF);
      else
         New_Text := new String' (Text);
      end if;

      if Highlight_Sloc then
         declare
            Matched   : Match_Array (0 .. 3);
            File      : constant Pattern_Matcher :=
              Compile ("([^:]*):(\d+):(\d+:)?");
            Highlight : Gdk_Color;
            Last      : Natural;

         begin
            Match (File, New_Text.all, Matched);

            if Matched (0) /= No_Match then
               Highlight := Parse (Highlight_File);
               Alloc (Get_Default_Colormap, Highlight);

               Insert
                 (Console.Text,
                  Fore  => Color,
                  Chars => New_Text (New_Text'First .. Matched (1).First - 1));

               if Matched (3) = No_Match then
                  Last := Matched (2).Last;
               else
                  Last := Matched (3).Last - 1;
               end if;

               Insert
                 (Console.Text,
                  Fore => Highlight,
                  Chars => New_Text (Matched (1).First .. Last));
               Insert
                 (Console.Text,
                  Fore  => Color,
                  Chars => New_Text (Last + 1 .. New_Text'Last));

               Free (New_Text);
               return;
            end if;
         end;
      end if;

      Insert (Console.Text,
              Fore  => Color,
              Chars => New_Text.all);
      Free (New_Text);
   end Insert;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Console     : Glide_Console := Glide_Console (Widget);
      Position    : constant Gint := Get_Position (Console.Text);
      Contents    : constant String := Get_Chars (Console.Text, 0);
      Start       : Natural := Natural (Position);
      Last        : Natural := Start;
      Pattern     : constant Pattern_Matcher :=
        Compile ("^([^:]*):(\d+):(\d+:)?");
      Matched     : Match_Array (0 .. 3);
      Line        : Positive;
      Column      : Positive;
      Ignored     : Boolean;

   begin
      if Contents'Length = 0 then
         return False;
      end if;

      while Start > Contents'First
        and then Contents (Start - 1) /= ASCII.LF
      loop
         Start := Start - 1;
      end loop;

      while Last < Contents'Last and then Contents (Last + 1) /= ASCII.LF loop
         Last := Last + 1;
      end loop;

      Match (Pattern, Contents (Start .. Last), Matched);

      if Matched (0) /= No_Match then
         Line :=
           Positive'Value (Contents (Matched (2).First .. Matched (2).Last));

         if Matched (3) = No_Match then
            Column := 1;
         else
            Column := Positive'Value
                        (Contents (Matched (3).First .. Matched (3).Last - 1));
         end if;

         if Matched (1).First < Matched (1).Last then
            Go_To (Console.Kernel,
                   Contents (Matched (1).First .. Matched (1).Last),
                   Line, Column, Success => Ignored);
         end if;
      end if;

      return False;

   exception
      when Constraint_Error =>
         return False;
   end On_Button_Release;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Gint_Xml.Node_Ptr; User : Kernel_Handle)
      return Gtk_Widget
   is
      pragma Warnings (Off, Node);
      pragma Warnings (Off, User);
      Console : Glide_Console;
   begin
      if Node.Tag.all = "Console" then
         Gtk_New (Console, User);
         return Gtk_Widget (Console);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Widget.all in Glide_Console_Record'Class then
         N := new Node;
         N.Tag := new String' ("Console");
         return N;
      end if;

      return null;
   end Save_Desktop;


begin
   Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
     (Save_Desktop'Access, Load_Desktop'Access);
end Glide_Consoles;
