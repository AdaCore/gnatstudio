-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
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

with Basic_Types;       use Basic_Types;
with Glib;              use Glib;
with Gdk.Event;         use Gdk.Event;
with Gdk.Types;         use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Widget;        use Gtk.Widget;
with Gtkada.Handlers;   use Gtkada.Handlers;

package body Gtkada.Entry_Completion is

   procedure On_Destroy (The_Entry : access Gtk_Widget_Record'Class);
   --  Callback when the widget is destroyed.

   function On_Entry_Tab
     (The_Entry : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event) return Boolean;
   --  Handles the completion key in the entry.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (The_Entry : out Gtkada_Entry) is
   begin
      The_Entry := new Gtkada_Entry_Record;
      Gtkada.Entry_Completion.Initialize (The_Entry);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (The_Entry : access Gtkada_Entry_Record'Class) is
   begin
      Gtk.GEntry.Initialize (The_Entry);

      Widget_Callback.Connect
        (The_Entry, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));
      Return_Callback.Connect
        (The_Entry, "key_press_event",
         Return_Callback.To_Marshaller (On_Entry_Tab'Access));
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (The_Entry : access Gtk_Widget_Record'Class) is
   begin
      Free (Gtkada_Entry (The_Entry).Completions);
   end On_Destroy;

   ---------------------
   -- Set_Completions --
   ---------------------

   procedure Set_Completions
     (The_Entry   : access Gtkada_Entry_Record;
      Completions : Basic_Types.String_Array_Access) is
   begin
      Free (The_Entry.Completions);
      The_Entry.Completions := Completions;
      The_Entry.Completion_Index := Completions'First - 1;
   end Set_Completions;

   ------------------
   -- On_Entry_Tab --
   ------------------

   function On_Entry_Tab
     (The_Entry : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event) return Boolean
   is
      GEntry : Gtkada_Entry := Gtkada_Entry (The_Entry);

      function Next_Matching
        (T : String; Start_At, End_At : Integer) return Integer;
      --  Return the integer of the first possible completion for T, after
      --  index Start_At, and found before End_At.
      --  Integer'First is returned if no completion was found.

      -------------------
      -- Next_Matching --
      -------------------

      function Next_Matching
        (T : String; Start_At, End_At : Integer) return Integer is
      begin
         for S in Start_At .. End_At loop
            if GEntry.Completions (S)'Length >= T'Length
              and then GEntry.Completions (S)
              (GEntry.Completions (S)'First
                 .. GEntry.Completions (S)'First + T'Length - 1) = T
            then
               return S;
            end if;
         end loop;

         return Integer'First;
      end Next_Matching;

   begin
      if (Get_Key_Val (Event) = GDK_Tab
            or else Get_Key_Val (Event) = GDK_KP_Tab)
        and then GEntry.Completions /= null
      then
         declare
            T                     : constant String := Get_Text (GEntry);
            Completion, Tmp       : String_Access;
            Index, S, First_Index : Integer;

         begin
            if GEntry.Completion_Index = Integer'First then
               GEntry.Last_Position := Integer (Get_Position (GEntry));
               First_Index := Next_Matching
                 (T, GEntry.Completions'First, GEntry.Completions'Last);

               if First_Index /= Integer'First then
                  S := First_Index;
                  Completion := new String'(GEntry.Completions (S)
                    (GEntry.Completions (S)'First + T'Length
                     .. GEntry.Completions (S)'Last));

                  loop
                     S := Next_Matching (T, S + 1, GEntry.Completions'Last);
                     exit when S = Integer'First;

                     Index := Completion'First;
                     while Index <= Completion'Last
                       and then Completion (Index) =
                       GEntry.Completions (S)(Index - Completion'First
                          + GEntry.Completions (S)'First + T'Length)
                     loop
                        Index := Index + 1;
                     end loop;

                     Tmp := new String'
                       (Completion (Completion'First .. Index - 1));
                     Free (Completion);
                     Completion := Tmp;
                  end loop;

                  if Completion'Length /= 0 then
                     GEntry.Completion_Index := Integer'First;
                     Append_Text (GEntry, Completion.all);
                     Set_Position (GEntry, -1);

                  else
                     GEntry.Completion_Index := GEntry.Completions'First - 1;
                     Free (Completion);
                  end if;
                  return True;
               end if;

            else
               First_Index := Next_Matching
                 (T (T'First .. GEntry.Last_Position),
                  GEntry.Completion_Index + 1, GEntry.Completions'Last);

               if First_Index = Integer'First then
                  First_Index := GEntry.Completions'First - 1;
                  Delete_Text (GEntry, Gint (GEntry.Last_Position), -1);
               end if;
            end if;

            GEntry.Completion_Index := First_Index;

            if First_Index >= GEntry.Completions'First then
               Set_Text (GEntry, GEntry.Completions (First_Index).all);
               Set_Position (GEntry, -1);
            end if;
         end;

         return True;
      end if;

      GEntry.Completion_Index := Integer'First;
      return False;

   exception
      when others =>
         GEntry.Completion_Index := Integer'First;
         return False;
   end On_Entry_Tab;

end Gtkada.Entry_Completion;
