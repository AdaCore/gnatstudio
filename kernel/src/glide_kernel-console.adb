-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with GNAT.Regpat;       use GNAT.Regpat;
with Glide_Main_Window; use Glide_Main_Window;
with Glide_Page;        use Glide_Page;
with Gdk.Color;         use Gdk.Color;
with Gtk.Text;          use Gtk.Text;
with Gtk.Widget;        use Gtk.Widget;
with GVD.Process;       use GVD.Process;

package body Glide_Kernel.Console is

   --  ??? Preferences

   Highlight_File : constant String := "#FF0000000000";

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Kernel         : access Kernel_Handle_Record'Class;
      Text           : String;
      Highlight_Sloc : Boolean := True)
   is
      Top       : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      Console   : constant Gtk_Text :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Console;

   begin
      if Highlight_Sloc then
         declare
            Matched   : Match_Array (0 .. 3);
            File      : constant Pattern_Matcher :=
              Compile ("([^:]*):(\d+):(\d+:)?");
            Highlight : Gdk_Color;
            Last      : Natural;

         begin
            Match (File, Text, Matched);

            if Matched (0) /= No_Match then
               Highlight := Parse (Highlight_File);
               Alloc (Get_Default_Colormap, Highlight);

               Insert
                 (Console,
                  Chars => Text (Text'First .. Matched (1).First - 1));

               if Matched (3) = No_Match then
                  Last := Matched (2).Last;
               else
                  Last := Matched (3).Last - 1;
               end if;

               Insert
                 (Console,
                  Fore => Highlight,
                  Chars => Text (Matched (1).First .. Last));
               Insert (Console, Chars => Text (Last + 1 .. Text'Last));

               return;
            end if;
         end;
      end if;

      Insert (Console, Chars => Text);
   end Insert;

end Glide_Kernel.Console;
