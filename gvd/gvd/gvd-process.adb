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

with Glib; use Glib;
with Gdk.Input;
with Gdk.Types;
with Gdk.Color; use Gdk.Color;
with Gdk.Font;  use Gdk.Font;
with Gtk.Text;  use Gtk.Text;
with Gtk.Main;  use Gtk.Main;

with Ada.Text_IO;     use Ada.Text_IO;
with Process_Tab_Pkg; use Process_Tab_Pkg;
with Gdk.Pixmap;      use Gdk.Pixmap;
with Gtk.Style;       use Gtk.Style;
with Gtk.Clist;       use Gtk.Clist;
with Gtkada.Canvas;   use Gtkada.Canvas;
with Gtkada.Types;    use Gtkada.Types;
with Odd.Pixmaps;     use Odd.Pixmaps;
with Display_Items;   use Display_Items;
with Generic_Values;  use Generic_Values;
with Debugger.Gdb;    use Debugger.Gdb;

with Unchecked_Conversion;

package body Odd.Process is

   TTY_Emulation    : Boolean := False;

   function To_Gint is new Unchecked_Conversion (File_Descriptor, Gint);

   ---------------------------
   --  Internal procedures  --
   ---------------------------

   package My_Input is new Gdk.Input.Input_Add (Debugger_Descriptor);

   procedure Output_Handler
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition);
   --  Handler called when the debugger sends some output.

   --------------------
   -- Output_Handler --
   --------------------

   procedure Output_Handler
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition)
   is
      Font   : Gdk_Font;
      Win    : Process_Tab_Access :=
        Process_Tab_Access (Debugger.Window);
      Result : Expect_Match;
      Pid    : Pipes_Id_Access := Get_Process (Debugger.Debugger.all);

   begin
      Expect (Pid.all, Result, "\(gdb\) ", 0);

      --  Need to Parse (Expect_Out)

      if TTY_Emulation then
         Put (Expect_Out (Get_Process (Debugger.Debugger.all).all));
      else
         Freeze (Win.Debugger_Text);
         Load (Font, "-adobe-helvetica-bold-*-*-*-*-140-*-*-*-*-*-*");
         Insert
           (Win.Debugger_Text,
            Font,
            Black (Get_System),
            White (Get_System),
            Expect_Out (Get_Process (Debugger.Debugger.all).all));
         Win.Edit_Pos := Get_Length (Win.Debugger_Text);
         Thaw (Win.Debugger_Text);
         Set_Position (Win.Debugger_Text, Gint (Win.Edit_Pos));
      end if;
   end Output_Handler;

   ---------------------
   -- Create_Debugger --
   ---------------------

   procedure Create_Debugger
     (Window : access Gtk_Window_Record'Class;
      Params : Argument_List)
   is
      Id     : Gint;
      Win    : Process_Tab_Access := Process_Tab_Access (Window);
      Infile : File_Type;

   begin
      Win.Debugger.Window := Window.all'Access;

      --  ??? This should be a parameter

      Win.Debugger.Debugger := new Gdb_Debugger;
      Initialize (Win.Debugger.Debugger);

      Open (Infile, In_File, "odd_main.adb");
      declare
         S      : String (1 .. 1024);
         Last   : Natural;
         Row    : Gint;
         Pixmap : Gdk.Gdk_Pixmap;
         Mask   : Gdk.Gdk_Bitmap;
         Style  : Gtk_Style := Get_Style (Win.Editor_Text);
         Texts  : constant Chars_Ptr_Array := (0 => Null_Ptr);

      begin
         Realize (Win.Editor_Text);
         Create_From_Xpm_D
           (Pixmap, Get_Clist_Window (Win.Editor_Text),
            Mask, Get_White (Style), stop_xpm);

         while not End_Of_File (Infile) loop
            Get_Line (File => Infile, Item => S, Last => Last);
            Row := Append (Win.Editor_Text, Texts);
            Set_Pixtext
              (Win.Editor_Text, Row, 0, S (1 .. Last), 5, Pixmap, Mask);
         end loop;
      end;
      Close (Infile);

      Id := My_Input.Add
        (To_Gint (Get_Output_Fd (Get_Process (Win.Debugger.Debugger.all).all)),
         Gdk.Types.Input_Read,
         Output_Handler'Access,
         Win.Debugger'Access);
   end Create_Debugger;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command (Debugger : Debugger_Descriptor; Command : String) is
      Win    : Process_Tab_Access :=
        Process_Tab_Access (Debugger.Window);
      The_Type : Generic_Type_Access;
      --  Item   : Display_Item;

   begin
      if Command'Length > 6
        and then Command (Command'First .. Command'First + 5) = "graph "
      then
         --  Handle "graph" commands (print, display)

         if Command'Length > 11
           and then Command (Command'First + 6 .. Command'First + 10) = "print"
         then
            declare
               Var : String := Command (Command'First + 12 .. Command'Last);
            begin
               The_Type := Parse_Type (Debugger.Debugger.all, Var);

               if The_Type /= null then
                  Parse_Value (Debugger.Debugger.all, Var, The_Type);
                  --  Gtk_New (Item, Get_Window (Win.Data_Canvas), Var,
                  --           ???);
                  --  Put (Win.Data_Canvas, Item, 10, 10);
               end if;
            end;
         else
            Send (Get_Process (Debugger.Debugger.all).all,
              Command (Command'First + 6 .. Command'Last));
         end if;

      else
         if Command = "quit" then
            Main_Quit;
         end if;

         --  Regular debugger command, send it.

         Send (Get_Process (Debugger.Debugger.all).all, Command);
      end if;
   end Send_Command;

end Odd.Process;
