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
with Gdk.Color;    use Gdk.Color;
with Gdk.Font;     use Gdk.Font;
with Gtk.Text;     use Gtk.Text;
with Gtk.Main;     use Gtk.Main;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Label;    use Gtk.Label;
with Odd_Intl;            use Odd_Intl;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
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
with Debugger.Jdb;    use Debugger.Jdb;
with Odd.Strings;     use Odd.Strings;
with Process_Proxies; use Process_Proxies;

with Main_Debug_Window_Pkg;  use Main_Debug_Window_Pkg;

with Unchecked_Conversion;

package body Odd.Process is

   function To_Gint is new Unchecked_Conversion (File_Descriptor, Gint);

   package My_Input is new Gdk.Input.Input_Add (Debugger_Process_Tab_Record);

   procedure Text_Output_Handler
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String);
   --  Standard handler to add gdb's input and output to the debugger
   --  window.

   procedure Output_Available
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition);
   --  Called whenever some output becomes available from the debugger.
   --  All it does is read all the available data and call the filters
   --  that were set for the debugger.

   -------------
   -- Convert --
   -------------

   function Convert
     (Descriptor : GNAT.Expect.Process_Descriptor) return Debugger_Process_Tab
   is
      Page      : Gtk_Widget;
      Num_Pages : Gint :=
        Gint (Page_List.Length
          (Get_Children (Main_Debug_Window.Process_Notebook)));
      Process   : Debugger_Process_Tab;
   begin
      --  For all the process tabs in the application, check whether
      --  this is the one associated with Pid.

      for Page_Num in 0 .. Num_Pages - 1 loop
         Page := Get_Nth_Page (Main_Debug_Window.Process_Notebook, Page_Num);
         if Page /= null then
            Process := Process_User_Data.Get (Page);

            --  Note: The process might have been already killed when this
            --  function is called.

            if Get_Descriptor
              (Get_Process (Process.Debugger.all)).all = Descriptor
            then
               return Process;
            end if;
         end if;
      end loop;

      raise Debugger_Not_Found;
   end Convert;

   -------------------------
   -- Text_Output_Handler --
   -------------------------

   procedure Text_Output_Handler
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String)
   is
      Process    : Debugger_Process_Tab := Convert (Descriptor);
      Font       : Gdk_Font;
   begin
      Freeze (Process.Debugger_Text);
      Load (Font, "-adobe-helvetica-bold-*-*-*-*-140-*-*-*-*-*-*");
      Insert (Process.Debugger_Text,
              Font,
              Black (Get_System),
              White (Get_System),
              Str);
      Process.Edit_Pos := Get_Length (Process.Debugger_Text);
      Thaw (Process.Debugger_Text);
      Set_Position (Process.Debugger_Text, Gint (Process.Edit_Pos));
   end Text_Output_Handler;

   ----------------------
   -- Output_Available --
   ----------------------

   procedure Output_Available
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition)
   is
      Result : Expect_Match;
   begin
      --  Get everything that is available (and transparently call the
      --  output filters set for Pid). Since this function might be called
      --  when the process is killed on exit, we have to test whether it is
      --  still valid.

      Wait (Get_Process (Debugger.Debugger.all), Result, ".+", Timeout => 0);
   end Output_Available;

   ---------------------
   -- Create_Debugger --
   ---------------------

   function Create_Debugger
     (Params       : Argument_List;
      Process_Name : String := "")
     return Debugger_Process_Tab
   is
      Process  : Debugger_Process_Tab;
      Id       : Gint;
      Infile   : File_Type;
      Top      : Main_Debug_Window_Access renames Main_Debug_Window;
      Label    : Gtk_Label;

   begin
      Process := new Debugger_Process_Tab_Record;
      Initialize (Process);

      --  Spawn the debugger
      --  ??? This should be a parameter
      --  ??? Params should be passed on to the debugger

      Process.Debugger := new Gdb_Debugger;
      --  Process.Debugger := new Jdb_Debugger;

      Spawn (Process.Debugger.all, Params, new Gui_Process_Proxy, "");

      --  Add a new page to the notebook

      if Process_Name = "" then
         if Params'Length > 0 then
            Gtk_New (Label, "Gdb - " & Params (Params'First).all);
         else
            Gtk_New (Label, "Gdb -" &
              Guint'Image (Page_List.Length (Get_Children
                (Top.Process_Notebook)) + 1));
         end if;
      else
         Gtk_New (Label, Process_Name);
      end if;

      Append_Page (Top.Process_Notebook, Process.Process_Paned, Label);
      --  Set_Page (Top.Process_Notebook, Gint (Next_Tab));
      Show_All (Top.Process_Notebook);
      Set_Page (Top.Process_Notebook, -1);

      Process_User_Data.Set (Process.Process_Paned, Process.all'Access);

      Add_Output_Filter
        (Get_Descriptor (Get_Process (Process.Debugger.all)).all,
         Text_Output_Handler'Access);
--        Add_Input_Filter (Get_Process (Process.Debugger.all).all,
      --                          Text_Output_Handler'Access);
      Id := My_Input.Add
        (To_Gint (Get_Output_Fd
                  (Get_Descriptor (Get_Process (Process.Debugger.all)).all)),
         Gdk.Types.Input_Read,
         Output_Available'Access,
         My_Input.Data_Access (Process));

      Initialize (Process.Debugger);

      Open (Infile, In_File, "odd_main.adb");
      declare
         S      : String (1 .. 1024);
         Last   : Natural;
         Row    : Gint;
         Pixmap : Gdk.Gdk_Pixmap;
         Mask   : Gdk.Gdk_Bitmap;
         Style  : Gtk_Style := Get_Style (Process.Editor_Text);
         Texts  : constant Chars_Ptr_Array := (0 => Null_Ptr);
         Line   : Natural := 1;

      begin
         Realize (Process.Editor_Text);
         Create_From_Xpm_D
           (Pixmap, Get_Clist_Window (Process.Editor_Text),
            Mask, Get_White (Style), stop_xpm);

         while not End_Of_File (Infile) loop
            Get_Line (File => Infile, Item => S, Last => Last);
            Row := Append (Process.Editor_Text, Texts);

            if Line_Contains_Code
              (Process.Debugger.all, "odd_main.adb", Line)
            then
               Set_Pixtext
                 (Process.Editor_Text, Row, 0, S (1 .. Last), 5, Pixmap, Mask);
            else
               Set_Text
                 (Process.Editor_Text, Row, 0, S (1 .. Last));
            end if;

            Line := Line + 1;
         end loop;
      end;
      Close (Infile);

      return Process;
   end Create_Debugger;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command (Debugger : Debugger_Process_Tab;
                                   Command  : String)
   is
      The_Type : Generic_Type_Access;
      Item     : Display_Item;
      Command2 : String := To_Lower (Command);
      First    : Natural := Command2'First;

   begin
      --  Command has been converted to lower-cases, but the new version
      --  should be used only to compare with our standard list of commands.
      --  We should pass the original string to the debugger, in case we are
      --  in a case-sensitive language.

      --  Ignore the blanks at the beginning of lines

      Skip_Blanks (Command2, First);

      if First + 12 <= Command2'Last
        and then Command2 (First .. First + 12) = "graph display"
      then
         declare
            Var : String := Command (First + 14 .. Command2'Last);
         begin
            The_Type := Parse_Type (Debugger.Debugger.all, Var);

            if The_Type /= null then
               Parse_Value (Debugger.Debugger.all, Var, The_Type);

               Gtk_New (Item, Get_Window (Debugger.Data_Canvas),
                        Var, The_Type, Auto_Refresh => True);
               Put (Debugger.Data_Canvas, Item);
            end if;
         end;

      elsif First + 10 <= Command2'Length
        and then Command2 (First .. First + 10) = "graph print"
      then
         declare
            Var : String := Command (First + 12 .. Command2'Last);
         begin
            The_Type := Parse_Type (Debugger.Debugger.all, Var);

            if The_Type /= null then
               Parse_Value (Debugger.Debugger.all, Var, The_Type);

               Gtk_New (Item, Get_Window (Debugger.Data_Canvas),
                        Var, The_Type, Auto_Refresh => False);
               Put (Debugger.Data_Canvas, Item);
            end if;
         end;

      elsif Command2 = "quit" then
         Main_Quit;

      else
         --  Regular debugger command, send it.
         Send (Get_Process (Debugger.Debugger.all), Command);
      end if;
   end Process_User_Command;

end Odd.Process;
