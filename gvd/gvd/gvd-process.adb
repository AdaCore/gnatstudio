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
with Gtk.Text;     use Gtk.Text;
with Gtk.Main;     use Gtk.Main;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Label;    use Gtk.Label;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Text_IO;     use Ada.Text_IO;
with Process_Tab_Pkg; use Process_Tab_Pkg;
with Gtkada.Canvas;   use Gtkada.Canvas;
with Odd.Pixmaps;     use Odd.Pixmaps;
with Display_Items;   use Display_Items;
with Generic_Values;  use Generic_Values;
with Debugger.Gdb;    use Debugger.Gdb;
with Debugger.Jdb;    use Debugger.Jdb;
with Odd.Strings;     use Odd.Strings;
with Process_Proxies; use Process_Proxies;
with Gtkada.Code_Editors; use Gtkada.Code_Editors;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with GNAT.Regpat;     use GNAT.Regpat;

with Main_Debug_Window_Pkg;  use Main_Debug_Window_Pkg;

with Unchecked_Conversion;

pragma Warnings (Off, Debugger.Jdb);

package body Odd.Process is

   ---------------
   -- Constants --
   ---------------

   Editor_Font_Size : constant Gint := 10;
   --  Size of the font used in the editor.

   Editor_Font : constant String := "Courier";
   --  Font used in the editor.

   Comments_Color : constant String := "red";
   --  Color used for comments.

   Strings_Color  : constant String := "brown";
   --  Color used for strings.

   Keywords_Color : constant String := "blue";
   --  Color used for keywords.

   Debugger_Highlight_Color : constant String := "blue";
   --  Color used for highlighting in the debugger window.

   Debugger_Font_Size : constant Gint := 10;
   --  Size of the font used in the debugger text window.

   Debugger_Font : constant String := "Courier";
   --  Font used in the debugger text window.



   function To_Gint is new Unchecked_Conversion (File_Descriptor, Gint);

   package My_Input is new Gdk.Input.Input_Add (Debugger_Process_Tab_Record);

   procedure Output_Available
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition);
   --  Called whenever some output becomes available from the debugger.
   --  All it does is read all the available data and call the filters
   --  that were set for the debugger.

   procedure Text_Output_Handler
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String);
   --  Standard handler to add gdb's input and output to the debugger
   --  window.

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
              (Get_Process (Process.Debugger)).all = Descriptor
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
     (Process : Debugger_Process_Tab;
      Str     : String)
   is
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      Start   : Positive := Str'First;
   begin
      Freeze (Process.Debugger_Text);

      while Start <= Str'Last loop
         Match (Highlighting_Pattern (Process.Debugger),
                Str (Start .. Str'Last),
                Matched);
         if Matched (0) /= No_Match then
            if Matched (0).First - 1 >= Start then
               Insert (Process.Debugger_Text,
                       Process.Debugger_Text_Font,
                       Black (Get_System),
                       Null_Color,
                       Str (Start .. Matched (0).First - 1));
            end if;

            Insert (Process.Debugger_Text,
                    Process.Debugger_Text_Font,
                    Process.Debugger_Text_Highlight_Color,
                    Null_Color,
                    Str (Matched (0).First .. Matched (0).Last));
            Start := Matched (0).Last + 1;
         else
            Insert (Process.Debugger_Text,
                    Process.Debugger_Text_Font,
                    Black (Get_System),
                    Null_Color,
                    Str (Start .. Str'Last));
            Start := Str'Last + 1;
         end if;
      end loop;

      Thaw (Process.Debugger_Text);
   end Text_Output_Handler;

   -------------------------
   -- Text_Output_Handler --
   -------------------------

   procedure Text_Output_Handler
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String)
   is
      Process : Debugger_Process_Tab := Convert (Descriptor);
      Matched : Match_Array (0 .. 2);
   begin
      --  Do not show the output if we have an internal command
      if not Is_Internal_Command (Get_Process (Process.Debugger)) then
         Text_Output_Handler (Process, Str);
         Process.Edit_Pos := Get_Length (Process.Debugger_Text);
         Set_Point (Process.Debugger_Text, Process.Edit_Pos);
      end if;

      --  ??? This is specific to gdb...

      Match (Compile (ASCII.SUB & ASCII.SUB
                      & "([^:]+):(\d+):\d+:beg:", Multiple_Lines),
             Str, Matched);

      --  End of gdb specific section

      Put_Line (Str);
      if Matched (0) /= No_Match then
         --  Get everything in the buffer (since the following command
         --  needs to interact with the debugger, and we want to hide its
         --  output).

         --  Empty_Buffer (Get_Process (Process.Debugger));
         Wait_Prompt (Process.Debugger);

         --  Display the file

         Set_Internal_Command (Get_Process (Process.Debugger), True);
         Load_File (Process.Editor_Text,
                    Str (Matched (1).First .. Matched (1).Last),
                    Process.Debugger);

         --  ??? Should also display the correct line

         Set_Internal_Command (Get_Process (Process.Debugger), False);
      end if;
   end Text_Output_Handler;

   ----------------------
   -- Output_Available --
   ----------------------

   procedure Output_Available
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition)
   is
   begin
      --  Get everything that is available (and transparently call the
      --  output filters set for Pid).
      --  Nothing should be done if we are already processing a command
      --  (ie somewhere we are blocked on a Wait call for this Debugger),
      --  since otherwise that Wait won't see the output and will lose some
      --  output. We don't have to do that anyway, since the other Wait will
      --  indirectly call the output filter.

      if not Command_In_Process (Get_Process (Debugger.Debugger)) then
         Empty_Buffer (Get_Process (Debugger.Debugger),
                       At_Least_One => True);
      end if;
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
      Top      : Main_Debug_Window_Access renames Main_Debug_Window;
      Label    : Gtk_Label;

   begin
      Process := new Debugger_Process_Tab_Record;
      Initialize (Process);

      --  Spawn the debugger
      --  ??? This should be a parameter

      Process.Debugger := new Gdb_Debugger;
      --  Process.Debugger := new Jdb_Debugger;

      Spawn (Process.Debugger, Params, new Gui_Process_Proxy, "");

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
      Show_All (Top.Process_Notebook);
      Set_Page (Top.Process_Notebook, -1);

      Process_User_Data.Set (Process.Process_Paned, Process.all'Access);

      --  Allocate the colors for highlighting. This needs to be done before
      --  Initializing the debugger, since some output will be output at that
      --  time.

      Process.Debugger_Text_Highlight_Color :=
        Parse (Debugger_Highlight_Color);
      Alloc (Get_System, Process.Debugger_Text_Highlight_Color);

      Process.Debugger_Text_Font :=
        Get_Gdkfont (Debugger_Font, Debugger_Font_Size);

      --  Set the output filter, so that we output everything in the Gtk_Text
      --  window.

      Add_Output_Filter
        (Get_Descriptor (Get_Process (Process.Debugger)).all,
         Text_Output_Handler'Access);
      Id := My_Input.Add
        (To_Gint
         (Get_Output_Fd
          (Get_Descriptor (Get_Process (Process.Debugger)).all)),
         Gdk.Types.Input_Read,
         Output_Available'Access,
         My_Input.Data_Access (Process));

      --  Initialize the debugger.

      Initialize (Process.Debugger);

      Configure (Process.Editor_Text, Editor_Font, Editor_Font_Size, stop_xpm,
                 Comments_Color => Comments_Color,
                 Strings_Color  => Strings_Color,
                 Keywords_Color => Keywords_Color);
      Set_Current_Language (Process.Editor_Text,
                            Get_Language (Process.Debugger));
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

      Set_Internal_Command (Get_Process (Debugger.Debugger), False);

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
            Set_Internal_Command (Get_Process (Debugger.Debugger), True);
            The_Type := Parse_Type (Debugger.Debugger, Var);

            if The_Type /= null then
               Parse_Value (Debugger.Debugger, Var, The_Type);

               Gtk_New (Item, Get_Window (Debugger.Data_Canvas),
                        Var, The_Type, Auto_Refresh => True);
               Put (Debugger.Data_Canvas, Item);
            end if;
            Set_Internal_Command (Get_Process (Debugger.Debugger), False);
            --  ??? Problem: this hides the end prompt...
         end;

      elsif First + 10 <= Command2'Length
        and then Command2 (First .. First + 10) = "graph print"
      then
         declare
            Var : String := Command (First + 12 .. Command2'Last);
         begin
            Set_Internal_Command (Get_Process (Debugger.Debugger), True);
            The_Type := Parse_Type (Debugger.Debugger, Var);

            if The_Type /= null then
               Parse_Value (Debugger.Debugger, Var, The_Type);

               Gtk_New (Item, Get_Window (Debugger.Data_Canvas),
                        Var, The_Type, Auto_Refresh => False);
               Put (Debugger.Data_Canvas, Item);
            end if;
            Set_Internal_Command (Get_Process (Debugger.Debugger), False);
            --  ??? Problem: this hides the end prompt...
         end;

      elsif Command2 = "quit" then
         Main_Quit;

      else
         --  Regular debugger command, send it.
         Send (Get_Process (Debugger.Debugger), Command);

         Wait_Prompt (Debugger.Debugger);
      end if;

      Set_Internal_Command (Get_Process (Debugger.Debugger), True);
   end Process_User_Command;

end Odd.Process;
