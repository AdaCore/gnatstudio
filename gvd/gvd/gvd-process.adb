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

with Glib;         use Glib;

with Gdk.Input;
with Gdk.Types;
with Gdk.Cursor;   use Gdk.Cursor;
with Gdk.Color;    use Gdk.Color;
with Gdk.Cursor;   use Gdk.Cursor;
with Gdk.Types;    use Gdk.Types;
with Gdk.Window;   use Gdk.Window;

with Gtk.Bin;      use Gtk.Bin;
with Gtk.Text;     use Gtk.Text;
with Gtk.Main;     use Gtk.Main;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Label;    use Gtk.Label;

with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Text_IO;     use Ada.Text_IO;
with Process_Tab_Pkg; use Process_Tab_Pkg;
with Gtkada.Canvas;   use Gtkada.Canvas;
with Odd.Pixmaps;     use Odd.Pixmaps;
with Display_Items;   use Display_Items;
with Debugger.Gdb;    use Debugger.Gdb;
with Debugger.Jdb;    use Debugger.Jdb;
with Odd.Strings;     use Odd.Strings;
with Process_Proxies; use Process_Proxies;
with Gtkada.Code_Editors; use Gtkada.Code_Editors;
with GNAT.Regpat;     use GNAT.Regpat;

with Main_Debug_Window_Pkg;  use Main_Debug_Window_Pkg;
with System;
with Unchecked_Conversion;

pragma Warnings (Off, Debugger.Jdb);

package body Odd.Process is

   package My_Input is new Gdk.Input.Input_Add (Debugger_Process_Tab_Record);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Gint is new Unchecked_Conversion (File_Descriptor, Gint);

   procedure Output_Available
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition);
   --  Called whenever some output becomes available from the debugger.
   --  All it does is read all the available data and call the filters
   --  that were set for the debugger.

   procedure Text_Output_Handler
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address);
   --  Standard handler to add gdb's input and output to the debugger
   --  window.

   -------------
   -- Convert --
   -------------

   function Convert
     (Main_Debug_Window : access Main_Debug_Window_Record'Class;
      Descriptor : GNAT.Expect.Process_Descriptor) return Debugger_Process_Tab
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
      Str     : String;
      Is_Command : Boolean := False)
   is
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      Start   : Positive := Str'First;

   begin
      Freeze (Process.Debugger_Text);
      Set_Point (Process.Debugger_Text, Get_Length (Process.Debugger_Text));

      --  Should all the string be highlighted ?

      if Is_Command then
         Insert (Process.Debugger_Text,
                 Process.Debugger_Text_Font,
                 Process.Debugger_Text_Highlight_Color,
                 Null_Color,
                 Str);

      --  If not, highlight only parts of it

      else
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
      end if;

      Thaw (Process.Debugger_Text);
   end Text_Output_Handler;

   procedure Text_Output_Handler
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address)
   is
      function To_Main_Debug_Window is new
        Unchecked_Conversion (System.Address, Main_Debug_Window_Access);
      Process     : constant Debugger_Process_Tab :=
        Convert (To_Main_Debug_Window (Window), Descriptor);

      File_First  : Natural;
      File_Last   : Positive;
      Line        : Natural;
      First, Last : Natural;
      Initial_Internal_Command : Boolean;

   begin
      Found_File_Name
        (Process.Debugger,
         Str, File_First, File_Last, First, Last, Line);

      --  Do not show the output if we have an internal command
      --  ??? Should output it anyway if in -fullname mode (external
      --  IDE - e.g Emacs - support).

      if Is_Internal_Command (Get_Process (Process.Debugger)) then
         Initial_Internal_Command := True;
      else
         Initial_Internal_Command := False;

         if First = 0 then
            Text_Output_Handler (Process, Str);
         else
            Text_Output_Handler (Process, Str (Str'First .. First - 1));
            Text_Output_Handler (Process, Str (Last + 1 .. Str'Last));
         end if;

         Process.Edit_Pos := Get_Length (Process.Debugger_Text);
         Set_Point (Process.Debugger_Text, Process.Edit_Pos);
      end if;

      --  Do we have a file name ?

      if File_First /= 0 then

         --  Get everything in the buffer (since the following command
         --  needs to interact with the debugger, and we want to hide its
         --  output).

         Wait_Prompt (Process.Debugger);

         --  Override the language currently defined in the editor.
         --  Since the text file has been given by the debugger, the language
         --  to use is the one currently defined by the debugger.
         Set_Current_Language
           (Process.Editor_Text, Get_Language (Process.Debugger));

         --  Display the file

         Set_Internal_Command (Get_Process (Process.Debugger), True);
         Load_File
           (Process.Editor_Text,
            Str (File_First .. File_Last),
            Process.Debugger);

         --  Restore the initial status of the process. We can not force it to
         --  False, since, at least with gdb, the "info line" command used in
         --  Load_File will also output a file reference, and thus we have a
         --  recursive call to Text_Output_Handler.
         Set_Internal_Command
           (Get_Process (Process.Debugger),
            Initial_Internal_Command);
      end if;

      if Line /= 0 then
         Set_Line (Process.Editor_Text, Line);
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
         Empty_Buffer
           (Get_Process (Debugger.Debugger),
            At_Least_One => True);
      end if;
   end Output_Available;

   ---------------------
   -- Create_Debugger --
   ---------------------

   function Create_Debugger
     (Window          : access
        Main_Debug_Window_Pkg.Main_Debug_Window_Record'Class;
      Kind            : Debugger_Type;
      Executable      : String;
      Params          : Argument_List;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "";
      Title           : String := "") return Debugger_Process_Tab
   is
      Process : Debugger_Process_Tab;
      Id      : Gint;
      Label   : Gtk_Label;

   begin
      Process := new Debugger_Process_Tab_Record;
      Process.Window := Window.all'Access;
      Initialize (Process);

      --  Allocate the colors for highlighting. This needs to be done before
      --  Initializing the debugger, since some file name might be output at
      --  that time.

      Process.Debugger_Text_Highlight_Color :=
        Parse (Debugger_Highlight_Color);

      Alloc (Get_System, Process.Debugger_Text_Highlight_Color);

      Process.Debugger_Text_Font :=
        Get_Gdkfont (Debugger_Font, Debugger_Font_Size);

      Align_On_Grid (Process.Data_Canvas, Align_Items_On_Grid);

      --  Spawn the debugger

      case Kind is
         when Gdb_Type =>
            Process.Debugger := new Gdb_Debugger;
         when Jdb_Type =>
            Process.Debugger := new Jdb_Debugger;
         when others =>
            raise Debugger_Not_Supported;
      end case;

      Spawn
        (Process.Debugger,
         Executable,
         Params,
         new Gui_Process_Proxy,
         Window.all'Access,
         Remote_Host,
         Remote_Target,
         Remote_Protocol,
         Debugger_Name);

      --  Add a new page to the notebook

      if Title = "" then
         if Params'Length > 0 then
            Gtk_New (Label, "Gdb - " & Params (Params'First).all);
         else
            Gtk_New (Label, "Gdb -" &
              Guint'Image (Page_List.Length (Get_Children
                (Window.Process_Notebook)) + 1));
         end if;
      else
         Gtk_New (Label, Title);
      end if;

      Append_Page (Window.Process_Notebook, Process.Process_Paned, Label);
      Show_All (Window.Process_Notebook);
      Set_Page (Window.Process_Notebook, -1);
      Process_User_Data.Set (Process.Process_Paned, Process.all'Access);

      --  Initialize the code editor.
      --  This should be done before initializing the debugger, in case the
      --  debugger outputs a file name that should be displayed in the editor.
      --  The language of the editor will automatically be set by the output
      --  filter.

      Configure (Process.Editor_Text, Editor_Font, Editor_Font_Size,
                 dot_xpm, arrow_xpm,
                 Comments_Color    => Comments_Color,
                 Strings_Color     => Strings_Color,
                 Keywords_Color    => Keywords_Color,
                 Show_Line_Numbers => Editor_Show_Line_Nums);

      --  Set the output filter, so that we output everything in the Gtk_Text
      --  window.

      Add_Output_Filter
        (Get_Descriptor (Get_Process (Process.Debugger)).all,
         Text_Output_Handler'Access, Window.all'Address);
      Id := My_Input.Add
        (To_Gint
         (Get_Output_Fd
          (Get_Descriptor (Get_Process (Process.Debugger)).all)),
         Gdk.Types.Input_Read,
         Output_Available'Access,
         My_Input.Data_Access (Process));

      --  Initialize the debugger, and possibly get the name of the initial
      --  file.
      Initialize (Process.Debugger);

      return Process;
   end Create_Debugger;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command (Debugger : Debugger_Process_Tab;
                                   Command  : String)
   is
      Item     : Display_Item;
      Command2 : String := To_Lower (Command);
      First    : Natural := Command2'First;
      Cursor   : Gdk_Cursor;
   begin

      Gdk_New (Cursor, Gdk.Types.Watch);
      Set_Cursor (Get_Window (Debugger.Window), Cursor);
      Destroy (Cursor);

      --  ??? Should forbid commands that modify the configuration of the
      --  debugger, like "set annotate" for gdb, otherwise we can't be sure
      --  what to expect from the debugger.

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
         Gtk_New (Item, Get_Window (Debugger.Data_Canvas),
                  Variable_Name => Command (First + 14 .. Command2'Last),
                  Debugger      => Debugger,
                  Auto_Refresh  => True);
         if Item /= null then
            Put (Debugger.Data_Canvas, Item);
         end if;
         Display_Prompt (Debugger.Debugger);

      elsif First + 10 <= Command2'Length
        and then Command2 (First .. First + 10) = "graph print"
      then
         Gtk_New (Item, Get_Window (Debugger.Data_Canvas),
                  Variable_Name => Command (First + 12 .. Command2'Last),
                  Debugger      => Debugger,
                  Auto_Refresh  => False);
         if Item /= null then
            Put (Debugger.Data_Canvas, Item);
         end if;
         Display_Prompt (Debugger.Debugger);

      --  ??? An internal debugging macro, to be deleted
      elsif First + 3 <= Command2'Length
        and then Command2 (First .. First + 3) = "play"
      then
         Send (Get_Process (Debugger.Debugger), "break exception");
         Wait_Prompt (Debugger.Debugger);
         Send (Get_Process (Debugger.Debugger), "run");
         Wait_Prompt (Debugger.Debugger);
         Process_User_Command (Debugger, "graph print Parse::A");
         Text_Output_Handler (Debugger, "" & ASCII.LF);
         Process_User_Command (Debugger, "graph print A");
         Text_Output_Handler (Debugger, "" & ASCII.LF);
         Process_User_Command (Debugger, "graph print Parse::S");
         Text_Output_Handler (Debugger, "" & ASCII.LF);
         Process_User_Command (Debugger, "graph print Parse::V");
         Text_Output_Handler (Debugger, "" & ASCII.LF);
         Process_User_Command (Debugger, "graph print Parse::Act");

      elsif Command2 = "quit" then
         Main_Quit;

      else
         --  Regular debugger command, send it.
         Send (Get_Process (Debugger.Debugger), Command);

         Wait_Prompt (Debugger.Debugger);
      end if;

      Set_Internal_Command (Get_Process (Debugger.Debugger), False);

      --  Put back the standard cursor

      Gdk_New (Cursor, Gdk.Types.Left_Ptr);
      Set_Cursor (Get_Window (Debugger.Window), Cursor);
      Destroy (Cursor);
   end Process_User_Command;

   ---------------------
   -- Input_Available --
   ---------------------

   procedure Input_Available
     (Debugger  : Standard_Input_Package.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition)
   is
      Tab       : Debugger_Process_Tab;
      Buffer    : String (1 .. 8192);
      Len       : Natural;

   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Debugger.Process_Notebook)));
      Get_Line (Buffer, Len);
      Process_User_Command (Tab, Buffer (1 .. Len));
   end Input_Available;

end Odd.Process;
