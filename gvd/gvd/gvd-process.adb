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
with Ada.Text_IO; use Ada.Text_IO;
with Process_Tab_Pkg; use Process_Tab_Pkg;
with Gtkada.Canvas; use Gtkada.Canvas;
with Display_Items; use Display_Items;

package body Odd_Tools.Process is

   --  ??? Eventually, these constants will be moved in a Parameters package.

   Default_Debugger : aliased String := "gdb";
   TTY_Emulation    : Boolean := False;

   ---------------------------
   --  Internal procedures  --
   ---------------------------

   package My_Input is new Gdk.Input.Input_Add (Debugger_Descriptor);

   procedure Output_Handler
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition);
   --  Handler called when the debugger sends some output.

   -------------------------------
   --  Target dependent section --
   -------------------------------

   function Dup (Old_Fd : File_Descriptor) return File_Descriptor;
   pragma Import (C, Dup);

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor);
   pragma Import (C, Dup2);

   type Pipe_Type is record
      Input, Output : File_Descriptor;
   end record;

   procedure Create_Pipe (Pipe : access Pipe_Type);
   pragma Import (C, Create_Pipe, "pipe");

   function Fork return Integer;
   pragma Import (C, Fork);

   procedure Execvp (File : String; Args : System.Address);
   pragma Import (C, Execvp);

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Input        : File_Descriptor;
      Output       : File_Descriptor;
      Error        : File_Descriptor) return Integer;
   --  Similar to GNAT.OS_Lib.Non_Blocking_Spawn, but also redirect the
   --  standard input, output and error of the spawned process.

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Input        : File_Descriptor;
      Output       : File_Descriptor;
      Error        : File_Descriptor) return Integer
   is
      Pid      : Integer;
      Arg_List : array (1 .. Args'Length + 2) of System.Address;
      Arg      : String_Access;

   begin
      Arg := new String (1 .. Program_Name'Length + 1);
      Arg (1 .. Program_Name'Length) := Program_Name;
      Arg (Arg'Last)                 := Ascii.NUL;
      Arg_List (1)                   := Arg.all'Address;

      for J in 1 .. Args'Length loop
         Arg := new String (1 .. Args (J + Args'First - 1)'Length + 1);
         Arg (1 .. Arg'Last - 1) := Args (J + Args'First - 1).all;
         Arg (Arg'Last) := Ascii.NUL;
         Arg_List (J + 1) := Arg.all'Address;
      end loop;

      Arg_List (Arg_List'Last) := System.Null_Address;
      Pid := Fork;

      if Pid = 0 then
         Dup2 (Input, 0);
         Dup2 (Output, 1);
         Dup2 (Error, 2);
         Close (Input);
         Close (Output);

         if Error /= Output then
            Close (Error);
         end if;

         Execvp (Program_Name, Arg_List'Address);
      end if;

      return Pid;
   end Non_Blocking_Spawn;

   procedure Output_Handler
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition)
   is
      Buffer : aliased String (1 .. 4096);
      N      : Integer;
      Font   : Gdk_Font;
      Win    : Process_Tab_Access :=
        Process_Tab_Access (Debugger.Window);

   begin
      loop
         N := Read (File_Descriptor (Source), Buffer'Address, Buffer'Length);

         --  Need to Parse (Buffer (1 .. N));

         if TTY_Emulation then
            Put (Buffer (1 .. N));
         else
            Freeze (Win.Debugger_Text);
            Load (Font,
                  "-adobe-courier-medium-i-*-*-15-*-*-*-*-*-*-*");
            Insert
              (Win.Debugger_Text,
               Font,
               Black (Get_System),
               White (Get_System),
               Buffer (1 .. N));
            Win.Edit_Pos := Get_Length (Win.Debugger_Text);
            Thaw (Win.Debugger_Text);
            Set_Position (Win.Debugger_Text, Gint (Win.Edit_Pos));
         end if;

         exit when N < Buffer'Length;
      end loop;
   end Output_Handler;

   ---------------------
   -- Create_Debugger --
   ---------------------

   procedure Create_Debugger
     (Window : access Gtk_Window_Record'Class;
      Params : Argument_List)
   is
      Pipe1    : aliased Pipe_Type;
      Pipe2    : aliased Pipe_Type;
      Id       : Gint;
      Win      : Process_Tab_Access := Process_Tab_Access (Window);

   begin
      --  Redirect the debugger input on one pipe, and the output on another
      --  pipe.

      Create_Pipe (Pipe1'Unchecked_Access);
      Create_Pipe (Pipe2'Unchecked_Access);
      Win.Debugger.Input_Channel := Pipe1.Output;
      Win.Debugger.Output_Channel := Pipe2.Input;
      Win.Debugger.Name := Default_Debugger'Access;
      Win.Debugger.Window := Window.all'Access;

      --  Install a handler to receive the debugger's output.

      Id := My_Input.Add
        (Gint (Win.Debugger.Output_Channel),
         Gdk.Types.Input_Read,
         Output_Handler'Access,
         Win.Debugger'Access);
      Win.Debugger.Process :=
        Non_Blocking_Spawn
          (Win.Debugger.Name.all,
           Params,
           Pipe1.Input,
           Pipe2.Output,
           Pipe2.Output);
      Close (Pipe1.Input);
      Close (Pipe2.Output);
   end Create_Debugger;

   ------------------
   -- Send_Command --
   ------------------

   EOL : constant String := (1 => ASCII.LF);

   procedure Send_Command (Debugger : Debugger_Descriptor; Command : String) is
      N      : Integer;
      Buffer : aliased String (1 .. 4096);
      Win    : Process_Tab_Access :=
        Process_Tab_Access (Debugger.Window);
      Item   : Display_Item;
   begin
      if Command'Length > 6
        and then Command (Command'First .. Command'First + 5) = "graph "
      then
         --  Handle "graph" commands (print, display)

         N := Write (Debugger.Input_Channel,
                     Command (Command'First + 6)'Address,
                     Command'Length - 6);
         N := Write (Debugger.Input_Channel, EOL'Address, EOL'Length);

         if Command'Length > 11
           and then Command (Command'First + 6 .. Command'First + 10) = "print"
         then
            loop
               N := Read (Debugger.Output_Channel,
                          Buffer'Address, Buffer'Length);
               Gtk_New (Item, Get_Window (Win.Data_Canvas),
                        Command (Command'First + 12 .. Command 'Last),
                        Buffer (1 .. N));
               Put (Win.Data_Canvas, Item, 10, 10);
               exit when N < Buffer'Length;
            end loop;
         end if;

      else
         if Command = "quit" then
            Main_Quit;
         end if;

         --  Regular debugger command, send it.
         N := Write (Debugger.Input_Channel, Command'Address, Command'Length);
         N := Write (Debugger.Input_Channel, EOL'Address, EOL'Length);
      end if;
   end Send_Command;

end Odd_Tools.Process;
