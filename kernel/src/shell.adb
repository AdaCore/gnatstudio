-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Glib.Object;          use Glib.Object;
with String_List_Utils;    use String_List_Utils;
with Generic_List;
with Interactive_Consoles; use Interactive_Consoles;
with Glide_Intl;           use Glide_Intl;
with Traces;               use Traces;
with Histories;            use Histories;
with Gtkada.MDI;           use Gtkada.MDI;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Handlers;      use Gtkada.Handlers;

package body Shell is

   Me : constant Debug_Handle := Create ("Shell");

   type Command_Information is record
      Command         : GNAT.OS_Lib.String_Access;
      Usage           : GNAT.OS_Lib.String_Access;
      Description     : GNAT.OS_Lib.String_Access;
      Minimum_Args    : Natural;
      Maximum_Args    : Natural;
      Command_Handler : Module_Command_Function;
   end record;

   procedure Free (X : in out Command_Information);
   --  Free memory associated with X.

   package Command_List is new Generic_List (Command_Information);

   type Shell_Module_Id_Record is new Module_ID_Record with record
      Commands_List : Command_List.List;
      --  The list of all registered commands
   end record;
   type Shell_Module_Id_Access is access all Shell_Module_Id_Record;

   procedure Destroy (Module : in out Shell_Module_Id_Record);
   --  Free the memory associated with the module

   Shell_Module_Id : Shell_Module_Id_Access;

   function Interpret_Command_Handler
     (Input  : String;
      Kernel : access GObject_Record'Class) return String;
   --  Launch the command interpreter for Input and return the output.

   function Console_Delete_Event
     (Console : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Prevent the destruction of the console in the MDI

   function Module_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Command handler for the module commands.

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Input  : String;
      Kernel : access GObject_Record'Class) return String
   is
      S : constant String := Interpret_Command (Kernel_Handle (Kernel), Input);
   begin
      if S = ""
        or else S (S'Last) = ASCII.LF
        or else S (S'Last) = ASCII.CR
      then
         return S;
      else
         return S & ASCII.LF;
      end if;
   end Interpret_Command_Handler;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Shell_Module_Id_Record) is
   begin
      Command_List.Free (Module.Commands_List);
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Command_Information) is
   begin
      Free (X.Command);
      Free (X.Usage);
      Free (X.Description);
   end Free;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Interactive : Interactive_Console;
      Child : MDI_Child;
   begin
      Shell_Module_Id := new Shell_Module_Id_Record;
      Register_Module
        (Module                  => Module_ID (Shell_Module_Id),
         Kernel                  => Kernel,
         Module_Name             => "Shell",
         Priority                => Default_Priority);

      Gtk_New (Interactive,
               "GPS> ",
               Interpret_Command_Handler'Access,
               GObject (Kernel),
               Get_Pref (Kernel, Source_Editor_Font),
               History_List => Get_History (Kernel),
               Key          => "shell",
               Wrap_Mode    => Wrap_Char);
      Set_Completion_Handler (Interactive, Commands_As_List'Access);
      Child := Put
        (Get_MDI (Kernel), Interactive,
         Iconify_Button or Maximize_Button,
         Focus_Widget => Gtk_Widget (Get_View (Interactive)),
         Default_Width => 400,
         Default_Height => 100);
      Set_Title (Child, -"Shell");
      Set_Dock_Side (Child, Bottom);
      Dock_Child (Child);

      --  Only remember the last 100 commands.
      Set_Max_Length (Get_History (Kernel).all, 100, "shell");
      Allow_Duplicates (Get_History (Kernel).all, "shell", True, True);

      Return_Callback.Connect
        (Interactive, "delete_event",
         Return_Callback.To_Marshaller (Console_Delete_Event'Access));

      Register_Command
        (Kernel,
         Command      => "help",
         Usage        => "help",
         Description  => -"List recognized commands.",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => Module_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => "echo",
         Usage        => "echo",
         Description  => -"Display a line of text.",
         Minimum_Args => 0,
         Maximum_Args => Natural'Last,
         Handler      => Module_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => "insmod",
         Usage        => "insmod shared-lib module",
         Description  => -"Dynamically register from shared-lib a new module.",
         Minimum_Args => 2,
         Maximum_Args => 2,
         Handler      => Module_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => "lsmod",
         Usage        => "lsmod",
         Description  => -"List modules currently loaded.",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Module_Command_Handler'Access);
   end Register_Module;

   --------------------------
   -- Console_Delete_Event --
   --------------------------

   function Console_Delete_Event
     (Console : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Console);
   begin
      return True;
   end Console_Delete_Event;

   ----------------------
   -- Commands_As_List --
   ----------------------

   function Commands_As_List
     (Prefix : String;
      Kernel : access Glib.Object.GObject_Record'Class)
      return String_List_Utils.String_List.List
   is
      pragma Unreferenced (Kernel);
      use String_List_Utils.String_List;
      use type Command_List.List_Node;
      L       : String_List_Utils.String_List.List := Null_List;
      Current : Command_List.List_Node :=
        Command_List.First (Shell_Module_Id.Commands_List);
   begin
      while Current /= Command_List.Null_Node loop
         declare
            S : constant String := Command_List.Data (Current).Command.all;
         begin
            if S'Length >= Prefix'Length
              and then S (S'First .. S'First + Prefix'Length - 1) = Prefix
            then
               Prepend (L, S);
            end if;
         end;
         Current := Command_List.Next (Current);
      end loop;

      return L;
   end Commands_As_List;

   ----------------------------
   -- Module_Command_Handler --
   ----------------------------

   function Module_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String
   is
      use String_List_Utils.String_List;
      use type Command_List.List_Node;

      Command_Node : Command_List.List_Node;
      L            : String_List_Utils.String_List.List;
      L2           : String_List_Utils.String_List.List_Node;
      Success      : Boolean;
      Result       : GNAT.OS_Lib.String_Access;

      procedure Insert (S : String);
      --  Appends S & ASCII.LF to Result.
      --  Result must not be set to Null when calling this subprogram.

      procedure Insert (S : String) is
         R : constant String := Result.all & S & ASCII.LF;
      begin
         Free (Result);
         Result := new String'(R);
      end Insert;

   begin
      Result := new String'("");

      if Command = "help" then
         if Args'Length = 0 then
            Insert (-"The following commands are defined:");

            L := Commands_As_List ("", Kernel);
            String_List_Utils.Sort (L);

            L2 := First (L);
            while L2 /= Null_Node loop
               Insert (" " & Data (L2));
               L2 := String_List_Utils.String_List.Next (L2);
            end loop;

            Free (L);

            Insert
              (-"Type ""help <cmd>"" to get help about a specific command.");

         else
            Command_Node := Command_List.First (Shell_Module_Id.Commands_List);

            while Command_Node /= Command_List.Null_Node loop
               declare
                  Data : constant Command_Information :=
                    Command_List.Data (Command_Node);
               begin
                  if Data.Command.all = Args (Args'First).all then
                     Insert (-("Usage: ") & Data.Usage.all);
                     Insert (Data.Description.all);
                  end if;
               end;

               Command_Node := Command_List.Next (Command_Node);
            end loop;
         end if;

      elsif Command = "echo" then
         for Index in Args'Range loop
            Insert (Args (Index).all);
         end loop;

      elsif Command = "insmod" then
         Dynamic_Register_Module
           (Kernel, Args (Args'First).all, Args (Args'First + 1).all, Success);

         if Success then
            return (-"Module successfully loaded.") & ASCII.LF;
         else
            return (-"Couldn't load module.") & ASCII.LF;
         end if;

      elsif Command = "lsmod" then
         declare
            use type Module_List.List_Node;
            Current : Module_List.List_Node;
            List    : constant Module_List.List := List_Of_Modules (Kernel);
         begin
            Current := Module_List.First (List);

            while Current /= Module_List.Null_Node loop
               Insert (Module_Name (Module_List.Data (Current)));
               Current := Module_List.Next (Current);
            end loop;
         end;
      end if;

      declare
         R : constant String := Result.all;
      begin
         Free (Result);
         return R;
      end;
   end Module_Command_Handler;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command      : String;
      Usage        : String;
      Description  : String;
      Minimum_Args : Natural := 0;
      Maximum_Args : Natural := 0;
      Handler      : Module_Command_Function)
   is
      use Command_List;
      pragma Unreferenced (Kernel);

      Node : List_Node;
   begin
      if Command = "" or else Shell_Module_Id = null then
         return;
      end if;

      Node := First (Shell_Module_Id.Commands_List);

      --  Check that the command is not already registered.

      while Node /= Null_Node loop
         if Data (Node).Command.all = Command then
            Trace
              (Me,
               "Interactive command " & Command & " is already registered");

            return;
         end if;

         Node := Next (Node);
      end loop;

      Append (Shell_Module_Id.Commands_List,
              (Command         => new String'(Command),
               Usage           => new String'(Usage),
               Description     => new String'(Description),
               Minimum_Args    => Minimum_Args,
               Maximum_Args    => Maximum_Args,
               Command_Handler => Handler));
   end Register_Command;

   -----------------------
   -- Interpret_Command --
   -----------------------

   function Interpret_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : Argument_List) return String
   is
      use type Command_List.List_Node;

      Command_Node : Command_List.List_Node;
      Data   : Command_Information;

   begin
      if Shell_Module_Id = null then
         return -"Shell module not initialized";
      end if;

      Command_Node := Command_List.First (Shell_Module_Id.Commands_List);
      while Command_Node /= Command_List.Null_Node loop
         Data := Command_List.Data (Command_Node);
         if Data.Command.all = Command then
            if Data.Minimum_Args <= Args'Length
              and then Args'Length <= Data.Maximum_Args
            then
               return Data.Command_Handler (Kernel, Command, Args);
            else
               Trace (Me, "Incorrect number of arguments for " & Command);
               return -"Incorrect number of arguments." & ASCII.LF
                 & Data.Usage.all;
            end if;
         end if;
         Command_Node := Command_List.Next (Command_Node);
      end loop;

      return -"Command not recognized";
   end Interpret_Command;

   -----------------------
   -- Interpret_Command --
   -----------------------

   function Interpret_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String) return String
   is
      Args         : Argument_List_Access;
   begin
      if Command = "" then
         return "";
      end if;

      Trace (Me, "Launching interactive command: " & Command);

      Args := Argument_String_To_List (Command);

      declare
         R : constant String := Interpret_Command
           (Kernel,
            Command => Args (Args'First).all,
            Args    => Args (Args'First + 1 .. Args'Last));
      begin
         Free (Args);

         return R;
      end;
   end Interpret_Command;

   -----------------------
   -- Interpret_Command --
   -----------------------

   procedure Interpret_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String) is
   begin
      Insert (Kernel, Interpret_Command (Kernel, Command), False);
   end Interpret_Command;

   -----------------------
   -- Interpret_Command --
   -----------------------

   procedure Interpret_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) is
   begin
      Insert (Kernel, Interpret_Command (Kernel, Command, Args), False);
   end Interpret_Command;

   -----------------
   -- Create_Mark --
   -----------------

   function Create_Mark
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename : String;
      Line     : Natural := 1;
      Column   : Natural := 1;
      Length   : Natural := 0) return String is
   begin
      return Interpret_Command
        (Kernel,
         "create_mark -l" & Line'Img
         & " -c" & Column'Img
         & " -L" & Length'Img
         & " " & Filename);
   end Create_Mark;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename           : String;
      Line               : Natural := 1;
      Highlight_Category : String;
      Highlight          : Boolean := True)
   is
      Args    : Argument_List (1 .. 3);
      Command : String_Access;
   begin
      Args (1) := new String'(Filename);
      Args (2) := new String'(Line'Img);
      Args (3) := new String'(Highlight_Category);

      if Highlight then
         Command := new String'("src.highlight");

      else
         Command := new String'("src.unhighlight");
      end if;

      if Line = 0 then
         Interpret_Command (Kernel, Command.all, Args (1) & Args (3));
      else
         Interpret_Command (Kernel, Command.all, Args);
      end if;

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      Free (Command);
   end Highlight_Line;

end Shell;
