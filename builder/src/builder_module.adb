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

with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Gtk.Main;                use Gtk.Main;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Window;              use Gtk.Window;

with Glide_Intl;              use Glide_Intl;

with GVD.Status_Bar;          use GVD.Status_Bar;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Console;    use Glide_Kernel.Console;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Kernel.Project;    use Glide_Kernel.Project;

with Glide_Main_Window;       use Glide_Main_Window;

with GVD.Dialogs;             use GVD.Dialogs;

with GNAT.Expect;             use GNAT.Expect;
with GNAT.Regpat;             use GNAT.Regpat;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Ada.Exceptions;          use Ada.Exceptions;
with Traces;                  use Traces;

package body Builder_Module is

   Builder_Module_Id   : Module_ID;
   Builder_Module_Name : constant String := "Builder";

   Me : constant Debug_Handle := Create (Builder_Module_Name);

   type Builder_Data is record
      Kernel     : Kernel_Handle;
      Descriptor : Process_Descriptor_Access;
   end record;

   package Builder_Idle is new Gtk.Main.Idle (Builder_Data);

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize Builder module.

   function Idle_Build (Data : Builder_Data) return Boolean;
   --  Called by the Gtk main loop when idle.
   --  Handle on going build.

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Build
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Make menu

   procedure On_Run
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Run menu

   procedure On_Stop_Build
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Stop Build menu

   --------------
   -- On_Build --
   --------------

   procedure On_Build
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top       : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Fd        : Process_Descriptor_Access;
      Args      : Argument_List_Access;
      Title     : String_Access;
      --  ??? Should get the name of the real main
      Project   : constant String := Get_Project_File_Name (Kernel);
      Cmd       : constant String :=
        "gnatmake -P" & Project & " "
        & Scenario_Variables_Cmd_Line (Kernel)
        & " ";
      Id        : Idle_Handler_Id;

   begin
      if Get_Current_Explorer_Context (Kernel) /= null then
         Title := new String'
           (File_Information (File_Selection_Context_Access (
             Get_Current_Explorer_Context (Kernel))));
      else
         return;
      end if;

      Set_Busy (Kernel);

      if Project = "" then
         --  This is the default internal project

         Args := Argument_String_To_List ("gnatmake -d " & Title.all);
         Console.Insert (Kernel, "gnatmake " & Title.all, False);

      else
         Args := Argument_String_To_List (Cmd & Title.all & " -d");
         Console.Insert (Kernel, Cmd & Title.all, False);
      end if;

      Free (Title);
      Top.Interrupted := False;
      Fd := new Process_Descriptor;
      Non_Blocking_Spawn
        (Fd.all, Args (Args'First).all, Args (Args'First + 1 .. Args'Last),
         Err_To_Out  => True);
      --   ??? Free (Args);
      Id := Builder_Idle.Add (Idle_Build'Access, (Kernel, Fd));
   end On_Build;

   ----------------
   -- Idle_Build --
   ----------------

   function Idle_Build (Data : Builder_Data) return Boolean is
      Kernel  : Kernel_Handle renames Data.Kernel;
      Fd      : Process_Descriptor renames Data.Descriptor.all;

      Top     : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Matched : Match_Array (0 .. 0);
      Result  : Expect_Match;
      Matcher : constant Pattern_Matcher := Compile
        ("completed ([0-9]+) out of ([0-9]+) \((.*)%\)\.\.\.$",
         Multiple_Lines);

   begin
      if Top.Interrupted then
         Interrupt (Fd);
         Console.Insert (Kernel, "<^C>");
      end if;

      loop
         Expect (Fd, Result, ".+", Timeout => 0);

         exit when Result = Expect_Timeout;

         declare
            S : constant String := Expect_Out (Fd);
         begin
            Match (Matcher, S, Matched);

            if Matched (0) = No_Match then
               Console.Insert (Kernel, S, Add_LF => False);
            else
               Print_Message
                 (Top.Statusbar, GVD.Status_Bar.Help,
                  S (S'First + 1 .. S'Last));
            end if;
         end;
      end loop;

      return True;

   exception
      when Process_Died =>
         Console.Insert (Kernel, Expect_Out (Fd), Add_LF => False);
         --  ??? Check returned status.

         Set_Busy (Kernel, False);

         if Top.Interrupted then
            Top.Interrupted := False;
            Print_Message
              (Top.Statusbar, GVD.Status_Bar.Help, -"build interrupted.");
         else
            Print_Message
              (Top.Statusbar, GVD.Status_Bar.Help, -"build completed.");
         end if;

         Close (Fd);
         --  ??? Free (Data.Descriptor);
         return False;

      when E : others =>
         Set_Busy (Kernel, False);
         Close (Fd);
         --  ??? Free (Data.Descriptor);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Idle_Build;

   ------------
   -- On_Run --
   ------------

   procedure On_Run
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Arguments : constant String := Simple_Entry_Dialog
        (Parent  => Get_Main_Window (Kernel),
         Title   => -"Arguments Selection",
         Message => -"Enter the arguments to your application:",
         Key     => "glide_run_arguments");

   begin
      if Arguments = ""
        or else Arguments (Arguments'First) /= ASCII.NUL
      then
         null;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Run;

   -------------------
   -- On_Stop_Build --
   -------------------

   procedure On_Stop_Build
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
   begin
      Top.Interrupted := True;
   end On_Stop_Build;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Build : constant String := '/' & (-"Build") & '/';
      Mitem : Gtk_Menu_Item;
   begin
      Register_Menu (Kernel, "/_" & (-"Build"), Ref_Item => -"Debug");
      Register_Menu (Kernel, Build, -"Check File", "", null);
      Register_Menu (Kernel, Build, -"Compile File", "", null);
      Register_Menu (Kernel, Build, -"Make", "", On_Build'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Build, Mitem);
      Register_Menu
        (Kernel, Build, -"Execute...", Stock_Execute, On_Run'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Build, Mitem);
      Register_Menu
        (Kernel, Build, -"Stop Build", Stock_Stop, On_Stop_Build'Access);
   end Initialize_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      Builder_Module_Id := Register_Module
        (Module_Name  => Builder_Module_Name,
         Priority     => Default_Priority,
         Initializer  => Initialize_Module'Access);
   end Register_Module;

end Builder_Module;
