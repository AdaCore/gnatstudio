-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Traces;                  use Traces;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Builder_Module is

   Timeout : constant Guint32 := 50;
   --  Timeout in millisecond to check the build process
   --  <preferences>

   Builder_Module_Id   : Module_ID;
   Builder_Module_Name : constant String := "Builder";

   Me : constant Debug_Handle := Create (Builder_Module_Name);

   type Builder_Data is record
      Kernel     : Kernel_Handle;
      Descriptor : Process_Descriptor_Access;
   end record;

   package Builder_Idle is new Gtk.Main.Timeout (Builder_Data);

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize Builder module.

   function Idle_Build (Data : Builder_Data) return Boolean;
   --  Called by the Gtk main loop when idle.
   --  Handle on going build.

   procedure Set_Sensitive_Menus
     (Kernel    : Kernel_Handle;
      Sensitive : Boolean);
   --  Change the sensitive aspect of the build menu items.

   procedure Free (Ar : in out String_List);
   procedure Free (Ar : in out String_List_Access);
   --  Free the memory associate with Ar.

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Check_Syntax
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Check Syntax menu

   procedure On_Compile
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Compile menu

   procedure On_Build
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Make menu

   procedure On_Run
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Run menu

   procedure On_Stop_Build
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Stop Build menu

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out String_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   procedure Free (Ar : in out String_List_Access) is
      procedure Free is new
        Ada.Unchecked_Deallocation (String_List, String_List_Access);
   begin
      if Ar /= null then
         Free (Ar.all);
         Free (Ar);
      end if;
   end Free;

   -------------------------
   -- Set_Sensitive_Menus --
   -------------------------

   procedure Set_Sensitive_Menus
     (Kernel    : Kernel_Handle;
      Sensitive : Boolean)
   is
      Build : constant String := '/' & (-"Build") & '/';
   begin
      Set_Sensitive (Find_Menu_Item
        (Kernel, Build & (-"Check Syntax")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Build & (-"Compile File")), Sensitive);
      Set_Sensitive (Find_Menu_Item (Kernel, Build & (-"Make")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Build & (-"Stop Build")), not Sensitive);
   end Set_Sensitive_Menus;

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
      Id        : Timeout_Handler_Id;

   begin
      if Get_Current_Explorer_Context (Kernel) /= null then
         Title := new String'
           (File_Information (File_Selection_Context_Access (
             Get_Current_Explorer_Context (Kernel))));
      else
         return;
      end if;

      --  ??? Ask for saving sources/projects before building

      Push_State (Kernel, Processing);
      Console.Clear (Kernel);
      Set_Sensitive_Menus (Kernel, False);

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
      Free (Args);
      Id := Builder_Idle.Add (Timeout, Idle_Build'Access, (Kernel, Fd));
   end On_Build;

   ---------------------
   -- On_Check_Syntax --
   ---------------------

   procedure On_Check_Syntax
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
   begin
      --  ???
      null;
   end On_Check_Syntax;

   ----------------
   -- On_Compile --
   ----------------

   procedure On_Compile
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
   begin
      --  ???
      null;
   end On_Compile;

   ----------------
   -- Idle_Build --
   ----------------

   function Idle_Build (Data : Builder_Data) return Boolean is
      Kernel  : Kernel_Handle renames Data.Kernel;
      Fd      : Process_Descriptor_Access := Data.Descriptor;

      Top     : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Matched : Match_Array (0 .. 3);
      Result  : Expect_Match;
      Matcher : constant Pattern_Matcher := Compile
        ("completed ([0-9]+) out of ([0-9]+) \((.*)%\)\.\.\.$",
         Multiple_Lines);
      Timeout : Integer := 0;

      procedure Free is new Ada.Unchecked_Deallocation
        (Process_Descriptor'Class, Process_Descriptor_Access);

   begin
      if Top.Interrupted then
         Interrupt (Fd.all);
         Console.Insert (Kernel, "<^C>");
         Top.Interrupted := False;
         Print_Message
           (Top.Statusbar, GVD.Status_Bar.Help, -"Interrupting build...");
         Timeout := 10;
      end if;

      loop
         Expect (Fd.all, Result, ".+", Timeout => Timeout);

         exit when Result = Expect_Timeout;

         declare
            S : constant String := Expect_Out (Fd.all);
         begin
            Match (Matcher, S, Matched);

            if Matched (0) = No_Match then
               Console.Insert (Kernel, S, Add_LF => False);
            else
               Set_Fraction
                 (Top.Statusbar,
                  Gdouble'Value
                    (S (Matched (3).First .. Matched (3).Last)) / 100.0);
               Set_Progress_Text
                 (Top.Statusbar, S (S'First + 1 .. Matched (2).Last));
            end if;
         end;
      end loop;

      return True;

   exception
      when Process_Died =>
         Console.Insert (Kernel, Expect_Out (Fd.all), Add_LF => True);
         --  ??? Check returned status.
         Console.Insert (Kernel, -"process terminated.");
         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Close (Fd.all);
         Free (Fd);

         return False;

      when E : others =>
         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Close (Fd.all);
         Free (Fd);
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
      if Arguments = "" or else Arguments (Arguments'First) /= ASCII.NUL then
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
      Register_Menu (Kernel, Build, -"Check Syntax", "",
                     On_Check_Syntax'Access);
      Register_Menu (Kernel, Build, -"Compile File", "", On_Compile'Access);
      Register_Menu (Kernel, Build, -"Make", "", On_Build'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Build, Mitem);
      Register_Menu
        (Kernel, Build, -"Execute...", Stock_Execute, On_Run'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Build, Mitem);
      Register_Menu
        (Kernel, Build, -"Stop Build", Stock_Stop, On_Stop_Build'Access);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Build & (-"Stop Build")), False);
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
