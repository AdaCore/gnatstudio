-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                 Copyright (C) 2001-2010, AdaCore                  --
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

with Ada.Unchecked_Deallocation;

with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.MDI;                 use Gtkada.MDI;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Commands;        use GPS.Kernel.Commands;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;

with Language_Handlers;          use Language_Handlers;
with Entities;                   use Entities;
with Entities.Queries;           use Entities.Queries;
with Build_Command_Manager;
with Builder_Facility_Module;
with Traces;                     use Traces;
with Commands;                   use Commands;

with Commands.Generic_Asynchronous;

package body Builder_Module is

   Xrefs_Loading_Queue : constant String := "xrefs_loading";

   type Builder_Module_ID_Record is
     new GPS.Kernel.Modules.Module_ID_Record
   with record
      Build_Count : Natural := 0;
      --  Number of on-going builds
   end record;
   type Builder_Module_ID_Access is access all Builder_Module_ID_Record;
   --  Data stored with the module id

   Builder_Module_ID : Builder_Module_ID_Access;

   type LI_Handler_Iterator_Access_Access is access LI_Handler_Iterator_Access;

   procedure Interrupt_Xrefs_Loading
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Interrupts all xrefs loading

   -------------------------
   -- Load_Xref_In_Memory --
   -------------------------

   type All_LI_Information_Command is new Root_Command with record
      Iter : Recursive_LI_Information_Iterator;
      Count, Total : Natural := 0;
      Chunk_Size   : Natural := 10;  --  ??? Should be configurable
   end record;

   overriding function Progress
     (Command : access All_LI_Information_Command) return Progress_Record;
   overriding function Execute
     (Command : access All_LI_Information_Command) return Command_Return_Type;
   overriding function Name
     (Command : access All_LI_Information_Command) return String;

   --------------------------------
   -- Computing cross-references --
   --------------------------------

   type Compute_Xref_Data is record
      Kernel : Kernel_Handle;
      Iter   : LI_Handler_Iterator_Access_Access;
      LI     : Natural;
   end record;
   type Compute_Xref_Data_Access is access Compute_Xref_Data;

   procedure Deep_Free (D : in out Compute_Xref_Data_Access);
   --  Free memory associated to D

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (LI_Handler_Iterator_Access, LI_Handler_Iterator_Access_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Compute_Xref_Data, Compute_Xref_Data_Access);

   procedure Xref_Iterate
     (Xref_Data : in out Compute_Xref_Data_Access;
      Command   : Command_Access;
      Result    : out Command_Return_Type);
   --  Query the Xref information for the next files in the project

   package Xref_Commands is new Commands.Generic_Asynchronous
     (Data_Type => Compute_Xref_Data_Access,
      Free      => Deep_Free);

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Compute_Xref
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Compute Xref information menu

   procedure On_Load_Xref_In_Memory
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Build->Load Xref info

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data : access Hooks_Data'Class);
   --  Called when the compilation is finished

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Called when the compilation is starting

   procedure Load_Xref_In_Memory (Kernel : access Kernel_Handle_Record'Class);
   --  Load the Xref info in memory, in a background task

   procedure On_Tools_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Interrupt menu

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time the project view has changed, ie potentially the list
   --  of main units.

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time a new project is loaded

   procedure Compile_Command
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Command handler for the "compile" command

   ---------------------
   -- Compile_Command --
   ---------------------

   procedure Compile_Command
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      C      : Xref_Commands.Generic_Asynchronous_Command_Access;
   begin
      if Command = "compute_xref" then
         Xref_Commands.Create
           (C, -"Computing C/C++ xref info",
            new Compute_Xref_Data'(Kernel, new LI_Handler_Iterator_Access, 0),
            Xref_Iterate'Access);

         Launch_Synchronous (Command_Access (C), 0.01);
         Unref (Command_Access (C));
         Build_Command_Manager.Launch_Target
           (Kernel,
            Builder_Facility_Module.Registry,
            "Build All", "xref",
            GNATCOLL.VFS.No_File,
            Extra_Args  => null,
            Quiet       => True,
            Synchronous => True,
            Dialog      => Build_Command_Manager.Force_No_Dialog,
            Background  => False,
            Main        => "");

      elsif Command = "compute_xref_bg" then
         Xref_Commands.Create
           (C, -"Computing C/C++ xref info",
            new Compute_Xref_Data'(Kernel, new LI_Handler_Iterator_Access, 0),
            Xref_Iterate'Access);

         Launch_Background_Command
           (Kernel, Command_Access (C), True, True, "");

         Build_Command_Manager.Launch_Target
           (Kernel,
            Builder_Facility_Module.Registry,
            "Build All", "xref",
            GNATCOLL.VFS.No_File,
            Extra_Args  => null,
            Quiet       => True,
            Synchronous => False,
            Background  => False,
            Dialog      => Build_Command_Manager.Force_No_Dialog,
            Main        => "");
      end if;
   end Compile_Command;

   ------------------
   -- Xref_Iterate --
   ------------------

   procedure Xref_Iterate
     (Xref_Data : in out Compute_Xref_Data_Access;
      Command   : Command_Access;
      Result    : out Command_Return_Type)
   is
      pragma Warnings (Off, Xref_Data);
      D            : Compute_Xref_Data_Access renames Xref_Data;
      Handler      : constant Language_Handler :=
                       Get_Language_Handler (D.Kernel);
      Num_Handlers : constant Natural := LI_Handlers_Count (Handler);
      Not_Finished : Boolean;
      LI           : LI_Handler;
      New_Handler  : Boolean := False;

      procedure Errors (Msg : String);
      --  Print the Msg as an error in the GPS console

      ------------
      -- Errors --
      ------------

      procedure Errors (Msg : String) is
      begin
         Insert (D.Kernel, Msg, Mode => Console.Error);
      end Errors;

   begin
      if D.LI /= 0 and then D.Iter.all /= null then
         Continue (D.Iter.all.all, Errors'Unrestricted_Access, Not_Finished);
      else
         Not_Finished := True;
      end if;

      while Not_Finished loop
         D.LI := D.LI + 1;

         if D.LI > Num_Handlers then
            Insert (D.Kernel, -"Finished parsing all source files");

            Result := Success;
            return;
         end if;

         Free (D.Iter.all);

         LI := Get_Nth_Handler (Handler, D.LI);
         if LI /= null then
            New_Handler := True;
            D.Iter.all := new LI_Handler_Iterator'Class'
              (Generate_LI_For_Project
                 (Handler      => LI,
                  Lang_Handler => Get_Language_Handler (D.Kernel),
                  Project      => Get_Project (D.Kernel),
                  Errors       => Errors'Unrestricted_Access,
                  Recursive    => True));
            Continue (D.Iter.all.all,
                      Errors'Unrestricted_Access,
                      Not_Finished);
         end if;
      end loop;

      if New_Handler then
         Insert (D.Kernel, -"Parsing source files for "
                 & Get_Name (Get_Nth_Handler (Handler, D.LI)));
      end if;

      Set_Progress (Command, (Running, D.LI, Num_Handlers));

      Result := Execute_Again;
      return;
   end Xref_Iterate;

   ---------------------
   -- On_Compute_Xref --
   ---------------------

   procedure On_Compute_Xref
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);

      C : Xref_Commands.Generic_Asynchronous_Command_Access;
   begin
      Build_Command_Manager.Launch_Target
        (Kernel,
         Builder_Facility_Module.Registry,
         "Build All", "xref",
         GNATCOLL.VFS.No_File,
         Extra_Args  => null,
         Quiet       => True,
         Synchronous => False,
         Background  => False,
         Dialog      => Build_Command_Manager.Force_No_Dialog,
         Main        => "");
      Xref_Commands.Create
        (C, -"Computing C/C++ xref info",
         new Compute_Xref_Data'(Kernel, new LI_Handler_Iterator_Access, 0),
         Xref_Iterate'Access);

      Launch_Background_Command
        (Kernel, Command_Access (C), True, True, "");

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Compute_Xref;

   ----------------------------
   -- On_Load_Xref_In_Memory --
   ----------------------------

   procedure On_Load_Xref_In_Memory
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);
   begin
      Load_Xref_In_Memory (Kernel);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Load_Xref_In_Memory;

   -----------------------------
   -- On_Compilation_Starting --
   -----------------------------

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      pragma Unreferenced (Data);
   begin
      Interrupt_Xrefs_Loading (Kernel);
      Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count + 1;

      return True;
   end On_Compilation_Starting;

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Data);
   begin
      if Builder_Module_ID.Build_Count > 0 then
         Builder_Module_ID.Build_Count := Builder_Module_ID.Build_Count - 1;
      end if;

      if Builder_Module_ID.Build_Count = 0 then
         if Automatic_Xrefs_Load.Get_Pref then
            Load_Xref_In_Memory (Kernel);
         end if;
      end if;
   end On_Compilation_Finished;

   -----------------------------
   -- Interrupt_Xrefs_Loading --
   -----------------------------

   procedure Interrupt_Xrefs_Loading
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Kill_File_Iteration (Kernel, Xrefs_Loading_Queue);
   end Interrupt_Xrefs_Loading;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access All_LI_Information_Command) return String is
      pragma Unreferenced (Command);
   begin
      return -"load xref info";
   end Name;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Command : access All_LI_Information_Command) return Progress_Record is
   begin
      return Progress_Record'
        (Activity => Running,
         Current  => Command.Count,
         Total    => Command.Total);
   end Progress;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access All_LI_Information_Command) return Command_Return_Type
   is
   begin
      Next (Command.Iter, Steps => Command.Chunk_Size,
            Count => Command.Count, Total => Command.Total);

      if Command.Count >= Command.Total then
         Free (Command.Iter);
         return Success;
      else
         return Execute_Again;
      end if;
   end Execute;

   -------------------------
   -- Load_Xref_In_Memory --
   -------------------------

   procedure Load_Xref_In_Memory
     (Kernel : access Kernel_Handle_Record'Class)
   is
      C : constant Command_Access := new All_LI_Information_Command;

   begin
      Start (All_LI_Information_Command (C.all).Iter,
             Get_Language_Handler (Kernel),
             Get_Project (Kernel).Start (Recursive => True));
      Launch_Background_Command
        (Kernel,
         C,
         Active     => True,
         Show_Bar   => True,
         Queue_Id   => "load xrefs info",
         Block_Exit => False);
   end Load_Xref_In_Memory;

   ------------------------
   -- On_Tools_Interrupt --
   ------------------------

   procedure On_Tools_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
   begin
      --  Check whether the current MDI child can handle interrupt on its own

      if Child = null
        or else Child.all not in GPS_MDI_Child_Record'Class
        or else not Interrupt (GPS_MDI_Child (Child))
      then
         --  Else default is to kill the last process we started
         Interrupt_Latest_Task (Kernel);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Tools_Interrupt;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      if Automatic_Xrefs_Load.Get_Pref then
         Load_Xref_In_Memory (Kernel_Handle (Kernel));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_View_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Build_Menu : constant String := '/' & (-"_Build") & '/';
      Tools : constant String := '/' & (-"Tools") & '/';
      Mitem : Gtk_Menu_Item;
   begin
      --  This memory is allocated once, and lives as long as the application

      Builder_Module_ID := new Builder_Module_ID_Record;
      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Builder",
         Priority    => Default_Priority);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Build_Menu, Mitem, Ref_Item => -"Settings");

      Register_Menu
        (Kernel, Build_Menu, -"Recompute _Xref info", "",
         On_Compute_Xref'Access, Ref_Item => -"Settings");
      Register_Menu
        (Kernel, Build_Menu, -"Load Xref info in memory", "",
         On_Load_Xref_In_Memory'Access, Ref_Item => -"Settings");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Build_Menu, Mitem, Ref_Item => -"Settings");

      Gtk_New (Mitem);
      Register_Menu (Kernel, Tools, Mitem);
      Register_Menu
        (Kernel, Tools, -"_Interrupt", Stock_Stop, On_Tools_Interrupt'Access,
         null, GDK_C, Control_Mask + Shift_Mask);

      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_View_Changed_Hook,
         Func   => Wrapper (On_View_Changed'Access),
         Name   => "builder_module.on_view_changed");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Compilation_Finished_Hook,
         Func   => Wrapper (On_Compilation_Finished'Access),
         Name   => "load_xrefs");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Compilation_Starting_Hook,
         Func   => Wrapper (On_Compilation_Starting'Access),
         Name   => "stop_load_xrefs");
      Add_Hook
        (Kernel => Kernel,
         Hook   => Project_Changed_Hook,
         Func   => Wrapper (On_Project_Changed'Access),
         Name   => "interrupt_xrefs_loading");

      Register_Command
        (Kernel, "compute_xref",
         Handler => Compile_Command'Access);
   end Register_Module;

   ---------------
   -- Deep_Free --
   ---------------

   procedure Deep_Free (D : in out Compute_Xref_Data_Access) is
   begin
      if D /= null then
         Unchecked_Free (D.Iter);
         Unchecked_Free (D);
      end if;
   end Deep_Free;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Interrupt_Xrefs_Loading (Kernel);
   end On_Project_Changed;

end Builder_Module;
