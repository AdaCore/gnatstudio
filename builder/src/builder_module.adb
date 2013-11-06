------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.MDI;                 use Gtkada.MDI;

with Commands.Interactive;       use Commands, Commands.Interactive;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.Commands;        use GPS.Kernel.Commands;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Xref;            use GPS.Kernel.Xref;
with GPS.Stock_Icons;            use GPS.Stock_Icons;

with Build_Command_Utils;
with Builder_Facility_Module;
with Commands.Builder;           use Commands.Builder;
with Default_Preferences;        use Default_Preferences;
with Xref;                       use Xref;

package body Builder_Module is
   Me : constant Trace_Handle := Create ("Builder");

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

   procedure Interrupt_Xrefs_Loading
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Interrupts all xrefs loading

   --------------------
   -- Menu Callbacks --
   --------------------

   type Recompute_Xref_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Recompute_Xref_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Build->Compute Xref information menu

   type Load_Xref_In_Memory_Command
      is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Load_Xref_In_Memory_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Build->Load Xref info

   procedure On_Compilation_Finished
     (Kernel : access Kernel_Handle_Record'Class;
      Data : access Hooks_Data'Class);
   --  Called when the compilation is finished

   function On_Compilation_Starting
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Called when the compilation is starting

   type Interrupt_Tool_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Interrupt_Tool_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

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
      pragma Unreferenced (Data);
   begin
      if Command = "compute_xref" then
         Launch_Target
           (Builder_Facility_Module.Builder,
            "Build All", "xref",
            GNATCOLL.VFS.No_File,
            Extra_Args  => null,
            Quiet       => True,
            Synchronous => True,
            Dialog      => Build_Command_Utils.Force_No_Dialog,
            Background  => False,
            Main        => GNATCOLL.VFS.No_File);

      elsif Command = "compute_xref_bg" then
         Launch_Target
           (Builder_Facility_Module.Builder,
            "Build All", "xref",
            GNATCOLL.VFS.No_File,
            Extra_Args  => null,
            Quiet       => True,
            Synchronous => False,
            Background  => False,
            Dialog      => Build_Command_Utils.Force_No_Dialog,
            Main        => GNATCOLL.VFS.No_File);
      end if;
   end Compile_Command;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Recompute_Xref_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command, Context);
   begin
      Launch_Target
        (Builder_Facility_Module.Builder,
         "Build All", "xref",
         GNATCOLL.VFS.No_File,
         Extra_Args  => null,
         Quiet       => False,
         Synchronous => False,
         Background  => False,
         Dialog      => Build_Command_Utils.Force_No_Dialog,
         Main        => GNATCOLL.VFS.No_File);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Load_Xref_In_Memory_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      GPS.Kernel.Xref.Load_Xref_In_Memory (Kernel, C_Only => False);
      return Commands.Success;
   end Execute;

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
         GPS.Kernel.Xref.Compilation_Finished
           (Kernel,
            C_Only => Automatic_Xrefs_Load /= null
            and then not Automatic_Xrefs_Load.Get_Pref);
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

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Interrupt_Tool_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
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
      return Commands.Success;
   end Execute;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Trace (Me, "Project view changed, loading xref in memory");
      GPS.Kernel.Xref.Load_Xref_In_Memory
        (Kernel, C_Only => Automatic_Xrefs_Load /= null
         and then not Automatic_Xrefs_Load.Get_Pref);
   exception
      when E : others =>
         Trace (Me, E);
   end On_View_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command    : Interactive_Command_Access;
   begin
      --  This memory is allocated once, and lives as long as the application

      Builder_Module_ID := new Builder_Module_ID_Record;
      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Builder",
         Priority    => Default_Priority);

      if not Active (Xref.SQLITE) then
         Command := new Recompute_Xref_Command;
         Register_Action
           (Kernel, "Recompute Xref Info", Command,
            Description => -"Reload the contents of all ALI files");

         Command := new Load_Xref_In_Memory_Command;
         Register_Action
           (Kernel, "Load Xref in memory", Command,
            Description => -"Load all ALI file in memory, for faster queries");
      end if;

      Command := new Interrupt_Tool_Command;
      Register_Action
        (Kernel, "Interrupt", Command,
         Description =>
           -"Interrupt the tasks performed in the background by GPS",
         Stock_Id   => GPS_Stop_Task,
         Accel_Key  => GDK_C,
         Accel_Mods => Primary_Mod_Mask + Shift_Mask);

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

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Interrupt_Xrefs_Loading (Kernel);
   end On_Project_Changed;

end Builder_Module;
