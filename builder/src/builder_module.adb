------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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

with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
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
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Stock_Icons;            use GPS.Stock_Icons;

with Build_Command_Utils;
with Builder_Facility_Module;
with Commands.Builder;           use Commands.Builder;

package body Builder_Module is

   Xrefs_Loading_Queue : constant String := "xrefs_loading";

   type Builder_Module_ID_Record is
     new GPS.Kernel.Modules.Module_ID_Record with null record;
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

   type Interrupt_Tool_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Interrupt_Tool_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

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
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      --  This memory is allocated once, and lives as long as the application

      Builder_Module_ID := new Builder_Module_ID_Record;
      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Builder",
         Priority    => Default_Priority);

      Register_Action
        (Kernel, "Interrupt", new Interrupt_Tool_Command,
         Description =>
           -"Interrupt the tasks performed in the background by GPS",
         Stock_Id   => GPS_Stop_Task);

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
