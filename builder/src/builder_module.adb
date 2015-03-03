------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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
with Commands.Builder;           use Commands.Builder;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;

with Builder_Facility_Module;    use Builder_Facility_Module;
with Build_Command_Utils;        use Build_Command_Utils;

package body Builder_Module is

   type Interrupt_Tool_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Interrupt_Tool_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

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
            "Build All", "default",
            GNATCOLL.VFS.No_File,
            Extra_Args  => null,
            Quiet       => True,
            Synchronous => True,
            Dialog      => Build_Command_Utils.Force_No_Dialog,
            Via_Menu    => False,
            Background  => False,
            Main        => GNATCOLL.VFS.No_File);

      elsif Command = "compute_xref_bg" then
         Launch_Target
           (Builder_Facility_Module.Builder,
            "Build All", "default",
            GNATCOLL.VFS.No_File,
            Extra_Args  => null,
            Quiet       => True,
            Synchronous => False,
            Background  => False,
            Dialog      => Build_Command_Utils.Force_No_Dialog,
            Via_Menu    => False,
            Main        => GNATCOLL.VFS.No_File);
      end if;
   end Compile_Command;

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
      Register_Action
        (Kernel, "Interrupt", new Interrupt_Tool_Command,
         Description =>
           -"Interrupt the tasks performed in the background by GPS",
         Icon_Name  => "gps-stop-symbolic");

      Register_Command
        (Kernel, "compute_xref",
         Handler => Compile_Command'Access);
      Register_Command
        (Kernel, "compute_xref_bg",
         Handler => Compile_Command'Access);
   end Register_Module;

end Builder_Module;
