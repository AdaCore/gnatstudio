------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.File_Selector;       use Gtkada.File_Selector;
with Gtkada.MDI;                 use Gtkada.MDI;

with Commands.Interactive;       use Commands, Commands.Interactive;
with Commands.Builder;           use Commands.Builder;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;

with Builder_Facility_Module;
with Build_Command_Utils;        use Build_Command_Utils;
with Interactive_Consoles;       use Interactive_Consoles;

package body Builder_Module is

   type Interrupt_Tool_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Interrupt_Tool_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Run_Export_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Run_Export_Command;
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
            Main        => GNATCOLL.VFS.No_File,
            Main_Project => GNATCOLL.Projects.No_Project);

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
            Main         => GNATCOLL.VFS.No_File,
            Main_Project => GNATCOLL.Projects.No_Project);
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

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Run_Export_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Child  : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
      W      : Gtk_Widget;
   begin
      if Child /= null then
         if Child.all in GPS_MDI_Child_Record'Class then
            W := GPS_MDI_Child (Child).Get_Actual_Widget;
         else
            W := Child.Get_Widget;
         end if;

         if W.all in Interactive_Console_Record'Class then
            declare
               F : constant Virtual_File :=
                 Select_File
                   (Title             => -"Select File to Export from " &
                    Child.Get_Title,
                    File_Pattern      => +("*.txt"),
                    Pattern_Name      => -"Text files",
                    Default_Name      => +"content.txt",
                    Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                    Kind              => Save_File);
            begin
               if F = GNATCOLL.VFS.No_File
                 or else Interactive_Console (W).Export (F)
               then
                  return Commands.Success;
               end if;
            end;
         end if;
      end if;

      return Commands.Failure;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Action
        (Kernel, "Interrupt", new Interrupt_Tool_Command,
         Description  =>
           -"Interrupt the tasks performed in the background by GPS",
         Icon_Name    => "gps-stop-symbolic",
         For_Learning => True);

      Register_Action
        (Kernel, "export console to file", new Run_Export_Command,
         -"Export the output of run to a text file",
         Icon_Name => "gps-save-symbolic",
         Category => -"Run");

      Register_Command
        (Kernel, "compute_xref",
         Handler => Compile_Command'Access);
      Register_Command
        (Kernel, "compute_xref_bg",
         Handler => Compile_Command'Access);
   end Register_Module;

end Builder_Module;
