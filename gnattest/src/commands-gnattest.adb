------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Basic_Types;
with Entities;
with GNATCOLL.VFS;
with GNATTest_Module;
with GPS.Kernel.Contexts;
with Ada.Strings.Unbounded;
with GPS.Editors;

package body Commands.GNATTest is

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Go_To_Test_Command_Type;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      Kernel : constant GPS.Kernel.Kernel_Handle :=
        GPS.Kernel.Get_Kernel (Context.Context);
      Entity : constant Entities.Entity_Information :=
        GPS.Kernel.Contexts.Get_Entity (Context.Context);
      Unit_Name       : Ada.Strings.Unbounded.Unbounded_String;
      Subprogram_Name : Ada.Strings.Unbounded.Unbounded_String;
      File            : GNATCOLL.VFS.Virtual_File;
      Line            : Integer;
      Column          : Basic_Types.Visible_Column_Type;
   begin
      GNATTest_Module.Find
        (Entity,
         Command.To_Test,
         Unit_Name,
         Subprogram_Name,
         Line,
         Column);

      File := GPS.Kernel.Create
        (GNATCOLL.VFS.Filesystem_String
           (Ada.Strings.Unbounded.To_String (Unit_Name)),
         Kernel);

      declare
         use type Entities.Source_File;

         Buffer : constant GPS.Editors.Editor_Buffer'Class :=
           Kernel.Get_Buffer_Factory.Get (File);

         Editor : constant GPS.Editors.Editor_View'Class :=
           Buffer.Open;

         Location : constant GPS.Editors.Editor_Location'Class :=
           Buffer.New_Location (Line, Column);
      begin
         Editor.Cursor_Goto (Location, Raise_View => True);
      end;

      Command.Command_Finished (True);

      return Commands.Success;
   end Execute;

   ----------
   -- Name --
   ----------

   overriding
   function Name (X : access Go_To_Test_Command_Type) return String is
   begin
      pragma Unreferenced (X);
      return "go to test";
   end Name;

   ----------
   -- Free --
   ----------

   overriding procedure Free (X : in out Go_To_Test_Command_Type) is
   begin
      null;
   end Free;

end Commands.GNATTest;
