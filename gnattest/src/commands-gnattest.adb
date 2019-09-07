------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

with GNATCOLL.VFS;              use GNATCOLL.VFS;
with Basic_Types;
with GNATTest_Module;
with GPS.Kernel.Contexts;
with Ada.Strings.Unbounded;
with Xref;                      use Xref;

package body Commands.GNATTest is

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Go_To_Tested_Command_Type;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      Kernel : constant GPS.Kernel.Kernel_Handle :=
        GPS.Kernel.Get_Kernel (Context.Context);
      Entity : constant Root_Entity'Class :=
        GPS.Kernel.Contexts.Get_Entity (Context.Context);

      File            : Virtual_File;
      Subprogram_Name : Ada.Strings.Unbounded.Unbounded_String;
      Line            : Integer;
      Column          : Basic_Types.Visible_Column_Type;
   begin
      if Entity /= No_Root_Entity
        and then Is_Subprogram (Entity)
      then
         GNATTest_Module.Find_Tested
           (GPS.Kernel.Contexts.File_Information (Context.Context),
            File,
            Subprogram_Name,
            Line,
            Column);

         GNATTest_Module.Open_File
           (Kernel,
            Project =>
              GPS.Kernel.Contexts.Project_Information (Context.Context),
            File    => File,
            Line    => Line,
            Column  => Column,
            Subprogram_Name =>
              Ada.Strings.Unbounded.To_String (Subprogram_Name));

         Command.Command_Finished (True);
      end if;

      return Commands.Success;
   end Execute;

   ----------
   -- Name --
   ----------

   overriding
   function Name (X : access Go_To_Tested_Command_Type) return String is
   begin
      pragma Unreferenced (X);
      return "go to test";
   end Name;

end Commands.GNATTest;
