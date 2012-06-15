------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

with GNATCOLL.SQL.Sqlite;

with GPS.Kernel.Console;  use GPS.Kernel.Console;
with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;
with GPS.Kernel.Project;  use GPS.Kernel.Project;

package body GPS.Kernel.Xref is

   use Xref;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (Self  : GPS_Xref_Database;
      Error : String)
   is
   begin
      Insert (Kernel => Self.Kernel,
              Text   => Error,
              Add_LF => True,
              Mode   => GPS.Kernel.Console.Error);
   end On_Error;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Self   : GPS_Xref_Database_Access;
      Kernel : Kernel_Handle)
   is
      Dir : Virtual_File;
      File : Virtual_File;

   begin
      Self.Kernel := Kernel;

      Dir := Get_Project (Kernel).Object_Dir;

      File := Create_From_Dir
        (Dir       => Dir,
         Base_Name => "gnatinspect.db");

      Self.Setup_DB (GNATCOLL.SQL.Sqlite.Setup (+File.Full_Name.all));
   end Setup;

   -------------------------------
   -- Ensure_Context_Up_To_Date --
   -------------------------------

   procedure Ensure_Context_Up_To_Date (Context : Selection_Context) is
      use Entities;
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_Entity_Name_Information (Context)
        and then Has_Line_Information (Context)
        and then Has_File_Information (Context)
      then
         declare
            Handler : Entities.LI_Handler;
            File    : Entities.Source_File;

         begin
            File :=
              Get_Or_Create
                (Db   => Get_Database (Kernel),
                 File => File_Information (Context));

            Handler :=
              Get_LI_Handler (Get_Database (Kernel), Get_Filename (File));

            if Has_Unresolved_Imported_Refs (Handler) then
               Set_Update_Forced (Handler);
               Update_Xref (File);
            end if;
         end;
      end if;
   end Ensure_Context_Up_To_Date;

end GPS.Kernel.Xref;
