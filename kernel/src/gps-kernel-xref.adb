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
with GNATCOLL.Traces;           use GNATCOLL.Traces;

with GPS.Kernel.Console;  use GPS.Kernel.Console;
with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;
with GPS.Kernel.Project;  use GPS.Kernel.Project;

with Entities.Queries; use Entities.Queries;

package body GPS.Kernel.Xref is

   use Xref;
   use type Entities.Entity_Information;
   use type Entities.File_Location;

   Me     : constant Trace_Handle := Create ("Entities.Queries", Off);

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

   ----------------------
   -- Find_Declaration --
   ----------------------

   --  ??? Should be renamed Find_Entity ? Find ?

   procedure Find_Declaration
     (Db              : Xref_Database'Class;
      File            : Virtual_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Entity          : out GNATCOLL.Xref.Entity_Information;
      Closest_Ref     : out GNATCOLL.Xref.Entity_Reference;
      Status          : out Entities.Queries.Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False;
      Fuzzy_Expected  : Boolean := False)
   is
      --  ??? Fuzzy_Expected should be renamed to fallback_on_constructs
      pragma Unreferenced (Check_Decl_Only, Fuzzy_Expected);

   begin
      Closest_Ref := Db.Get_Entity
        (Name   => Entity_Name,
         File   => File,
         Line   => Line,
         Column => Visible_Column (Column));

      Entity := Closest_Ref.Entity;

      if Entity = No_Entity then
         Status := Entity_Not_Found;
         Entity := No_Entity;
         Trace (Me, "Entity not found");
         return;
      end if;

      --  At this point Entity is resolved

      Status := Success;

      --  ??? Do we want to port this?

--        if Active (Constructs_Heuristics)
--          and then Db.Construct_Db_Locks = 0
--          and then
--            (Status = Entity_Not_Found
--             or else
--               ((Status = Fuzzy_Match
--                 or else Status = Overloaded_Entity_Found)
--                and then not Fuzzy_Expected))
--        then
   end Find_Declaration;

   ------------------------
   -- Get_Context_Entity --
   ------------------------

   function Get_Context_Entity
     (Context : Selection_Context;
      Ask_If_Overloaded : Boolean := False) return General_Entity
   is
      E : General_Entity;
   begin
      if Active (Entities.SQLITE) then
         E.Entity := Get_Entity (Context, Ask_If_Overloaded);
      end if;

      E.Old_Entity := Get_Entity (Context, Ask_If_Overloaded);

      return E;
   end Get_Context_Entity;

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
