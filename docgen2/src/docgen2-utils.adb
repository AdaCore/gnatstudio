------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Entities.Queries;                 use Entities.Queries;
with Projects;                         use Projects;

with Basic_Types;
with GPS.Kernel.Console;               use GPS.Kernel.Console;
with GPS.Kernel.Messages.Tools_Output; use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Project;               use GPS.Kernel.Project;

package body Docgen2.Utils is

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Source_File;
      Loc    : Source_Location;
      Msg    : String)
   is
      Line : constant String := Natural'Image (Loc.Line);
      Col  : constant String := Natural'Image (Loc.Column);
      Err  : constant String :=
               Get_Filename (File).Display_Base_Name & ":" &
               Line (Line'First + 1 .. Line'Last) & ":" &
               Col (Col'First + 1 .. Col'Last) & ": " & Msg;
   begin
      Insert (Kernel, Err,
              Mode => Error);
      Parse_File_Locations (Kernel, Err, "Documentation");
   end Warning;

   ------------------
   -- Is_Spec_File --
   ------------------

   function Is_Spec_File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return Get_Registry (Kernel).Tree.Info (File).Unit_Part = Unit_Spec;
   end Is_Spec_File;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Construct : String;
      Loc       : Source_Location;
      File      : Source_File;
      Lang      : Language_Access) return Entity_Information
   is
      Entity        : Entity_Information;
      Current_Loc   : File_Location;

   begin
      Current_Loc :=
        (File   => File,
         Line   => Loc.Line,
         Column => Basic_Types.Visible_Column_Type (Loc.Column));

      Entity := Get_Or_Create
        (Kernel.Symbols.Find (Construct),
         File,
         Current_Loc.Line,
         Current_Loc.Column,
         Allow_Create => False);

      if Entity = null and then Construct (Construct'First) = '"' then
         --  Handle "="-like subprograms, that are stored whithout the '"' in
         --  the entities database
         Entity := Get_Entity
           (Kernel, Construct (Construct'First + 1 .. Construct'Last - 1),
            Loc, File, Lang);
      end if;

      if Entity = null and then Get_Name (Lang) = "Ada" then
         for J in reverse Construct'Range loop
            --  ??? Ada Specific ... should use language service
            --  Need to define it !
            if Construct (J) = '.' then
               Current_Loc.Column :=
                 Basic_Types.Visible_Column_Type
                   (Loc.Column + J + 1 - Construct'First);

               Entity := Get_Or_Create
                 (Kernel.Symbols.Find (Construct (J + 1 .. Construct'Last)),
                  File,
                  Current_Loc.Line,
                  Current_Loc.Column,
                  Allow_Create => False);

               exit;
            end if;
         end loop;
      end if;

      return Entity;
   end Get_Entity;

   ----------------------------
   -- Get_Declaration_Entity --
   ----------------------------

   function Get_Declaration_Entity
     (Construct : String;
      Loc       : Source_Location;
      File      : Source_File;
      Db        : Entities_Database;
      Lang      : Language_Access) return Entity_Information
   is
      Entity        : Entity_Information;
      Entity_Status : Find_Decl_Or_Body_Query_Status;
      Current_Loc   : File_Location;

   begin
      Current_Loc :=
        (File   => File,
         Line   => Loc.Line,
         Column => Basic_Types.Visible_Column_Type (Loc.Column));

      Find_Declaration
        (Db              => Db,
         File_Name       => Get_Filename (File),
         Entity_Name     => Construct,
         Line            => Current_Loc.Line,
         Column          => Current_Loc.Column,
         Entity          => Entity,
         Status          => Entity_Status,
         Check_Decl_Only => False);

      if Entity = null and then Construct (Construct'First) = '"' then
         --  Handle "="-like subprograms, that are stored whithout the '"' in
         --  the entities database
         Entity := Get_Declaration_Entity
           (Construct (Construct'First + 1 .. Construct'Last - 1),
            Loc, File, Db, Lang);
      end if;

      if Entity = null and then Get_Name (Lang) = "Ada" then
         for J in Construct'Range loop
            --  ??? Ada Specific ... should use language service
            --  Need to define it !
            if Construct (J) = '.' then
               Current_Loc.Column :=
                 Basic_Types.Visible_Column_Type
                   (Loc.Column + J + 1 - Construct'First);

               Find_Declaration
                 (Db,
                  File_Name       => Get_Filename (File),
                  Entity_Name     => Construct (J + 1 .. Construct'Last),
                  Line            => Current_Loc.Line,
                  Column          => Current_Loc.Column,
                  Entity          => Entity,
                  Status          => Entity_Status,
                  Check_Decl_Only => False);

               exit when Entity /= null;
            end if;
         end loop;
      end if;

      return Entity;
   end Get_Declaration_Entity;

end Docgen2.Utils;
