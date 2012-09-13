------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2012, AdaCore                       --
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
with GNATCOLL.Symbols;

with Basic_Types;
with Entities.Queries;
with GPS.Kernel.Contexts;
with Language;
with Language_Handlers;

package body CodePeer.Module.Filters is

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Ada_Generic_Filter_Record;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);

      use type Entities.Entity_Information;
      use type Entities.File_Location;
      use type Language.Language_Category;
      use type GNATCOLL.Symbols.Symbol;
      use type Language.Construct_Access;

      Kernel     : constant GPS.Kernel.Kernel_Handle :=
        GPS.Kernel.Get_Kernel (Context);
      File_Name  : constant GNATCOLL.VFS.Virtual_File :=
        GPS.Kernel.Contexts.File_Information (Context);
      Languages  : constant Language_Handlers.Language_Handler :=
        GPS.Kernel.Get_Language_Handler (Kernel);
      Database   : constant Entities.Entities_Database :=
        GPS.Kernel.Get_Database (Kernel);
      Constructs : Language.Construct_List;
      Unit       : Language.Construct_Access;
      Entity     : Entities.Entity_Information;
      Status     : Entities.Queries.Find_Decl_Or_Body_Query_Status;
      Location   : Entities.File_Location;

   begin
      Entities.Parse_File_Constructs
        (Language_Handlers.Get_LI_Handler_From_File
           (Languages, File_Name),
         Languages,
         File_Name,
         Constructs);

      --  Lookup for the last reported package or subprogram, it represents
      --  compilation unit.

      Constructs.Current := Constructs.First;

      while Constructs.Current /= null loop
         if Constructs.Current.Name /= GNATCOLL.Symbols.No_Symbol
           and (Constructs.Current.Category = Language.Cat_Package
                  or Constructs.Current.Category = Language.Cat_Procedure
                  or Constructs.Current.Category = Language.Cat_Function)
         then
            Unit := Constructs.Current;
         end if;

         Constructs.Current := Constructs.Current.Next;
      end loop;

      if Unit = null then
         return False;
      end if;

      --  Lookup for declaration of compilation unit.

      Entities.Queries.Find_Declaration
        (Db              => Database,
         File_Name       => File_Name,
         Entity_Name     => "",
         Line            => Unit.Sloc_Entity.Line,
         Column          => Basic_Types.Visible_Column_Type
           (Unit.Sloc_Entity.Column
            + GNATCOLL.Symbols.Get (Unit.Name)'Length - 1),
         Entity          => Entity,
         Status          => Status,
         Check_Decl_Only => True);

      if Entity = null then
         return False;
      end if;

      if Entities.Get_Kind (Entity).Is_Generic then
         return True;
      end if;

      --  Lookup for declaration and check is it declaration of generic

      Location := Entities.Get_Declaration_Of (Entity);

      if Location = Entities.No_File_Location then
         return False;
      end if;

      Entities.Queries.Find_Declaration
        (Db              => Database,
         File_Name       => Entities.Get_Filename (Location.File),
         Entity_Name     => "",
         Line            => Location.Line,
         Column          => Location.Column,
         Entity          => Entity,
         Status          => Status,
         Check_Decl_Only => True);

      if Entity = null then
         return False;
      end if;

      return Entities.Get_Kind (Entity).Is_Generic;
   end Filter_Matches_Primitive;

end CodePeer.Module.Filters;
