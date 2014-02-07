------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2012-2014, AdaCore                  --
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
with GPS.Kernel.Contexts;
with Language;
with Language_Handlers;        use Language_Handlers;
with Language_Utils;           use Language_Utils;
with Xref;                     use Xref;

package body CodePeer.Module.Filters is

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Ada_Generic_Filter_Record;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);

      use type Language.Language_Category;
      use type GNATCOLL.Symbols.Symbol;
      use type Language.Construct_Access;

      Kernel     : constant GPS.Kernel.Kernel_Handle :=
        GPS.Kernel.Get_Kernel (Context);
      File_Name  : constant GNATCOLL.VFS.Virtual_File :=
        GPS.Kernel.Contexts.File_Information (Context);
      Languages  : constant Language_Handlers.Language_Handler :=
        GPS.Kernel.Get_Language_Handler (Kernel);
      Constructs : Language.Construct_List;
      Unit       : Language.Construct_Access;
      Entity     : General_Entity;

   begin
      Parse_File_Constructs
        (Get_Language_From_File (Languages, File_Name),
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

      Language.Free (Constructs);

      if Unit = null then
         return False;
      end if;

      --  Lookup for declaration of compilation unit.

      Entity := Kernel.Databases.Get_Entity
        (Name => "",
         Loc  => (File   => File_Name,
                  Line   => Unit.Sloc_Entity.Line,
                  Column => Basic_Types.Visible_Column_Type
                    (Unit.Sloc_Entity.Column
                     + GNATCOLL.Symbols.Get (Unit.Name)'Length - 1)));

      if Entity = No_General_Entity then
         return False;
      end if;

      return Kernel.Databases.Is_Generic (Entity);
   end Filter_Matches_Primitive;

end CodePeer.Module.Filters;
