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

with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.Symbols;         use GNATCOLL.Symbols;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with Basic_Types;              use Basic_Types;
with GPS.Kernel.Contexts;      use GPS.Kernel, GPS.Kernel.Contexts;
with Language;                 use Language;
with Language.Ada;             use Language.Ada;
with Language_Handlers;        use Language_Handlers;
with Language_Utils;           use Language_Utils;
with Xref;                     use Xref;

package body CodePeer.Module.Filters is

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   --  ??? The performance of the filter below is very bad, and it causes
   --  GPS to freeze when recomputing the menu validities
   --  (see automated test H624-007 for a reproducer)
   --  So we use these global variables as a cache for the latest file.
   --  We are making the assumption that a given file will not change from
   --  being not generic/separate to becoming one often. When this happens,
   --  the filter will fail, which is probably fine.
   Latest_File_Tested : Virtual_File := No_File;
   Latest_Result : Boolean := False;

   overriding function Filter_Matches_Primitive
     (Filter  : access Ada_Generic_Or_Separate_Filter_Record;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel     : constant Kernel_Handle := Get_Kernel (Context);
      File_Name  : constant Virtual_File := File_Information (Context);
      Languages  : constant Language_Handler := Get_Language_Handler (Kernel);
      Lang       : constant Language_Access :=
        Get_Language_From_File (Languages, File_Name);
      Constructs : Construct_List;
      Unit       : Construct_Access;

   begin
      --  Quick check on the language

      if Lang /= Ada_Lang then
         return False;
      end if;

      --  Quick check on whether we have a separate:

      if not Kernel.Registry.Tree.Root_Project.Is_Aggregate_Project
        and then Kernel.Registry.Tree.Info (File_Name).Unit_Part =
                   Unit_Separate
      then
         return True;
      end if;

      --  Look whether we have this in the immediate cache
      if File_Name = Latest_File_Tested then
         return Latest_Result;
      end if;

      --  We passed the cache test? cache this result.
      Latest_File_Tested := File_Name;

      --  Otherwise check if we have a generic

      Parse_File_Constructs
        (Lang,
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
            exit;
         end if;

         Constructs.Current := Constructs.Current.Next;
      end loop;

      if Unit = null then
         Free (Constructs);
         Latest_Result := False;
         return False;
      end if;

      --  Lookup for declaration of compilation unit.

      declare
         Entity : constant Root_Entity'Class := Kernel.Databases.Get_Entity
           (Name => Get (Unit.Name).all,
            Loc  => (File   => File_Name,
                     Project => Project_Information (Context),
                     Line   => Unit.Sloc_Entity.Line,
                     Column => Visible_Column_Type
                       (Unit.Sloc_Entity.Column)));
      begin
         Free (Constructs);

         if Entity = No_Root_Entity then
            Latest_Result := False;
            return False;
         end if;

         Latest_Result := Is_Generic (Entity);
         return Latest_Result;
      end;
   end Filter_Matches_Primitive;

end CodePeer.Module.Filters;
