------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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

with Language;      use Language;
with Language.Ada;  use Language.Ada;

package body GNATdoc.Backend.Base is

   -----------------------
   -- Get_Resource_File --
   -----------------------

   function Get_Resource_File
     (Self      : Base_Backend'Class;
      File_Name : GNATCOLL.VFS.Filesystem_String)
      return GNATCOLL.VFS.Virtual_File
   is
      Backend : constant Filesystem_String := Filesystem_String (Self.Name);
      Dir     : GNATCOLL.VFS.Virtual_File :=
        Self.Context.Kernel.Get_Share_Dir.Create_From_Dir
          ("gnatdoc").Create_From_Dir (Backend);
   begin
      --  Special case: check for this in order to be able to work
      --  in the development environment

      if not Dir.Is_Directory then
         Dir := Create_From_Dir
           (Get_Share_Dir (Self.Context.Kernel).Get_Parent,
            "gnatdoc/resources/" & Backend);
      end if;

      return Dir.Create_From_Dir (File_Name);
   end Get_Resource_File;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Backend : in out Base_Backend;
      Context : access constant Docgen_Context) is
   begin
      Backend.Context := Context;
   end Initialize;

   ------------------
   -- Process_File --
   ------------------

   overriding procedure Process_File
     (Self : in out Base_Backend;
      Tree : access Tree_Type)
   is
      procedure Generate_Documentation
        (Entity : Entity_Id; Scope_Level : Natural);

      ----------------------------
      -- Generate_Documentation --
      ----------------------------

      procedure Generate_Documentation
        (Entity : Entity_Id; Scope_Level : Natural)
      is
         procedure Classify_Entity
           (Entity   : Entity_Id;
            Parent   : Entity_Id;
            Entities : in out Collected_Entities);
         --  Classify the entity in one of the following categories: Method,
         --  subprogram, tagged type, record type, type, variable or package.

         ---------------------
         -- Classify_Entity --
         ---------------------

         In_Pkg_Generic_Formals : Boolean := True;

         procedure Classify_Entity
           (Entity   : Entity_Id;
            Parent   : Entity_Id;
            Entities : in out Collected_Entities) is
         begin
            --  Package generic formals are stored at the beginning of the
            --  list of entities

            if In_Pkg_Generic_Formals then
               if Is_Generic_Formal (Entity) then
                  Entities.Generic_Formals.Append (Entity);
                  Self.Entities.Generic_Formals.Append (Entity);
                  return;
               end if;

               In_Pkg_Generic_Formals := False;
            end if;

            if Is_Package (Entity) then
               Entities.Pkgs.Append (Entity);
               Self.Entities.Pkgs.Append (Entity);

            elsif Get_Kind (Entity) = E_Variable then
               Entities.Variables.Append (Entity);
               Self.Entities.Variables.Append (Entity);

            elsif LL.Is_Type (Entity) then
               if Get_Kind (Entity) = E_Class then
                  Entities.CPP_Classes.Append (Entity);
                  Self.Entities.CPP_Classes.Append (Entity);

               elsif Is_Tagged_Type (Entity) then
                  if Get_Kind (Entity) = E_Interface then
                     Entities.Interface_Types.Append (Entity);
                     Self.Entities.Interface_Types.Append (Entity);
                  else
                     Entities.Tagged_Types.Append (Entity);
                     Self.Entities.Tagged_Types.Append (Entity);
                  end if;

               elsif Is_Class_Or_Record_Type (Entity) then
                  Entities.Record_Types.Append (Entity);
                  Self.Entities.Record_Types.Append (Entity);

               elsif LL.Is_Access (Entity) then
                  Entities.Access_Types.Append (Entity);
                  Self.Entities.Access_Types.Append (Entity);

               else
                  Entities.Simple_Types.Append (Entity);
                  Self.Entities.Simple_Types.Append (Entity);
               end if;

            elsif LL.Is_Subprogram (Entity) then

               --  C/C++ macros unsupported yet???

               if Get_Kind (Entity) = E_Macro then
                  null;

               elsif Get_Kind (Parent) = E_Class
                 and then LL.Is_Primitive (Entity)
               then
                  --  This is not fully correct since we should check that
                  --  it is NOT defined as "void" (but this information is
                  --  not available in Xref ???)

                  if Get_Kind (Entity) = E_Procedure
                    and then Get_Short_Name (Entity) = Get_Short_Name (Parent)
                  then
                     Entities.CPP_Constructors.Append (Entity);
                     Self.Entities.CPP_Constructors.Append (Entity);
                  else
                     Append_Unique_Elmt (Entities.Methods, Entity);
                     Append_Unique_Elmt (Self.Entities.Methods, Entity);
                  end if;

               elsif In_Ada_Language (Entity) then
                  if LL.Is_Primitive (Entity) then
                     Append_Unique_Elmt (Entities.Methods, Entity);
                     Append_Unique_Elmt (Self.Entities.Methods, Entity);
                  else
                     Entities.Subprgs.Append (Entity);
                     Self.Entities.Subprgs.Append (Entity);
                  end if;

               else
                  Entities.Subprgs.Append (Entity);
                  Self.Entities.Subprgs.Append (Entity);
               end if;
            end if;
         end Classify_Entity;

         Entities : Collected_Entities;

      begin
         for Current of Get_Entities (Entity).all loop
            Classify_Entity (Current, Entity, Entities);
         end loop;

         Base_Backend'Class
           (Self).Generate_Lang_Documentation
           (Tree, Entity, Entities, Scope_Level);

         --  Handle nested Ada packages

         for Nested of Entities.Pkgs loop
            Generate_Documentation (Nested, Scope_Level + 1);
         end loop;

         --  Handle nested C++ classes

         for Nested of Entities.CPP_Classes loop
            Generate_Documentation (Nested, Scope_Level + 1);
         end loop;
      end Generate_Documentation;

      Lang   : constant Language_Access :=
        Self.Context.Lang_Handler.Get_Language_From_File (Tree.File);
      Is_Ada : constant Boolean         :=
        Lang.all in Language.Ada.Ada_Language'Class;

   begin
      if No (Tree.Tree_Root) then
         --  Skip files without entities information.

         return;

      elsif not Is_Ada and then Self.Context.Options.Skip_C_Files then
         --  Skip non-Ada files except when they are activated

         return;
      end if;

      Self.Src_Files.Append (Tree.File);

      if Is_Ada then
         Generate_Documentation
           (Get_Entities (Tree.Tree_Root).First_Element, 0);

      else
         Generate_Documentation (Tree.Tree_Root, 0);
      end if;
   end Process_File;

end GNATdoc.Backend.Base;
