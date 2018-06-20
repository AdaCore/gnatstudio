------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with String_Utils; use String_Utils;
with Language.Libclang_Tree; use Language.Libclang_Tree;
with clang_c_Index_h; use clang_c_Index_h;
with Libclang.File;

with Interfaces.C;
with GNATCOLL.Projects;
use GNATCOLL.Projects;
with Language.Libclang; use Language.Libclang;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with GPS.Core_Kernels; use GPS.Core_Kernels;
with GPS.Editors; use GPS.Editors;
with Basic_Types; use Basic_Types;
with Interfaces.C.Pointers;
with GPS.Kernel.Properties; use GPS.Kernel.Properties;
with GPS.Kernel; use GPS.Kernel;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with Language; use Language;
with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Conversion;

with Clang_Buffer_Facilities; use Clang_Buffer_Facilities;

package body Clang_Xref is

   use type Interfaces.C.unsigned;

   --  Quick design notes about clang cross references:
   --
   --  * Entities store a location, not a cursor. Either way that would mean
   --    that entities might not be valid after file modification. At least
   --    this way if the entity has not moved, the instance will still be valid
   --
   --  * A reference cache exists, but a lot of operations on entities use the
   --    translation units directly. Since we keep a limited number of
   --    translation units in memory at all times, it means that doing complex
   --    operations on every reference might, by definition, be costly in terms
   --    of computation power.

   Me : constant Trace_Handle := GNATCOLL.Traces.Create
     ("GPS.KERNEL.LIBCLANG");

   function Kernel return Core_Kernel is
     (Core_Kernel (Clang_Module_Id.Get_Kernel));
   --  Shortcut to get the kernel

   use Libclang.Index;
   use GNATCOLL.VFS;

   function Get_Clang_Cursor (E : Clang_Entity) return Clang_Cursor;
   function Get_Clang_Cursor
     (Kernel : Core_Kernel; Loc : General_Location) return Clang_Cursor;
   --  Returns the clang cursor associated to the entity, or to the general
   --  location. Used everywhere in clang_xref as a bridge to clang's
   --  translation units

   function To_General_Location
     (K : Core_Kernel; Clang_Loc : Clang_Location) return General_Location;
   --  Returns a general location corresponding to the clang_location. Handles
   --  line_offset -> column conversion

   function Type_As_Entity
     (From_Entity : Clang_Entity; Typ : Clang_Type) return Clang_Entity;
   --  Creates an entity from a Clang type

   function Cursor_As_Entity
     (Db : Clang_Database;
      Kernel : Core_Kernel;
      Cursor : Clang_Cursor) return Clang_Entity;
   --  Creates an entity from a Clang cursor

   function Cursor_As_Entity
     (From_Entity : Clang_Entity; Cursor : Clang_Cursor) return Clang_Entity
   is
     (Cursor_As_Entity (From_Entity.Db, Kernel, Cursor));
   --  Creates an entity from a Clang cursor

   function Methods
     (Entity : Clang_Entity) return Cursors_Arrays.Array_Type;
   --  For a given clang entity, returns a vector of clang cursors

   function Methods
     (Type_Decl : Clang_Cursor) return Cursors_Arrays.Array_Type;

   function To_Entity_Array
     (From_Entity : Clang_Entity;
      Cursors : Cursors_Arrays.Array_Type;
      Unique : Boolean := False) return Entity_Array;
   --  Convenience function to convert a vector of clang cursors to an array of
   --  clang entities

   function To_General_Location
     (K : Core_Kernel; F : Virtual_File;
      Offset : Offset_T) return General_Location;
   --  Convert a file offset to a general location, used to resolve references
   --  from the reference cache

   function Find_All_References_Local
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Include_Renames       : Boolean := True;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class;
   --  Helper function for find all references, for the case of a locally
   --  declared entity. It will also work, but is not currently used, for the
   --  case in which we're searching for a globally declared entity in a single
   --  file.

   function Find_All_References_Global
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Include_Renames       : Boolean := True;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class;
   --  Helper function for find all references, for the case of a public
   --  globally declared entity.

   function Overrides
     (Cursor : Clang_Cursor) return Cursors_Arrays.Array_Type;
   --  Returns an array of clang cursors, with a cursor for every entity which
   --  overrides Cursor

   function Parent_Types
     (C : Clang_Cursor; Recursive : Boolean) return Cursors_Arrays.Array_Type;
   --  Return an array of clang_cursors that is the chain of types that are
   --  parents of the type pointed to by C

   function Find_All_References_In_Scope
     (Entity : Clang_Entity;
      In_Scope : Clang_Entity) return Clang_Reference_Iterator;
   --  Return an iterator containing all the references to Entity in the scope
   --  of In_Scope, where In_Scope needs to be an entity with a Body.

   function Child_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Cursors_Arrays.Array_Type;
   --  Return an array of type that is the flattened tree of all the children
   --  types of Entity, where entity is a type. If Recursive is true, the
   --  search for child types will be recursive, and so the returned array will
   --  contain the whole tree of child types. If false, it will contain only
   --  the first level of the tree.

   use type Clang_Cursor_Kind;

   function Get_Line_Offset
     (K : Core_Kernel; L : General_Location) return Natural;
   --  Return the line offset corresponding to the Location L

   function Get_Body (Entity : Clang_Entity) return Clang_Entity;
   --  Helper to get the first body of an entity if entity is a decl

   function To_Root_Entity (Entity : Clang_Entity) return Root_Entity'Class;
   --  Converts a Clang_Entity to Root_Entity. In effect, this amounts to
   --  checking if the Clang_Entity is Null, and converting to the proper
   --  No_Root_Entity constant in that case

   function Get_Caller
     (Clang_Db : Clang_Database;
      Ref_Loc : General_Location) return Root_Entity'Class;
   --  Get the entity in which the call corresponding to the location Ref_Loc
   --  is made

   ---------
   -- "=" --
   ---------

   overriding function "=" (L, R : Clang_Entity) return Boolean is
   begin
      if L.Loc = R.Loc then
         return True;
      else
         --  The Locs are not equal, so if either of L or R is equal to
         --  No_Clang_Entity, then the entities are not the same by definition.
         if L.Loc = No_Location or R.Loc = No_Location then
            return False;
         end if;

         --  We want a special case in the equal function, so that the decl of
         --  a function is equal to the body of a function
         --  ??? Why not use the USR here from the start ?
         declare
            R_Loc, L_Loc : Clang_Raw_Location;
         begin
            R_Loc :=  Value (Location (Canonical (Get_Clang_Cursor (L))));
            L_Loc := Value (Location (Canonical (Get_Clang_Cursor (R))));
            return R_Loc = L_Loc;
         end;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Clang_Reference) return Boolean is
      Ret : Boolean;
   begin
      Ret := Left.Loc = Right.Loc;
      return Ret;
   end "=";

   overriding function "="
     (Left, Right : Clang_Reference_Iterator) return Boolean is
   begin
      return Left.Elements.Element (Left.Current_Index)
        = Right.Elements.Element (Right.Current_Index);
   end "=";

   ---------------------
   -- Get_Line_Offset --
   ---------------------

   function Get_Line_Offset
     (K : Core_Kernel; L : General_Location) return Natural
   is
      Line_Offset : constant Natural :=
        K.Get_Buffer_Factory.Get
          (L.File, Open_View => False, Focus => False, Open_Buffer => True)
        .New_Location (L.Line, L.Column).Line_Offset;

   begin
      return Line_Offset + 1;
   end Get_Line_Offset;

   ----------------------
   -- Get_Clang_Cursor --
   ----------------------

   function Get_Clang_Cursor (E : Clang_Entity) return Clang_Cursor
   is
      Ret : Clang_Cursor;
   begin
      Ret := Get_Clang_Cursor (Kernel, E.Ref_Loc);

      if Kind (Ret) = CXCursor_InclusionDirective then
         return Ret;
      end if;

      if Offset (Location (Ret)) = 0 then
         Ret := Get_Clang_Cursor (Kernel, E.Ref_Loc);
      else
         Ret := Referenced (Ret);
      end if;

      return Ret;
   end Get_Clang_Cursor;

   ----------------------
   -- Get_Clang_Cursor --
   ----------------------

   function Get_Clang_Cursor
     (Kernel : Core_Kernel; Loc : General_Location) return Clang_Cursor
   is
      Ret : Clang_Cursor;
      Line_Offset : constant Natural := Get_Line_Offset (Kernel, Loc);
      TU : constant Clang_Translation_Unit :=
        Translation_Unit (Kernel, Loc.File, Reparse => False);

   begin
      Ret := Cursor_At (TU, Loc.File, Loc.Line, Line_Offset);

      --  For some stdlib functions, libclang will present an unexposed_attr
      --  for the location at which the function is declared. A sufficient
      --  workaround is usually to get the cursor at one column before.

      --  NB: We never want to return an UnexposedAttr since it means it's
      --  unsupported anyway, so no harm in trying to recover a meaningful
      --  cursor

      if Ret.kind = CXCursor_UnexposedAttr then
         Ret := Cursor_At
           (TU, Loc.File, Loc.Line, Natural'Max (1, Line_Offset - 1));
      end if;

      --  If you ask for a cursor that is in the brackets section of an
      --  inclusion directive, like this:
      --
      --  #include <test.h>
      --             ^ ask for cursor here
      --
      --  Libclang will return a NoDeclFound cursor. We want to get the
      --  inclusion directive cursor, so test if we are in that case, and
      --  then return the proper cursor

      if Kind (Ret) = CXCursor_NoDeclFound then
         declare
            Tmp : constant Clang_Cursor
              := Cursor_At (TU, Loc.File, Loc.Line, 1);
         begin
            if Kind (Tmp) = CXCursor_InclusionDirective then
               Ret := Tmp;
            end if;
         end;
      end if;

      return Ret;
   end Get_Clang_Cursor;

   ----------------
   -- Get_Entity --
   ----------------

   overriding function Get_Entity
     (Db : Clang_Database;
      General_Db : General_Xref_Database;
      Name : String;
      Loc : General_Location) return Root_Entity'Class
   is
      --  Name is not needed here, since we can resolve the entity via
      --  location. Name could be used to verify that the user's request
      --  is coherent at a later stage though.

      --  General_Db is not used either. It could be used to fall back on the
      --  regular XRefs database, but we do not anticipate needing this

      pragma Unreferenced (Name, General_Db);

      C : constant Clang_Cursor := Get_Clang_Cursor (Kernel, Loc);
   begin
      if C = No_Cursor then

         --  No entity found

         return No_Root_Entity;
      else

         --  The cursor is used to resolve the final location of the entity.
         --  No cursor is stored though, because cursors are invalidated every
         --  time a TU is parsed.

         return Cursor_As_Entity (Clang_Entity'(Db => Db,
                                                others => <>), C);
      end if;
   end Get_Entity;

   --------------
   -- Is_Fuzzy --
   --------------

   overriding function Is_Fuzzy (Entity : Clang_Entity) return Boolean

   --  By design clang cross refs are never fuzzy

   is (False);

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Entity : Clang_Entity) return String

   --  Name is stored at entity creation

   is (+Entity.Name);

   ----------------------
   -- Get_Display_Kind --
   ----------------------

   overriding function Get_Display_Kind
     (Entity : Clang_Entity) return String

   --  Display kind is the pure clang kind as a string for the moment. Might
   --  want to have a better display kind at some point

   is (Spelling (Kind (Get_Clang_Cursor (Entity))));

   --------------------
   -- Qualified_Name --
   --------------------

   overriding function Qualified_Name
     (Entity : Clang_Entity) return String is
   begin

      --  USR is not really a qualified name per se, but it is unique per
      --  entity, which is what is needed in this case

      return Get_Name (Entity);
   end Qualified_Name;

   ----------
   -- Hash --
   ----------

   overriding function Hash
     (Entity : Clang_Entity) return Integer
   is
      Curs : constant Clang_Cursor := Get_Clang_Cursor (Entity);
      H : constant Hash_Type := Hash (Curs);

      --  Why on earth would you have a Hash function that returns an Integer
      --  I don't know. Normal conversion expectedly raises an exception on the
      --  high half of the Hash_Type so we use Unchecked_Conversion.

      function Convert
      is new Ada.Unchecked_Conversion (Hash_Type, Integer);
   begin
      return Convert (H);
   end Hash;

   -------------------------
   -- To_General_Location --
   -------------------------

   function To_General_Location
     (K : Core_Kernel; Clang_Loc : Clang_Location) return General_Location
   is
      Raw_Loc : constant Clang_Raw_Location := Value (Clang_Loc);
   begin
      declare

         --  We use editor buffers to resolve the offset to a (line, column)
         --  tuple. This might not be the most efficient way, but it is the
         --  easiest, and has proved not to be a bottleneck so far

         Loc : constant Editor_Location'Class :=
           K.Get_Buffer_Factory.Get
             (Raw_Loc.File, Open_View => False,
              Focus => False, Open_Buffer => True)
           .New_Location_Offset
             (Natural (Raw_Loc.Line),
              Character_Offset_Type (Raw_Loc.Column));

      begin
         return General_Location'
           (Raw_Loc.File,

            --  ??? Project is not used apparently, so no need to resolve it

            No_File,
            Loc.Line, Loc.Column);
      end;
   end To_General_Location;

   ----------------------
   -- Cursor_As_Entity --
   ----------------------

   function Cursor_As_Entity
     (Db : Clang_Database;
      Kernel : Core_Kernel;
      Cursor : Clang_Cursor) return Clang_Entity
   is
      Lang : constant String :=
        Kernel.Lang_Handler.Get_Language_From_File
          (GNATCOLL.VFS.Create
             (Filesystem_String
                    (To_String
                     (clang_getTranslationUnitSpelling
                          (clang_Cursor_getTranslationUnit (Cursor))))));

      --  As described in the intro, entities referencing runtime
      --  variables/constants store locations rather than cursors, and
      --  cursors are re-resolved every time via get_clang_cursor.

      Res : constant Clang_Entity :=
        Clang_Entity'
          (Db              => Db,
           Name            => +Spelling (Referenced (Cursor)),
           Loc             =>
             To_General_Location
               (Kernel,
                Location (Referenced (Cursor))),
           Ref_Loc         =>
             To_General_Location
               (Kernel,
                Location (Cursor)),
           Has_Type_Inst   => False,
           From_Lang       => +Lang,
           Clang_Type_Inst => <>);

   begin
      if Res.Loc.File /= No_File
        and then Kernel.Lang_Handler.Get_Language_From_File (Res.Loc.File) = ""
      then
         Set_Language_From_File
           (Kernel_Handle (Kernel), Res.Loc.File, Lang);
      end if;

      return (if Cursor /= No_Cursor then Res
              else No_Clang_Entity);
   end Cursor_As_Entity;

   ---------------------
   -- Get_Declaration --
   ---------------------

   overriding function Get_Declaration
     (Entity : Clang_Entity) return General_Entity_Declaration
   is
      Cursor : Clang_Cursor;
      Def : Clang_Cursor;
   begin

      --  There is a limitation here about built-in types, but no idea about
      --  how to solve that correctly ..

      if Entity.Loc = No_Location then
         return No_General_Entity_Declaration;
      end if;

      Cursor := Get_Clang_Cursor (Entity);

      --  Special case for inclusion directives
      if Kind (Cursor) = CXCursor_InclusionDirective then
         return General_Entity_Declaration'
           (Loc                      =>
              General_Location'
                (Libclang.File.File
                     (clang_getIncludedFile (Cursor)),
                 No_File,
                 1, 1),
            Name                     => +"",
            Body_Is_Full_Declaration => False);
      end if;

      --  In some cases we cannot get the cursor from a decl again, because
      --  (for example) it's from a file that doesn't make sense on its own. In
      --  this case we want to just return the location stored in the entity,
      --  since it's probably already the decl place

      if Cursor = No_Cursor then
         return General_Entity_Declaration'
           (Loc => Entity.Loc,
            Name => Entity.Name, Body_Is_Full_Declaration => False);
      end if;

      --  Get the referenced cursor, which gives us the declaration in most
      --  cases

      Def := Referenced (Cursor);

      if Kind (Def) = CXCursor_InvalidFile then
         Def := Cursor;
      end if;

      --  If the referenced cursor is the same as the original cursor, it means
      --  entity points to a function/method body, which is it's own entity.
      --  We need to get the canonical cursor, which is the first place where
      --  entity has been declared

      if Cursor = Def then
         Def := Canonical (Cursor);
      end if;

      return General_Entity_Declaration'
        (Loc => To_General_Location (Kernel, Location (Def)),
         Name => +Spelling (Def),
         Body_Is_Full_Declaration => Is_Definition (Def));
   end Get_Declaration;

   ---------------------------
   -- Caller_At_Declaration --
   ---------------------------

   overriding function Caller_At_Declaration
     (Entity : Clang_Entity) return Root_Entity'Class
   is
   begin
      return Get_Caller (Entity.Db, Entity.Get_Declaration.Loc);
   end Caller_At_Declaration;

   -------------------------
   -- To_General_Location --
   -------------------------

   function To_General_Location
     (K : Core_Kernel;
      F : Virtual_File;
      Offset : Offset_T) return General_Location
   is
      Line   : Integer;
      Column : Visible_Column_Type;
   begin
      Offset_To_Line_Column (K, F, Integer (Offset), Line, Column);
      return General_Location'
        (F,
         No_File,
         Line, Column);
   end To_General_Location;

   --------------
   -- Get_Body --
   --------------

   overriding function Get_Body
     (Entity : Clang_Entity;
      After  : General_Location := No_Location)
      return General_Location
   is
      pragma Unreferenced (After);
   begin
      if Entity = No_Clang_Entity then
         return No_Location;
      end if;

      declare

         use Clang_Symbol_Table_Pkg;

         --  Get the clang context containing the entity references cache, as
         --  well as the cursor's USR, so as to be able to query the ref cache

         Cursor      : constant Clang_Cursor := Get_Clang_Cursor (Entity);
         USR_Sym     : constant Clang_Symbol :=
           Clang_Symbol_Table.Find (USR (Cursor));

         --  Try to use Definition from libclang. It will only work as long
         --  as the definition is in the same translation unit though, so not
         --  sufficient

         Entity_Body : constant Clang_Cursor := Definition (Cursor);

         --  V is used to store an vector of entity references. Used while
         --  scanning for the body

         V           : Info_Vectors;

         --  Return value holders
         Loc         : Offset_T := 0;
         File        : Virtual_File;

      begin
         if Cursor = No_Cursor or Get (USR_Sym) = "" then
            return No_Location;
         end if;

         if Entity_Body /= No_Cursor then
            return To_General_Location (Kernel, Location (Entity_Body));
         else

            --  TODO ??? It might be interesting to have a toplevel cache that
            --  stores whether a particular USR has a body somewhere in the
            --  project or not (and maybe even where).

            --  Iterate on every file's cache
            Toplevel_Loop : for C of Get_Active_Files loop

               --  If this file's cache contains some references to the entity
               --  for which we're trying to find the body, then explore those
               --  refs to see if one is a body. We might store the information
               --  on the entity's cache for the file, so as to avoid
               --  unnecessary iterations, but this has not been proved
               --  a bottleneck.

               if C.Map.Contains (USR_Sym) then

                  V := C.Map.Element (USR_Sym);

                  for I in V.Decls.First_Index .. V.Decls.Last_Index loop
                     --  If a reference is a body definition, then store the
                     --  corresponding location, which will make the nested
                     --  loops exit

                     if V.Decls.Element (I).Is_Def then
                        Loc := V.Decls.Element (I).Loc;
                        File := GNATCOLL.VFS.Create
                          (Filesystem_String (+C.File_Name));
                        exit Toplevel_Loop;
                     end if;

                  end loop;
               end if;
            end loop Toplevel_Loop;

            return To_General_Location (Kernel, File, Loc);

         end if;
      end;
   end Get_Body;

   --------------
   -- Get_Body --
   --------------

   function Get_Body (Entity : Clang_Entity) return Clang_Entity
   is
   begin
      return Cursor_As_Entity
        (Entity, Get_Clang_Cursor (Kernel, Get_Body (Entity)));
   end Get_Body;

   -----------------
   -- Get_Type_Of --
   -----------------

   overriding function Get_Type_Of
     (Entity : Clang_Entity) return Root_Entity'Class
   is
   begin

      --  Return a type entity for Entity. Note that we might want to return
      --  Entity for the case in which Entity is a type entity. In this case
      --  Get_Clang_Cursor won't work and this will crash

      return Type_As_Entity (Entity, Get_Type (Get_Clang_Cursor (Entity)));
   end Get_Type_Of;

   --------------------
   -- Type_As_Entity --
   --------------------

   function Type_As_Entity
     (From_Entity : Clang_Entity; Typ : Clang_Type) return Clang_Entity
   is
   begin

      --  Return a type based entity. When resolving, this will use the type.
      --  Note that those entities will NEVER work after a reparse, unlike
      --  location based entities that might work if the location is still
      --  valid, so those are best used as transient entities.

      --  ??? TODO : For that reason, we might want to resolve those entities
      --  to a location when we can, and let As_Type do its job.

      return
        Clang_Entity'(From_Entity.Db,
                      +Spelling (Typ),
                      To_General_Location
                        (Kernel, Location (Declaration (Typ))),
                      No_Location,
                      From_Entity.From_Lang,
                      True, Typ);
   end Type_As_Entity;

   -------------------
   -- Returned_Type --
   -------------------

   overriding function Returned_Type
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      Cursor : constant Clang_Cursor :=
        Get_Clang_Cursor (Kernel, Entity.Get_Declaration.Loc);
      T : Clang_Type;
   begin

      --  Only functions have a return type

      if Is_Function (Kind (Cursor)) then
         T := Result_Type (Get_Type (Cursor));
         return Type_As_Entity (Entity, T);
      end if;

      return No_Root_Entity;
   end Returned_Type;

   --------------------
   -- Parent_Package --
   --------------------

   overriding function Parent_Package
     (Entity : Clang_Entity) return Root_Entity'Class is
      pragma Unreferenced (Entity);
   begin

      --  No packages in C/C++, maybe make it work for namespaces ??? Look up
      --  how C++ namespaces work.

      return No_Root_Entity;
   end Parent_Package;

   -------------
   -- As_Type --
   -------------

   function As_Type (Entity : Clang_Entity) return Clang_Type

   --  Abstract type based entities, by enabling to retrieve the type of entity
   --  for location based and for type based entities

   is (if Entity.Has_Type_Inst then Entity.Clang_Type_Inst
       else Get_Type (Get_Clang_Cursor (Entity)));

   -------------
   -- Methods --
   -------------

   function Methods
     (Type_Decl : Clang_Cursor) return Cursors_Arrays.Array_Type
   is
   begin
      if Is_Object_Type (Kind (Type_Decl)) then

         --  Straightforwardly get every children of decl that is a method

         return Get_Children (Type_Decl, CXCursor_CXXMethod);

      else
         return Cursors_Arrays.Empty_Array;
      end if;

   end Methods;

   -------------
   -- Methods --
   -------------

   function Methods
     (Entity : Clang_Entity) return Cursors_Arrays.Array_Type
   is
      T_Decl : constant Clang_Cursor := Declaration (As_Type (Entity));
   begin
      return Methods (T_Decl);
   end Methods;

   ------------------
   -- Pointed_Type --
   ------------------

   overriding function Pointed_Type
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      Res_T : constant Clang_Type := Pointee_Type (As_Type (Entity));
   begin

      --  We don't check wether As_Type (Entity) is a pointer type, just if the
      --  result is valid

      if Res_T.kind = CXType_Invalid then
         return No_Root_Entity;
      else
         return Type_As_Entity (Entity, Res_T);
      end if;
   end Pointed_Type;

   -----------------
   -- Renaming_Of --
   -----------------

   overriding function Renaming_Of
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      T : constant Clang_Type := As_Type (Entity);
   begin

      --  Only kind of entity which can be assimilated to a renaming in C/C++
      --  is the typedef, so handle this case.

      if T.kind = CXType_Typedef then
         return Type_As_Entity
           (Entity, Typedef_Underlying_Type (Declaration (T)));
      end if;

      return No_Root_Entity;
   end Renaming_Of;

   ---------------------
   -- Is_Primitive_Of --
   ---------------------

   overriding function Is_Primitive_Of
     (Entity : Clang_Entity) return Entity_Array
   is
      Method_Cursor : constant Clang_Cursor := Get_Clang_Cursor (Entity);

      use Cursors_Arrays;

      function Method_Is_Not_Overridden_In_Type
        (Type_Decl : Clang_Cursor) return Boolean
      is
        (not (for some C of Array_Type'(Methods (Type_Decl))
              => Contains (Overrides (C), Method_Cursor)));

      Method_Type : constant Clang_Cursor := Semantic_Parent (Method_Cursor);

      function Internal
        (T : Clang_Cursor) return Array_Type;

      function Internal
        (T : Clang_Cursor) return Array_Type
      is
         Children_Types : constant Array_Type :=
           Filter (Child_Types (Cursor_As_Entity (Entity, T), False),
                   Method_Is_Not_Overridden_In_Type'Access);
      begin
         return Children_Types & Id_Flat_Map (Children_Types, Internal'Access);
      end Internal;

   begin
      return To_Entity_Array
        (Entity, Array_Type'(1 => Method_Type) & Internal (Method_Type));
   end Is_Primitive_Of;

   -----------------
   -- Has_Methods --
   -----------------

   overriding function Has_Methods (E : Clang_Entity) return Boolean
   is
   begin

      --  Every object type might potentially have methods

      return Is_Object_Type (Kind (Get_Clang_Cursor (E)));
   end Has_Methods;

   ---------------
   -- Is_Access --
   ---------------

   overriding function Is_Access (E : Clang_Entity) return Boolean
   is
      T : constant Clang_Type := As_Type (E);
      use clang_c_Index_h;
   begin

      --  A type might be an access in the Ada sense either if it is a literal
      --  pointer type, or if it is a typedef whose underlying type is a
      --  pointer type.

      return T.kind = CXType_Pointer
        or else (T.kind = CXType_Typedef and then
                 Typedef_Underlying_Type
                   (Declaration (T)).kind = CXType_Pointer);
   end Is_Access;

   -----------------
   -- Is_Abstract --
   -----------------

   overriding function Is_Abstract
     (E  : Clang_Entity) return Boolean
   is
     (for some M of Methods (E) => Is_Method_Pure_Virtual (M));

   --------------
   -- Is_Array --
   --------------

   overriding function Is_Array
     (E  : Clang_Entity) return Boolean
   is
   begin
      return Is_Array (As_Type (E).kind);
   end Is_Array;

   ------------------------------
   -- Is_Printable_In_Debugger --
   ------------------------------

   overriding function Is_Printable_In_Debugger
     (E : Clang_Entity) return Boolean is
      pragma Unreferenced (E);
   begin

      --  Let's assume everything is printable in debugger for now

      return True;

   end Is_Printable_In_Debugger;

   -------------
   -- Is_Type --
   -------------

   overriding function Is_Type
     (E  : Clang_Entity) return Boolean
   is
     (E.Has_Type_Inst or else Is_Type (Get_Clang_Cursor (E).kind));

   -------------------
   -- Is_Subprogram --
   -------------------

   overriding function Is_Subprogram
     (E  : Clang_Entity) return Boolean is
   begin
      return Is_Subprogram (Get_Clang_Cursor (E).kind);
   end Is_Subprogram;

   ------------------
   -- Is_Container --
   ------------------

   overriding function Is_Container
     (E  : Clang_Entity) return Boolean is
   begin
      return Is_Container (Get_Clang_Cursor (E).kind);
   end Is_Container;

   ----------------
   -- Is_Generic --
   ----------------

   overriding function Is_Generic
     (E  : Clang_Entity) return Boolean is
   begin
      return Is_Generic (Get_Clang_Cursor (E).kind);
   end Is_Generic;

   ---------------
   -- Is_Global --
   ---------------

   overriding function Is_Global
     (E  : Clang_Entity) return Boolean
   is
      function Internal (C : Clang_Cursor) return Boolean;
      function Internal (C : Clang_Cursor) return Boolean is
         P : constant Clang_Cursor := Lexical_Parent (C);
      begin

         --  An entity is a global if its parent is the translation unit.

         return P.kind in CXCursor_TranslationUnit | CXCursor_Namespace
           or else (Is_Type (P.kind) and then Internal (P));
      end Internal;
   begin
      return Internal (Get_Clang_Cursor (E));
   end Is_Global;

   ---------------------
   -- Is_Static_Local --
   ---------------------

   overriding function Is_Static_Local
     (E  : Clang_Entity) return Boolean
   is
   begin
      return Linkage (Get_Clang_Cursor (E)) = CXLinkage_Internal;
   end Is_Static_Local;

   --------------------------
   -- Is_Predefined_Entity --
   --------------------------

   overriding function Is_Predefined_Entity
     (E  : Clang_Entity) return Boolean
   is
      Typ : constant Clang_Type := As_Type (E);
   begin
      return Typ.kind in CXType_FirstBuiltin .. CXType_LastBuiltin
        --  Consider "void ()" as a predefined entity
        or else (Typ.kind = CXType_FunctionProto
                   and then E.Loc.File = No_File);
   end Is_Predefined_Entity;

   -------------------
   -- Documentation --
   -------------------

   overriding procedure Documentation
     (Handler           : Language_Handlers.Language_Handler;
      Entity            : Clang_Entity;
      Formater          : access Profile_Formater'Class;
      Check_Constructs  : Boolean := True;
      Look_Before_First : Boolean := True)
   is

      --  Constructs are irrelevant, and we might ponder if we want such an
      --  implementation detail in the XRef interface at some point, eg. is
      --  this really needed ? why not check constructs in any case in the
      --  ada case anyway ?

      --  Handler is not needed, apparently not needed in the ada case either,
      --  maybe remove at some point ???

      --  Look_Before_First => implementation detail too .. not needed with
      --  libclang

      pragma Unreferenced (Handler, Check_Constructs, Look_Before_First);

      --  We'll need the buffer to get the comment's text.

      Buf : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (Entity.Loc.File, Open_View => False, Focus => False);

      --  We get the range for the comment associated with the entity's cursor

      Doc_Range : constant Clang_Source_Range :=
        Comment_Range (Get_Clang_Cursor (Entity));
      Loc_Start, Loc_End : Sloc_T;
   begin

      --  Store the range

      Loc_Start := To_Sloc_T (Range_Start (Doc_Range));
      Loc_End := To_Sloc_T (Range_End (Doc_Range));

      --  Add the doc to the formater. TODO ??? Very raw, at least remove stars
      --  and everything, at best use doxygen format to format the doc to html
      --  as it is the standard in C/C++ world. Also maybe add profile info.

      Formater.Add_Comments
        (Buf.Get_Chars (Buf.New_Location (Loc_Start.Line, Loc_Start.Column),
         Buf.New_Location (Loc_End.Line, Loc_End.Column)));
   end Documentation;

   ------------------
   -- End_Of_Scope --
   ------------------

   overriding function End_Of_Scope
     (Entity : Clang_Entity) return General_Location

   --  The end of scope is the end of the extent for the corresponding cursor.

   is
     (To_General_Location
        (Kernel,
         Range_End
           (Extent (Get_Clang_Cursor (Entity)))));

   ---------------------
   -- Is_Parameter_Of --
   ---------------------

   overriding function Is_Parameter_Of
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin

      --  Since we store the declaration of the cursor in entity, if it's a
      --  parameter, its kind should be parmdecl

      if C.kind = CXCursor_ParmDecl then
         return Cursor_As_Entity
           (Entity, Semantic_Parent (C));
      end if;

      return No_Root_Entity;
   end Is_Parameter_Of;

   ---------------
   -- Overrides --
   ---------------

   function Overrides
     (Cursor : Clang_Cursor) return Cursors_Arrays.Array_Type
   is

      type Cursor_Ptr is access Clang_Cursor;
      pragma Convention (C, Cursor_Ptr);

      type C_Cursor_Array is array (Natural range <>) of aliased Clang_Cursor;
      package C_Cursor_Ptrs is new Interfaces.C.Pointers
        (Natural, Clang_Cursor, C_Cursor_Array, No_Cursor);

      use Interfaces.C;

      --  We import the clang procedure manually because the signature
      --  generated by fdump-ada-spec does not suit us.

      procedure Get_Overridden_Cursors
        (Cursor : Clang_Cursor;
         Overridden : access Cursor_Ptr;
         Num_Overridden : access unsigned);
      pragma Import (C, Get_Overridden_Cursors, "clang_getOverriddenCursors");

      C : aliased Cursor_Ptr;
      Nb_Cursors : aliased unsigned;

      C_Ptr : C_Cursor_Ptrs.Pointer;

   begin

      --  We only get the first cursor, because this is all that is needed
      --  for fullfilling the xref Overrides function, but we might want
      --  to decouple that into an helper at some point, and use it for
      --  Is_Primitive_Of

      Get_Overridden_Cursors (Cursor, C'Access, Nb_Cursors'Access);

      C_Ptr := C_Cursor_Ptrs.Pointer (C);

      declare
         Cursors : Cursors_Arrays.Array_Type (1 .. Positive (Nb_Cursors));
         Current : Positive := 1;
         use C_Cursor_Ptrs;
      begin
         while Current <= Positive (Nb_Cursors) loop
            Cursors (Current) := C_Ptr.all;
            Current := Current + 1;
            C_Ptr := C_Ptr + 1;
         end loop;

         Dispose_Overridden (C);

         return Cursors;
      end;
   end Overrides;

   ---------------
   -- Overrides --
   ---------------

   overriding function Overrides
     (Entity : Clang_Entity) return Root_Entity'Class
   is
   begin
      --  Get the first of the overridden cursors

      return Cursor_As_Entity
        (Entity, Overrides (Get_Clang_Cursor (Entity)) (1));
   end Overrides;

   -----------------
   -- Instance_Of --
   -----------------

   overriding function Instance_Of
     (Entity : Clang_Entity) return Root_Entity'Class is (No_Root_Entity);

   -------------
   -- Methods --
   -------------

   overriding function Methods
     (Entity            : Clang_Entity;
      Include_Inherited : Boolean) return Entity_Array
   is
      pragma Unreferenced (Include_Inherited);
   begin
      return To_Entity_Array (Entity, Methods (Entity));
   end Methods;

   ---------------------
   -- To_Entity_Array --
   ---------------------

   function To_Entity_Array
     (From_Entity : Clang_Entity;
      Cursors : Cursors_Arrays.Array_Type;
      Unique : Boolean := False) return Entity_Array
   is
      Ret : Entity_Array (1 .. Natural (Cursors'Length));
   begin

      --  Do a straightforward conversion of cursors to entity pointers, and
      --  store that into the return array

      for I in Ret'Range loop
         Ret (I) :=
           new Clang_Entity'(Cursor_As_Entity (From_Entity, Cursors (I)));
      end loop;

      return (if Unique then Entity_Arrays.Unique (Ret)
              else Ret);
   end To_Entity_Array;

   ------------
   -- Fields --
   ------------

   overriding function Fields
     (Entity            : Clang_Entity) return Entity_Array
   is
      use Cursors_Arrays;

      function Get_Fields (C : Clang_Cursor) return Array_Type;
      function Get_Fields (C : Clang_Cursor) return Array_Type
      is
      begin
         return (Get_Children (C, CXCursor_FieldDecl)
                 & Id_Flat_Map (Parent_Types (C, True), Get_Fields'Access));
      end Get_Fields;

      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin
      if Is_Object_Type (C.kind) then

         --  Make an array of the field declarations

         return To_Entity_Array (Entity, Unique (Get_Fields (C)));
      end if;

      return No_Entity_Array;
   end Fields;

   --------------
   -- Literals --
   --------------

   overriding function Literals
     (Entity            : Clang_Entity) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin
      return To_Entity_Array
        (Entity, Get_Children (C, CXCursor_EnumConstantDecl));
   end Literals;

   -----------------------
   -- Formal_Parameters --
   -----------------------

   overriding function Formal_Parameters
     (Entity            : Clang_Entity) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin

      if Entity.Is_Generic then
         return To_Entity_Array
           (Entity, Get_Children (C, CXCursor_TemplateTypeParameter));
      end if;

      return No_Entity_Array;
   end Formal_Parameters;

   ---------------------
   -- Discriminant_Of --
   ---------------------

   overriding function Discriminant_Of
     (Entity            : Clang_Entity) return Root_Entity'Class
   is
      --  Not applicable to C/C++
      (No_Root_Entity);

   -------------------
   -- Discriminants --
   -------------------

   overriding function Discriminants
     (Entity            : Clang_Entity) return Entity_Array
   is
      --  Not applicable to C/C++
     (No_Entity_Array);

   --------------------
   -- Component_Type --
   --------------------

   overriding function Component_Type
     (Entity : Clang_Entity) return Root_Entity'Class
   is
   begin
      if Entity.Is_Array then
         return Type_As_Entity
           (Entity, Element_Type (As_Type (Entity)));
      end if;
      return No_Root_Entity;
   end Component_Type;

   -----------------
   -- Index_Types --
   -----------------

   overriding function Index_Types

   --  Index type is a builtin type in C, and our entity model is
   --  not well suited for keeping info about built in types (no declaration
   --  location).

     (Entity : Clang_Entity) return Entity_Array is (No_Entity_Array);

   -----------------
   -- Child_Types --
   -----------------

   overriding function Child_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Entity_Array
   is
     (To_Entity_Array (Entity, Child_Types (Entity, Recursive)));

   function Child_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Cursors_Arrays.Array_Type
   is
      function Child_Types_Rec
        (C : Clang_Cursor) return Cursors_Arrays.Array_Type
      is (Child_Types (Cursor_As_Entity (Entity, C), True));

      use Cursors_Arrays;

      Refs : constant Clang_Entity_Ref_Vector
        := Clang_Reference_Iterator (Find_All_References (Entity)).Elements;

      Cursors : Cursors_Arrays.Array_Type (1 .. Positive (Refs.Length));
      Count : Positive := 1;

      function Get_Class_For_Base
        (Base_Cursor : Clang_Cursor) return Clang_Cursor;
      --  Clang is stupid, it cannot get the parent of a base specifier when
      --  you got the cursor to the base specifier from a from_location call.
      --  For this reason we'll trick clang to get the class, having the
      --  guarantee that a location just outside of the base specifier's
      --  span will refer to the class decl.

      function Get_Class_For_Base
        (Base_Cursor : Clang_Cursor) return Clang_Cursor
      is
         Loc : General_Location :=
           To_General_Location (Kernel, Location (Base_Cursor));
         use type Basic_Types.Visible_Column_Type;
      begin
         Loc.Column := Loc.Column - 1;
         return Get_Clang_Cursor (Kernel, Loc);
      end Get_Class_For_Base;

   begin
      for Ref of Refs.all loop
         if Ref.Kind = CXCursor_CXXBaseSpecifier then
            Cursors (Count) :=
              Get_Clang_Cursor (Kernel, Ref.Get_Location);
            Count := Count + 1;
         end if;
      end loop;

      declare
         Children : constant Cursors_Arrays.Array_Type :=
           Id_Map (Cursors (1 .. Count - 1), Get_Class_For_Base'Access);
      begin
         return (if Recursive
                 then Children & Cursors_Arrays.Id_Flat_Map
                   (Children, Child_Types_Rec'Access)
                 else Children);
      end;
   end Child_Types;

   ------------------
   -- Parent_Types --
   ------------------

   function Parent_Types
     (C : Clang_Cursor; Recursive : Boolean) return Cursors_Arrays.Array_Type
   is
      use Cursors_Arrays;

      function Get_Base_Classes (Type_Decl : Clang_Cursor) return Array_Type;
      function Get_Base_Classes (Type_Decl : Clang_Cursor) return Array_Type
      is
      begin
         return (Id_Map
                  (Get_Children (Type_Decl, CXCursor_CXXBaseSpecifier),
                   Referenced'Access));
      end Get_Base_Classes;

      function Get_Ancestors (Type_Decl : Clang_Cursor) return Array_Type;
      function Get_Ancestors (Type_Decl : Clang_Cursor) return Array_Type
      is
         Base_Classes : constant Array_Type := Get_Base_Classes (Type_Decl);
      begin
         return Base_Classes
           & Id_Flat_Map (Base_Classes, Get_Ancestors'Access);
      end Get_Ancestors;

   begin
      return (if Recursive then Get_Ancestors (C) else Get_Base_Classes (C));
   end Parent_Types;

   ------------------
   -- Parent_Types --
   ------------------

   overriding function Parent_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin
      return
        (if Is_Object_Type (C.kind)
         then To_Entity_Array (Entity, Parent_Types (C, Recursive))
         else No_Entity_Array);
   end Parent_Types;

   ----------------
   -- Parameters --
   ----------------

   overriding function Parameters
     (Entity : Clang_Entity) return Parameter_Array

   --  TODO ??? Implement

   is (No_Parameters);

   ---------------------------
   -- Clang_Entities_Cursor --
   ---------------------------

   type Clang_Entities_Cursor (Nb_Entities : Positive)
   is new Abstract_Entities_Cursor with record
      Entities_Array : Entity_Array (1 .. Nb_Entities);
      Current : Positive := 1;
   end record;

   overriding procedure Destroy (Iter : in out Clang_Entities_Cursor) is null;
   overriding procedure Next (Iter : in out Clang_Entities_Cursor);

   overriding function At_End
     (Iter : Clang_Entities_Cursor) return Boolean
   is
     (Iter.Current > Iter.Entities_Array'Length);

   overriding function Get
     (Iter : Clang_Entities_Cursor) return Root_Entity'Class
   is
     (Iter.Entities_Array (Iter.Current).all);

   overriding procedure Next (Iter : in out Clang_Entities_Cursor)
   is
   begin
      Iter.Current := Iter.Current + 1;
   end Next;

   -----------------------------
   -- Get_All_Called_Entities --
   -----------------------------

   overriding function Get_All_Called_Entities
     (Entity : Clang_Entity) return Abstract_Entities_Cursor'Class
   is
      use Cursors_Arrays;

      function Get_Real_Ref (C : Clang_Cursor) return Clang_Cursor
      is
        (
         --  When the call expr references a method, we actually want to get
         --  the member reference expression to have correct locations.
         if Kind (Referenced (C)) = CXCursor_CXXMethod
         then Get_Children (C, CXCursor_MemberRefExpr) (1)
         else C);

      function Get_Calls (C : Clang_Cursor) return Array_Type;
      function Get_Calls (C : Clang_Cursor) return Array_Type
      is
      begin
         return (Get_Children (C, CXCursor_CallExpr)
                 & Id_Flat_Map (Get_Children (C), Get_Calls'Access));
      end Get_Calls;

      CC : constant Clang_Cursor := Get_Clang_Cursor (Entity);

      Body_Entity : Clang_Entity := Entity;
      Body_CC : Clang_Cursor := CC;

   begin
      if Kind (CC) = CXCursor_FunctionDecl then
         Body_Entity := Get_Body (Entity);
         Body_CC := Get_Clang_Cursor (Body_Entity);
      end if;

      declare
         Calls : constant Array_Type :=
           Id_Map (Get_Calls (Body_CC), Get_Real_Ref'Access);

         Call_Entities : constant Entity_Array := To_Entity_Array
           (Body_Entity, Calls, Unique => True);
      begin
         return
           (if Call_Entities'Length = 0 then No_Entities_Cursor
            else Clang_Entities_Cursor'
              (Call_Entities'Length, Call_Entities, 1));
      end;
   end Get_All_Called_Entities;

   -------------------------
   -- Find_All_References --
   -------------------------

   overriding function Find_All_References
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Include_Renames       : Boolean := True;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class
   is
      Dummy_Use_Index_Context_Manager : Use_Index;
      --  This variable takes a lock on the index so that it is not modified
      --  while we search it
   begin
      Trace (Me, "Find all references for entity " & (+Entity.Name));

      --  This function is cut in three cases: Finding references for an
      --  entity declared in the global scope, finding references for an
      --  entity declared in the local scope, and finding references into the
      --  limited scope of a specific entity. This is due to the fact that the
      --  reference cache only stores references to global entities, for space
      --  reasons.

      if In_Scope /= No_Root_Entity then
         return Find_All_References_In_Scope (Entity, Clang_Entity (In_Scope));
      elsif Entity.Is_Global then
         return Find_All_References_Global
           (Entity, In_File, In_Scope, Include_Overriding, Include_Overridden,
            Include_Implicit, Include_All, Include_Renames, Kind);
      else
         return Find_All_References_Local
           (Entity, In_File, In_Scope, Include_Overriding, Include_Overridden,
            Include_Implicit, Include_All, Include_Renames, Kind);
      end if;
   end Find_All_References;

   use GNATCOLL.VFS;

   ----------------------------------------------
   --  Local indexer declaration and functions --
   ----------------------------------------------

   --  For find all references for a locally declared variable, we cannot
   --  use the cache so we create a new indexer that we will run only on the
   --  current translation unit, but this time indexing local declarations too.

   type Indexer_Data is record
      Ret_Iterator  : access Clang_Reference_Iterator;
      Clang_Db      : Clang_Database;
      File          : Virtual_File;
      Sought_Cursor : Clang_Cursor;
      Entity_Name   : Symbol;
   end record;

   procedure Index_Declaration
     (Client_Data : in out Indexer_Data; Info : Clang_Decl_Info);
   procedure Index_Reference
     (Client_Data : in out Indexer_Data; Info : Clang_Ref_Info);

   function Abort_Query
     (Dummy_Client_Data : in out Indexer_Data) return Boolean is (False);
   procedure Diagnostic
     (Client_Data : in out Indexer_Data;
      Diagnostics : Clang_Diagnostic_Set) is null;
   procedure Entered_Main_File
     (Client_Data : in out Indexer_Data; File : Virtual_File) is null;
   procedure Included_File
     (Client_Data : in out Indexer_Data;
      Included_File_Info : Clang_Included_File_Info) is null;
   procedure Started_Translation_Unit
     (Client_Data : in out Indexer_Data) is null;

   procedure Index_Declaration
     (Client_Data : in out Indexer_Data;
      Info        : Clang_Decl_Info)
   is
      Loc : constant Clang_Location := +Info.loc;
   begin

      --  We only want to index decls in main file

      if Is_From_Main_File (Loc)
      and then
        Clang_Cursor
          (Info.entityInfo.cursor) = Client_Data.Sought_Cursor

      --  Make sure this is the cursor we're searching for

      then
         Client_Data.Ret_Iterator.Elements.Append
           (Clang_Reference'
              (To_General_Location (Kernel, Loc),
               Client_Data.Entity_Name, Client_Data.Clang_Db,
               Info.cursor.kind));
      end if;
   end Index_Declaration;

   procedure Index_Reference
     (Client_Data : in out Indexer_Data;
      Info   : Clang_Ref_Info)
   is
      Loc : constant Clang_Location := +Info.loc;
   begin

      --  We only want to index decls in main file

      if Is_From_Main_File (Loc) then

         --  Make sure this is the cursor we're searching for

         if Clang_Cursor
           (Info.referencedEntity.cursor) = Client_Data.Sought_Cursor
         then
            Client_Data.Ret_Iterator.Elements.Append
              (Clang_Reference'
                 (To_General_Location (Kernel, Loc),
                  Client_Data.Entity_Name, Client_Data.Clang_Db,
                  Info.cursor.kind));
         end if;
      end if;
   end Index_Reference;

   package Indexer is new Source_File_Indexer
     (Client_Data_T => Indexer_Data);

   -------------------------------
   -- Find_All_References_Local --
   -------------------------------

   function Find_All_References_Local
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Include_Renames       : Boolean := True;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class
   is
      pragma Unreferenced
        (In_Scope, Include_Overriding, Include_Overridden,
         Include_Implicit, Include_All, Include_Renames, Kind, In_File);
      --  TODO ??? Options are not handled yet

      --  We want to store entity names as symbols for convenience, use the
      --  Clang context symbol table

      Entity_Name : constant Symbol :=
        Clang_Symbol_Table.Find (Entity.Get_Name);

      --  Construct the return value, with an empty vector

      Ret : aliased Clang_Reference_Iterator :=
        Clang_Reference_Iterator'
          (Entity        => Entity,
           Db            => Entity.Db,
           Elements      => new Clang_Entity_Ref_Vectors.Vector,
           Current_Index => 1);

      --  Create the callback data instance for the indexer, as well as the
      --  indexing action.

      Index_Data : constant Indexer_Data := Indexer_Data'
        (Ret_Iterator  => Ret'Unchecked_Access,
         Clang_Db      => Entity.Db,
         File          => Entity.Ref_Loc.File,
         Sought_Cursor => Referenced (Get_Clang_Cursor (Entity)),
         Entity_Name   => Entity_Name);

   begin
      Indexer.Index_Translation_Unit
        (Get_Index_Action, Index_Data, CXIndexOpt_IndexFunctionLocalSymbols,
         Translation_Unit (Kernel, Entity.Ref_Loc.File));

      return Ret;
   end Find_All_References_Local;

   ----------------------------------
   -- Find_All_References_In_Scope --
   ----------------------------------

   function Find_All_References_In_Scope
     (Entity : Clang_Entity;
      In_Scope : Clang_Entity) return Clang_Reference_Iterator
   is
      --  Special case when the scope is a declaration that has a body that can
      --  be explored, we want to explore the body, not the decl.
      --  TODO ??? Explore other decls when it makes sense
      Real_Scope : constant Clang_Entity :=
        (if Kind (Get_Clang_Cursor (In_Scope)) = CXCursor_FunctionDecl
         then Get_Body (In_Scope)
         else In_Scope);

      Searched : constant Clang_Cursor := Get_Clang_Cursor (Entity);

      use Cursors_Arrays;

      function Is_Reference (C : Clang_Cursor) return Boolean
      is (Same_Entity (Referenced (C), Searched)

          --  For convenience, some clang cursor will return something via the
          --  Referenced call, but the cursor is not a reference. We want to
          --  consider only reference cursors.
          and then Kind (C)
            in CXCursor_DeclRefExpr   | CXCursor_MemberRefExpr
              | CXCursor_TypeRef      | CXCursor_TemplateRef
              | CXCursor_NamespaceRef | CXCursor_MemberRef
              | CXCursor_LabelRef     | CXCursor_OverloadedDeclRef
              | CXCursor_VariableRef);

      function Find_References (Scope : Clang_Cursor) return Array_Type;
      function Find_References (Scope : Clang_Cursor) return Array_Type
      is
         Children : constant Array_Type := Get_Children (Scope);
      begin
         return Filter (Children, Is_Reference'Access)
           & Id_Flat_Map (Children, Find_References'Access);
      end Find_References;

      Ret_Vec : constant Clang_Entity_Ref_Vector :=
        new Clang_Entity_Ref_Vectors.Vector;

      function Eq_Loc (L, R : Clang_Cursor) return Boolean is
         (Offset (Location (L)) = Offset (Location (R)));

      function Unique_Loc is new Cursors_Arrays.Unique_Gen (Eq_Loc);
   begin

      for Cursor of Unique_Loc
        (Find_References (Get_Clang_Cursor (Real_Scope)))
      loop
         declare
         begin
            Ret_Vec.Append
              (Clang_Reference'
                 (To_General_Location (Kernel, Location (Cursor)),
                  Clang_Symbol_Table.Find (+Entity.Name),
                  Real_Scope.Db,
                  Kind (Cursor)));
         end;
      end loop;

      return
        Clang_Reference_Iterator'
          (Entity        => Entity,
           Db            => Entity.Db,
           Elements      => Ret_Vec,
           Current_Index => 1);
   end Find_All_References_In_Scope;

   --------------------------------
   -- Find_All_References_Global --
   --------------------------------

   function Find_All_References_Global
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Include_Renames       : Boolean := True;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class
   is
      pragma Unreferenced
        (In_Scope, Include_Overriding, Include_Overridden,
         Include_Implicit, Include_All, Include_Renames, Kind);

      --  TODO ??? Options are not handled yet

      --  Retrieve information about the entity, USR_Sym to search for symbol
      --  in maps, Name to store in references

      USR_Sym : constant Clang_Symbol :=
        Clang_Symbol_Table.Find (USR (Get_Clang_Cursor (Entity)));
      Name : constant Symbol := Clang_Symbol_Table.Find (Entity.Get_Name);

      use File_To_Refs_Maps;

      --  Construct the return value, with an empty vector

      Ret : constant Clang_Reference_Iterator :=
        Clang_Reference_Iterator'
          (Entity        => Entity,
           Db            => Entity.Db,
           Elements      => new Clang_Entity_Ref_Vectors.Vector,
           Current_Index => 1);

      procedure Find_References_In_File_Map
        (M : File_Cache_Access);
      --  Helper function that will do the job of finding references for a
      --  single file's map.

      procedure Find_References_In_File_Map
        (M : File_Cache_Access)
      is
         --  Temporary for info vectors
         V : Info_Vectors;
         F : constant Virtual_File := Create (+(+M.File_Name));
      begin
         --  Check if the map contains the entity we're searching for

         if M.Map.Contains (USR_Sym) then

            --  If it does, get declarations and references in turn

            V := M.Map.Element (USR_Sym);

            for I in V.Decls.First_Index .. V.Decls.Last_Index loop
               Ret.Elements.Append
                 (Clang_Reference'
                    (Loc      => To_General_Location
                       (Kernel, F, V.Decls.Element (I).Loc),
                     Clang_Db => Entity.Db,
                     Name     => Name,
                     Kind     =>
                       Clang_Cursor_Kind (V.Decls.Element (I).Kind)));
            end loop;

            for I in V.Refs.First_Index .. V.Refs.Last_Index loop
               Ret.Elements.Append
                 (Clang_Reference'
                    (Loc      => To_General_Location
                         (Kernel, F, V.Refs.Element (I).Loc),
                     Clang_Db => Entity.Db,
                     Name     => Name,
                     Kind     =>
                       Clang_Cursor_Kind (V.Refs.Element (I).Cursor_Kind)));
            end loop;

         end if;
      end Find_References_In_File_Map;
   begin

      if In_File /= No_File then
         if Crossrefs_Cache.Map.Contains
           (Construct_Cache_Key (Kernel, In_File))
         then
            Find_References_In_File_Map
              (Crossrefs_Cache.Map.Element
                 (Construct_Cache_Key (Kernel, In_File)));
         end if;
      else
         for C of Get_Active_Files loop
            Find_References_In_File_Map (C);
         end loop;
      end if;

      return Ret;
   end Find_All_References_Global;

   ----------------
   -- Get_Entity --
   ----------------

   overriding function Get_Entity
     (Ref : Clang_Reference) return Root_Entity'Class
   is
   begin

      --  TODO ??? This is badly inefficient in the case the number of distinct
      --  files in which entity is referenced is bigger than the size of the
      --  LRU cache. We might store the entity that is in the original TU for
      --  the search but it's not sure it will work in every case

      return
        Get_Entity
          (Ref.Clang_Db,
           Kernel.Databases, "",
           Ref.Get_Location);
   end Get_Entity;

   ---------------------
   -- Get_Entity_Name --
   ---------------------

   overriding function Get_Entity_Name
     (Ref : Clang_Reference) return String
   is
   begin

      --  Name is cached for efficiency

      return Get (Ref.Name).all;
   end Get_Entity_Name;

   ------------------
   -- Get_Location --
   ------------------

   overriding function Get_Location
     (Ref : Clang_Reference) return General_Location is
   begin
      return Ref.Loc;
   end Get_Location;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Clang_Reference_Iterator) return Boolean
   is
   begin
      return Iter.Current_Index > Natural (Iter.Elements.Length);
   end At_End;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Clang_Reference_Iterator)
   is
   begin
      Iter.Current_Index := Iter.Current_Index + 1;
   end Next;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : Clang_Reference_Iterator) return Root_Entity_Reference'Class
   is
   begin
      if not Iter.At_End then
         return Iter.Elements.Element (Iter.Current_Index);
      end if;
      return No_Root_Entity_Reference;
   end Get;

   ----------------
   -- Get_Entity --
   ----------------

   overriding function Get_Entity
     (Iter : Clang_Reference_Iterator) return Root_Entity'Class
   is
   begin
      return Iter.Entity;
   end Get_Entity;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Iter : in out Clang_Reference_Iterator) is
   begin
      Unchecked_Free (Iter.Elements);
   end Destroy;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   overriding function Get_Current_Progress
     (Iter : Clang_Reference_Iterator) return Integer is
   begin
      return Iter.Current_Index;
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   overriding function Get_Total_Progress
     (Iter : Clang_Reference_Iterator) return Integer
   is
     (Integer (Iter.Elements.Length));

   ----------------------
   -- Get_Display_Kind --
   ----------------------

   overriding function Get_Display_Kind
     (Ref  : Clang_Reference) return String is ("");

   --------------------
   -- To_Root_Entity --
   --------------------

   function To_Root_Entity (Entity : Clang_Entity) return Root_Entity'Class is
     (if Entity = No_Clang_Entity then No_Root_Entity
      else Entity);

   ----------------
   -- Get_Caller --
   ----------------

   function Get_Caller
     (Clang_Db : Clang_Database;
      Ref_Loc : General_Location) return Root_Entity'Class
   is
      Dummy_Use_Index : Use_Index;
      --  This variable takes a lock on the index so that it is not modified
      --  while we search it

      Sloc : constant Sloc_T := Sloc_T'(Line   => Ref_Loc.Line,
                                        Column => Ref_Loc.Column,
                                        -- TODO: KLUDGE ???
                                        Index  => 0);

      File_Tree : constant Semantic_Tree'Class :=
        Kernel.Get_Abstract_Tree_For_File ("CLANG", Ref_Loc.File);

      Sem_Parent : constant Semantic_Node'Class :=
        File_Tree.Node_At (Sloc, (Cat_Function, Cat_Method, Cat_Constructor));

   begin

      --  If the reference is to a toplevel node, it won't have any parent. In
      --  this case, we return No_Root_Entity

      if Sem_Parent = No_Semantic_Node then
         return No_Root_Entity;
      else
         declare
            Parent : constant Clang_Node := Clang_Node (Sem_Parent);

            Ret    : constant Clang_Entity :=
              Cursor_As_Entity
                (Clang_Db, Kernel, Parent.Cursor);
         begin
            return
              (if Ret.Ref_Loc = Ref_Loc then No_Root_Entity
               else To_Root_Entity (Ret));
         end;
      end if;
   end Get_Caller;

   ----------------
   -- Get_Caller --
   ----------------

   overriding function Get_Caller
     (Ref : Clang_Reference) return Root_Entity'Class
   is
     (Get_Caller (Ref.Clang_Db, Get_Location (Ref)));

end Clang_Xref;
