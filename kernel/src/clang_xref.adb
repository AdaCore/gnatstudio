------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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
with clang_c_Index_h;

with Interfaces.C;
with GNATCOLL.Projects;
use GNATCOLL.Projects;
with Language.Libclang; use Language.Libclang;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with GPS.Editors; use GPS.Editors;
use clang_c_Index_h;

package body Clang_Xref is

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

   use Libclang.Index;

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
     (Cursor_As_Entity (From_Entity.Db, From_Entity.Kernel, Cursor));
   --  Creates an entity from a Clang cursor

   function Methods
     (Entity : Clang_Entity) return Cursors_Vectors.Vector;
   --  For a given clang entity, returns a vector of clang cursors

   function Get_Children
     (Cursor : Clang_Cursor;
      Kind : Clang_Cursor_Kind) return Cursors_Vectors.Vector;
   --  Get all the children of cursor that have the given kind. Utility
   --  function

   function To_Entity_Array
     (From_Entity : Clang_Entity;
      Cursors : Cursors_Vectors.Vector) return Entity_Array;
   --  Convenience function to convert a vector of clang cursors to an array of
   --  clang entities

   use GNATCOLL.VFS;

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
      Kind                  : String := "")
      return Root_Reference_Iterator'Class;
   --  Helper function for find all references, for the case of a public
   --  globally declared entity.

   function Get_Project (E : Clang_Entity) return Project_Type
   is
     (File_Info'Class
        (E.Kernel.Registry.Tree.Info_Set (E.Loc.File).First_Element).Project);
   --  Helper function to retrieve an entity's project

   ------------------
   -- Get_Children --
   ------------------

   use type Clang_Cursor_Kind;

   function Get_Children
     (Cursor : Clang_Cursor;
      Kind : Clang_Cursor_Kind) return Cursors_Vectors.Vector
   is
      function Has_Kind (C : Clang_Cursor) return Boolean is
        (Libclang.Index.Kind (C) = Kind);
      --  Filter predicate, returns true if C has kind Kind
   begin
      return Get_Children (Cursor, Has_Kind'Access);
   end Get_Children;

   --------------------
   -- Get_Clang_Node --
   --------------------

   function Get_Clang_Cursor (E : Clang_Entity) return Clang_Cursor is
   begin
      return
        Clang_Node
          --  Go through the abstract tree to get the cursor, will handle
          --  necessary TU fetching and update transparently
          (E.Kernel.Get_Abstract_Tree_For_File (E.Loc.File).Node_At
           (Sloc_T'(E.Loc.Line, E.Loc.Column, 0))).Cursor;
   end Get_Clang_Cursor;

   --------------------
   -- Get_Clang_Node --
   --------------------

   function Get_Clang_Cursor
     (Kernel : Core_Kernel; Loc : General_Location) return Clang_Cursor
   is
   begin
      return
        Clang_Node
          (Kernel.Get_Abstract_Tree_For_File (Loc.File).Node_At
           (Sloc_T'(Loc.Line, Loc.Column, 0))).Cursor;
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

      C : constant Clang_Cursor := Referenced
        (Get_Clang_Cursor (Db.Kernel, Loc));
   begin
      if C = No_Cursor then

         --  No entity found

         return No_Root_Entity;
      else

         --  The cursor is used to resolve the final location of the entity.
         --  No cursor is stored though, because cursors are invalidated every
         --  time a TU is parsed.

         return Cursor_As_Entity (Clang_Entity'(Db => Db,
                                                Kernel => Db.Kernel,
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

   --  Named is stored at entity creation

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

      return USR (Get_Clang_Cursor (Entity));
   end Qualified_Name;

   ----------
   -- Hash --
   ----------

   overriding function Hash
     (Entity : Clang_Entity) return Integer
   is
   begin
      return Integer (Hash (Get_Clang_Cursor (Entity)));
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
           .New_Location (Natural (Raw_Loc.Offset));

      begin
         return General_Location'
           (Raw_Loc.File,

            --  ??? Project is not used apparently, so no need to resolve it

            GNATCOLL.Projects.No_Project,
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
   begin

      --  As described in the intro, entities referencing runtime
      --  variables/constants store locations rather than cursors, and
      --  cursors are re-resolved every time via get_clang_cursor.

      return Clang_Entity'
        (Db => Db,
         Name       => +Spelling (Cursor),
         Loc        =>
           To_General_Location
             (Db.Kernel,
              Location (Referenced (Cursor))),
         Kernel     => Kernel,
         Has_Type_Inst => False,
         Clang_Type_Inst => <>);
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

      --  Get the referenced cursor, which gives us the declaration in most
      --  cases

      Def := Referenced (Cursor);

      --  If the referenced cursor is the same as the original cursor, it means
      --  entity points to a function/method body, which is it's own entity.
      --  We need to get the canonical cursor, which is the first place where
      --  entity has been declared

      if Cursor = Def then
         Def := Canonical (Cursor);
      end if;

      return General_Entity_Declaration'
        (Loc => To_General_Location (Entity.Kernel, Location (Def)),
         Name => +Spelling (Def),
         Body_Is_Full_Declaration => Is_Definition (Def));
   end Get_Declaration;

   ---------------------------
   -- Caller_At_Declaration --
   ---------------------------

   overriding function Caller_At_Declaration
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      pragma Unreferenced (Entity);
   begin

      --  ??? TODO : Implement

      return No_Root_Entity;
   end Caller_At_Declaration;

   -------------------------
   -- To_General_Location --
   -------------------------

   function To_General_Location
     (K : Core_Kernel;
      F : Virtual_File;
      Offset : Offset_T) return General_Location
   is
      Ed : constant Editor_Buffer'Class := K.Get_Buffer_Factory.Get
        (F, Open_View => False, Focus => False, Open_Buffer => True);
      Loc : constant Editor_Location'Class :=
        Ed.New_Location (Natural (Offset));
   begin
      return
        General_Location'
          (File    => F,
           Project => GNATCOLL.Projects.No_Project,
           Line    => Loc.Line,
           Column  => Loc.Column);
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

      --  Get the clang context containing the entity references cache, as well
      --  as the cursor's USR, so as to be able to query the ref cache

      Cursor : constant Clang_Cursor := Get_Clang_Cursor (Entity);
      Context : Clang_Context := TU_Source.Context (Get_Project (Entity));
      USR_Sym : constant Symbol := Context.Sym_Table.Find (USR (Cursor));

      --  Try to use Definition from libclang. It will only work as long as the
      --  definition is in the same translation unit though, so not sufficient

      Entity_Body : constant Clang_Cursor := Definition (Cursor);

      --  V is used to store an vector of entity references. Used while
      --  scanning for the body

      V : Info_Vectors;

      use VFS_To_Refs_Maps;

      --  Return value holder
      Ret : General_Location := No_Location;
   begin
      if Entity_Body = No_Cursor then
         return To_General_Location (Entity.Kernel, Location (Entity_Body));
      else

         --  Iterate on every file's cache

         for C in Context.Refs.Iterate loop

            --  Exit as soon as we have found  the first body

            exit when Ret /= No_Location;

            --  If this file's cache contains some references to the entity for
            --  which we're trying to find the body, then explore those refs
            --  to see if one is a body. We might store the information on
            --  the entity's cache for the file, so as to avoid unnecessary
            --  iterations, but this has not been proved a bottleneck.

            if Element (C).Contains (USR_Sym) then

               V := Element (C).Element (USR_Sym);

               for I in V.Decls.First_Index .. V.Decls.Last_Index loop

                  --  Exit as soon as we have found  the first body

                  exit when Ret /= No_Location;

                  --  If a reference is a body definition, then store the
                  --  corresponding location, which will make the nested
                  --  loops exit

                  if V.Decls.Element (I).Is_Def then
                     Ret := To_General_Location
                       (Entity.Kernel, Key (C), V.Decls.Element (I).Loc);
                  end if;
               end loop;
            end if;
         end loop;

         return Ret;

      end if;
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
        Clang_Entity'(From_Entity.Kernel,
                      From_Entity.Db,
                      +Spelling (Typ),
                      To_General_Location
                        (From_Entity.Kernel, Location (Declaration (Typ))),
                      True, Typ);
   end Type_As_Entity;

   -------------------
   -- Returned_Type --
   -------------------

   overriding function Returned_Type
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      Cursor : constant Clang_Cursor :=
        Get_Clang_Cursor (Entity.Kernel, Entity.Get_Declaration.Loc);
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
     (Entity : Clang_Entity) return Cursors_Vectors.Vector
   is
      T_Decl : constant Clang_Cursor := Declaration (As_Type (Entity));
   begin
      if Is_Object_Type (Kind (T_Decl)) then

         --  Straightforwardly get every children of decl that is a method

         return Get_Children (T_Decl, CXCursor_CXXMethod);

      else
         return Cursors_Vectors.Empty_Vector;
      end if;
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
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin

      --  A function can only be a "primitive" in the Ada sense if it is a
      --  method in C++. This notion is not applicable to the C language.

      --  TODO ??? Reimplement, this method is completely wrong. What we
      --  should do is:
      --
      --  1. Call overrides on the current method recursively, to
      --  determine all the bases methods that it overrides (we will have
      --  to make override work with multiple inheritance for this to
      --  work correcly with multiple inheritance), and get the types
      --  of all those overriden methods
      --
      --  2. Call Child_Types on the class of the current method, and
      --  then add all the returned types to the list constructed in 1.

      if
        Kind (C) = CXCursor_CXXMethod
      then
         declare
            Class : constant Clang_Cursor := Semantic_Parent (C);
            Base_Classes : Cursors_Vectors.Vector :=
              Get_Children (Class, CXCursor_CXXBaseSpecifier);
            Ret : Entity_Array (1 .. Natural (Base_Classes.Length) + 1);
         begin
            Ret (1) := new Clang_Entity'(Cursor_As_Entity (Entity, Class));
            for J in 2 .. Ret'Length loop
               Ret (J) := new Clang_Entity'
                 (Cursor_As_Entity
                    (Entity,
                     Referenced (Base_Classes (J - 1))));
            end loop;
            return Ret;
         end;
      end if;
      return No_Entity_Array;
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
     (E  : Clang_Entity) return Boolean is
   begin

      --  An entity is a global if its parent is the translation unit.
      --  TODO ??? Heuristic is probably wrong with namespaces.

      return
        Lexical_Parent (Get_Clang_Cursor (E)).kind = CXCursor_TranslationUnit;
   end Is_Global;

   ---------------------
   -- Is_Static_Local --
   ---------------------

   overriding function Is_Static_Local
     (E  : Clang_Entity) return Boolean
   is
      pragma Unreferenced (E);
   begin

      --  TODO ??? Not implemented

      return False;
   end Is_Static_Local;

   --------------------------
   -- Is_Predefined_Entity --
   --------------------------

   overriding function Is_Predefined_Entity
     (E  : Clang_Entity) return Boolean is
      pragma Unreferenced (E);
   begin

      --  TODO ??? Not implemented

      return False;
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
        Entity.Kernel.Get_Buffer_Factory.Get
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
        (Entity.Kernel,
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

   overriding function Overrides
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      type Cursor_Ptr is access Clang_Cursor;
      pragma Convention (C, Cursor_Ptr);

      use Interfaces.C;

      --  We import the clang procedure manually because the signature
      --  generated by fdump-ada-spec does not suit us.

      procedure Get_Overriden_Cursors
        (Cursor : Clang_Cursor;
         Overridden : access Cursor_Ptr;
         Num_Overridden : access unsigned);
      pragma Import (C, Get_Overriden_Cursors, "clang_getOverriddenCursors");

      C : aliased Cursor_Ptr;
      Ign : aliased unsigned;

      Ret : Clang_Entity;
   begin

      --  We only get the first cursor, because this is all that is needed
      --  for fullfilling the xref Overrides function, but we might want
      --  to decouple that into an helper at some point, and use it for
      --  Is_Primitive_Of

      Get_Overriden_Cursors
        (Get_Clang_Cursor (Entity), C'Access, Ign'Access);

      Ret := Cursor_As_Entity (Entity, C.all);

      Dispose_Overriden (C);

      return Ret;
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
      Cursors : Cursors_Vectors.Vector) return Entity_Array
   is
      Ret : Entity_Array (1 .. Natural (Cursors.Length));
   begin

      --  Do a straightforward conversion of cursors to entity pointers, and
      --  store that into the return array

      for I in Ret'Range loop
         Ret (I) :=
           new Clang_Entity'(Cursor_As_Entity (From_Entity, Cursors (I)));
      end loop;

      return Ret;
   end To_Entity_Array;

   ------------
   -- Fields --
   ------------

   overriding function Fields
     (Entity            : Clang_Entity) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin
      if Is_Object_Type (C.kind) then

         --  Make an array of the field declarations TODO ??? For a type with
         --  bases, will miss the parent types's fields

         return To_Entity_Array (Entity, Get_Children (C, CXCursor_FieldDecl));
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

   --  ??? TODO : Index type is a builtin type in C, and our entity model is
   --  not well suited for keeping info about built in types (no declaration
   --  location).

     (Entity : Clang_Entity) return Entity_Array is (No_Entity_Array);

   -----------------
   -- Child_Types --
   -----------------

   overriding function Child_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Entity_Array

   --  TODO ??? Implement

   is (No_Entity_Array);

   ------------------
   -- Parent_Types --
   ------------------

   overriding function Parent_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);

      --  Return an array containing all the ancestors of the types contained
      --  in the 'Entities' array

      function Children_Parent_Types
        (Es : Entity_Array) return Entity_Array
      is
        (if Es = No_Entity_Array then No_Entity_Array
         else Parent_Types (Es (Es'First).all, True)
              & Children_Parent_Types (Es (Es'First + 1 .. Es'Last)));
   begin
      if Is_Object_Type (C.kind) then
         declare
            Parents : constant Entity_Array :=
              To_Entity_Array
                (Entity, Get_Children (C, CXCursor_CXXBaseSpecifier));
         begin
            return
              (if Recursive

               --  If recursive, we want the parent types plus all their
               --  ancestors

               then Parents & Children_Parent_Types (Parents)

               --  Else, we only want the parent types

               else Parents);
         end;
      end if;
      return No_Entity_Array;
   end Parent_Types;

   ----------------
   -- Parameters --
   ----------------

   overriding function Parameters
     (Entity : Clang_Entity) return Parameter_Array

   --  TODO ??? Implement

   is (No_Parameters);

   -----------------------------
   -- Get_All_Called_Entities --
   -----------------------------

   overriding function Get_All_Called_Entities
     (Entity : Clang_Entity) return Abstract_Entities_Cursor'Class

   --  TODO ??? Implement

   is (No_Entities_Cursor);

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
      Kind                  : String := "")
      return Root_Reference_Iterator'Class
   is
   begin

      --  This function is cut in two cases: Finding references for an entity
      --  declared in the global scope and finding references for an entity
      --  declared in the local scope. This is due to the fact that the
      --  reference cache only stores references to global entities, for
      --  space reasons.

      if Entity.Is_Global then
         return Find_All_References_Global
           (Entity, In_File, In_Scope, Include_Overriding, Include_Overridden,
            Include_Implicit, Include_All, Kind);
      else
         return Find_All_References_Local
           (Entity, In_File, In_Scope, Include_Overriding, Include_Overridden,
            Include_Implicit, Include_All, Kind);
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
     (Client_Data : in out Indexer_Data) return Boolean is (False);
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
              (Client_Data.File, To_Offset_T (Loc),
               Client_Data.Entity_Name, Client_Data.Clang_Db));
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
                 (Client_Data.File, To_Offset_T (Loc),
                  Client_Data.Entity_Name, Client_Data.Clang_Db));
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
      Kind                  : String := "")
      return Root_Reference_Iterator'Class
   is
      pragma Unreferenced
        (In_Scope, Include_Overriding, Include_Overridden,
         Include_Implicit, Include_All, Kind, In_File);

      --  TODO ??? Options are not handled yet

      Project : constant Project_Type := Get_Project (Entity);

      --  We want to store entity names as symbols for convenience, use the
      --  Clang context symbol table

      Entity_Name : constant Symbol :=
        TU_Source.Context (Project).Sym_Table.Find (Entity.Get_Name);

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
         File          => Entity.Loc.File,
         Sought_Cursor => Referenced (Get_Clang_Cursor (Entity)),
         Entity_Name   => Entity_Name);

      Index_Action : constant Clang_Index_Action :=
        TU_Source.Context (Project).Index_Action;
   begin
      Indexer.Index_Translation_Unit
        (Index_Action, Index_Data, CXIndexOpt_IndexFunctionLocalSymbols,
         TU_Source.Translation_Unit (Entity.Kernel, Entity.Loc.File));

      return Ret;
   end Find_All_References_Local;

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
      Kind                  : String := "")
      return Root_Reference_Iterator'Class
   is
      pragma Unreferenced
        (In_Scope, Include_Overriding, Include_Overridden,
         Include_Implicit, Include_All, Kind);

      --  TODO ??? Options are not handled yet

      --  Retrieve the context which contains the reference cache, as well as
      --  the symbol table

      Context : Clang_Context := TU_Source.Context (Get_Project (Entity));

      --  Retrieve information about the entity, USR_Sym to search for symbol
      --  in maps, Name to store in references

      USR_Sym : constant Symbol :=
        Context.Sym_Table.Find (USR (Get_Clang_Cursor (Entity)));
      Name : constant Symbol := Context.Sym_Table.Find (Entity.Get_Name);

      use VFS_To_Refs_Maps;

      --  Construct the return value, with an empty vector

      Ret : constant Clang_Reference_Iterator :=
        Clang_Reference_Iterator'
          (Entity        => Entity,
           Db            => Entity.Db,
           Elements      => new Clang_Entity_Ref_Vectors.Vector,
           Current_Index => 1);

      procedure Find_References_In_File_Map
        (F : Virtual_File; M : Sym_To_Loc_Map);
      --  Helper function that will do the job of finding references for a
      --  single file's map.

      procedure Find_References_In_File_Map
        (F : Virtual_File; M : Sym_To_Loc_Map)
      is
         --  Temporary for info vectors
         V : Info_Vectors;
      begin
         --  Check if the map contains the entity we're searching for

         if M.Contains (USR_Sym) then

            --  If it does, get declarations and references in turn

            V := M.Element (USR_Sym);

            for I in V.Decls.First_Index .. V.Decls.Last_Index loop
               Ret.Elements.Append
                 (Clang_Reference'
                    (File     => F,
                     Offset   => V.Decls.Element (I).Loc,
                     Clang_Db => Entity.Db,
                     Name     => Name));
            end loop;

            for I in V.Refs.First_Index .. V.Refs.Last_Index loop
               Ret.Elements.Append
                 (Clang_Reference'
                    (File     => F,
                     Offset   => V.Refs.Element (I).Loc,
                     Clang_Db => Entity.Db,
                     Name     => Name));
            end loop;

         end if;
      end Find_References_In_File_Map;
   begin

      if In_File /= No_File then
         if Context.Refs.Contains (In_File) then
            Find_References_In_File_Map
              (In_File, Context.Refs.Element (In_File));
         end if;
      else
         for C in Context.Refs.Iterate loop
            Find_References_In_File_Map (Key (C), Element (C));
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
           Ref.Clang_Db.Kernel.Databases, "",
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
      return To_General_Location (Ref.Clang_Db.Kernel, Ref.File, Ref.Offset);
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
      Free (Iter.Elements);
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
     (Ref  : Clang_Reference) return String is
     ("");

end Clang_Xref;
