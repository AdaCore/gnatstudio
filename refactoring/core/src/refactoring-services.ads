-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2010, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides a number of services to query and modify source
--  code. These operations occur at the syntactic and semantic levels (as
--  opposed to the GPS.Editors API, for instance, which operates at the
--  character level).

with Ada.Strings.Unbounded;
with Basic_Types;
with Entities.Queries;

package Refactoring.Services is

   function Get_Entity_Access
     (Context : Factory_Context;
      Entity  : Entities.Entity_Information)
      return Language.Tree.Database.Entity_Access;
   --  Return a pointer to the declaration of Entity. This pointer can be used
   --  to retrieve additional data about the entity (read directly from the
   --  source file).
   --  This returns a pointer to the first declaration (aka "public view") of
   --  the entity. You might need to call Get_Last_Visible_Declaration if you
   --  want the declaration as visible from a specific part of the code (this
   --  could for instance be the declaration in the private part).
   --  Returns Null_Entity_Access if this could not be retrieved.

   function Accepts_Primitive_Ops
     (Context        : Factory_Context;
      Entity         : Entities.Entity_Information;
      Current_Offset : Basic_Types.String_Index_Type) return Boolean;
   --  Whether the entity is an instance of a class or interface.
   --  This returns null for a "'Class".

   -------------------------
   -- Entity declarations --
   -------------------------

   type Entity_Declaration is tagged private;
   No_Entity_Declaration : constant Entity_Declaration;

   function Get_Declaration
     (Context : Factory_Context;
      Entity  : Entities.Entity_Information) return Entity_Declaration;
   --  Return the declaration of the entity. From this, one can extract the
   --  initial value,  the type (as set by the user, whether it is a constant,
   --  and other attributes).

   procedure Free (Self : in out Entity_Declaration);
   --  Free the memory used by Self

   function Initial_Value (Self : Entity_Declaration) return String;
   --  Return the initial value of the entity, as set in its declaration. For
   --  instance, if the entity is declared as
   --     A : Integer := 2 + 3;
   --  then the initial_value is "2 + 3".
   --  The empty string is returned if no initial value was specified

   function Display_As_Parameter
     (Self    : Entity_Declaration;
      Context : Factory_Context;
      PType   : Entities.Queries.Parameter_Type) return String;
   --  Return the declaration of the entity as it should be displayed in a
   --  parameter list. This includes the name of the variable.

   function Display_As_Variable
     (Self  : Entity_Declaration) return String;
   --  Return the declaration of the entity as it should be displayed in a
   --  variable declaration. This includes the name of the variable

   procedure Create_Marks
     (Self   : in out Entity_Declaration;
      Buffer : GPS.Editors.Editor_Buffer'Class);
   --  Creates marks in the editor corresponding to the declaration of the
   --  entity.
   --  The result must be freed by the user.

   procedure Remove (Self : Entity_Declaration);
   --  Remove the declaration of the entity from the source file.
   --  You must have called Create_Marks first.

private

   type Editor_Mark_Access is access all GPS.Editors.Editor_Mark'Class;

   type Entity_Declaration is tagged record
      Entity : Entities.Entity_Information;
      Decl   : Ada.Strings.Unbounded.Unbounded_String;

      SFirst, SLast : Language.Source_Location;
      First, Last   : Editor_Mark_Access;
      --  From the start of the entity name to the ";"

      Shared : Boolean;
      --  Whether multiple entities share the same declaration

      Equal_Loc : Integer := -1;
      --  Location of ":=" in Decl
   end record;

   No_Entity_Declaration : constant Entity_Declaration :=
     (Entity    => null,
      Equal_Loc => -1,
      SFirst    => <>,
      SLast     => <>,
      First     => null,
      Last      => null,
      Shared    => False,
      Decl      => Ada.Strings.Unbounded.Null_Unbounded_String);
end Refactoring.Services;
