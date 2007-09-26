-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2007, AdaCore                    --
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

--  This packages analyzes trees in order to link various parts of constructs,
--  for example a subprogram spec and its body.

with Language.Tree.Database; use Language.Tree.Database;
with Ada_Semantic_Tree.Units; use Ada_Semantic_Tree.Units;

package Ada_Semantic_Tree.Parts is

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  This assistant has to be registered to the database before any of the
   --  queries in this file can work.

   function Get_Assistant
     (Db : Construct_Database_Access) return Database_Assistant_Access;
   --  Return the assistant responsible of doing the parts analysis.

   procedure Analyze_Unit
     (Assistant : Database_Assistant_Access;
      Unit      : Unit_Access);
   --  Perform a full parts analysis on the unit given in parameter if needed.
   --  This is usually done automatically when calling the queries of this
   --  package, except for the ones marked as Unchecked.

   function Get_First_Occurence (Entity : Entity_Access) return Entity_Access;
   --  Return the first declaration occurence of this entity.

   function Get_Second_Occurence (Entity : Entity_Access) return Entity_Access;
   --  Return the second declaration occurence of this entity, for example the
   --  body of an entity Null_Entity_Access if none.

   function Get_Third_Occurence (Entity : Entity_Access) return Entity_Access;
   --  Return the second declaration occurence of this entity, for example the
   --  full view of a protected type that has a partial view and a public view,
   --  Null_Entity_Access if none.

   function Get_Most_Complete_View
     (Entity : Entity_Access) return Entity_Access;
   --  Return the most complete view accessible for the iterator given in
   --  parameter.

   function Is_Most_Complete_View (Entity : Entity_Access) return Boolean;
   --  Return false if we have build a most complete view of the entity pointed
   --  by the iterator, false otherwise.

   function Is_First_Occurence (Entity : Entity_Access) return Boolean;
   --  Return True if the iterator given in parameter is the first known
   --  occurence, according to the trees.

   function Are_Same_Entity (Left, Right : Entity_Access) return Boolean;
   --  Return true if E1 and E2 are two parts of the same entity.

   function Unchecked_Are_Same_Entity
     (Assistant   : Database_Assistant_Access;
      Left, Right : Entity_Access) return Boolean;
   --  Same as before, but doesn't ensure that the units are up to date and
   --  doesn't do assistant resolution (which is why it has to be given in
   --  parameter).

   function Get_Last_Visible_Declaration
     (Entity : Entity_Access;
      File   : Structured_File_Access;
      Offset : Natural) return Entity_Access;
   --  Return the last declaration that is visible according to the file
   --  & offset given in parameter.

   function Unchecked_Is_In_Scope
     (Assistant   : Database_Assistant_Access;
      Scope       : Entity_Access;
      Entity      : Entity_Access) return Boolean;
   --  Return true if Entity is within Scope, taking into account body / spec
   --  information.
   --  For performances purpose, function does not ensures that the information
   --  is up to date - the caller should call Analyse_Unit on the relevant
   --  enclosing units.

end Ada_Semantic_Tree.Parts;
