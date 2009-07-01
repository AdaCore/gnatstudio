-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2007, AdaCore                 --
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

with Language.Tree.Database; use Language.Tree.Database;

package Language.Tree.Ada is

   function Is_Enum_Type
     (Tree : Construct_Tree;
      It   : Construct_Tree_Iterator) return Boolean;
   --  Return tree if the construct given in parameter is a type enumeration

   type Ada_Tree_Language is new Tree_Language with private;
   --  This type provides an ada implementation to the language-specific tree
   --  functionalitites.

   overriding
   function Get_Language
     (Tree : access Ada_Tree_Language) return Language_Access;
   --  See inherited documentation

   overriding
   function Get_Name_Index
     (Lang      : access Ada_Tree_Language;
      Construct : Simple_Construct_Information) return String;
   --  See inherited documentation

   overriding
   function Get_Documentation
     (Lang   : access Ada_Tree_Language;
      Buffer : String;
      Tree   : Construct_Tree;
      Node   : Construct_Tree_Iterator) return String;
   --  See inherited documentation

   overriding
   procedure Diff
     (Lang                   : access Ada_Tree_Language;
      Old_Tree, New_Tree     : Construct_Tree;
      Callback               : Diff_Callback);
   --  See inherited documentation

   Ada_Tree_Lang : constant Tree_Language_Access;

   function Same_Profile
     (Left_Tree    : Construct_Tree;
      Left_Sb      : Construct_Tree_Iterator;
      Right_Tree   : Construct_Tree;
      Right_Sb     : Construct_Tree_Iterator)
      return Boolean;
   --  Return true if both subprogram have the same profile. This check that
   --  the parameter type have the same name - won't work if one of the two
   --  views is using a fully qualified expression and the other not.
   --  The two constructs are assume to be of the same category.

   function Is_Compilation_Unit (It : Construct_Tree_Iterator) return Boolean;
   --  Return true if the iterator given in parameter is a compilation unit

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  Register a generic assistant for Ada.

   function Get_Ref_Key
     (Db : Construct_Database_Access)
      return Construct_Annotations_Pckg.Annotation_Key;
   --  This call returns a key used to store a reference on a construct. Each
   --  ada module can use it in different contextes. In order to avoid
   --  conflicts, we centralize here the documentation about its usage. It's
   --  determined by the nature of the category of the object.
   --
   --  Cat_Use, Cat_With: the referenced unit. This is handled by
   --    Language.Ada_Dependencies.
   --  Cat_Type (and assimilated) : the type hierarchy. This is handled by
   --    Language.Ada_Type_Tree

private

   type Ada_Tree_Language is new Tree_Language with null record;

   Ada_Tree_Lang : constant Tree_Language_Access := new Ada_Tree_Language;

end Language.Tree.Ada;
