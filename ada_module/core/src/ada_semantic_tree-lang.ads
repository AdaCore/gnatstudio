------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with GNATCOLL.Symbols;            use GNATCOLL.Symbols;
with Language.Profile_Formaters;  use Language.Profile_Formaters;

with GNATCOLL.VFS;                use GNATCOLL.VFS;

package Ada_Semantic_Tree.Lang is

   function Is_Enum_Type
     (Tree : Construct_Tree;
      It   : Construct_Tree_Iterator) return Boolean;
   pragma Inline (Is_Enum_Type);
   --  Return tree if the construct given in parameter is a type enumeration

   type Ada_Tree_Language is new Tree_Language with private;
   --  This type provides an ada implementation to the language-specific tree
   --  functionalitites.

   overriding function Get_Language
     (Tree : access Ada_Tree_Language) return Language_Access;
   overriding function Get_Name_Index
     (Lang      : access Ada_Tree_Language;
      Construct : Simple_Construct_Information) return GNATCOLL.Symbols.Symbol;
   overriding function Find_Reference_Details
     (Lang    : access Ada_Tree_Language;
      File    : Structured_File_Access;
      Index   : String_Index_Type) return Entity_Reference_Details;
   overriding procedure Get_Profile
     (Lang       : access Ada_Tree_Language;
      Entity     : Entity_Access;
      Formater   : access Profile_Formater'Class;
      With_Aspects : Boolean := False);
   overriding procedure Diff
     (Lang               : access Ada_Tree_Language;
      Old_Tree, New_Tree : Construct_Tree;
      Callback           : Diff_Callback);
   overriding function Get_Declaration
     (Lang   : access Ada_Tree_Language;
      Entity : Entity_Access) return Entity_Access;
   overriding function Find_Declaration
     (Lang     : access Ada_Tree_Language;
      File     : Structured_File_Access;
      Line     : Integer;
      Column   : String_Index_Type) return Entity_Access;
   overriding function Find_First_Part
     (Lang   : access Ada_Tree_Language;
      Entity : Entity_Access) return Entity_Access;
   overriding function Find_Next_Part
     (Lang   : access Ada_Tree_Language;
      Entity : Entity_Access) return Entity_Access;
   --  See inherited documentation

   Ada_Tree_Lang : constant Tree_Language_Access;

   function Same_Profile
     (Left_Tree    : Construct_Tree;
      Left_Sb      : Construct_Tree_Iterator;
      Right_Tree   : Construct_Tree;
      Right_Sb     : Construct_Tree_Iterator) return Boolean;
   --  Return true if both subprogram have the same profile. This check that
   --  the parameter type have the same name - won't work if one of the two
   --  views is using a fully qualified expression and the other not.
   --  The two constructs are assume to be of the same category.

   function Is_Compilation_Unit (It : Construct_Tree_Iterator) return Boolean;
   pragma Inline (Is_Compilation_Unit);
   --  Return true if the iterator given in parameter is a compilation unit

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  Register a generic assistant for Ada

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

   type Ada_Language_Handler is new
     Abstract_Language_Handler_Record with null record;
   --  Dummy language handler, considering all the files as being Ada files.
   --  used e.g. for testing purpose.

   overriding function Get_Language_From_File
     (Handler           : access Ada_Language_Handler;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False) return Language_Access;

   overriding function Get_Tree_Language_From_File
     (Handler           : access Ada_Language_Handler;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False)
      return Tree_Language_Access;

private

   type Ada_Tree_Language is new Tree_Language with null record;

   Ada_Tree_Lang : constant Tree_Language_Access := new Ada_Tree_Language;

end Ada_Semantic_Tree.Lang;
