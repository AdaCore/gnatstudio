------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.VFS; use GNATCOLL.VFS;

package Ada_Semantic_Tree.Std_Entities is

   procedure Register_Assistant
     (Database : Construct_Database_Access; From_File : Virtual_File);

   type Std_Description is private;
   --  Describes an attribute or a pragma

   type Context_Of_Use is
     (C_Unknown,
      C_Object,
      C_Object_Array,
      C_Type,
      C_Type_Array);

   type Context_Of_Use_Array is array (Context_Of_Use) of Boolean;
   pragma Pack (Context_Of_Use_Array);

   procedure Get_Possible_Aspects
     (Db      : Construct_Database_Access;
      Prefix  : String;
      Context : Context_Of_Use_Array;
      Result  : in out Entity_List);
   --  Add to the entity list the list of apects matching the context of use
   --  and the prefix given in parameter.

   procedure Get_Possible_Pragmas
     (Db      : Construct_Database_Access;
      Prefix  : String;
      Context : Context_Of_Use_Array;
      Result  : in out Entity_List);
   --  Add to the entity list the list of pragmas matching the context of use
   --  and the prefix given in parameter.

   procedure Get_Possible_Attributes
     (Db      : Construct_Database_Access;
      Prefix  : String;
      Context : Context_Of_Use_Array;
      Result  : in out Entity_List);
   --  Add to the entity list the list of attributes matching the context of
   --  use and the prefix given in parameter.

   procedure Get_Possible_Standard_Entities
     (Db                       : Construct_Database_Access;
      Prefix                   : String;
      Is_Partial               : Boolean;
      Result                   : in out Entity_List;
      Exclude_Standard_Package : Boolean := False);
   --  Add to the entity list the list of entities coming from the standard
   --  package matching the prefix given in parameter.

   procedure Get_Possible_Standard_Exceptions
     (Db                       : Construct_Database_Access;
      Prefix                   : String;
      Is_Partial               : Boolean;
      Result                   : in out Entity_List;
      Exclude_Standard_Package : Boolean := False);
   --  Add to the entity list the list of exceptions coming from the standard
   --  package matching the prefix given in parameter. Adds Standard too since
   --  it can lead to an exception name.

private

   type Origin_Kind is
     (Default, Ada_Standard, GNAT_Specific);

   type Std_Description_Record is record
      Name          : String_Access;
      Documentation : String_Access;
      Index         : String_Access;
      Category      : Language_Category := Cat_Unknown;
      Origin        : Origin_Kind := Default;
   end record;

   type Std_Description is access all Std_Description_Record;

end Ada_Semantic_Tree.Std_Entities;
