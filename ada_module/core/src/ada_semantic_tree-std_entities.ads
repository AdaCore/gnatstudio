-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with GNAT.Strings; use GNAT.Strings;

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

private

   type Std_Description_Record is record
      Name          : String_Access;
      Documentation : String_Access;
      Index         : String_Access;
      Category      : Language_Category := Cat_Unknown;
   end record;

   type Std_Description is access all Std_Description_Record;

end Ada_Semantic_Tree.Std_Entities;
