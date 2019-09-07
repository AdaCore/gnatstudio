------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

--  This package provides a way to optimize virtual file comparison by giving
--  and ordering number between them.

with Ada.Containers.Ordered_Maps;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package Virtual_File_Indexes is

   type Comparison_Optimizer is private;
   --  This type stores and ordered list of virtual files. Copying this object
   --  does not copy the underlying structure.

   function Create return Comparison_Optimizer;
   --  Return a new comparison optimized.

   procedure Destroy (Opt : in out Comparison_Optimizer);
   --  Destroy a comparizon optimizer

   type VF_Key is private;
   --  This type holds a key to a file along with its ordered number. This is
   --  automatically updated after each file insertion in the optimizer. The
   --  user do not need to call Get_Key a second time to have the updated
   --  key.

   function Get_Key
     (Opt : Comparison_Optimizer; File : Virtual_File) return VF_Key;
   --  Return the key corresponding to the file given in parameter. If the file
   --  is not yet in the optimizer, it will get added and all the keys will
   --  be updated. This operation is possibly time consuming, as it involves
   --  file comparison, so the result should be cached in order to take
   --  advantage of the optimization.

   function "<" (Left, Right : VF_Key) return Boolean;
   --  Compares two keys. The efficiency of this operation is close to a simple
   --  integer comparison.

   overriding function "=" (Left, Right : VF_Key) return Boolean;
   --  Returns true if the two keys are equals. The efficiency of this
   --  operation is close to a simple integer comparison.

private

   type VF_Key_Record is record
      Order : Integer;
      File  : Virtual_File;
   end record;

   type VF_Key is access all VF_Key_Record;

   package VF_Map is new Ada.Containers.Ordered_Maps (Virtual_File, VF_Key);
   use VF_Map;

   type Comparison_Optimizer_Record is record
      Map : VF_Map.Map;
   end record;

   type Comparison_Optimizer is access all Comparison_Optimizer_Record;

end Virtual_File_Indexes;
