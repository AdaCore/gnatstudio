-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2005                         --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

package Docgen_Registry is

   type Output_Type is (Text, Binary);

   type String_Access is access String;

   type Output_Description is record
      Format      : Output_Type;
      Description : String_Access;
      Extension   : String_Access;
   end record;
   type Output_Description_Access is access Output_Description;

   procedure Insert (O : Output_Description);
   --  Insert a new format into the registry

   function Length return Natural;
   --  Returns the number of entry into the output registry

   function Get (Index : in Positive) return Output_Description_Access;
   --  Returns the Index'th output in the table or null if Index > Length

end Docgen_Registry;
