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

with GNAT.Table;

package body Docgen_Registry is

   package Doc_Table is
     new GNAT.Table (Output_Description_Access, Natural, 1, 5, 20);

   ------------
   -- Insert --
   ------------

   procedure Insert (O : Output_Description) is
   begin
      Doc_Table.Append (new Output_Description'(O));
   end Insert;

   ------------
   -- Length --
   ------------

   function Length return Natural is
   begin
      return Natural (Doc_Table.Last);
   end Length;

   ---------
   -- Get --
   ---------

   function Get (Index : in Positive) return Output_Description_Access is
   begin
      if Index <= Length then
         return Doc_Table.Table (Index);
      else
         return null;
      end if;
   end Get;

begin
   Doc_Table.Init;
end Docgen_Registry;
