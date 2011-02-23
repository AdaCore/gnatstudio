------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

package body Templates_Parser.Query is

   ---------------
   -- Composite --
   ---------------

   function Composite
     (Association : Templates_Parser.Association)
      return Tag is
   begin
      if Association.Kind = Composite then
         return Association.Comp_Value;
      else
         raise Constraint_Error
           with Variable (Association) & " is not a composite tag.";
      end if;
   end Composite;

   ----------
   -- Kind --
   ----------

   function Kind
     (Association : Templates_Parser.Association)
      return Association_Kind is
   begin
      return Association.Kind;
   end Kind;

   ------------------
   -- Nested_Level --
   ------------------

   function Nested_Level (T : Tag) return Positive is
   begin
      return T.Data.Nested_Level;
   end Nested_Level;

   --------------
   -- Variable --
   --------------

   function Variable
     (Association : Templates_Parser.Association)
      return String is
   begin
      return To_String (Association.Variable);
   end Variable;

end Templates_Parser.Query;
