------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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
