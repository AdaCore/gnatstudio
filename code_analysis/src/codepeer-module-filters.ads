------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2012-2014, AdaCore                  --
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

package CodePeer.Module.Filters is

   type Ada_Generic_Or_Separate_Filter_Record is
     new GPS.Kernel.Action_Filter_Record with null record;
   --  A filter that matches when the current file is either a generic or
   --  a separate Ada package.

   overriding function Filter_Matches_Primitive
     (Filter  : access Ada_Generic_Or_Separate_Filter_Record;
      Context : Selection_Context) return Boolean;
   --  Whether the context matches Filter.

end CodePeer.Module.Filters;
