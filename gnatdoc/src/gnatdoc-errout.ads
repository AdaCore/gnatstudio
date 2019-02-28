------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

--  This package contains routines to output error messages and warnings.

private package GNATdoc.Errout is

   procedure Error
     (Context : access constant Docgen_Context;
      Loc     : General_Location;
      Msg     : String);
   --  Report the error message Msg on location Loc

   procedure Error
     (Context : access constant Docgen_Context;
      Entity  : Root_Entity'Class;
      Msg     : String);
   --  Report the warning message Msg on the location of Entity

   procedure Warning
     (Context : access constant Docgen_Context;
      Loc     : General_Location;
      Msg     : String);
   --  Report the warning message Msg on location Loc

   procedure Warning
     (Context : access constant Docgen_Context;
      Entity  : Root_Entity'Class;
      Msg     : String);
   --  Report the warning message Msg on the location of Entity

end GNATdoc.Errout;
