------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

--  This package provides functions for drawing tooltips representing entity
--  informations.

with Cairo;            use Cairo;

with GPS.Kernel;       use GPS.Kernel;

with Entities;         use Entities;
with Entities.Queries; use Entities.Queries;

with Language.Tree.Database; use Language.Tree.Database;

package Entities.Tooltips is

   function Draw_Tooltip
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Entity_Information;
      Ref           : Entity_Reference;
      Status        : Find_Decl_Or_Body_Query_Status;
      Accurate_Xref : Boolean;
      Draw_Border   : Boolean) return Cairo_Surface;
   --  Return a tooltip representing Entity.

   function Draw_Tooltip
     (Kernel       : access Kernel_Handle_Record'Class;
      Entity      : Entity_Access;
      Draw_Border : Boolean;
      Guess       : Boolean := False) return Cairo_Surface;
   --  Same as above, based on an entity access. If guess is true then the
   --  entity information is a guess - may not be the actual one for the
   --  tooltip.

end Entities.Tooltips;
