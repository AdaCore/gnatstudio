------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Language; use Language;
with Language.Tree.Database; use Language.Tree.Database;
with Entities.Queries; use Entities.Queries;

package Entities.Tooltips_Assistant is

   type Tooltip_Information is record
      Is_Spec    : Boolean;
      Visibility : Construct_Visibility;
      Category   : Language_Category;
   end record;

   function Is_Tooltip_Guess
     (Status        : Find_Decl_Or_Body_Query_Status;
      Accurate_Xref : Boolean) return Boolean;
   --  return if the tooltip is a guess

   function Get_Tooltip_Guess_Message return String;
   --  return the message to add on top of the tooltip when guess is true

   function Get_Tooltip_Header
      (Entity : Entity_Information) return String;
   --  Return a string in pango markup format to represent the header of a
   --  tooltip.

   function Get_Tooltip_Information
      (Entity : Entity_Information) return Tooltip_Information;
   --  Return information to be able to display the right icon
   --  depending on category and visibility.

end Entities.Tooltips_Assistant;
