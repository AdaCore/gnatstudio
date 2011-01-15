-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2010-2011, AdaCore                  --
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

with Language; use Language;
with Language_Handlers; use Language_Handlers;
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
   --  return if the tooltip is a guess.

   function Get_Tooltip_Guess_Message return String;
   --  return the message to add on top of the tooltip when guess is true

   function Get_Tooltip_Documentation
     (Handler    : Language_Handler;
      Database   : Construct_Database_Access;
      Entity : Entity_Information) return String;
   --  Return the documentation for the entity (prefixed by a LF char if not
   --  null)
   --  Return empty string if documentation cannot be found in the construct
   --  database

   function Get_Tooltip_Header
      (Entity : Entity_Information) return String;
   --  Return a string in pango markup format to represent the header of a
   --  tooltip.

   function Get_Tooltip_Information
      (Entity : Entity_Information) return Tooltip_Information;
   --  Return information to be able to display the right icon
   --  depending on category and visibility

end Entities.Tooltips_Assistant;
