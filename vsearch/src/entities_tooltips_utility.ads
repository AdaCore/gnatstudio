------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

--  This package provides functions to get informations representing entity
--  informations and needed to display tooltips (in GPS & GNATbench).

with Language; use Language;
with GPS.Kernel;       use GPS.Kernel;
with Language.Tree.Database; use Language.Tree.Database;
with Xref; use Xref;

package Entities_Tooltips_Utility is

   type Tooltip_Information is record
      Is_Spec    : Boolean;
      Visibility : Construct_Visibility;
      Category   : Language_Category;
   end record;

   function Get_Tooltip_Information
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class) return Tooltip_Information;
   --  Return information to be able to display the right icon
   --  depending on category and visibility.

   function Get_Tooltip_Information
     (Entity : Entity_Access) return Tooltip_Information;
   --  Return information to be able to display the right icon
   --  depending on category and visibility.

   function Get_Tooltip_Header
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class) return String;
   --  Return the header of the tooltip

   function Get_Tooltip_Header
     (Entity      : Entity_Access) return String;
   --  Return the header of the tooltip

   function Get_Tooltip_Documentation
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Root_Entity'Class;
      Ref           : Root_Entity_Reference'Class) return String;
   --  Return the documentation of the tooltip

   function Get_Tooltip_Documentation
     (Kernel  : access Kernel_Handle_Record'Class;
      Entity  : Entity_Access) return String;
   --  Return the documentation of the tooltip

   function Is_Guess
     (Entity : Root_Entity'Class) return Boolean;
   --  return true if entity information is a guess

   function Tooltip_Guess_Message return String;
   --  Message to display in tooltips when an entity is not up-to-date

end Entities_Tooltips_Utility;
