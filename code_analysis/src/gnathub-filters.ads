------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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
--  Filter to be used by messages container to manage visibility of GNAThub
--  messages.

with GPS.Kernel.Messages;

package GNAThub.Filters is

   type Message_Filter is
     new GPS.Kernel.Messages.Abstract_Message_Filter with private;
   type Filter_Access is access all GNAThub.Filters.Message_Filter'Class;

   procedure Fill
     (Self       : in out Message_Filter;
      Tools      : Tools_Ordered_Sets.Set;
      Severities : Severities_Ordered_Sets.Set;
      Rules      : Rule_Sets.Set);

   procedure Clear (Self : in out Message_Filter);

   procedure Add_Tool
     (Self : in out Message_Filter;
      Tool : Tool_Access);

   procedure Add_Severity
     (Self     : in out Message_Filter;
      Severity : Severity_Access);

   procedure Add_Rule
     (Self : in out Message_Filter;
      Rule : Rule_Access);

   overriding function Apply
     (Self    : in out Message_Filter;
      Message : GPS.Kernel.Messages.Abstract_Message'Class)
      return GPS.Kernel.Messages.Filter_Result;

private

   type Message_Filter is
     new GPS.Kernel.Messages.Abstract_Message_Filter with record
      Tools      : Tools_Ordered_Sets.Set;
      Severities : Severities_Ordered_Sets.Set;
      Rules      : Rule_Sets.Set;
   end record;

end GNAThub.Filters;
