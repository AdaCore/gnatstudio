------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

private

   type Message_Filter is
     new GPS.Kernel.Messages.Abstract_Message_Filter with record
      Tools      : Tool_Vectors.Vector;
      Severities : Severity_Vectors.Vector;
      Rules      : Rule_Sets.Set;
   end record;

   overriding function Apply
     (Self    : in out Message_Filter;
      Message : GPS.Kernel.Messages.Abstract_Message'Class)
      return GPS.Kernel.Messages.Filter_Result;

end GNAThub.Filters;
