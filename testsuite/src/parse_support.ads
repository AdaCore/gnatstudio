------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with Debugger;                use Debugger;
with GVD.Variables.Types;     use GVD.Variables.Types;
with GVD.Types;               use GVD.Types;
with Language;                use Language;

package Parse_Support is

   procedure Print
      (Self : GVD_Type_Holder;
       Lang : not null access Language_Root'Class;
       Name : String);
   --  Print the contents of Self on stdout

   procedure Blocking_Run
     (Debugger : not null access Debugger_Root'Class;
      Kind     : Debugger_Type);
   --  Execute the 'run' command in a blocking way, waiting for the prompt
   --  before returning.

end Parse_Support;
