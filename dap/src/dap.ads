------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

--  Main package in hierarchy that uses debugger adapter protocol
--  for debugging.

package DAP is

   pragma Pure;

   type Client_Id_Type is new Natural;
   --  Used to count started debuggers and identify instances

   subtype Valid_Client_Id_Type is Client_Id_Type
     range 1 .. Client_Id_Type'Last;
   --  Valid range for real instance identifier

   No_Client : constant Client_Id_Type := 0;
   --  Constant for absent instance

end DAP;
