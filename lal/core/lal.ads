------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2017-2019, AdaCore                   --
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
--  Root package of libAdaLang integration module.

package LAL is

   pragma Pure;

   type Use_LAL_Kinds is
     (Use_LAL_In_Editor,
      Use_LAL_In_Outline,
      Use_LAL_In_Shell,
      Use_LAL_In_Info,
      Use_LAL_In_GNATHUB,
      Use_LAL_In_COV,
      Use_LAL_In_Indent,
      Use_LAL_In_Highlight);

   type Use_LAL_Configuration is array (Use_LAL_Kinds) of Boolean;

end LAL;
