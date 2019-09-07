------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

--  This package contains implementation of the CodePeer's message categories
--  filter criteria editor. It is used by Summary Report form.

with CodePeer.Generic_Criteria_Editors;

package CodePeer.Categories_Criteria_Editors is
  new CodePeer.Generic_Criteria_Editors
    (CodePeer.Message_Category,
     CodePeer.Message_Category_Access,
     CodePeer.Get_Name,
     CodePeer.Get_Tooltip,
     CodePeer.Less,
     CodePeer.Message_Category_Sets);
