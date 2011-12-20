------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

--  <description>
--  This package provides the subprograms needed by the XML dumping
--  of a Code_Analysis tree structure on the standard output.
--  </description>

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with XML_Utils;         use XML_Utils;
with Code_Analysis;     use Code_Analysis;

package Code_Analysis_XML is

   procedure Dump_XML
     (Projects : Code_Analysis_Tree;
      Parent   : Node_Ptr);
   --  Starts a dominos calling to the xml dumping subprograms
   --  following the Code_Analysis tree structure.

   generic
      with procedure On_New_File
        (Project : GNATCOLL.Projects.Project_Type;
         File    : GNATCOLL.VFS.Virtual_File);
   procedure Parse_XML
     (Project  : GNATCOLL.Projects.Project_Type;
      Node     : Node_Ptr);
   --  Starts a dominos calling to the xml parsing subprograms
   --  to fill the Code_Analysis tree structure.

end Code_Analysis_XML;
