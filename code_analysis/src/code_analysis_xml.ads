-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2007-2010, AdaCore                  --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

--  <description>
--  This package provides the subprograms needed by the XML dumping
--  of a Code_Analysis tree structure on the standard output.
--  </description>

with GNATCOLL.VFS;  use GNATCOLL.VFS;
with XML_Utils;     use XML_Utils;
with Code_Analysis; use Code_Analysis;
with Projects;      use Projects;

package Code_Analysis_XML is

   procedure Dump_XML
     (Projects : Code_Analysis_Tree;
      Parent   : Node_Ptr);
   --  Starts a dominos calling to the xml dumping subprograms
   --  following the Code_Analysis tree structure.

   generic
      with procedure On_New_File
        (Project : Project_Type;
         File    : GNATCOLL.VFS.Virtual_File);
   procedure Parse_XML
     (Project  : Project_Type;
      Node     : Node_Ptr);
   --  Starts a dominos calling to the xml parsing subprograms
   --  to fill the Code_Analysis tree structure.

end Code_Analysis_XML;
