-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

--  This package provides the subprograms needed to create a HTML
--  documentation of the project.

--  The procedure Doc_HTML_Create of the Doc_Subprogram_Type in the
--  package Docgen will be called each time a piece of the HTML
--  documentation should be created. By regarding the contents of
--  the passed parameter Info : Doc_Info the subprogram will know which
--  of the private subprograms is to be called to generate the right
--  part of the documentation.
--
--  For each processed source file a .htm file will be generated,
--  but the program will also generate some index packages
--  (unit_index.htm, sub_index.htm, type_index.htm) and for using
--  frames the index file will be created too (index.htm).

with Ada.Text_IO;
with Glide_Kernel;

package Docgen.Html_Output is

   procedure Doc_HTML_Create
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      File          : in Ada.Text_IO.File_Type;
      Info          : in out Docgen.Doc_Info;
      Doc_Directory : String;
      Doc_Suffix    : String);
   --  This procedure is called every time the HTML files are concerned.
   --  What happens with the given information (which of the procedures
   --  below will be called) depands on the contents and the kind of the
   --  Doc_Info type.

end Docgen.Html_Output;
