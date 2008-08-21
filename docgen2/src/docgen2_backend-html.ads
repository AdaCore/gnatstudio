-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2007-2008, AdaCore                --
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

--  HTML Backend for docgen2

package Docgen2_Backend.HTML is

   type HTML_Backend_Record is new Backend_Record with null record;

   overriding function Get_Template
     (Backend    : access HTML_Backend_Record;
      System_Dir : String;
      Kind       : Template_Kind) return String;
   --  See inherited doc.

   overriding function Get_Support_Dir
     (Backend    : access HTML_Backend_Record;
      System_Dir : String) return String;
   --  See inherited doc.

   overriding function To_Destination_Name
     (Backend  : access HTML_Backend_Record;
      Basename : String)
      return String;
   --  See inherited doc.

   overriding function To_Destination_Name
     (Backend  : access HTML_Backend_Record;
      Src_File : String;
      Pkg_Nb   : Natural)
      return String;
   --  See inherited doc.

   overriding function To_Href
     (Backend  : access HTML_Backend_Record;
      Location : String;
      Src_File : String;
      Pkg_Nb   : Natural)
      return String;
   --  See inherited doc.

   overriding function Gen_Paragraph
     (Backend : access HTML_Backend_Record;
      Msg     : String) return String;
   --  See inherited doc.

   overriding function Gen_Ref
     (Backend : access HTML_Backend_Record;
      Name    : String) return String;
   --  See inherited doc.

   overriding function Gen_Href
     (Backend                : access HTML_Backend_Record;
      Name, Href, Title : String)
      return String;
   --  See inherited doc.

   overriding function Gen_Tag
     (Backend : access HTML_Backend_Record;
      Tag     : Language_Entity;
      Value   : String;
      Emphasis : Boolean := False)
      return String;
   --  See inherited doc.

   overriding function Gen_User_Tag
     (Backend    : access HTML_Backend_Record;
      User_Tag   : String;
      Attributes : String;
      Value      : String) return String;
   --  See inherited doc.

   overriding function Filter
     (Backend  : access HTML_Backend_Record;
      S        : String) return String;
   --  See inherited doc

end Docgen2_Backend.HTML;
