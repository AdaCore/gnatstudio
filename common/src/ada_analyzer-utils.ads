-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

--  This package provide a set of function used to get further ada-specific
--  information from the analyzis done by the ada analyzer
--  ??? This interface should probably be made language independent.

with Language.Tree; use Language.Tree;

package Ada_Analyzer.Utils is

   function Get_Parent_Type
     (Buffer : String; Construct : Construct_Information)
      return String;
   --  Return the name of the parent type of the construct given in parameter,
   --  or an empty string if none. Parent type is a very large notion here,
   --  it's in general the type on wich the type is based, base type of a
   --  new type or a subtype, pointed type of an acces type, parent of a
   --  tagged type...

   function Is_Access
     (Buffer : String; Construct : Construct_Information)
      return Boolean;
   --  Return true if the type of the construct given in parameter is an
   --  access type, false otherwise.

   function Get_Typename
     (Buffer : String; Construct : Construct_Information)
      return Composite_Identifier;
   --  Provided that the construct given in parameter is a variable/parameter/
   --  constant declaration, this function returns its type.

end Ada_Analyzer.Utils;
