-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides some general subprograms to manipulate languages, and
--  find the language given any filename.
--  Note that this type is abstract, and overloaded by two other types, one for
--  GVD standalone that simply uses the file extensions to guess the language,
--  and the other one for GPS that uses the project files and their naming
--  scheme.

with Language;
with GNAT.OS_Lib;

package Language_Handlers is

   type Language_Handler_Record is abstract tagged private;
   type Language_Handler is access all Language_Handler_Record'Class;

   function Get_Language_From_File
     (Handler         : access Language_Handler_Record;
      Source_Filename : String) return Language.Language_Access is abstract;
   --  Return the name of the language used for Source_Filename.
   --  null is returned if the language wasn't recognized.

   function Get_Language_From_File
     (Handler         : access Language_Handler_Record;
      Source_Filename : String) return String is abstract;
   --  Same as above, but return the language name in a canonical form.
   --  The empty string is returned if the language wasn't recognized.

   ------------------
   -- Registration --
   ------------------
   --  Languages must be registered before they are available to Gvd or GPS.

   procedure Register_Language
     (Handler : access Language_Handler_Record;
      Name    : String;
      Lang    : Language.Language_Access) is abstract;
   --  Register a new language (or override the old definition if any).
   --  No copy of Lang is done.

   function Known_Languages
     (Handler : access Language_Handler_Record)
      return GNAT.OS_Lib.Argument_List is abstract;
   --  Return the list of known languages.
   --  The returned value must be freed by the caller.

private
   type Language_Handler_Record is abstract tagged null record;

end Language_Handlers;
