------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

--  This is a general unknown language that does not provide anything.

package Language.Unknown is

   type Unknown_Language is new Language_Root with private;

   Unknown_Lang : constant Language_Access;
   --  Class constant for the Unknown language.

   overriding procedure Parse_Constructs
     (Lang   : access Unknown_Language;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer : UTF8_String;
      Result : out Construct_List);

   overriding procedure Parse_Entities
     (Lang     : access Unknown_Language;
      Buffer   : String;
      Callback : Entity_Callback);

   overriding function Is_Simple_Type
     (Lang : access Unknown_Language;
      Str : String) return Boolean;

   overriding function Keywords
     (Lang : access Unknown_Language) return Strings.String_Access;

   overriding function Keywords
     (Lang : access Unknown_Language)
      return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Keywords
     (Lang : access Unknown_Language) return GNAT.Strings.String_List;

   overriding function Get_Language_Context
     (Lang : access Unknown_Language) return Language_Context_Access;

   overriding function Dereference_Name
     (Lang : access Unknown_Language;
      Name : String) return String;

   overriding function Array_Item_Name
     (Lang  : access Unknown_Language;
      Name  : String;
      Index : String) return String;

   overriding function Record_Field_Name
     (Lang  : access Unknown_Language;
      Name  : String;
      Field : String) return String;

private
   type Unknown_Language is new Language_Root with null record;

   overriding function Get_Name (Lang : access Unknown_Language) return String;
   --  See inherited documentation

   Unknown_Lang : constant Language_Access := new Unknown_Language;
end Language.Unknown;
