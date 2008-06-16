-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                   Copyright (C) 2000-2008, AdaCore                --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This is a general unknown language that does not provide anything.

package Language.Unknown is

   type Unknown_Language is new Language_Root with private;

   Unknown_Lang : constant Language_Access;
   --  Class constant for the Unknown language.

   procedure Parse_Constructs
     (Lang   : access Unknown_Language;
      Buffer : String;
      Result : out Construct_List);

   procedure Parse_Entities
     (Lang     : access Unknown_Language;
      Buffer   : String;
      Callback : Entity_Callback);

   function Is_Simple_Type
     (Lang : access Unknown_Language;
      Str : String) return Boolean;

   function Keywords
     (Lang : access Unknown_Language) return Strings.String_Access;

   function Keywords
     (Lang : access Unknown_Language) return Pattern_Matcher_Access;

   function Keywords
     (Lang : access Unknown_Language) return GNAT.Strings.String_List;

   function Get_Language_Context
     (Lang : access Unknown_Language) return Language_Context_Access;

   function Dereference_Name
     (Lang : access Unknown_Language;
      Name : String) return String;

   function Array_Item_Name
     (Lang  : access Unknown_Language;
      Name  : String;
      Index : String) return String;

   function Record_Field_Name
     (Lang  : access Unknown_Language;
      Name  : String;
      Field : String) return String;

   function Get_Project_Fields
     (Lang : access Unknown_Language) return Project_Field_Array;

private
   type Unknown_Language is new Language_Root with null record;

   function Get_Name (Lang : access Unknown_Language) return String;
   --  See inherited documentation

   Unknown_Lang : constant Language_Access := new Unknown_Language;
end Language.Unknown;
