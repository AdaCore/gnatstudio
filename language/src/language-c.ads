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

--  This is the general C (non debugger specific) support package.
--  See language.ads for a complete spec.

package Language.C is

   type C_Language is new Language_Root with private;

   C_Lang : constant Language_Access;
   --  Class constant for the C language.

   -------------
   -- Parsing --
   -------------

   overriding function Is_Simple_Type
     (Lang : access C_Language; Str : String) return Boolean;

   ------------------
   -- Highlighting --
   ------------------

   overriding function Keywords
     (Lang : access C_Language) return Strings.String_Access;

   overriding function Keywords
     (Lang : access C_Language) return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Keywords
     (Lang : access C_Language) return GNAT.Strings.String_List;

   overriding function Get_Language_Context
     (Lang : access C_Language) return Language_Context_Access;

   -------------------
   -- Block_Folding --
   -------------------

   overriding function Is_Foldable_Block
     (Lang : access C_Language; Cat : Language_Category) return Boolean;

   --------------
   -- Explorer --
   --------------

   overriding function Explorer_Regexps
     (Lang : access C_Language) return Explorer_Categories;

   ------------------------
   -- Naming Conventions --
   ------------------------

   overriding function Dereference_Name
     (Lang : access C_Language;
      Name : String) return String;

   overriding function Array_Item_Name
     (Lang  : access C_Language;
      Name  : String;
      Index : String) return String;

   overriding function Record_Field_Name
     (Lang  : access C_Language;
      Name  : String;
      Field : String) return String;

   overriding function Scope_Separator
     (Lang : access C_Language) return String;

   ----------------------
   -- Source Analyzing --
   ----------------------

   overriding
   procedure Parse_Constructs
     (Lang    : access C_Language;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer  : UTF8_String;
      Result  : out Construct_List);

   overriding
   procedure Parse_Entities
     (Lang     : access C_Language;
      Buffer   : String;
      Callback : Entity_Callback);

   overriding
   procedure Format_Buffer
     (Lang            : access C_Language;
      Buffer          : String;
      Replace         : Replace_Text_Callback;
      From, To        : Natural := 0;
      Indent_Params   : Indent_Parameters := Default_Indent_Parameters;
      Case_Exceptions : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception;
      Is_Optional_Keyword : access function (S : String)
                                             return Boolean := null);

private
   type C_Language is new Language_Root with null record;

   overriding function Get_Name (Lang : access C_Language) return String;
   overriding function Comment_Line
     (Lang    : access C_Language;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String;

   overriding function Entities_Indexed (Self : C_Language) return Boolean;
   --  Unconditionally return True. This enables storing all the C entities
   --  in the structure Entities_Search_Tries, and it is required to give
   --  support for entities completion (see Completion-C packages) and
   --  navigation in Ada sources through entities imported from C.

   C_Lang : constant Language_Access := new C_Language;
end Language.C;
