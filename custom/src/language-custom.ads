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

--  This is a generic language that it customized from a XML file.
--  See language.ads for a complete spec.

with GNAT.Expect;
with GNAT.Strings;

with XML_Utils;

with Language_Handlers;
with GPS.Kernel;
with Ada.Strings.Wide_Wide_Maps;  use Ada.Strings.Wide_Wide_Maps;

package Language.Custom is

   type Custom_Language is new Language_Root with private;
   type Custom_Language_Access is access all Custom_Language'Class;

   procedure Initialize
     (Handler : access Language_Handlers.Language_Handler_Record'Class;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Top     : XML_Utils.Node_Ptr);
   --  Initialize a new language based on the contents of an XML node.
   --  The language and its extensions are automatically registered in the
   --  handler.

   overriding procedure Free (Lang : in out Custom_Language);

   -------------
   -- Parsing --
   -------------

   overriding function Is_Simple_Type
     (Lang : access Custom_Language; Str : String) return Boolean;

   overriding function Keywords
     (Lang : access Custom_Language) return Strings.String_Access;

   overriding function Keywords
     (Lang : access Custom_Language) return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Keywords
     (Lang : access Custom_Language) return GNAT.Strings.String_List;

   overriding function Is_Word_Char
     (Lang : access Custom_Language;
      Char : Wide_Wide_Character) return Boolean;

   overriding function Get_Language_Context
     (Lang : access Custom_Language) return Language_Context_Access;

   --------------
   -- Explorer --
   --------------

   overriding function Explorer_Regexps
     (Lang : access Custom_Language) return Explorer_Categories;

   ------------------------
   -- Naming conventions --
   ------------------------

   overriding function Dereference_Name
     (Lang : access Custom_Language;
      Name : String) return String;

   overriding function Array_Item_Name
     (Lang  : access Custom_Language;
      Name  : String;
      Index : String) return String;

   overriding function Record_Field_Name
     (Lang  : access Custom_Language;
      Name  : String;
      Field : String) return String;

   ------------------
   -- Field access --
   ------------------

   overriding function Get_Name (Lang : access Custom_Language) return String;
   --  Return the name associated with Lang.

   ----------------------
   -- Source Analyzing --
   ----------------------

   --  The following primitives are either defaulting to the Language_Root
   --  implementation, or redefined using a shared library specified in the
   --  xml file.

   overriding procedure Parse_Constructs
     (Lang   : access Custom_Language;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer : UTF8_String;
      Result : out Construct_List);

   overriding procedure Format_Buffer
     (Lang                : access Custom_Language;
      Buffer              : String;
      Replace             : Replace_Text_Callback;
      From, To            : Natural := 0;
      Indent_Params       : Indent_Parameters := Default_Indent_Parameters;
      Case_Exceptions     : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception;
      Is_Optional_Keyword : access function (S : String)
                                             return Boolean := null);

   overriding procedure Parse_Entities
     (Lang     : access Custom_Language;
      Buffer   : String;
      Callback : Entity_Callback);

   overriding procedure Get_Indentation_Parameters
     (Lang         : access Custom_Language;
      Params       : out Indent_Parameters;
      Indent_Style : out Indentation_Kind);

private

   type Explorer_Categories_Access is access all Explorer_Categories;
   type Project_Field_Array_Access is access all Project_Field_Array;

   function Comment_Line
     (Lang    : access Custom_Language;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String;

   type Custom_Language is new Language_Root with record
      Categories       : Explorer_Categories_Access;
      Keywords         : GNAT.Expect.Pattern_Matcher_Access;
      Keywords_Regexp  : Strings.String_Access;
      Keywords_List    : GNAT.Strings.String_List_Access;
      Context          : Language_Context_Access;
      Name             : XML_Utils.String_Ptr;
      Project_Fields   : Project_Field_Array_Access;
      Parent           : Language_Access;
      Next             : Custom_Language_Access;
      Word_Chars       : Wide_Wide_Character_Set;
   end record;

end Language.Custom;
