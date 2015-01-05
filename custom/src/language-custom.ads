------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

with Interfaces.C.Strings;
with System;

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
      Indent_Offset       : Natural := 0;
      Case_Exceptions     : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception;
      Is_Optional_Keyword : access function (S : String)
                                             return Boolean := null);

   overriding procedure Parse_Entities
     (Lang     : access Custom_Language;
      Buffer   : String;
      Callback : Entity_Callback);

private

   type Explorer_Categories_Access is access all Explorer_Categories;
   type Project_Field_Array_Access is access all Project_Field_Array;

   -----------------------------------------------
   -- C interface to source analyzing functions --
   -----------------------------------------------

   use Interfaces.C.Strings;

   function New_Construct return Construct_Access;
   --  Export allocator to C via callbacks
   pragma Convention (C, New_Construct);

   procedure Set_Construct
     (Construct      : Construct_Access;
      Symbols        : GNATCOLL.Symbols.Symbol_Table_Access;
      Category       : Language_Category;
      Name           : chars_ptr;
      Profile        : chars_ptr;
      Sloc_Start     : Source_Location;
      Sloc_Entity    : Source_Location;
      Sloc_End       : Source_Location;
      Is_Declaration : Boolean;
      Prev, Next     : Construct_Access);
   --  Export setter to C via callbacks
   pragma Convention (C, Set_Construct);

   type Comment_Line_Proc is access
     function (Line : String; Comment : Boolean; Length : Integer)
               return chars_ptr;
   pragma Convention (C, Comment_Line_Proc);
   --  C profile for Comment_Line:
   --  typedef char * (*comment_line_proc)
   --    (char *line, char comment, int length);

   type Parse_Constructs_Proc is access
     procedure (Buffer        : String;
                Result        : out Construct_List;
                Symbols       : GNATCOLL.Symbols.Symbol_Table_Access;
                Length        : Integer;
                New_Construct : System.Address;
                Set_Construct : System.Address);
   pragma Convention (C, Parse_Constructs_Proc);
   --  C profile for Parse_Constructs:
   --
   --  typedef struct
   --    {
   --      int line;
   --      int column;
   --      int index;
   --    } source_location;
   --
   --  typedef enum
   --    {cat_unknown,
   --
   --     cat_package,
   --     cat_namespace,
   --     cat_task,
   --     cat_procedure,
   --     cat_function,
   --     cat_method,
   --     cat_constructor,
   --     cat_destructor,
   --     cat_protected,
   --     cat_entry,
   --
   --     cat_class,
   --     cat_structure,
   --     cat_union,
   --     cat_type,
   --     cat_subtype,
   --     cat_variable,
   --     cat_local_variable,
   --     cat_parameter,
   --     cat_literal,
   --     cat_representation_clause,
   --
   --     cat_with,
   --     cat_use,
   --     cat_include,
   --
   --     cat_loop_statement,
   --     cat_if_statement,
   --     cat_case_statement,
   --     cat_select_statement,
   --     cat_accept_statement,
   --     cat_declare_block,
   --     cat_simple_block,
   --     cat_exception_handler} language_category;
   --
   --  typedef void *construct_access;
   --
   --  typedef construct_access (*new_construct_proc) (void);
   --
   --  typedef void (*set_construct_proc)
   --    (construct_access construct,
   --     void* symbols,
   --     language_category category,
   --     char *name,
   --     char *profile,
   --     source_location *sloc_start,
   --     source_location *sloc_entity,
   --     source_location *sloc_end,
   --     char is_declaration,
   --     construct_access prev,
   --     construct_access next);
   --
   --  typedef struct
   --    {
   --      construct_access first, current, last;
   --    } construct_list;
   --
   --  typedef void (*parse_constructs_proc)
   --    (char *buffer,
   --     construct_list *result,
   --     void* symbols,  //  passed as-is to set_construct
   --     int length,
   --     new_construct_proc new_construct,
   --     set_construct_proc set_construct);

   type Format_Buffer_Proc is access
     procedure (Buffer        : String;
                Replace       : System.Address;
                From, To      : Integer;
                Indent_Params : Indent_Parameters;
                Length        : Integer);
   pragma Convention (C, Format_Buffer_Proc);
   --  C profile for Format_Buffer:
   --
   --  typedef void (*replace_text_cb)
   --    (int line, int first, int last, char *replace);
   --
   --  typedef enum {unchanged, upper, lower, mixed, smart_mixed} casing_type;
   --
   --  typedef struct
   --    {
   --      int indent_level;
   --      int indent_continue;
   --      int indent_decl;
   --      int tab_width;
   --      char indent_case_extra;
   --      casing_type reserved_casing;
   --      casing_type ident_casing;
   --      char format_operators;
   --      char use_tabs;
   --      char align_on_colons;
   --      char align_on_arrows;
   --    } indent_parameters;
   --
   --  typedef void (*format_buffer_proc)
   --    (char *buffer, replace_text_cb replace,
   --     int from, int to, indent_parameters *indent_params, int length);

   type Parse_Entities_Proc is access
     procedure (Buffer   : String;
                Callback : System.Address;
                Length   : Integer);
   pragma Convention (C, Parse_Entities_Proc);
   --  C profile for Parse_Entities:
   --
   --  typedef enum
   --    {normal_text,
   --     identifier_text,
   --     keyword_text,
   --     comment_text,
   --     character_text,
   --     string_text} language_entity;
   --
   --  typedef char (*entity_cb)
   --    (language_entity entity,
   --     source_location *sloc_start,
   --     source_location *sloc_end,
   --     char partial_entity);
   --
   --  typedef void (*parse_entities_proc)
   --    (char *buffer, int length, entity_cb callback);

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
      Comment_Line     : Comment_Line_Proc;
      Parse_Constructs : Parse_Constructs_Proc;
      Format_Buffer    : Format_Buffer_Proc;
      Parse_Entities   : Parse_Entities_Proc;
      Parent           : Language_Access;
      Next             : Custom_Language_Access;
      Word_Chars       : Wide_Wide_Character_Set;
   end record;

end Language.Custom;
