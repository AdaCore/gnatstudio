-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
--                              ACT-Europe                           --
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

--  This is a generic language that it customized from a XML file.
--  See language.ads for a complete spec.

with System;
with Interfaces.C.Strings;
with Glib.Xml_Int;

package Language.Custom is

   type Custom_Language is new Language_Root with private;
   type Custom_Language_Access is access all Custom_Language'Class;

   procedure Initialize
     (Lang : access Custom_Language'Class;
      Top  : Glib.Xml_Int.Node_Ptr);
   --  Initialize Lang based on the contents of an XML node.

   -------------
   -- Parsing --
   -------------

   function Is_Simple_Type
     (Lang : access Custom_Language; Str : String) return Boolean;

   function Keywords (Lang : access Custom_Language)
     return GNAT.Regpat.Pattern_Matcher;

   function Get_Language_Context
     (Lang : access Custom_Language) return Language_Context_Access;

   --------------
   -- Explorer --
   --------------

   function Explorer_Regexps
     (Lang : access Custom_Language) return Explorer_Categories;

   ------------------------
   -- Naming conventions --
   ------------------------

   function Dereference_Name
     (Lang : access Custom_Language;
      Name : String) return String;

   function Array_Item_Name
     (Lang  : access Custom_Language;
      Name  : String;
      Index : String) return String;

   function Record_Field_Name
     (Lang  : access Custom_Language;
      Name  : String;
      Field : String) return String;

   ---------------------
   -- Project support --
   ---------------------

   function Get_Project_Fields
     (Lang : access Custom_Language) return Project_Field_Array;

   ------------------
   -- Field access --
   ------------------

   function Get_Name (Lang : access Custom_Language) return String;
   --  Return the name associated with Lang.

   function Get_Spec_Suffix (Lang : access Custom_Language) return String;
   --  Return the spec suffix associated with Lang.

   function Get_Body_Suffix (Lang : access Custom_Language) return String;
   --  Return the body suffix associated with Lang.

   ----------------------
   -- Source Analyzing --
   ----------------------

   --  The following primitives are either defaulting to the Language_Root
   --  implementation, or redefined using a shared library specified in the
   --  xml file.

   function Comment_Line
     (Lang : access Custom_Language;
      Line : String) return String;

   function Uncomment_Line
     (Lang : access Custom_Language;
      Line : String) return String;

   procedure Parse_Constructs
     (Lang   : access Custom_Language;
      Buffer : String;
      Result : out Construct_List);

   procedure Format_Buffer
     (Lang          : access Custom_Language;
      Buffer        : String;
      Replace       : Replace_Text_Callback;
      From, To      : Natural := 0;
      Indent_Params : Indent_Parameters := Default_Indent_Parameters);

   procedure Parse_Entities
     (Lang     : access Custom_Language;
      Buffer   : String;
      Callback : Entity_Callback);

private

   type Explorer_Categories_Access is access all Explorer_Categories;
   type Pattern_Matcher_Access is access all GNAT.Regpat.Pattern_Matcher;
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
     function (Line : String; Length : Integer) return chars_ptr;
   pragma Convention (C, Comment_Line_Proc);
   --  C profile for Comment/Uncomment_Line:
   --  typedef char * (*comment_line_proc) (char *line, int length);

   type Parse_Constructs_Proc is access
     procedure (Buffer        : String;
                Result        : out Construct_List;
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

   type Custom_Language is new Language_Root with record
      Categories       : Explorer_Categories_Access;
      Keywords         : Pattern_Matcher_Access;
      Context          : Language_Context_Access;
      Name             : Glib.String_Ptr;
      Spec_Suffix      : Glib.String_Ptr;
      Body_Suffix      : Glib.String_Ptr;
      Project_Fields   : Project_Field_Array_Access;
      Comment_Line     : Comment_Line_Proc;
      Uncomment_Line   : Comment_Line_Proc;
      Parse_Constructs : Parse_Constructs_Proc;
      Format_Buffer    : Format_Buffer_Proc;
      Parse_Entities   : Parse_Entities_Proc;
      Parent           : Language_Access;
      Next             : Custom_Language_Access;
   end record;

end Language.Custom;
