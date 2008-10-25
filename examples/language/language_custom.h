/* See comments in language_custom.ads */

typedef char * (*comment_line_proc) (char *line, char comment, int length);

typedef struct
  {
    int line;
    int column;
    int index;
  } source_location;

typedef enum
  {cat_unknown,

   cat_package,
   cat_namespace,
   cat_task,
   cat_procedure,
   cat_function,
   cat_method,
   cat_constructor,
   cat_destructor,
   cat_protected,
   cat_entry,

   cat_class,
   cat_structure,
   cat_union,
   cat_type,
   cat_subtype,
   cat_variable,
   cat_local_variable,
   cat_parameter,
   cat_literal,
   cat_representation_clause,

   cat_with,
   cat_use,
   cat_include,

   cat_loop_statement,
   cat_if_statement,
   cat_case_statement,
   cat_select_statement,
   cat_accept_statement,
   cat_declare_block,
   cat_simple_block,
   cat_exception_handler} language_category;

typedef void *construct_access;

typedef construct_access (*new_construct_proc) (void);

typedef void (*set_construct_proc)
  (construct_access construct,
   language_category category,
   char *name,
   char *profile,
   source_location *sloc_start,
   source_location *sloc_entity,
   source_location *sloc_end,
   char is_declaration,
   construct_access prev,
   construct_access next);

typedef struct
  {
    construct_access first, current, last;
  } construct_list;

typedef void (*parse_constructs_proc)
  (char *buffer,
   construct_list *result,
   int length,
   new_construct_proc new_construct,
   set_construct_proc set_construct);

typedef void (*replace_text_cb)
  (int line, int first, int last, char *replace);

typedef enum {unchanged, upper, lower, mixed, smart_mixed} casing_type;

typedef struct
  {
    int indent_level;
    int indent_continue;
    int indent_decl;
    int tab_width;
    char indent_case_extra;
    casing_type reserved_casing;
    casing_type ident_casing;
    char format_operators;
    char use_tabs;
    char align_on_colons;
    char align_on_arrows;
  } indent_parameters;

typedef void (*format_buffer_proc)
  (char *buffer, replace_text_cb replace,
   int from, int to, indent_parameters *indent_params, int length);

typedef enum
  {normal_text,
   identifier_text,
   keyword_text,
   comment_text,
   character_text,
   string_text} language_entity;

typedef char (*entity_cb)
  (language_entity entity,
   source_location *sloc_start,
   source_location *sloc_end,
   char partial_entity);

typedef void (*parse_entities_proc)
  (char *buffer, entity_cb callback, int length);

