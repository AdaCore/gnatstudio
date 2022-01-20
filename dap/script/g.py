# the whole JSON specification: dict(String, JSONObject)
data = dict()

# holds the JSON specification definitions: dict(String, JSONObject)
definitions = dict()

# Translation table from specification type to Ada compliant type
# dict (tuple(String, ...): String | None)
type_translation_table = {
    tuple(["array"]): None,
    tuple(["boolean"]): "Boolean",
    tuple(["integer"]): "LSP_Number",
    tuple(["number"]): "LSP_Number",  # there is no float in the latest spec
    tuple(["object"]): None,
    tuple(["string"]): "Virtual_String",
    # sorting it to have a sure way of finding then in the dict, i.e:
    # typeTranslation[tuple(sorted(['null','string']))]
    # is the same as
    # typeTranslation[tuple(sorted(['string', 'null']))]
    tuple(sorted(["array", "boolean", "integer", "null", "number", "object",
                  "string"])): "LSP_Any",
    tuple(sorted(["integer", "string"])): "LSP_Number_Or_String",
    tuple(sorted(["string", "null"])): "Virtual_String"
}

# used to check that type triage did not miss a typeform the definition
all_types = []

# Set of all enumerated types: set(String)
enum_types_list = set()

# dict(String: dict(String: String))
generated_types = dict()

# dict(String: Boolean) TODO: change it to set
generated_access_types = dict()

# dict(String: Boolean)
generated_controlled_types = dict()

# dict(String: dict(String: String))
generated_vectors = dict()

# graph used to generate the dependency list for types in the JSON specification
graph = None

# Ada reserved names, illegal tokens and theirs correct matching replacement
reserved_names = {
    "all": "a_all",
    "function": "a_function",
    "interface": "a_interface",
    "type": "a_type",
    "__restart": "a_restart",
    "source": "a_source",
    "body": "a_body",
    "message": "a_message"  # name clashing with Message record type
}

# dict(String : JSONObject)
no_deps = dict()

# dict(String : JSONObject)
enum_types = dict()

# dict(String : JSONObject)
record_types = dict()

# dict(String : JSONObject)
record_with_ref_types = dict()

# dict(String : JSONObject)
complex_types = dict()

# open file to write to
output = None
