import json
import sys


# region Tools
def remove_json_comment_node(json_object):
    """
    Make a depth first traversal of the json object
    and using a predicate and a function it changes to the json object

    :param json_object: JSONObject
    """

    def is_text(key):
        """
        Returns True if the key is considered non essential text
        :param key: String
        :return: Boolean
        """
        return (str(key) == "description" or
                str(key) == "title" or
                str(key) == "enumDescriptions")

    def remove_key(json_obj, key):
        """
        Removes a key from a JSONObject
        :param json_obj: JSONObject
        :param key: String
        """
        del json_obj[key]

    if isinstance(json_object, dict):
        for k in list(json_object.keys()):
            if is_text(k):
                remove_key(json_object, k)
        for k in list(json_object.keys()):
            remove_json_comment_node(json_object[k])


def pretty_print(json_object, indent=2):
    """
    Take a json object and pretty print it

    :param json_object: the json object to be printed
    :param indent: optional indent (int) (default: 2)
    """
    print(json.dumps(json_object, indent=indent))


def eprint(*args, **kwargs):
    """
    Prints on the standard error output (stderr).
    Behaves like print function.

    :param args: behaves like print function
    :param kwargs: beehaves like print function
    """
    print(*args, file=sys.stderr, **kwargs)


def epretty_print(json_object, indent=2):
    """
    Take a json object and pretty print it on standard error output

    :param json_object: the json object to be printed
    :param indent: optional indent (int) (default: 2)
    """
    eprint(json.dumps(json_object, indent=indent))


def number_of_types(typeArr):
    """
    :param typeArr: [[(Name, JSONSpec)]]
    :return: Number
    """
    return sum(list(map(lambda x: len(x), typeArr)))


def check_definitions_number_match(defs, typeArr):
    """
    :return: Boolean
    """
    return len(defs) == number_of_types(typeArr)


def get_key(field, reserved_names):
    """
    Returns the replacement string for the ada reserved keyword provided

    :param field: String
    :param reserved_names: dict(String, String)
    :return: the string replacement for `field` from `reserved_names`
    """
    lower = field.lower()
    if lower in reserved_names:
        return reserved_names[lower]
    else:
        return field


def create_dep_list(obj, enumTypes):
    """
    Returns a list of all the types that are needed to create/print the one
    passed as parameter

    :param obj: JSONObject {String: JSONObject}
    :param enumTypes: dict(String, JSONObject)
    :return: [String]
    """
    (record_name, record) = obj
    deps = []
    if 'type' in record and record['type'] == 'string':
        return deps

    if 'type' in record and record['type'] != 'string':
        if 'properties' not in record:
            return deps
        for attr_name, attr_ojb in record['properties'].items():
            if 'type' in attr_ojb:
                if attr_ojb['type'] == 'array':
                    if 'type' in attr_ojb['items']:
                        deps.append("DAP_"
                                    + attr_ojb['items']['type'].title()
                                    + "_Vector")
                    if '$ref' in attr_ojb['items']:
                        deps.append("DAP_"
                                    + attr_ojb['items']['$ref']
                                    .split('/')[-1]
                                    + "_Vector")
                if attr_ojb['type'] == 'object':
                    # let's juste assume that any "object" inside an
                    deps.append('DAP_String_Maps')
            if '$ref' in attr_ojb:
                dep_name = attr_ojb['$ref'].split('/')[-1]
                if dep_name not in list(map(lambda x: x[0], enumTypes)):
                    deps.append(dep_name)

    if 'allOf' in record:
        inner_record = record['allOf']
        for inner_obj in inner_record:
            if '$ref' in inner_obj:
                dep_name = inner_obj['$ref'].split('/')[-1]
                if dep_name not in list(map(lambda x: x[0], enumTypes)):
                    deps.append(dep_name)
            if 'properties' in inner_obj:
                inner_inner_obj = inner_obj['properties']
                for attr_name, attr_obj in inner_inner_obj.items():
                    if 'type' in attr_obj:
                        if attr_obj['type'] == 'array':
                            if 'type' in attr_obj['items']:
                                deps.append("DAP_"
                                            + attr_obj['items']['type'].title()
                                            + "_Vector")
                            if '$ref' in attr_obj['items']:
                                deps.append("DAP_"
                                            + attr_obj['items']['$ref']
                                            .split('/')[-1]
                                            + "_Vector")
                        if attr_obj['type'] == 'object':
                            deps.append('DAP_String_Maps')
                    if '$ref' in attr_obj:
                        dep_name = attr_obj['$ref'].split('/')[-1]
                        if dep_name not in list(map(lambda x: x[0], enumTypes)):
                            deps.append(dep_name)

    return deps

# endregion
