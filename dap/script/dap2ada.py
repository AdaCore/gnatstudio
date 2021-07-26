import g
import json
import networkx as nx
from generator_tools import generate_body, generate_spec
from tools import (create_dep_list, check_definitions_number_match,
                   number_of_types, eprint, remove_json_comment_node)

# region Load data from JSON
with open('dap.json', 'r') as json_file:
    g.data = json.load(json_file)

# endregion

# region populate global variables

remove_json_comment_node(g.data)
g.definitions = g.data['definitions']
g.graph = nx.DiGraph()
g.enum_types = {
    key: g.definitions[key]
    for key in sorted(g.definitions.keys())
    if 'type' in g.definitions[key].keys()
       and g.definitions[key]['type'] == 'string'
}
g.all_types.append(g.enum_types)

g.record_types = {
    key: g.definitions[key]
    for key in sorted(g.definitions.keys())
    if 'type' in g.definitions[key].keys()
       and g.definitions[key]['type'] != 'string'
       and str(g.definitions[key]).count("$ref") == 0
}
g.all_types.append(g.record_types)

g.record_with_ref_types = {
    key: g.definitions[key]
    for key in sorted(g.definitions.keys())
    if 'type' in g.definitions[key].keys()
       and g.definitions[key]['type'] != 'string'
       and str(g.definitions[key]).count("$ref") != 0
}
g.all_types.append(g.record_with_ref_types)

g.complex_types = {
    key: g.definitions[key]
    for key in sorted(g.definitions.keys())
    if 'type' not in g.definitions[key].keys()
}
g.all_types.append(g.complex_types)

dep_list = {
    edge_name: create_dep_list((edge_name, edge_dependencies), g.enum_types)
    for (edge_name, edge_dependencies) in g.definitions.items()
}

for edge_type_name, edge_type_dependencies in sorted(dep_list.items()):
    if not edge_type_dependencies:
        g.graph.add_edge(edge_type_name, '')
        g.no_deps[edge_type_name] = g.definitions[edge_type_name]
    else:
        for vv in sorted(edge_type_dependencies):
            g.graph.add_edge(edge_type_name, vv)

# endregion

# Safeguard checking if there is no types lost in triage
if not check_definitions_number_match(g.definitions, g.all_types):
    print("ERROR: JSON definitions({}) and Types({}) number mismatch".
          format(len(g.definitions), number_of_types(g.all_types)))

# Entrypoint
if __name__ == '__main__':
    generate_spec()

    # number of generated types omitting the Ada special types
    # such as vectors, maps, accesses.
    number_generated_types = len([x
                                  for x in g.generated_types
                                  if not x.count('DAP_')])
    number_requested = len(g.definitions)
    if number_requested != number_generated_types:
        message = ("ERROR: Generated Ada source code and JSON spec do not have"
                   + " the same number of types!")
        eprint(message)
        raise Exception("Execution stopped because: some types where lost " +
                        "during header generation.\n")
    generate_body()
