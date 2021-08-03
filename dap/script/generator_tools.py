import g
import itertools
import networkx as nx

from tools import get_key, remove_prefix


def gprint(*args, **kwargs):
    """
    Prints on the output specified by g.output.
    Behaves like print function.
    """
    print(*args, file=g.output, **kwargs)


def print_copyright():
    h = ("""
------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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
""")
    gprint(h.strip())


# region Print Ada Code
def print_header():
    """
    prints the header of the spec file
    """
    print_copyright()
    gprint("""--  generated do not edit
              with Ada.Containers.Hashed_Maps;
              with Ada.Containers.Vectors;
              with Ada.Finalization;
              with Ada.Streams;
              with Ada.Unchecked_Deallocation;
              with LSP.Types; use LSP.Types;
              with VSS.Strings; use VSS.Strings;

              package DAP.Tools is\n""")


def print_predefined_type(generated_types):
    """
    prints several predefined types
    """
    types = """
package DAP_String_Maps is new Ada.Containers.Hashed_Maps
  (Key_Type        => Virtual_String,
   Element_Type    => Virtual_String,
   Hash            => LSP.Types.Hash,
   Equivalent_Keys => VSS.Strings."=");
type Access_DAP_String_Map is access all DAP_String_Maps.Map;

package DAP_String_Vectors is new Ada.Containers.Vectors
(Positive, Virtual_String, "=");
type DAP_String_Vector is new DAP_String_Vectors.Vector with null record;
type Access_DAP_String_Vector is access all DAP_String_Vector;

package DAP_Integer_Vectors is new Ada.Containers.Vectors (Positive, Integer,
"=");
type DAP_Integer_Vector is new DAP_Integer_Vectors.Vector with null record;
type Access_DAP_Integer_Vector is access all DAP_Integer_Vector;\n\n"""
    generated_types['DAP_String_Map'] = dict()
    generated_types['DAP_String_Vector'] = dict()
    generated_types['DAP_Integer_Vector'] = dict()
    gprint(types)


def print_footer():
    """
    prints the footer of the package
    """
    gprint("""end DAP.Tools;""")


def print_enums(enums, generated_types, enum_types_list, reserved_names):
    """
    Prints all the enumerated types

    :param enums: dict(String: JSONObject)
    :param generated_types: dict(String: dict(String: String))
    {
        "ProtocolMessage": {
            "seq": "LSP_Number",
            "a_type": "Virtual_String"
        },
        "Source" : {
            "adapterData": "LSP_Any",
            "checksums": "DAP_Checksum_Vector",
            "name": "Virtual_String",
            "origin": "Virtual_String",
            "path": "Virtual_String",
            "presentationHint": "Virtual_String",
            "sourceReference": "LSP_Number",
            "sources": "DAP_Source_Vector"
        },
        ...
    }
    :param enum_types_list: set(String)
    {'InvalidatedAreas', 'DataBreakpointAccessType', 'ChecksumAlgorithm', ...}
    :param reserved_names: dict(String: String)
    {
        "all": "a_all",
        "function": "a_function",
        "interface": "a_interface",
        "type": "a_type",
        "__restart": "a_restart",
        "source": "a_source",
        "body": "a_body"
    }
    """

    def list_to_enum(lst):
        """
        return a string representation of a String list into String
        :param lst: [String]
        :return: String
        """
        s = ""
        for i in range(len(lst) - 1):
            s += get_key(lst[i], reserved_names) + ", "
        s += get_key(lst[-1], reserved_names)
        return s

    gprint("package Enums is")
    for enum_name, enum in sorted(enums.items()):
        enum_list = enum['enum'] if 'enum' in enum else enum['_enum']
        gprint("type {enum} is ({enum_list_str});"
               .format(enum=enum_name, enum_list_str=list_to_enum(enum_list)))
        enum_types_list.add(enum_name)
        # prefixing enum types with "Enums." to use a single dictionary and
        # be able to differentiate them from records
        generated_types["Enums." + enum_name] = dict()
    gprint("end Enums;\n")


def print_enums_access():
    """
    Prints the accesses for enumerated types
    """
    for e in sorted(g.enum_types):
        if ("Access_" + e) not in g.generated_access_types:
            gprint("type Access_{e} is access all Enums.{e};\n".format(e=e))
            g.generated_access_types["Access_" + e] = True


def print_sub_simple_record(obj, generated_types, typeTranslationTable,
                            reserved_names):
    """
    Prints a simple record
    :param obj: (String, JSONObject)
    :param generated_types: dict(String: dict(String: String))
    {
        "ProtocolMessage": {
            "seq": "LSP_Number",
            "a_type": "Virtual_String"
        },
        "Source" : {
            "adapterData": "LSP_Any",
            "checksums": "DAP_Checksum_Vector",
            "name": "Virtual_String",
            "origin": "Virtual_String",
            "path": "Virtual_String",
            "presentationHint": "Virtual_String",
            "sourceReference": "LSP_Number",
            "sources": "DAP_Source_Vector"
        },
        ...
    }
    :param typeTranslationTable: dict(Tuple: String | None)
    :param reserved_names: dict(String: String)
    {
        "all": "a_all",
        "function": "a_function",
        "interface": "a_interface",
        "type": "a_type",
        "__restart": "a_restart",
        "source": "a_source",
        "body": "a_body"
    }
    """

    def translate_type(typeScriptType):
        """
        Returns the corresponding Ada type to be used
        :param typeScriptType: String | [String]
        :return: String | None
        """
        if isinstance(typeScriptType, str):
            return typeTranslationTable[tuple([typeScriptType])]
        else:
            return typeTranslationTable[
                tuple(sorted(itertools.chain(*[typeScriptType])))]

    (recordName, record) = obj
    attr_list = dict()
    if 'properties' not in record.keys():  # treat the empty record
        gprint("type {type} is new Ada.Finalization.Controlled with null "
               "record;\n".format(type=recordName))
    else:
        gprint("type {type} is new Ada.Finalization.Controlled with record"
               .format(type=recordName))
        for typeName, typeType in sorted(record['properties'].items()):
            attr_name = get_key(typeName, reserved_names)
            trans_type = translate_type(typeType['type'])
            if trans_type is None and typeType['type'] == 'array':
                trans_type = ("DAP_" + typeType['items']['type'].title()
                              + "_Vector")
            if trans_type is None and typeType['type'] == 'object':
                trans_type = "DAP_String_Maps.Map"
            gprint("   {attr_name} : aliased {trans_type};"
                   .format(attr_name=attr_name, trans_type=trans_type))
            attr_list[attr_name] = trans_type
        gprint("end record;")
    if ("Access_" + recordName) not in g.generated_access_types:
        gprint("type Access_{type} is access all {type};\n"
               .format(type=recordName))
        g.generated_access_types["Access_" + recordName] = True
    else:
        gprint("\n")
    generated_types[recordName] = attr_list


def print_sub_simple_record_with_refs(obj, generated_types,
                                      typeTranslationTable, enumTypesList,
                                      reserved_names):
    """
    Prints a simple record with references to other types
    :param obj: (String, JSONObject)
    :param generated_types: dict(String: dict(String: String))
    {
        "ProtocolMessage": {
            "seq": "LSP_Number",
            "a_type": "Virtual_String"
        },
        "Source" : {
            "adapterData": "LSP_Any",
            "checksums": "DAP_Checksum_Vector",
            "name": "Virtual_String",
            "origin": "Virtual_String",
            "path": "Virtual_String",
            "presentationHint": "Virtual_String",
            "sourceReference": "LSP_Number",
            "sources": "DAP_Source_Vector"
        },
        ...
    }
    :param typeTranslationTable: dict(Tuple: String | None)
    :param enumTypesList: set(String)
    :param reserved_names: dict(String: String)
    {
        "all": "a_all",
        "function": "a_function",
        "interface": "a_interface",
        "type": "a_type",
        "__restart": "a_restart",
        "source": "a_source",
        "body": "a_body"
    }
    """

    def translate_type(typeScriptType):
        """
        Returns the corresponding Ada type to be used
        :param typeScriptType: String | [String]
        :return: String | None
        """
        if isinstance(typeScriptType, str):
            return typeTranslationTable[tuple([typeScriptType])]
        else:
            return typeTranslationTable[
                tuple(sorted(itertools.chain(*[typeScriptType])))]

    def type_or_ref(type_or_ref_obj):
        """
        return the corresponding Ada type
        :param type_or_ref_obj: JSONObject
        :return: String
        """
        if '$ref' in type_or_ref_obj:
            ref_type = type_or_ref_obj['$ref'].split('/')[-1]
            return ('', 'Enums.')[ref_type in enumTypesList] + ref_type
        else:
            return translate_type(type_or_ref_obj['type'])

    (recordName, record) = obj
    attr_list = dict()
    if 'properties' not in record.keys():  # treat the empty record
        gprint("type {type} is new Ada.Finalization.Controlled with null "
               "record;\n".format(type=recordName))
    else:  # treat non empty record
        gprint("type {type} is new Ada.Finalization.Controlled with record"
               .format(type=recordName))
        for typeName, typeType in sorted(record['properties'].items()):
            attr_name = get_key(typeName, reserved_names)
            trans_type = type_or_ref(typeType)
            if trans_type is None and typeType['type'] == 'array':
                if '$ref' in typeType['items']:
                    trans_type = "DAP_" + typeType['items']['$ref'].split('/')[
                        -1] + "_Vector"
                else:
                    trans_type = "DAP_" + typeType['items'][
                        'type'].title() + "_Vector"
            gprint("   {attr_name} : aliased {trans_type};"
                   .format(attr_name=attr_name, trans_type=trans_type))
            attr_list[attr_name] = trans_type
        gprint("end record;")
    if ("Access_" + recordName) not in g.generated_access_types:
        gprint("type Access_{type} is access all {type};"
               .format(type=recordName))
        g.generated_access_types["Access_" + recordName] = True
    gprint("\n")
    generated_types[recordName] = attr_list


def print_sub_complex_records(obj, generated_types, typeTranslationTable,
                              enumTypesList, reserved_names):
    """
    Prints a complex record
    :param obj: (String, JSONObject)
    :param generated_types: dict(String: dict(String: String))
    {
        "ProtocolMessage": {
            "seq": "LSP_Number",
            "a_type": "Virtual_String"
        },
        "Source" : {
            "adapterData": "LSP_Any",
            "checksums": "DAP_Checksum_Vector",
            "name": "Virtual_String",
            "origin": "Virtual_String",
            "path": "Virtual_String",
            "presentationHint": "Virtual_String",
            "sourceReference": "LSP_Number",
            "sources": "DAP_Source_Vector"
        },
        ...
    }
    :param typeTranslationTable: dict(Tuple: String | None)
    :param enumTypesList: set(String)
    :param reserved_names: dict(String: String)
    {
        "all": "a_all",
        "function": "a_function",
        "interface": "a_interface",
        "type": "a_type",
        "__restart": "a_restart",
        "source": "a_source",
        "body": "a_body"
    }
    """

    def translate_type(typeScriptType):
        """
        Returns the corresponding Ada type to be used
        :param typeScriptType: String | [String]
        :return: String | None
        """
        if isinstance(typeScriptType, str):
            return typeTranslationTable[tuple([typeScriptType])]
        else:
            return typeTranslationTable[
                tuple(sorted(itertools.chain(*[typeScriptType])))]

    def type_or_ref(type_or_ref_obj):
        """
        return the corresponding Ada type
        :param type_or_ref_obj: JSONObject
        :return: String
        """
        if '$ref' in type_or_ref_obj:
            ref_type = type_or_ref_obj['$ref'].split('/')[-1]
            return ('', 'Enums.')[ref_type in enumTypesList] + ref_type
        else:
            return translate_type(type_or_ref_obj['type'])

    (recordName, record) = obj
    attr_list = dict()
    heritage_name = record['allOf'][0]['$ref'].split('/')[-1]

    if 'properties' not in record['allOf'][1]:
        gprint("type {type} is new Ada.Finalization.Controlled with record"
               .format(type=recordName))
        for attr_name, type_type in sorted(
                generated_types[heritage_name].items()):
            gprint("{attr_name} : aliased {type_type};"
                   .format(attr_name=attr_name, type_type=type_type))
            attr_list[attr_name] = type_type
        gprint("end record;")
    else:
        new_properties = record['allOf'][1]['properties']
        gprint("type {type} is new Ada.Finalization.Controlled with record"
               .format(type=recordName))
        for attr_name, type_type in sorted(new_properties.items()):
            if attr_name == 'body' or attr_name == 'env':
                continue
            attr_name = get_key(attr_name, reserved_names)
            trans_type = type_or_ref(type_type)
            if trans_type is None and type_type['type'] == 'array':
                trans_type = ("DAP_" + type_type['items']['type']
                              + "_Vector")
            gprint("{attr_name} : aliased {trans_type};"
                   .format(attr_name=attr_name, trans_type=trans_type))
            attr_list[attr_name] = trans_type
        for attr_name, type_type in sorted(
                generated_types[heritage_name].items()):
            if attr_name not in attr_list:
                gprint("{attr_name} : aliased {type_type};"
                       .format(attr_name=attr_name, type_type=type_type))
        if ('body' in new_properties and 'properties' in new_properties[
            'body'] or 'env' in new_properties and 'properties' in
                new_properties['env']):
            props = new_properties['body']['properties']
            for atr_body_name, atr_body_type in sorted(props.items()):
                atr_body_name = ("body_" if 'body' in new_properties else
                                 'env') + atr_body_name
                trans_type = type_or_ref(atr_body_type)
                if trans_type is None and atr_body_type['type'] == 'array':
                    if '$ref' in atr_body_type['items']:
                        trans_type = ("DAP_"
                                      + atr_body_type['items']['$ref']
                                      .split('/')[-1] + "_Vector")
                    else:
                        trans_type = ("DAP_" +
                                      atr_body_type['items']['type'].title()
                                      + "_Vector")
                gprint("{attr_name} : aliased {trans_type};"
                       .format(attr_name=atr_body_name, trans_type=trans_type))
                attr_list[atr_body_name] = trans_type
        if len(new_properties) == 0:
            gprint("null;")
        gprint("end record;")
    if ("Access_" + recordName) not in g.generated_access_types:
        gprint("type Access_{type} is access all {type};"
               .format(type=recordName))
        g.generated_access_types["Access_" + recordName] = True
    gprint("\n")
    attr_list.update(
        dict(list(generated_types[heritage_name].items()) +
             list(attr_list.items())))
    generated_types[recordName] = attr_list


def print_finalize(generated_types: dict):
    have_vect_types = [k
                       for k, v in sorted(generated_types.items())
                       if str(v).count("Vector")]
    for typeName, attribs in sorted(generated_types.items()):
        if typeName in have_vect_types:
            gprint("overriding procedure Finalize (Self: in out {type_name});\n"
                   .format(type_name=typeName))


def print_free(generated_types: dict):
    for type_name, _ in sorted(generated_types.items()):
        free_type = None
        if type_name.startswith("DAP_") and type_name.endswith("_Vector"):
            # type_name starting with DAP_ are always like DAP_TYPE_Vector
            free_type = type_name.split("_")[1]
        if free_type in generated_types:
            gprint("procedure Free is new Ada.Unchecked_Deallocation" +
                   "(Object => {type}, Name => Access_{type});\n"
                   .format(type=free_type))
            continue
        if free_type and ("Enums." + free_type) in generated_types:
            gprint("procedure Free is new Ada.Unchecked_Deallocation" +
                   "(Object => Enums.{type}, Name => Access_{type});\n"
                   .format(type=free_type))
            continue


def print_object(definitions, simpleRecordTypes, simpleRecordWithRefTypes,
                 complexTypes, generated_types, graph,
                 typeTranslationTable, enum_types_list, reserved_names, obj):
    """
    This will print the corresponding Ada code for requested `obj`.
    Note that this will recursively print all needed types before the type
    you actually want to print (respects the type dependency).

    :param definitions: dict(String: JSONObject)
    :param simpleRecordTypes: dict(String: JSONObject)
    :param simpleRecordWithRefTypes: dict(String: JSONObject)
    :param complexTypes: dict(String: JSONObject)
    :param generated_types: dict(String: dict(String: String))
    {
        "ProtocolMessage": {
            "seq": "LSP_Number",
            "a_type": "Virtual_String"
        },
        "Source" : {
            "adapterData": "LSP_Any",
            "checksums": "DAP_Checksum_Vector",
            "name": "Virtual_String",
            "origin": "Virtual_String",
            "path": "Virtual_String",
            "presentationHint": "Virtual_String",
            "sourceReference": "LSP_Number",
            "sources": "DAP_Source_Vector"
        },
        ...
    }
    :param graph:
    :param typeTranslationTable: dict(Tuple: String | None)
    :param enum_types_list: set(String)
    :param reserved_names: dict(String: String)
    {
        "all": "a_all",
        "function": "a_function",
        "interface": "a_interface",
        "type": "a_type",
        "__restart": "a_restart",
        "source": "a_source",
        "body": "a_body"
    }
    :param obj: (String, JSONObject)
    """
    (recordName, record) = obj

    # early return in case the type is already generated
    if recordName in generated_types:
        return

    # recursive call to create type dependencies
    for k, v in sorted(list(nx.dfs_edges(graph, recordName))):
        if v == '' or v in generated_types:
            continue
        print_object(definitions, simpleRecordTypes, simpleRecordWithRefTypes,
                     complexTypes, generated_types, graph,
                     typeTranslationTable, enum_types_list, reserved_names,
                     (v, definitions[v]))

    obj = (recordName, definitions[recordName])
    if recordName in simpleRecordTypes:
        print_sub_simple_record(obj, generated_types, typeTranslationTable,
                                reserved_names)
    elif recordName in simpleRecordWithRefTypes:
        print_sub_simple_record_with_refs(obj, generated_types,
                                          typeTranslationTable, enum_types_list,
                                          reserved_names)
    elif recordName in complexTypes:
        print_sub_complex_records(obj, generated_types, typeTranslationTable,
                                  enum_types_list, reserved_names)


def print_vector_types(definitions, graph, generated_types, enumTypes):
    """
    this takes all Ada array (vector) types from the graph and prints them

    :param definitions: dict of (Name, JSONSpec)
    :param graph: digraph to take count of dependency between types
    :param generated_types: dict to keep track of generated types
    :param enumTypes:
    """
    for k in sorted(graph):
        if k.count("DAP_"):
            generated_types[k] = dict()
            # type_name starting with DAP_ are always of type DAP_TYPE_Vector
            t = k.split("_")[1]
            if t in definitions:
                if t in enumTypes:
                    gprint("type Access_{type} is access all Enums.{type};"
                           .format(type=t))
                else:
                    gprint("type {type};".format(type=t))
                    gprint("type Access_{type} is access all {type};"
                           .format(type=t))
                g.generated_access_types["Access_" + t] = True
                gprint("""package {k}s is new
                          Ada.Containers.Vectors(Positive, Access_{type});"""
                       .format(k=k, type=t))
                gprint("type {type} is new {type}s.Vector with null record;"
                       .format(type=k))
                gprint("type Access_{t} is access all {t};\n".format(t=k))
                g.generated_access_types["Access_" + k] = True


def print_write_procedures_spec(generated_types: dict):
    for k, v in sorted(generated_types.items()):
        atype = k
        truetype = k
        if k.endswith("_Maps") or k.endswith("_Vectors"):
            continue
        if k.count('.') and k.split('.')[-1] in g.enum_types:
            atype = k.split('.')[-1]
            truetype = atype
        gprint("""procedure Write_{type}
                   (S: access Ada.Streams.Root_Stream_Type'Class;
                    V: Access_{trueType});\n"""
               .format(type=atype, trueType=truetype))


def print_read_procedures_spec(generated_types: dict):
    for k, v in sorted(generated_types.items()):
        atype = k
        truetype = k
        if k.endswith("_Maps") or k.endswith("_Vectors"):
            continue
        if k.count('.') and k.split('.')[-1] in g.enum_types:
            atype = k.split('.')[-1]
            truetype = atype
        gprint("""procedure Read_{type}
                   (S: access Ada.Streams.Root_Stream_Type'Class;
                    V: Access_{trueType});\n"""
               .format(type=atype, trueType=truetype))


def print_write_enum(recordName, recordDef):
    gprint("""
procedure Write_{type}
  (S: access Ada.Streams.Root_Stream_Type'Class;
   V: Access_{type})
is
   JS : LSP.JSON_Streams.JSON_Stream'Class renames
   LSP.JSON_Streams.JSON_Stream'Class (S.all);

   function To_String (Value : Enums.{type}) return
   Virtual_String;

   function To_String (Value : Enums.{type}) return
   Virtual_String is
   begin
      case Value is""".format(type=recordName))
    r = "enum" if "enum" in recordDef else "_enum"
    for enum in sorted(recordDef[r]):
        gprint('when Enums.{t} => return "{truetype}";'
               .format(t=get_key(enum, g.reserved_names), truetype=enum))
    gprint("""      end case;
                end To_String;
            begin
               if V /= null then
                  JS.Write_String (To_String (V.all));
               end if;
            end Write_{type};\n"""
           .format(type=recordName))


def print_write_procedures_body(generated_types: dict):
    for (recordName, recordDef) in generated_types.items():
        # if recordName in g.enumTypes:
        if recordName.endswith("_Maps") or recordName.endswith("_Vectors"):
            continue
        if recordName.count('.') and recordName.split('.')[-1] in g.enum_types:
            print_write_enum(recordName.split('.')[-1],
                             g.definitions[recordName.split('.')[1]])
            continue
        if recordName.count("DAP"):
            if recordName.endswith("_Map"):
                # type_name starting with DAP_ are always like DAP_TYPE_Vector
                true_type = recordName.split('_')[1]
                if true_type == 'String':
                    gprint("""procedure Write_DAP_String_Map
                    (S : access Ada.Streams.Root_Stream_Type'Class;
                    V : Access_DAP_String_Map)
                    is
                        JS : LSP.JSON_Streams.JSON_Stream'Class renames
                        LSP.JSON_Streams.JSON_Stream'Class (S.all);
                    begin
                        JS.Start_Object;
                        declare
                            procedure WMap_Item (C : DAP_String_Maps.Cursor);
                            procedure WMap_Item (C : DAP_String_Maps.Cursor) is
                            begin
                                LSP.JSON_Streams.Key (JS'Access,
                                (DAP_String_Maps.Key (C)));
                                JS.Write_String (Virtual_String'(
                                DAP_String_Maps.Element (C)));
                            end WMap_Item;
                        begin
                            V.Iterate (WMap_Item'Access);
                        end;
                        JS.End_Object;
                    end Write_DAP_String_Map;\n""")
            elif recordName.endswith("_Vector"):
                atype = recordName
                # type_name starting with DAP_ are always
                # of type DAP_TYPE_Vector
                true_type = recordName.split('_')[1]
                if true_type == 'String':
                    gprint("""procedure Write_{type}
                           (S: access Ada.Streams.Root_Stream_Type'Class;
                            V: Access_{type})
                         is
                               JS : LSP.JSON_Streams.JSON_Stream'Class renames
                               LSP.JSON_Streams.JSON_Stream'Class (S.all);
                         begin
                               JS.Start_Array;
                               for J in V.First_Index .. V.Last_Index loop
                                  JS.Write_String (V.Element (J));
                               end loop;
                               JS.End_Array;
                         end Write_{type};\n""".format(type=atype))
                elif true_type == 'Integer':
                    gprint("""procedure Write_{type}
                               (S: access Ada.Streams.Root_Stream_Type'Class;
                                V: Access_{type})
                             is
                                   JS : LSP.JSON_Streams.JSON_Stream'Class
                                   renames
                                   LSP.JSON_Streams.JSON_Stream'Class (S.all);
                             begin
                                   JS.Start_Array;
                                   for J in V.First_Index .. V.Last_Index loop
                                      JS.Write_Integer
                                      (Interfaces.Integer_64 (V.Element (J)));
                                   end loop;
                                   JS.End_Array;
                             end Write_{type};\n""".format(type=atype))
                else:
                    gprint("""procedure Write_{type}
                           (S: access Ada.Streams.Root_Stream_Type'Class;
                            V: Access_{type})
                         is
                            JS : LSP.JSON_Streams.JSON_Stream'Class renames
                            LSP.JSON_Streams.JSON_Stream'Class (S.all);
                         begin
                            JS.Start_Array;
                            for J in V.First_Index .. V.Last_Index loop
                                Write_{truetype} (S, V.Element (J));
                            end loop;
                            JS.End_Array;
                         end Write_{type};\n"""
                           .format(type=atype, truetype=true_type))
            continue
        else:
            if len(g.generated_types[recordName]) == 0:
                gprint("""procedure Write_{type}
                (S: access Ada.Streams.Root_Stream_Type'Class;
                 V: Access_{type})
                 is
                    JS : LSP.JSON_Streams.JSON_Stream'Class renames
                    LSP.JSON_Streams.JSON_Stream'Class (S.all);
                 begin
                    --  this record is empty
                    pragma Unreferenced(V);
                    JS.Start_Object;
                    JS.End_Object;
                end Write_{type};\n""".format(type=recordName))
                continue
            gprint("""procedure Write_{type}
               (S: access Ada.Streams.Root_Stream_Type'Class;
                V: Access_{type})
             is
                JS : LSP.JSON_Streams.JSON_Stream'Class renames
                LSP.JSON_Streams.JSON_Stream'Class (S.all);
             begin
                JS.Start_Object;\n""".format(type=recordName))

            inv_reserved_words = {v: k for k, v in g.reserved_names.items()}
            for index_attrib in range(len(g.generated_types[recordName])):
                format_string = None
                attr_name, attr_type = (
                    list(g.generated_types[recordName].items())[
                        index_attrib])
                true_name = (inv_reserved_words[attr_name]
                             if attr_name in inv_reserved_words else
                             attr_name)
                if attr_type in g.generated_types:
                    if attr_type.count("Enums.") != 0:
                        # type_name starting with DAP_ are always
                        # of type DAP_TYPE_Vector
                        attr_type = attr_type.split('.')[1]
                    format_string = """JS.Key ("{true_name}");
                    Write_{attr_type} (S, V.{attr_name}'Access);\n"""
                elif attr_type == 'Boolean':
                    format_string = """JS.Key ("{true_name}");
                    JS.Write_Boolean (V.{attr_name});\n"""
                elif attr_type == 'LSP_Number':
                    format_string = """JS.Key ("{true_name}");
                    JS.Write_Integer (Interfaces.Integer_64
                           (V.{attr_name}));\n"""
                elif attr_type == 'Float':
                    format_string = """JS.Key ("{true_name}");
                    JS.Write_Integer (Interfaces.Integer_64
                           ( Float'(V.{attr_name})));\n"""
                elif attr_type == 'Virtual_String':
                    format_string = """JS.Key ("{true_name}");
                    JS.Write_String (V.{attr_name});\n"""
                elif attr_type == 'LSP_Any':
                    format_string = """JS.Key ("{true_name}");
                    Write_Any (S, V.{attr_name});\n"""
                elif attr_type == 'LSP_Number_Or_String':
                    format_string = """JS.Key ("{true_name}");
                    if V.{attr_name}.Is_Number then
                       JS.Write_Integer (Interfaces.Integer_64
                          (V.{attr_name}.Number));
                    elsif not Is_Empty (V.{attr_name}.String) then
                       JS.Write_String (V.{attr_name}.String);
                    end if;\n"""
                elif attr_type == 'DAP_String_Maps.Map':
                    format_string = """JS.Key ("{true_name}");
                    Write_DAP_String_Map (S, V.{attr_name}'Access);\n"""
                else:
                    raise Exception("Execution stopped because:\n" +
                                    attr_type + " of " + recordName +
                                    "is not handled correctly.")
                if format_string:
                    gprint(format_string.format(
                        attr_name=attr_name,
                        attr_type=attr_type,
                        true_name=true_name))
            gprint("""
                JS.End_Object;
             end Write_{type};\n""".format(type=recordName))


def print_read_enum(recordName, recordDef):
    gprint("""   procedure Read_{t}
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Access_{t})
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if V /= null then""".format(t=recordName))
    r = "enum" if "enum" in recordDef else "_enum"
    for i in range(len(recordDef[r])):
        if i == 0:
            gprint("""if Text = "{truetype}" then
                         V.all := Enums.{t};"""
                   .format(t=get_key(recordDef[r][i], g.reserved_names),
                           truetype=recordDef[r][i]))
        elif i == (len(recordDef[r]) - 1):
            gprint("""   else
                            V.all := Enums.{t}'First;
                         end if;
                      end if;
                   end Read_{t};\n""".format(t=recordName))
        else:
            gprint("""elsif Text = "{truetype}" then
                     V.all := Enums.{t};"""
                   .format(t=get_key(recordDef[r][i], g.reserved_names),
                           truetype=recordDef[r][i]))


def print_read_procedures_body():
    def print_body(obj):
        """
        Prints the needed function to read anonymous body

        :param obj: JSONObject {String: JSONObject}
        """
        gprint("""
        procedure Read_Body;

        procedure Read_Body is
        begin
        pragma Assert(JS.R.Is_Start_Object);
        JS.R.Read_Next;

        while not JS.R.Is_End_Object loop
        pragma Assert (JS.R.Is_Key_Name);
        declare
        Key : constant String := VSS.Strings.Conversions.To_UTF_8_String
        (JS.R.Key_Name);
        begin
        JS.R.Read_Next;\n""")
        bdy_lst = sorted([(x, y)
                          for x, y in obj.items()
                          if x.startswith("body_")])
        for index_attrib in range(len(bdy_lst)):
            attr_name, attr_type = bdy_lst[index_attrib]
            selector_name = attr_name
            attr_name = remove_prefix(attr_name, "body_")
            read_type = ""
            condition = ' Key = "{attr_name}" '.format(
                attr_name=(inv_reserved_words[attr_name]
                           if attr_name in inv_reserved_words else
                           attr_name))
            # the format of the condition string needs to happen now because
            # later it will be concatenated with the rest of the Ada code
            # and `attr_name` later refers to a record selector and
            # *must* not be a reserved Ada keyword
            expression = ""
            if attr_type == 'DAP_String_Maps.Map':
                expression = """Read_DAP_String_Map
                                (S, V.all.{selector_name}'Access);"""
            elif attr_type.count(".") != 0:
                read_type = attr_type.split('.')[1]
                expression = """Read_{read_type}
                                (S, V.all.{selector_name}'Access);"""
            elif attr_type == "LSP_Any":
                expression = "Read_Any (S, V.all.{selector_name});"
            elif attr_type == "LSP_Number":
                expression = "Read (S, V.all.{selector_name});"
            elif attr_type == "LSP_Number_Or_String":
                expression = """Read_LSP_Number_Or_String
                                (S, V.all.{selector_name});"""
            elif attr_type == "Virtual_String":
                expression = "Read_String (S, V.all.{selector_name});"
            elif attr_type == "Boolean":
                expression = "Read_Boolean (JS, V.all.{selector_name});"
            else:
                expression = """Read_{attr_type}
                                (S, V.all.{selector_name}'Access);"""

            format_string = " elsif " if index_attrib else " if "
            format_string += condition + " then " + expression + "\n"
            gprint(format_string.format(selector_name=selector_name,
                                        attr_type=attr_type,
                                        read_type=read_type))
        gprint("""else
                         JS.Skip_Value;
                      end if;
                    end;
                end loop;
                JS.R.Read_Next;
            end Read_Body;\n""")

    inv_reserved_words = {v: k for k, v in g.reserved_names.items()}
    for (recordName, recordDef) in g.generated_types.items():
        # if recordName in g.enumTypes:
        if recordName.endswith("_Maps") or recordName.endswith("_Vectors"):
            continue
            pass
        if recordName.count('.') and recordName.split('.')[-1] in g.enum_types:
            print_read_enum(recordName.split('.')[-1],
                            g.definitions[recordName.split('.')[1]])
            continue
        if recordName.count("DAP"):
            if recordName.endswith("_Map"):
                atype = recordName
                gprint("""procedure Read_{type}
                           (S: access Ada.Streams.Root_Stream_Type'Class;
                            V: Access_{truetype})
                         is
                               JS : LSP.JSON_Streams.JSON_Stream'Class renames
                               LSP.JSON_Streams.JSON_Stream'Class (S.all);
                         begin
                            pragma Assert (JS.R.Is_Start_Object);
                            JS.R.Read_Next;

                            while not JS.R.Is_End_Object loop
                               pragma Assert (JS.R.Is_Key_Name);
                               declare
                                  Key : Virtual_String;
                                  Value : Virtual_String;
                               begin
                                  LSP.Types.Read_String (S, Key);
                                  JS.R.Read_Next;
                                  LSP.Types.Read_String (S, Value);
                                  if V /= null then
                                    V.all.Insert (Key, Value);
                                  end if;
                               end;
                            end loop;

                            JS.R.Read_Next;
                         end Read_{type};\n"""
                       .format(type=atype, truetype=atype))
            elif recordName.endswith("_Vector"):
                atype = recordName
                truetype = recordName.split('_')[1]
                if truetype == 'String':
                    gprint("""procedure Read_{type}
                           (S: access Ada.Streams.Root_Stream_Type'Class;
                            V: Access_{type})
                         is
                               JS : LSP.JSON_Streams.JSON_Stream'Class renames
                               LSP.JSON_Streams.JSON_Stream'Class (S.all);
                               T  : Virtual_String;
                         begin
                            pragma Assert (JS.R.Is_Start_Array);
                            JS.R.Read_Next;

                            while not JS.R.Is_End_Array loop
                                LSP.Types.Read_String (S, T);
                                V.Append (T);
                            end loop;
                            JS.R.Read_Next;

                         end Read_{type};\n""".format(type=atype))
                elif truetype == 'Integer':
                    gprint("""procedure Read_{type}
                           (S: access Ada.Streams.Root_Stream_Type'Class;
                            V: Access_{type})
                         is
                               JS : LSP.JSON_Streams.JSON_Stream'Class renames
                               LSP.JSON_Streams.JSON_Stream'Class (S.all);
                               T  : LSP.Types.LSP_Number;
                         begin
                            pragma Assert (JS.R.Is_Start_Array);
                            JS.R.Read_Next;
                            while not JS.R.Is_End_Array loop
                                LSP.Types.Read (S, T);
                                V.Append (Integer (T));
                            end loop;
                            JS.R.Read_Next;

                         end Read_{type};\n""".format(type=atype))
                else:
                    t_truetype = (truetype
                                  if truetype not in g.enum_types
                                  else "Enums." + truetype)
                    gprint("""procedure Read_{type}
                           (S: access Ada.Streams.Root_Stream_Type'Class;
                            V: Access_{type})
                         is
                               JS : LSP.JSON_Streams.JSON_Stream'Class renames
                               LSP.JSON_Streams.JSON_Stream'Class (S.all);
                               T  : Access_{truetype};
                         begin
                            pragma Assert (JS.R.Is_Start_Array);
                            JS.R.Read_Next;

                            while not JS.R.Is_End_Array loop
                                T := new {t_truetype};
                                Read_{truetype} (S, T);
                                V.Append (T);
                            end loop;
                            JS.R.Read_Next;

                         end Read_{type};\n"""
                           .format(type=atype,
                                   truetype=truetype,
                                   t_truetype=t_truetype))
            continue
        else:
            if len(g.generated_types[recordName]) == 0:
                gprint("""procedure Read_{type}
                (S: access Ada.Streams.Root_Stream_Type'Class;
                 V: Access_{type})
                 is
                 begin
                    --  this record is empty
                    pragma Unreferenced(S);
                    pragma Unreferenced(V);
                end Read_{type};""".format(type=recordName))
                continue
            contains_body = str(recordDef).count("body") != 0
            gprint("""procedure Read_{type}
               (S: access Ada.Streams.Root_Stream_Type'Class;
                V: Access_{type})
             is
                   JS : LSP.JSON_Streams.JSON_Stream'Class renames
                   LSP.JSON_Streams.JSON_Stream'Class (S.all);
                   """.format(type=recordName))
            if contains_body:
                print_body(recordDef)
            gprint("""begin
                pragma Assert (JS.R.Is_Start_Object);
                JS.R.Read_Next;

                while not JS.R.Is_End_Object loop
                   pragma Assert (JS.R.Is_Key_Name);
                   declare
                      Key : constant String :=
                         VSS.Strings.Conversions.To_UTF_8_String
                         (JS.R.Key_Name);
                   begin
                      JS.R.Read_Next;""")
            for index_attrib in range(len(g.generated_types[recordName])):
                attr_name, attr_type = (sorted(
                    list(g.generated_types[recordName].items()))[index_attrib])
                if attr_name.startswith("body_"):  # discard body
                    continue
                read_type = ""
                condition = ' Key = "{attr_name}" '.format(
                    attr_name=(inv_reserved_words[attr_name]
                               if attr_name in inv_reserved_words else
                               attr_name))
                # the format of the condition string needs to happen now because
                # later it will be concatenated with the rest of the Ada code
                # and `attr_name` later refers to a record selector and
                # *must* not be a reserved Ada keyword
                expression = ""
                if attr_type == 'DAP_String_Maps.Map':
                    expression = """Read_DAP_String_Map
                                    (S, V.all.{attr_name}'Access);"""
                elif attr_type.count(".") != 0:
                    read_type = attr_type.split('.')[1]
                    expression = """Read_{read_type}
                                    (S, V.all.{attr_name}'Access);"""
                elif attr_type == "LSP_Any":
                    expression = "Read_Any (S, V.all.{attr_name});"
                elif attr_type == "LSP_Number":
                    expression = "Read (S, V.all.{attr_name});"
                elif attr_type == "LSP_Number_Or_String":
                    expression = """Read_LSP_Number_Or_String
                                    (S, V.all.{attr_name});"""
                elif attr_type == "Virtual_String":
                    expression = "Read_String (S, V.all.{attr_name});"
                elif attr_type == "Boolean":
                    expression = "Read_Boolean (JS, V.all.{attr_name});"
                else:
                    expression = """Read_{attr_type}
                                    (S, V.all.{attr_name}'Access);"""

                format_string = " elsif " if index_attrib else " if "
                format_string += condition + " then " + expression + "\n"
                gprint(format_string.format(attr_name=attr_name,
                                            attr_type=attr_type,
                                            read_type=read_type))
            if contains_body:
                gprint("""elsif Key = "body" then
                        Read_Body;\n""")
            gprint("""else
                         JS.Skip_Value;
                      end if;
                    end;
                end loop;
                JS.R.Read_Next;""")
            gprint("""end Read_{type};\n""".format(type=recordName))


def generate_spec():
    with open('../generated/dap-tools.ads', 'w') as g.output:
        print_header()
        print_enums(enums=g.enum_types,
                    generated_types=g.generated_types,
                    enum_types_list=g.enum_types_list,
                    reserved_names=g.reserved_names)
        print_predefined_type(generated_types=g.generated_types)
        print_vector_types(definitions=g.definitions,
                           graph=g.graph,
                           generated_types=g.generated_types,
                           enumTypes=g.enum_types)
        print_enums_access()

        for x in sorted(g.no_deps.items()):
            print_object(definitions=g.definitions,
                         simpleRecordTypes=g.record_types,
                         simpleRecordWithRefTypes=g.record_with_ref_types,
                         complexTypes=g.complex_types,
                         generated_types=g.generated_types,
                         graph=g.graph,
                         typeTranslationTable=g.type_translation_table,
                         enum_types_list=g.enum_types_list,
                         reserved_names=g.reserved_names,
                         obj=x)
        for x in sorted(g.definitions.items()):
            type_name = x[0]
            if type_name not in g.enum_types:
                print_object(definitions=g.definitions,
                             simpleRecordTypes=g.record_types,
                             simpleRecordWithRefTypes=g.record_with_ref_types,
                             complexTypes=g.complex_types,
                             generated_types=g.generated_types,
                             graph=g.graph,
                             typeTranslationTable=g.type_translation_table,
                             enum_types_list=g.enum_types_list,
                             reserved_names=g.reserved_names,
                             obj=x)
        print_finalize(g.generated_types)
        print_free(g.generated_types)
        print_read_procedures_spec(g.generated_types)
        print_write_procedures_spec(g.generated_types)
        print_footer()


def print_body_header():
    """
    prints the header of the spec file
    """
    h = """--  generated do not edit
    with Interfaces;
    with LSP.JSON_Streams;
    with VSS.Strings.Conversions;

    package body DAP.Tools is

    """
    print_copyright()
    gprint(h)


def print_body_footer():
    """
    prints the footer of the package
    """
    f = """end DAP.Tools;"""
    gprint(f)


def print_finalize_body(generated_types: dict):
    have_vect_types = [k
                       for k, v in sorted(generated_types.items())
                       if str(v).count("Vector")]
    for typeName, attribs in sorted(generated_types.items()):
        is_null = True
        if typeName in have_vect_types:
            gprint("""overriding procedure Finalize(Self: in out {type})
                      is begin""".format(type=typeName))
            for attrName, attrType in sorted(attribs.items()):
                if (attrType.count("DAP") and attrType.count("Vector") and
                        attrType.count("String") == 0 and
                        attrType.count("Integer") == 0):
                    is_null = False
                    gprint("""for E of Self.{attr_name} loop
                                 Free (E);
                              end loop;""".format(attr_name=attrName))
            if is_null:
                gprint("null;")

            gprint("end Finalize;\n")


def generate_body():
    with open('../generated/dap-tools.adb', 'w') as g.output:
        print_body_header()
        print_finalize_body(g.generated_types)
        print_write_procedures_body(g.generated_types)
        print_read_procedures_body()
        print_body_footer()
# endregion
