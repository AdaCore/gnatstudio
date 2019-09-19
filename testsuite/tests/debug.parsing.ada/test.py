import platform
import GPS
from gps_utils.internal.utils import *
import re

mode = "Mode:" + GPS.Preference("GPS6-Debugger-Debugger-Kind").get()


def check_simple(debug, name, type, value, description,
                 pattern=False, var=None, type_pattern=False,
                 descr_pattern=False):
    if (var is None):
        var = debug.get_variable_by_name(name)

    if (type_pattern):
        t = var.type_name
        gps_assert(re.search(type, t) is None, False,
                   mode + " Invalid type of " + name +
                   " is:" + t + " expected:" + type)
    else:
        gps_assert(var.type_name, type,
                   mode + " Invalid type of " + name)

    if (pattern):
        val = var.simple_value
        gps_assert(re.search(value, val) is None, False,
                   mode + " Invalid value of " + name + " " + val)
    else:
        gps_assert(var.simple_value, value,
                   mode + " Invalid value of " + name)

    if (descr_pattern):
        d = var.type_description
        gps_assert(re.search(description, d) is None, False,
                   mode + " Invalid type description of " + name +
                   " is:" + d + " expected:" + description)
    else:
        gps_assert(var.type_description, description,
                   mode + " Invalid type description of " + name)
    return var


@run_test_driver
def test_driver():
    debug = GPS.Debugger.get()
    yield wait_tasks(other_than=known_tasks)

    debug.send("begin")
    debug.break_at_exception(False)
    debug.send("run")
    yield wait_until_not_busy(debug)

    debug.frame_up()

    gps_assert(debug.get_variable_by_name("Non_Existant_Variable")
               is None, True,
               "Non_Existant_Variable exists")

    check_simple(debug, "A", "<4-byte integer>", "1", "Simple")
    check_simple(debug, "B", "<4-byte float>", "2.0", "Simple")
    check_simple(debug, "C", "<const character>", "65 'A'", "Simple")
    check_simple(debug, "Sh", "<2-byte integer>", "65", "Simple")
    check_simple(debug, "Ssh", "<1-byte integer>", "65", "Simple")
    check_simple(debug, "S", "string (1 .. 4)", '"abcd"', "String")
    check_simple(debug, "S2", "string (1 .. 4)", r'"ab["0a"]c"', "String")
    check_simple(debug, "S3", "string (1 .. 5)", r'"ab[""c"', "String")
    check_simple(debug, "S4", "string (1 .. 7)", r'"ab[""c""]"', "String")
    check_simple(debug, "S4", "string (1 .. 7)", r'"ab[""c""]"', "String")
    check_simple(debug, "Dur", "delta 1e-09", "0.5", "Simple")
    check_simple(debug, "R", "range 3 .. 6", "5", "Range  3 .. 6")
    check_simple(debug, "M", "mod 10", "8", "Modulo  10")
    check_simple(debug, "Act", "parse.access_type",
                 r"0x[0-9a-f]+", "Access", True)
    check_simple(debug, "My_Enum_Variable",
                 "parse.my_enum", "blue", "Enumeration")

    var = check_simple(debug, "T", "parse.integer_array", "",
                       "Array ( 1 ..  4)")
    children_list = var.children()
    gps_assert(len(children_list), 4, mode + " Invalid count of T children")
    check_simple(debug, "(1)", "integer", "2", "Simple",
                 False, children_list[0])
    check_simple(debug, "(2)", "integer", "3", "Simple",
                 False, children_list[1])
    check_simple(debug, "(3)", "integer", "4", "Simple",
                 False, children_list[2])
    check_simple(debug, "(4)", "integer", "5", "Simple",
                 False, children_list[3])

    var = check_simple(debug, "Ea", "parse.empty_array", "",
                       "Array ( 1 ..  0)")
    gps_assert(len(var.children()), 0, mode + " Invalid count of Ea children")

    var = check_simple(debug, "Ea2", "array (0 .. -1) of integer", "",
                       "Array ( 0 .. -1)")
    gps_assert(len(var.children()), 0, mode + " Invalid count of Ea2 children")

    var = check_simple(debug, "Aoa", "parse.array_of_access", "",
                       "Array ( 4 ..  6)")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode + " Invalid count of Aoa children")
    check_simple(debug, "(4)", "parse.access_type|access integer",
                 r"0x[0-9a-f]+", "Access", True, children_list[0], True)
    check_simple(debug, "(5)", "parse.access_type|access integer",
                 r"0x[0-9a-f]+", "Access", True, children_list[1], True)
    check_simple(debug, "(6)", "parse.access_type|access integer",
                 r"0x[0-9a-f]+", "Access", True, children_list[2], True)

    var = check_simple(debug, "Fiia", "parse.first_index_integer_array", "",
                       "Array ( 24 ..  26)")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode + " Invalid count of Fiia children")
    check_simple(debug, "(24)", "integer",
                 "3", "Simple", False, children_list[0])
    check_simple(debug, "(25)", "integer",
                 "4", "Simple", False, children_list[1])
    check_simple(debug, "(26)", "integer",
                 "5", "Simple", False, children_list[2])

    check_simple(debug, "Iaa", "parse.integer_array_access",
                 r"0x[0-9a-f]+", "Access", True)

    var = check_simple(debug, "U", "parse.integer_array2", "",
                       "Array ( 1 ..  2,  1 ..  3)")
    children_list = var.children()
    gps_assert(len(children_list), 6, mode +
               " Invalid count of U children")
    check_simple(debug, "(1,1)", "integer", "2", "Simple",
                 False, children_list[0])
    check_simple(debug, "(1,2)", "integer", "3", "Simple",
                 False, children_list[1])
    check_simple(debug, "(1,3)", "integer", "4", "Simple",
                 False, children_list[2])
    check_simple(debug, "(2,1)", "integer", "5", "Simple",
                 False, children_list[3])
    check_simple(debug, "(2,2)", "integer", "6", "Simple",
                 False, children_list[4])
    check_simple(debug, "(2,3)", "integer", "7", "Simple",
                 False, children_list[5])

    var = check_simple(debug, "Enum_Array_Variable", "parse.enum_array", "",
                       "Array ( 0 ..  2)")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode +
               " Invalid count of Enum_Array_Variable children")
    check_simple(debug, "(0)", "parse.my_enum", "red", "Enumeration",
                 False, children_list[0])
    check_simple(debug, "(1)", "parse.my_enum", "my_green", "Enumeration",
                 False, children_list[1])
    check_simple(debug, "(2)", "parse.my_enum", "blue", "Enumeration",
                 False, children_list[2])

    var = check_simple(debug, "Erm", "parse.enum_range_matrix", "",
                       "Array ( 0 ..  1,  0 ..  1)")
    children_list = var.children()
    gps_assert(len(children_list), 4, mode + " Invalid count of Erm children")
    check_simple(debug, "(0,0)", "integer", "0", "Simple",
                 False, children_list[0])
    check_simple(debug, "(0,1)", "integer", "0", "Simple",
                 False, children_list[1])
    check_simple(debug, "(1,0)", "integer", "0", "Simple",
                 False, children_list[2])
    check_simple(debug, "(1,1)", "integer", "0", "Simple",
                 False, children_list[3])

    var = check_simple(debug, "Negative_Array_Variable",
                       "parse.negative_array", "",
                       "Array (-50 .. -46)")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode +
               " Invalid count of Negative_Array_Variable children")
    check_simple(debug, "(-50)", "", "ABCDE", "Simple",
                 False, children_list[0])

    var = check_simple(debug, "Aa", "parse.array_of_array", "",
                       "Array ( 1 ..  2)")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Aa children")
    check_simple(debug, "(1)", "parse.first_index_integer_array", "",
                 "Array ( 24 ..  26)", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 3, mode +
               " Invalid count of Aa.(1) children")
    check_simple(debug, "(24)", "integer", "3",
                 "Simple", False, children_list1[0])
    check_simple(debug, "(25)", "integer", "4",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(26)", "integer", "5",
                 "Simple", False, children_list1[2])
    check_simple(debug, "(2)", "parse.first_index_integer_array", "",
                 "Array ( 24 ..  26)", False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 3, mode +
               " Invalid count of Aa.(1) children")
    check_simple(debug, "(24)", "integer", "6",
                 "Simple", False, children_list1[0])
    check_simple(debug, "(25)", "integer", "7",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(26)", "integer", "8",
                 "Simple", False, children_list1[2])

    var = check_simple(debug, "A3d", "parse.array_3d", "",
                       "Array ( 3 ..  4,  1 ..  2,  6 ..  7)")
    children_list = var.children()
    gps_assert(len(children_list), 8, mode + " Invalid count of A3d children")
    check_simple(debug, "(3,1,6)", "integer",
                 "1", "Simple", False, children_list[0])
    check_simple(debug, "(3,1,7)", "integer",
                 "2", "Simple", False, children_list[1])
    check_simple(debug, "(3,2,6)", "integer",
                 "1", "Simple", False, children_list[2])
    check_simple(debug, "(3,2,7)", "integer",
                 "2", "Simple", False, children_list[3])
    check_simple(debug, "(4,1,6)", "integer",
                 "1", "Simple", False, children_list[4])
    check_simple(debug, "(4,1,7)", "integer",
                 "2", "Simple", False, children_list[5])
    check_simple(debug, "(4,2,6)", "integer",
                 "1", "Simple", False, children_list[6])
    check_simple(debug, "(4,2,7)", "integer",
                 "2", "Simple", False, children_list[7])

    var = check_simple(debug, "Aos", "parse.array_of_string", "",
                       "Array ( 1 ..  2)")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Aos children")
    check_simple(debug, "(1)", "parse.string_access",
                 r"^0x[0-9a-f]+", "Access", True, children_list[0])
    check_simple(debug, "(2)", "parse.string_access",
                 r"^0x[0-9a-f]+", "Access", True, children_list[1])

    check_simple(debug, "Nr", "parse.null_record", "null record", "Record")

    var = check_simple(debug, "V", "parse.my_record", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of main::Anonymous_Var children")
    check_simple(debug, ".field1", "parse.access_type",
                 r"^0x[0-9a-f]+", "Access", True, children_list[0])
    check_simple(debug, ".field2", "string (1 .. 2)", '"ab"', "String",
                 False, children_list[1])

    check_simple(debug, "Mra", "parse.my_record_access",
                 r"^0x[0-9a-f]+", "Access", True)

    var = check_simple(debug, "W", "array (0 .. 1) of parse.my_record", "",
                       "Array ( 0 ..  1)")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of W children")
    check_simple(debug, "(0)", "parse.my_record", "", "Record",
                 False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of W.(0) children")
    check_simple(debug, ".field1", "parse.access_type",
                 r"^0x[0-9a-f]+", "Access", True, children_list1[0])
    check_simple(debug, ".field2", "string (1 .. 2)", '"ab"', "String",
                 False, children_list1[1])
    check_simple(debug, "(1)", "parse.my_record", "", "Record",
                 False, children_list[0])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of W.(1) children")
    check_simple(debug, ".field1", "parse.access_type",
                 r"^0x[0-9a-f]+", "Access", True, children_list1[0])
    check_simple(debug, ".field2", "string (1 .. 2)", '"rt"', "String",
                 False, children_list1[1])

    var = check_simple(debug, "Rr", "parse.record_of_record", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Rr children")
    check_simple(debug, ".casefield1", "parse.access_type", r"^0x[0-9a-f]+",
                 "Access", True, children_list[0])
    check_simple(debug, ".field2", "parse.my_record", "", "Record",
                 False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Rr.field2 children")
    check_simple(debug, ".field1", "parse.access_type", r"^0x[0-9a-f]+",
                 "Access", True, children_list1[0])
    check_simple(debug, ".field2", "string (1 .. 2)",
                 '"ab"', "String", False, children_list1[1])

    var = check_simple(debug, "Roa", "parse.record_of_array", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode + " Invalid count of Roa children")
    check_simple(debug, ".field1", "parse.integer_array", "",
                 "Array ( 1 ..  4)", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 4, mode +
               " Invalid count of Roa.field1 children")
    check_simple(debug, "(1)", "integer", "2", "Simple",
                 False, children_list1[0])
    check_simple(debug, "(2)", "integer", "3", "Simple",
                 False, children_list1[1])
    check_simple(debug, "(3)", "integer", "4", "Simple",
                 False, children_list1[2])
    check_simple(debug, "(4)", "integer", "5", "Simple",
                 False, children_list1[3])
    check_simple(debug, ".field2", "parse.integer_array", "",
                 "Array ( 1 ..  4)", False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 4, mode +
               " Invalid count of Roa.field2 children")
    check_simple(debug, "(1)", "integer", "2", "Simple",
                 False, children_list1[0])
    check_simple(debug, "(2)", "integer", "3", "Simple",
                 False, children_list1[1])
    check_simple(debug, "(3)", "integer", "4", "Simple",
                 False, children_list1[2])
    check_simple(debug, "(4)", "integer", "5", "Simple",
                 False, children_list1[3])
    check_simple(debug, ".field3", "integer", "1234",
                 "Simple", False, children_list[2])

    var = check_simple(debug, "X", "parse.integer_array3", "",
                       "Array ( 1 ..  10,  1 ..  20)")
    children_list = var.children()
    gps_assert(len(children_list), 12, mode + " Invalid count of X children")
    check_simple(debug, "(1,1)", "integer", "1",
                 "Simple", False, children_list[0])
    check_simple(debug, "(1,2)", "integer", "2",
                 "Simple", False, children_list[1])
    check_simple(debug, "(1,3)", "integer", "0 <18 times>",
                 "Repeat", False, children_list[2])
    check_simple(debug, "(2,1)", "integer", "0 <20 times>",
                 "Repeat", False, children_list[3])
    check_simple(debug, "(3,1)", "integer", "0 <20 times>",
                 "Repeat", False, children_list[4])
    check_simple(debug, "(4,1)", "integer", "0 <20 times>",
                 "Repeat", False, children_list[5])
    check_simple(debug, "(5,1)", "integer", "0 <20 times>",
                 "Repeat", False, children_list[6])
    check_simple(debug, "(6,1)", "integer", "0 <20 times>",
                 "Repeat", False, children_list[7])
    check_simple(debug, "(7,1)", "integer", "0 <20 times>",
                 "Repeat", False, children_list[8])
    check_simple(debug, "(8,1)", "integer", "0 <20 times>",
                 "Repeat", False, children_list[9])
    check_simple(debug, "(9,1)", "integer", "0 <20 times>",
                 "Repeat", False, children_list[10])
    check_simple(debug, "(10,1)", "integer", "0 <20 times>",
                 "Repeat", False, children_list[11])

    var = check_simple(debug, "Ar", "parse.array_of_record", "",
                       "Array ( 1 ..  2)")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Ar children")
    check_simple(debug, "(1)", "parse.my_record", "",
                 "Record", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ar.(1) children")
    check_simple(debug, ".field1", "parse.access_type", r"^0x[0-9a-f]+",
                 "Access", True, children_list1[0])
    check_simple(debug, ".field2", "string (1 .. 2)", '"ab"', "String",
                 False, children_list1[1])
    check_simple(debug, "(2)", "parse.my_record", "",
                 "Record", False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(2) children")
    check_simple(debug, ".field1", "parse.access_type", r"^0x[0-9a-f]+",
                 "Access", True, children_list1[0])
    check_simple(debug, ".field2", "string (1 .. 2)", '"cd"', "String",
                 False, children_list1[1])

    var = check_simple(debug, "Z", "parse.discriminants_record", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode + " Invalid count of Z children")
    check_simple(debug, ".a", "integer", "1",
                 "Simple", False, children_list[0])
    check_simple(debug, ".b", "boolean", "false",
                 "Simple", False, children_list[1])
    check_simple(debug, ".c", "<4-byte float>", "2.0",
                 "Simple", False, children_list[2])

    check_simple(debug, "As", "parse.access_subprogram", r"^0x[0-9a-f]+",
                 "Access", True)

    var = check_simple(debug, "Y", "parse.variable_record", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode + " Invalid count of Y children")
    check_simple(debug, ".a", "boolean", "true",
                 "Simple", False, children_list[0])
    check_simple(debug, "<variant part>", "parse.variable_record", "",
                 "Record", False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of Y.(var 1) children")
    check_simple(debug, ".b", "integer", "1",
                 "Simple", False, children_list1[0])
    check_simple(debug, "<variant part>", "parse.variable_record", "",
                 "Record", False, children_list[2])
    children_list1 = children_list[2].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Y.(var 2) children")
    check_simple(debug, ".c", "<4-byte float>", "<unknown>",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".d", "integer", "<unknown>",
                 "Simple", False, children_list1[1])

    var = check_simple(debug, "Y2", "parse.variable_record", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode + " Invalid count of Y2 children")
    check_simple(debug, ".a", "boolean", "false",
                 "Simple", False, children_list[0])
    check_simple(debug, "<variant part>", "parse.variable_record", "",
                 "Record", False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of Y2.(var 1) children")
    check_simple(debug, ".b", "integer", "<unknown>",
                 "Simple", False, children_list1[0])
    check_simple(debug, "<variant part>", "parse.variable_record", "",
                 "Record", False, children_list[2])
    children_list1 = children_list[2].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Y2.(var 2) children")
    check_simple(debug, ".c", "<4-byte float>", "1.0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".d", "integer", "2",
                 "Simple", False, children_list1[1])

    var = check_simple(debug, "Tt", "parse.tagged_type", "", "Class")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Tt children")
    check_simple(debug, ".a", "integer", "2",
                 "Simple", False, children_list[0])
    check_simple(debug, ".c", "character", "67 'C'",
                 "Simple", False, children_list[1])

    var = check_simple(debug, "Ctt2", "parse.child_tagged_type2", "", "Class")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode + " Invalid count of Ctt2 children")
    check_simple(debug, "<tagged_type>", "parse.tagged_type", "",
                 "Class", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ctt2.<tagged_type> children")
    check_simple(debug, ".a", "integer", "2",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".b", "character", "67 'C'",
                 "Simple", False, children_list1[1])

    var = check_simple(debug, "T_Ptr.all",
                       r"parse\.t_type|array \(1 \.\. 2\) of integer", "",
                       "Array ( 1 ..  2)", False, None, True)
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of T_Ptr.all children")
    check_simple(debug, "(1)", "integer", "13",
                 "Simple", False, children_list[0])
    check_simple(debug, "(2)", "integer", "17",
                 "Simple", False, children_list[1])

    var = check_simple(debug, "T_Ptr2.all",
                       "parse\.t_type|array \(2 \.\. 3\) of integer", "",
                       "Array ( 2 ..  3)", False, None, True)
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of T_Ptr2.all children")
    check_simple(debug, "(2)", "integer", "13",
                 "Simple", False, children_list[0])
    check_simple(debug, "(3)", "integer", "17",
                 "Simple", False, children_list[1])

    var = check_simple(debug, "Ba", "parse.big_array", "",
                       "Array ( 1 ..  1000)")
    children_list = var.children()
    gps_assert(len(children_list), 7, mode + " Invalid count of Ba children")
    check_simple(debug, "(1)", "parse.access_type|access integer",
                 r"^0x[0-9a-f]+",
                 "Access", True, children_list[0], True)
    check_simple(debug, "(2)", "parse.access_type|access integer",
                 r"^0x[0-9a-f]+",
                 "Access", True, children_list[1], True)
    check_simple(debug, "(3)", "parse.access_type|access integer",
                 r"^0x[0-9a-f]+",
                 "Access", True, children_list[2], True)
    check_simple(debug, "(4)", "parse.access_type|access integer",
                 r"^0x[0-9a-f]+",
                 "Access", True, children_list[3], True)
    check_simple(debug, "(5)", "parse.access_type|access integer",
                 r"^0x[0-9a-f]+ \<895 times\>", "Repeat",
                 True, children_list[4], True)
    check_simple(debug, "(900)", "parse.access_type|access integer",
                 r"^0x[0-9a-f]+",
                 "Access", True, children_list[5], True)
    check_simple(debug, "(901)", "parse.access_type|access integer",
                 r"^0x[0-9a-f]+ \<100 times\>", "Repeat",
                 True, children_list[6], True)

    var = check_simple(debug, "Ba2", "parse.big_array2", "",
                       "Array ( 1 ..  1000)")
    children_list = var.children()
    gps_assert(len(children_list), 7, mode + " Invalid count of Ba2 children")
    check_simple(debug, "(1)", "integer", "0",
                 "Simple", False, children_list[0])
    check_simple(debug, "(2)", "integer", "0",
                 "Simple", False, children_list[1])
    check_simple(debug, "(3)", "integer", "0",
                 "Simple", False, children_list[2])
    check_simple(debug, "(4)", "integer", "1",
                 "Simple", False, children_list[3])
    check_simple(debug, "(5)", "integer",
                 "0 <895 times>", "Repeat",
                 False, children_list[4])
    check_simple(debug, "(900)", "integer", "2",
                 "Simple", False, children_list[5])
    check_simple(debug, "(901)", "integer",
                 "0 <100 times>", "Repeat",
                 False, children_list[6])

    check_simple(debug, "RegExp", "string (1 .. 9)", r'"(\w)=(\w)"', "String")
    check_simple(debug, "Null_Ptr.all", "(array \(0 \.\. 0\) of integer)?",
                 r"(\<unknown\>)?", r"Access|Array \( 1 \.\.  0\)",
                 True, None, True, True)

    var = check_simple(debug, "Ra", "parse.record_array", "",
                       "Array ( 1 ..  10)")
    children_list = var.children()
    gps_assert(len(children_list), 10, mode + " Invalid count of Ra children")
    check_simple(debug, "(1)", "parse.point", "",
                 "Record", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(1) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(2)", "parse.point", "",
                 "Record", False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(2) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(3)", "parse.point", "",
                 "Record", False, children_list[2])
    children_list1 = children_list[2].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(3) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(4)", "parse.point", "",
                 "Record", False, children_list[3])
    children_list1 = children_list[3].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(4) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(5)", "parse.point", "",
                 "Record", False, children_list[4])
    children_list1 = children_list[4].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(5) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(6)", "parse.point", "",
                 "Record", False, children_list[5])
    children_list1 = children_list[5].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(6) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(7)", "parse.point", "",
                 "Record", False, children_list[6])
    children_list1 = children_list[6].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(7) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(8)", "parse.point", "",
                 "Record", False, children_list[7])
    children_list1 = children_list[7].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(8) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(9)", "parse.point", "",
                 "Record", False, children_list[8])
    children_list1 = children_list[8].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(9) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])
    check_simple(debug, "(10)", "parse.point", "",
                 "Record", False, children_list[9])
    children_list1 = children_list[9].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Ra.(10) children")
    check_simple(debug, ".x", "integer", "0",
                 "Simple", False, children_list1[0])
    check_simple(debug, ".y", "integer", "0",
                 "Simple", False, children_list1[1])

    var = check_simple(debug, "Nvp", "parse.null_variant_part", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 4, mode + " Invalid count of Nvp children")
    check_simple(debug, ".discr", "integer", "3",
                 "Simple", False, children_list[0])
    check_simple(debug, "<variant part>", "parse.null_variant_part", "",
                 "Record", False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of Nvp children")
    check_simple(debug, ".var_1", "integer", "<unknown>",
                 "Simple", False, children_list1[0])
    check_simple(debug, "<variant part>", "parse.null_variant_part", "",
                 "Record", False, children_list[2])
    children_list1 = children_list[2].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of Nvp children")
    check_simple(debug, ".var_2", "boolean", "<unknown>",
                 "Simple", False, children_list1[0])
    check_simple(debug, "<variant part>", "parse.null_variant_part",
                 "null record", "Record", False, children_list[3])
    gps_assert(len(children_list[3].children()), 0, mode +
               " Invalid count of Nvp children")

    check_simple(debug, "My_Str", "string (1 .. 6)", '"string"', "String")
    check_simple(debug, "Final_Result", "string (1 .. 20)",
                 '"The result is:      "', "String")
    check_simple(debug, "Final_Result2", "string (1 .. 20)",
                 '"The result is:      "', "String")

    var = check_simple(debug, "This", "parse.object", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode + " Invalid count of This children")
    check_simple(debug, ".attributes", "parse.attribute_arrP", "",
                 "Array ( 0 ..  1,  0 ..  1)", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 4, mode +
               " Invalid count of This.attributes children")
    check_simple(debug, "(0,0)", "parse.pointer|access parse.object2",
                 r"^0x[0-9a-f]+",
                 "Access", True, children_list1[0], True)
    check_simple(debug, "(0,1)", "parse.pointer|access parse.object2",
                 r"^0x[0-9a-f]+",
                 "Access", True, children_list1[1], True)
    check_simple(debug, "(1,0)", "parse.pointer|access parse.object2",
                 r"^0x[0-9a-f]+",
                 "Access", True, children_list1[2], True)
    check_simple(debug, "(1,1)", "parse.pointer|access parse.object2",
                 r"^0x[0-9a-f]+",
                 "Access", True, children_list1[3], True)

    var = check_simple(debug, "Scr", "parse.screen_image_type", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode + " Invalid count of Scr children")
    check_simple(debug, ".new_screen_image", "parse.screen_image_type_0", "",
                 "Array ( 1 ..  14)", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of Scr.new_screen_image children")
    check_simple(debug, "(1)", "parse.line_buffer_type", " <14 times>",
                 "Repeat", False, children_list1[0])
    children_list2 = children_list1[0].children()
    gps_assert(len(children_list2), 1, mode +
               " Invalid count of Scr.new_screen_image.(1) children")
    check_simple(debug, "(1)", "parse.screen_element_type", " <24 times>",
                 "Repeat", False, children_list2[0])
    children_list3 = children_list2[0].children()
    gps_assert(len(children_list3), 4, mode +
               " Invalid count of Scr.new_screen_image.(1).(1) children")
    check_simple(debug, ".char", "character", "32 ' '",
                 "Simple", False, children_list3[0])
    check_simple(debug, ".color", "parse.color_type", "cyan",
                 "Enumeration", False, children_list3[1])
    check_simple(debug, ".font", "parse.font_type", "small",
                 "Enumeration", False, children_list3[2])
    check_simple(debug, ".reverse_video", "boolean", "true",
                 "Simple", False, children_list3[3])

    var = check_simple(debug, "More_Fruits", "parse.fruits_array_type", "",
                       "Array ( 0 ..  1)")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of More_Fruits children")
    check_simple(debug, "(0)", "parse.fruit", "",
                 "Record", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 3, mode +
               " Invalid count of More_Fruits.(0) children")
    check_simple(debug, ".choice", "parse.choice_type", "apple",
                 "Enumeration", False, children_list1[0])
    check_simple(debug, "<variant part>", "parse.fruit", "",
                 "Record", False, children_list1[1])
    children_list2 = children_list1[1].children()
    gps_assert(len(children_list2), 1, mode +
               " Invalid count of More_Fruits.(0).<variant part> children")
    check_simple(debug, "abc", "integer", "<unknown>",
                 "Simple", False, children_list2[0])
    check_simple(debug, "<variant part>", "parse.fruit", "",
                 "Record", False, children_list1[2])
    children_list2 = children_list1[2].children()
    gps_assert(len(children_list2), 1, mode +
               " Invalid count of More_Fruits.(0).<variant part> children")
    check_simple(debug, "yxz", "integer", "20",
                 "Simple", False, children_list2[0])
    check_simple(debug, "(1)", "parse.fruit", "",
                 "Record", False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 3, mode +
               " Invalid count of More_Fruits.(1) children")
    check_simple(debug, ".choice", "parse.choice_type", "peach",
                 "Enumeration", False, children_list1[0])
    check_simple(debug, "<variant part>", "parse.fruit", "",
                 "Record", False, children_list1[1])
    children_list2 = children_list1[1].children()
    gps_assert(len(children_list2), 1, mode +
               " Invalid count of More_Fruits.(1).<variant part> children")
    check_simple(debug, "abc", "integer", "2000",
                 "Simple", False, children_list2[0])
    check_simple(debug, "<variant part>", "parse.fruit", "",
                 "Record", False, children_list1[2])
    children_list2 = children_list1[2].children()
    gps_assert(len(children_list2), 1, mode +
               " Invalid count of More_Fruits.(0).<variant part> children")
    check_simple(debug, "yxz", "integer", "<unknown>",
                 "Simple", False, children_list2[0])

    debug.frame_down()

    var = check_simple(debug, "Args",
                       "array (1 .. 3) of parse.tn_9305_014.string_access",
                       "", "Array ( 1 ..  3)")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode +
               " Invalid count of Args children")
    check_simple(debug, "(1)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[0])
    check_simple(debug, "(2)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[1])
    check_simple(debug, "(3)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[2])
    var = check_simple(debug, "Args2",
                       "array (1 .. 3, 1 .. 3) of " +
                       "parse.tn_9305_014.string_access", "",
                       "Array ( 1 ..  3,  1 ..  3)")
    children_list = var.children()
    gps_assert(len(children_list), 9, mode +
               " Invalid count of Args2 children")
    check_simple(debug, "(1,1)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[0])
    check_simple(debug, "(1,2)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[1])
    check_simple(debug, "(1,3)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[2])
    check_simple(debug, "(2,1)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[3])
    check_simple(debug, "(2,2)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[4])
    check_simple(debug, "(2,3)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[5])
    check_simple(debug, "(3,1)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[6])
    check_simple(debug, "(3,2)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[7])
    check_simple(debug, "(3,3)", "parse.tn_9305_014.string_access",
                 r"0x[0-9a-f]+", "Access", True, children_list[8])

    debug.send("cont")

    var = check_simple(debug, "Ut", "parse.union_type", "", "Union")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Ut children")
    check_simple(debug, ".b", "integer", "3",
                 "Simple", False, children_list[0])
    check_simple(debug, ".c", "<4-byte float>", r"^4.2",
                 "Simple", True, children_list[1])

    check_simple(debug, "A", "<4-byte integer>", "5", "Simple")

    var = check_simple(debug, "T", "<ref> parse.tagged_type", "", "Class")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of T children")
    check_simple(debug, ".a", "integer", "2",
                 "Simple", False, children_list[0])
    check_simple(debug, ".b", "character", "67 'C'",
                 "Simple", False, children_list[1])

    if platform.system().lower() == 'windows':
        var = check_simple(debug, "R", "<ref> parse.my_record", "", "Record")
    else:
        var = check_simple(debug, "R", "parse.my_record", "", "Record")

    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of R children")
    check_simple(debug, ".field1", "parse.access_type", r"0x[0-9a-f]+",
                 "Access", True, children_list[0])
    check_simple(debug, ".field2", "string (1 .. 2)", '"ab"',
                 "String", False, children_list[1])

    var = check_simple(debug, "X1", "parse_controlled.r2", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode + " Invalid count of X1 children")
    check_simple(debug, ".x", "parse_controlled.m2", "",
                 "Array ( 1 ..  2,  1 ..  1)", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of X1.x children")
    check_simple(debug, "(1,1)", "parse_controlled.t1", "",
                 "Class", False, children_list1[0])
    children_list2 = children_list1[0].children()
    gps_assert(len(children_list2), 1, mode +
               " Invalid count of X1.x.(1,1) children")
    check_simple(debug, "<Limited_Controlled>",
                 "Ada.Finalization.Limited_Controlled", "",
                 "Ada.Finalization.Limited_Controlled",
                 False, children_list2[0])
    check_simple(debug, "(2,1)", "parse_controlled.t1", "",
                 "Class", False, children_list1[1])
    children_list2 = children_list1[1].children()
    gps_assert(len(children_list2), 1, mode +
               " Invalid count of X1.x.(1,1) children")
    check_simple(debug, "<Limited_Controlled>",
                 "Ada.Finalization.Limited_Controlled", "",
                 "Ada.Finalization.Limited_Controlled",
                 False, children_list2[0])

    var = check_simple(debug, "Ustring",
                       "Ada.Strings.Unbounded.Unbounded_String",
                       '"not_set"', "Ada.Strings.Unbounded.Unbounded_String")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of Ustring children")
    check_simple(debug, "<Limited_Controlled>",
                 "Ada.Finalization.Limited_Controlled", "",
                 "Ada.Finalization.Limited_Controlled",
                 False, children_list[0])
    check_simple(debug, ".reference",
                 "ada.strings.unbounded.shared_string_access", r"0x[0-9a-f]+",
                 "Access", True, children_list[1])

    var = check_simple(debug, "Asu_Test", "parse.bar.matrix_map", "",
                       "Array ( 1 ..  2,  1 ..  1)")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of Asu_Test children")
    check_simple(debug, "(1,1)", "Ada.Strings.Unbounded.Unbounded_String", "",
                 "Ada.Strings.Unbounded.Unbounded_String",
                 False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Asu_Test.(1,1) children")
    check_simple(debug, "<Limited_Controlled>",
                 "Ada.Finalization.Limited_Controlled", "",
                 "Ada.Finalization.Limited_Controlled",
                 False, children_list1[0])
    check_simple(debug, ".reference",
                 "ada.strings.unbounded.shared_string_access", r"0x[0-9a-f]+",
                 "Access", True, children_list1[1])
    check_simple(debug, "(2,1)", "Ada.Strings.Unbounded.Unbounded_String", "",
                 "Ada.Strings.Unbounded.Unbounded_String",
                 False, children_list[1])
    children_list1 = children_list[1].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Asu_Test.(2,1) children")
    check_simple(debug, "<Limited_Controlled>",
                 "Ada.Finalization.Limited_Controlled", "",
                 "Ada.Finalization.Limited_Controlled",
                 False, children_list1[0])
    check_simple(debug, ".reference",
                 "ada.strings.unbounded.shared_string_access", r"0x[0-9a-f]+",
                 "Access", True, children_list1[1])

    var = check_simple(debug, "Asu_Test2", "parse.bar.matrix_map_instance", "",
                       "Class")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode +
               " Invalid count of Asu_Test2 children")
    check_simple(debug, ".map", "parse.bar.matrix_map", "",
                 "Array ( 1 ..  2,  1 ..  1)",
                 False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of Asu_Test2.map children")
    check_simple(debug, "(1,1)", "Ada.Strings.Unbounded.Unbounded_String", "",
                 "Ada.Strings.Unbounded.Unbounded_String",
                 False, children_list1[0])
    children_list2 = children_list1[0].children()
    gps_assert(len(children_list2), 2, mode +
               " Invalid count of Asu_Test2.map.(1,1) children")
    check_simple(debug, "<Limited_Controlled>",
                 "Ada.Finalization.Limited_Controlled", "",
                 "Ada.Finalization.Limited_Controlled",
                 False, children_list2[0])
    check_simple(debug, ".reference",
                 "ada.strings.unbounded.shared_string_access", r"0x[0-9a-f]+",
                 "Access", True, children_list2[1])
    check_simple(debug, "(2,1)", "Ada.Strings.Unbounded.Unbounded_String", "",
                 "Ada.Strings.Unbounded.Unbounded_String",
                 False, children_list1[1])
    children_list2 = children_list1[1].children()
    gps_assert(len(children_list2), 2, mode +
               " Invalid count of Asu_Test2.map.(2,1) children")
    check_simple(debug, "<Limited_Controlled>",
                 "Ada.Finalization.Limited_Controlled", "",
                 "Ada.Finalization.Limited_Controlled",
                 False, children_list2[0])
    check_simple(debug, ".reference",
                 "ada.strings.unbounded.shared_string_access", r"0x[0-9a-f]+",
                 "Access", True, children_list2[1])

    var = check_simple(debug, "My_Exception", "exception", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 7, mode +
               " Invalid count of My_Exception children")
    check_simple(debug, ".not_handled_by_others", "boolean", "false",
                 "Simple", False, children_list[0])
    check_simple(debug, ".lang", "character", "65 'A'",
                 "Simple", False, children_list[1])
    check_simple(debug, ".name_length", "natural", "30",
                 "Simple", False, children_list[2])
    check_simple(debug, ".full_name", "access character",
                 r'0x[0-9a-f]+ \<.+', "Access", True, children_list[3])
    check_simple(debug, ".htable_ptr", "access character",
                 r'0x[0-9a-f]+ \<.+', "Access", True, children_list[4])
    check_simple(debug, ".foreign_data", "access character",
                 r'0x[0-9a-f]+', "Access", True, children_list[5])
    check_simple(debug, ".raise_hook", "access character",
                 r'0x[0-9a-f]+', "Access", True, children_list[6])

    var = check_simple(debug, "NBI_N", "parse.value_type", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 4, mode +
               " Invalid count of NBI_N children")
    check_simple(debug, ".var", "parse.value_var_type", "v_null",
                 "Enumeration", False, children_list[0])
    check_simple(debug, "<variant part>", "parse.value_type", "null record",
                 "Record", False, children_list[1])
    check_simple(debug, "<variant part>", "parse.value_type", "",
                 "Record", False, children_list[2])
    children_list1 = children_list[2].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of NBI_N.<variant part> children")
    check_simple(debug, ".boolean_value", "boolean", "<unknown>",
                 "Simple", False, children_list1[0])
    check_simple(debug, "<variant part>", "parse.value_type", "",
                 "Record", False, children_list[3])
    children_list1 = children_list[3].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of NBI_N.<variant part> children")
    check_simple(debug, ".boolean_value", "boolean|integer", "<unknown>",
                 "Simple", False, children_list1[0], True)

    var = check_simple(debug, "NBI_B", "parse.value_type", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 4, mode +
               " Invalid count of NBI_B children")
    check_simple(debug, ".var", "parse.value_var_type", "v_boolean",
                 "Enumeration", False, children_list[0])
    check_simple(debug, "<variant part>", "parse.value_type", "null record",
                 "Record", False, children_list[1])
    check_simple(debug, "<variant part>", "parse.value_type", "",
                 "Record", False, children_list[2])
    children_list1 = children_list[2].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of NBI_B.<variant part> children")
    check_simple(debug, ".boolean_value", "boolean", "true",
                 "Simple", False, children_list1[0])
    check_simple(debug, "<variant part>", "parse.value_type", "",
                 "Record", False, children_list[3])
    children_list1 = children_list[3].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of NBI_B.<variant part> children")
    check_simple(debug, ".integer_value", "integer", "<unknown>",
                 "Simple", False, children_list1[0])

    var = check_simple(debug, "NBI_I", "parse.value_type", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 4, mode +
               " Invalid count of NBI_I children")
    check_simple(debug, ".var", "parse.value_var_type", "v_integer",
                 "Enumeration", False, children_list[0])
    check_simple(debug, "<variant part>", "parse.value_type", "null record",
                 "Record", False, children_list[1])
    check_simple(debug, "<variant part>", "parse.value_type", "",
                 "Record", False, children_list[2])
    children_list1 = children_list[2].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of NBI_I.<variant part> children")
    check_simple(debug, ".boolean_value", "boolean", "<unknown>",
                 "Simple", False, children_list1[0])
    check_simple(debug, "<variant part>", "parse.value_type", "",
                 "Record", False, children_list[3])
    children_list1 = children_list[3].children()
    gps_assert(len(children_list1), 1, mode +
               " Invalid count of NBI_I.<variant part> children")
    check_simple(debug, ".integer_value", "integer", "18",
                 "Simple", False, children_list1[0])

    check_simple(debug, "AP", "parse.access_procedure",
                 r'0x[0-9a-f]+', "Access", True)
    gps_assert(debug.get_variable_by_name("AP.all")
               is None, True,
               "AP.all exists")

    check_simple(debug, "AF", "parse.access_function",
                 r'0x[0-9a-f]+', "Access", True)
    gps_assert(debug.get_variable_by_name("AF.all")
               is None, True,
               "AF.all exists")

    var = check_simple(debug, "RAF", "parse.record_access_function",
                       "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode +
               " Invalid count of RAF children")
    check_simple(debug, ".i", "integer", "0",
                 "Simple", False, children_list[0])
    check_simple(debug, ".af", "parse.access_function", r'0x[0-9a-f]+',
                 "Access", True, children_list[1])
    check_simple(debug, ".i2", "integer", "0",
                 "Simple", False, children_list[0])

    debug.send("b swap")
    debug.send("cont")

    check_simple(debug, "Word.all", "string (1 .. 0)", '"qeaLfjb"', "String")

    if "Darwin" in platform.system():
        debug.send("c")
        yield wait_until_not_busy(debug)

    debug.send("q")
    yield wait_idle()
