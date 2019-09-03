import platform
import GPS
from gps_utils.internal.utils import *
import re

mode = "Mode:" + GPS.Preference("GPS6-Debugger-Debugger-Kind").get()


def check_simple(debug, name, type, value, description,
                 pattern=False, var=None):
    if (var is None):
        var = debug.get_variable_by_name(name)
        gps_assert(var.type_name, type,
                   mode + " Invalid type name of " + name)

    if (pattern):
        val = var.simple_value
        gps_assert(re.search(value, val) is None, False,
                   mode + " Invalid value of " + name + " " + val)
    else:
        gps_assert(var.simple_value, value,
                   mode + " Invalid value of " + name)

    gps_assert(var.type_description, description,
               mode + " Invalid type description value of " + name)
    return var


@run_test_driver
def test_driver():
    debug = GPS.Debugger.get()
    yield wait_tasks(other_than=known_tasks)

    debug.break_at_location(GPS.File("parse_cpp.cc"), 10)
    debug.send("run")
    yield wait_until_not_busy(debug)

    debug.frame_up()

    gps_assert(debug.get_variable_by_name("Parse::Non_Existant_Variable")
               is None, True,
               "Non_Existant_Variable exists")

    check_simple(debug, "A", "int", "1", "Simple")
    check_simple(debug, "B", "float", "2", "Simple")
    check_simple(debug, "C", "char", "65 'A'", "Simple")
    check_simple(debug, "Sh", "short", "65", "Simple")
    check_simple(debug, "L", "long", "32", "Simple")
    check_simple(debug, "Uns", "unsigned int", "33", "Simple")
    check_simple(debug, "UL", "unsigned long", "33", "Simple")
    check_simple(debug, "S", "char *", '^0x[0-9a-f]+ "abcd"', "Access", True)
    check_simple(debug, "S2", "char *",
                 r'^0x[0-9a-f]+ "ab\\nc"', "Access", True)
    check_simple(debug, "S3", "char *",
                 r'^0x[0-9a-f]+ "ab\[\\"c"', "Access", True)
    check_simple(debug, "Act", "int *", '^0x[0-9a-f]+', "Access", True)
    check_simple(debug, "My_Enum_Variable", "enum : ", "Blue", "Enumeration")

    var = check_simple(debug, "T", "int [4]", "", "Array ( 0 ..  3)")
    childs_list = var.children()
    gps_assert(len(childs_list), 4, mode + " Invalid count of T childs")
    check_simple(debug, "T[1]", "int", "2", "Simple", False, childs_list[0])
    check_simple(debug, "T[2]", "int", "3", "Simple", False, childs_list[1])
    check_simple(debug, "T[3]", "int", "4", "Simple", False, childs_list[2])
    check_simple(debug, "T[4]", "int", "5", "Simple", False, childs_list[3])

    check_simple(debug, "Ea", "", "^0x[0-9a-f]+", "Access", True)

    var = check_simple(debug, "Aoa", "int *[3]", "", "Array ( 0 ..  2)")
    childs_list = var.children()
    gps_assert(len(childs_list), 3, mode + " Invalid count of Aoa childs")
    check_simple(debug, "Aoa[0]", "int *",
                 "0x[0-9a-f]+", "Access", True, childs_list[0])
    check_simple(debug, "Aoa[1]", "int *",
                 "0x[0-9a-f]+", "Access", True, childs_list[1])
    check_simple(debug, "Aoa[2]", "int *",
                 "0x[0-9a-f]+", "Access", True, childs_list[2])

    var = check_simple(debug, "U", "int [2][3]", "",
                       "Array ( 0 ..  1,  0 ..  2)")
    childs_list = var.children()
    gps_assert(len(childs_list), 6, mode + " Invalid count of U childs")
    check_simple(debug, "U[0,0]", "int", "2", "Simple", False, childs_list[0])
    check_simple(debug, "U[0,1]", "int", "3", "Simple", False, childs_list[1])
    check_simple(debug, "U[0,2]", "int", "4", "Simple", False, childs_list[2])
    check_simple(debug, "U[1,0]", "int", "5", "Simple", False, childs_list[3])
    check_simple(debug, "U[1,1]", "int", "6", "Simple", False, childs_list[4])
    check_simple(debug, "U[1,2]", "int", "7", "Simple", False, childs_list[5])

    var = check_simple(debug, "A3d", "int [2][2][2]", "",
                       "Array ( 0 ..  1,  0 ..  1,  0 ..  1)")
    childs_list = var.children()
    gps_assert(len(childs_list), 8, mode + " Invalid count of A3d childs")
    check_simple(debug, "A3d[0,0,0]", "int",
                 "1", "Simple", False, childs_list[0])
    check_simple(debug, "A3d[0,0,1]", "int",
                 "2", "Simple", False, childs_list[1])
    check_simple(debug, "A3d[0,1,0]", "int",
                 "1", "Simple", False, childs_list[2])
    check_simple(debug, "A3d[0,1,1]", "int",
                 "2", "Simple", False, childs_list[3])
    check_simple(debug, "A3d[1,0,0]", "int",
                 "1", "Simple", False, childs_list[4])
    check_simple(debug, "A3d[1,0,1]", "int",
                 "2", "Simple", False, childs_list[5])
    check_simple(debug, "A3d[1,1,0]", "int",
                 "1", "Simple", False, childs_list[6])
    check_simple(debug, "A3d[1,1,1]", "int",
                 "2", "Simple", False, childs_list[7])

    check_simple(debug, "Iaa", "int **", "^0x[0-9a-f]+", "Access", True)

    var = check_simple(debug, "V", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 3, mode + " Invalid count of V childs")
    check_simple(debug, ".i", "int i", "33", "Simple", False, childs_list[0])
    check_simple(debug, ".field1", "My_Record_Typedef",
                 r"^0x[0-9a-f]+", "Access", True, childs_list[1])
    check_simple(debug, ".field2", "My_Record_Typedef",
                 r'^0x[0-9a-f]+ "ab"', "Access", True, childs_list[2])

    var = check_simple(debug, "Anonymous_Var", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode +
               " Invalid count of Anonymous_Var childs")
    check_simple(debug, ".a", "int a", "1", "Simple", False, childs_list[0])
    check_simple(debug, ".b", "int b", "2", "Simple", False, childs_list[1])

    var = check_simple(debug, "V2", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 3, mode + " Invalid count of V2 childs")
    check_simple(debug, ".i", "int i", "33", "Simple", False, childs_list[0])
    check_simple(debug, ".field1", "My_Record_Typedef",
                 r'^0x[0-9a-f]+', "Access", True, childs_list[1])
    check_simple(debug, ".field2", "My_Record_Typedef",
                 r'^0x[0-9a-f]+ "ab"', "Access", True, childs_list[2])

    var = check_simple(debug, "cl1", "CL", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 1, mode + " Invalid count of cl1 childs")
    check_simple(debug, ".x", "const double",
                 "5", "Simple", False, childs_list[0])

    var = check_simple(debug, "cl2", "CL2", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 1, mode + " Invalid count of cl2 childs")
    check_simple(debug, ".x", "int x", "10", "Simple", False, childs_list[0])

    var = check_simple(debug, "cl3", "CL3", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of cl3 childs")
    check_simple(debug, "<CL2 >", "public CL2", "",
                 "Class", False, childs_list[0])
    check_simple(debug, ".y", "int y", "11", "Simple", False, childs_list[1])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 1, mode + " Invalid count of cl3.CL2 childs")
    check_simple(debug, ".x", "int x", "10", "Simple", False, childs_list1[0])

    var = check_simple(debug, "cl4", "CL4", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 1, mode + " Invalid count of cl4 childs")
    check_simple(debug, "<CL2 >", "public CL2", "",
                 "Class", False, childs_list[0])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 1, mode + " Invalid count of cl4.CL2 childs")
    check_simple(debug, ".x", "int x", "10", "Simple", False, childs_list1[0])

    var = check_simple(debug, "Mror", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of Mror childs")
    check_simple(debug, ".c", " ", "", "Class", False, childs_list[0])
    check_simple(debug, ".d", "int d", "2", "Simple", False, childs_list[1])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 2, mode + " Invalid count of Mror.c childs")
    check_simple(debug, ".a", "int a", "1", "Simple", False, childs_list1[0])
    check_simple(debug, ".b", "My_Record_Of_Record::Field1_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list1[1])

    var = check_simple(debug, "Mrora", "My_Record_Of_Record [2]", "",
                       "Array ( 0 ..  1)")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of Mrora childs")
    check_simple(debug, "[0]", " ", "", "Class", False, childs_list[0])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of Mrora[0] childs")
    check_simple(debug, ".c", " ", "", "Class", False, childs_list1[0])
    check_simple(debug, ".d", "int d", "4", "Simple", False, childs_list1[1])
    childs_list2 = childs_list1[0].children()
    gps_assert(len(childs_list2), 2, mode +
               " Invalid count of Mrora[0].c childs")
    check_simple(debug, ".a", "int a", "3", "Simple", False, childs_list2[0])
    check_simple(debug, ".b", "My_Record_Of_Record::Field1_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list2[1])
    check_simple(debug, "[1]", " ", "", "Class", False, childs_list[1])
    childs_list1 = childs_list[1].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of Mrora[1] childs")
    check_simple(debug, ".c", " ", "", "Class", False, childs_list1[0])
    check_simple(debug, ".d", "int d", "2", "Simple", False, childs_list1[1])
    childs_list2 = childs_list1[0].children()
    gps_assert(len(childs_list2), 2, mode +
               " Invalid count of Mrora[0].c childs")
    check_simple(debug, ".a", "int a", "1", "Simple", False, childs_list2[0])
    check_simple(debug, ".b", "My_Record_Of_Record::Field1_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list2[1])

    var = check_simple(debug, "Mrorpa", "My_Record_Of_Record *[2]", "",
                       "Array ( 0 ..  1)")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of Mrorpa childs")
    check_simple(debug, "[0]", "My_Record_Of_Record *",
                 r'^0x[0-9a-f]+', "Access",  True, childs_list[0])
    check_simple(debug, "[1]", "My_Record_Of_Record *",
                 r'^0x[0-9a-f]+', "Access", True, childs_list[1])

    var = check_simple(debug, "Uni", "", "", "Union")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of Uni childs")
    check_simple(debug, ".a", "int a", "1", "Simple", False, childs_list[0])
    check_simple(debug, ".b", "float b",
                 r"^1.4", "Simple", True, childs_list[1])

    var = check_simple(debug, "Uni2", "", "", "Union")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of Uni2 childs")
    check_simple(debug, ".a", " ", "", "Class", False, childs_list[0])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 3, mode + " Invalid count of Uni2.a childs")
    check_simple(debug, ".i", "int i", "33", "Simple", False, childs_list1[0])
    check_simple(debug, ".field1", "My_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list1[1])
    check_simple(debug, ".field2", "My_Record",
                 r'^0x[0-9a-f]+ "ab"', "Access", True, childs_list1[2])
    check_simple(debug, ".b", "int b", "33", "Simple", False, childs_list[1])

    var = check_simple(debug, "Uni3", "", "", "Union")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of Uni3 childs")
    check_simple(debug, ".a", " ", "", "Class", False, childs_list[0])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 3, mode + " Invalid count of Uni2.a childs")
    check_simple(debug, ".i", "int i", "2", "Simple", False, childs_list1[0])
    check_simple(debug, ".field1", "My_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list1[1])
    check_simple(debug, ".field2", "My_Record",
                 r'^0x[0-9a-f]+ "ab"', "Access", True, childs_list1[2])
    check_simple(debug, ".b", "int b", "2", "Simple", False, childs_list[1])

    var = check_simple(debug, "Mrwu", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of Mrwu childs")
    check_simple(debug, ".field1", "", "", "Union", False, childs_list[0])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of Mrwu.field1 childs")
    check_simple(debug, ".a", "int i", "1", "Simple", False, childs_list1[0])
    check_simple(debug, ".b", "float b", r'^1.4',
                 "Simple", True, childs_list1[1])
    check_simple(debug, ".field2", "float field2",
                 r"^3.4", "Simple", True, childs_list[1])

    check_simple(debug, "as", "void (*)(void)",
                 r'^0x[0-9a-f]+ \<foo\(\)\>', "Access", True)

    var = check_simple(debug, "asa", "void (*[2])(void)",
                       "", "Array ( 0 ..  1)")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of asa childs")
    check_simple(debug, "[0]", "void (*)(void)",
                 r'^0x[0-9a-f]+ \<foo\(\)\>', "Access", True, childs_list[0])
    check_simple(debug, "[1]", "void (*)(void)",
                 r'^0x[0-9a-f]+ \<foo\(\)\>', "Access", True, childs_list[1])

    var = check_simple(debug, "Mrws", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of Mrws childs")
    check_simple(debug, ".field1", "My_Record_With_Subprogram",
                 r'^0x[0-9a-f]+ \<foo\(\)\>', "Access", True, childs_list[0])
    check_simple(debug, ".field2", "int field2", "1",
                 "Simple", False, childs_list[1])

    var = check_simple(debug, "Mrws2", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of Mrws2 childs")
    check_simple(debug, ".field1", "My_Record_With_Subprogram2",
                 "", "Array ( 0 ..  1)", False, childs_list[0])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of Mrws2.field1 childs")
    check_simple(debug, "[0]", "void (*", r'^0x[0-9a-f]+ \<bar\(int\)\>',
                 "Access", True, childs_list1[0])
    check_simple(debug, "[1]", "void (*", r'^0x[0-9a-f]+ \<bar\(int\)\>',
                 "Access", True, childs_list1[1])
    check_simple(debug, ".field2", "int field2", "1",
                 "Simple", False, childs_list[1])

    var = check_simple(debug, "tv", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of tv childs")
    check_simple(debug, ".tv_sec", "long", "1", "Simple",
                 False, childs_list[0])
    check_simple(debug, ".tv_usec", "long", "100000", "Simple",
                 False, childs_list[1])

    var = check_simple(debug, "list", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 1, mode + " Invalid count of list childs")
    check_simple(debug, ".chain", "tree_common", r'^0x[0-9a-f]+',
                 "Access", True, childs_list[0])

    var = check_simple(debug, "FC", "First_Class", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 3, mode + " Invalid count of FC childs")
    check_simple(debug, ".public_var", "int public_var", "1",
                 "Simple", False, childs_list[0])
    check_simple(debug, ".protected_var", "int protected_var", "2",
                 "Simple", False, childs_list[1])
    check_simple(debug, ".private_var", "int private_var", "3",
                 "Simple", False, childs_list[2])

    var = check_simple(debug, "SC", "Second_Class", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 4, mode + " Invalid count of SC childs")
    check_simple(debug, "<First_Class >", "public First_Class", "",
                 "Class", False, childs_list[0])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 3, mode +
               " Invalid count of SC.First_Class childs")
    check_simple(debug, ".public_var", "int public_var", "1",
                 "Simple", False, childs_list1[0])
    check_simple(debug, ".protected_var", "int protected_var", "2",
                 "Simple", False, childs_list1[1])
    check_simple(debug, ".private_var", "int private_var", "3",
                 "Simple", False, childs_list1[2])
    check_simple(debug, ".second_public_var", "int public_var", "4",
                 "Simple", False, childs_list[1])
    check_simple(debug, ".second_protected_var", "int protected_var", "5",
                 "Simple", False, childs_list[2])
    check_simple(debug, ".second_private_var", "int private_var", "6",
                 "Simple", False, childs_list[3])

    var = check_simple(debug, "SAC", " ", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 2, mode + " Invalid count of SAC childs")
    check_simple(debug, ".struct_public_var", "int struct_public_var", "10",
                 "Simple", False, childs_list[0])
    check_simple(debug, ".struct_private_var", "int struct_private_var", "11",
                 "Simple", False, childs_list[1])

    var = check_simple(debug, "MI", "Multiple_Inheritance", "", "Class")
    childs_list = var.children()
    gps_assert(len(childs_list), 5, mode + " Invalid count of MI childs")
    check_simple(debug, "<Second_Class>", "public Second_Class", "",
                 "Class", False, childs_list[0])
    childs_list1 = childs_list[0].children()
    gps_assert(len(childs_list1), 4, mode +
               " Invalid count of MI.Second_Class childs")
    check_simple(debug, "<First_Class >", "public First_Class", "",
                 "Class", False, childs_list1[0])
    childs_list2 = childs_list1[0].children()
    gps_assert(len(childs_list2), 3, mode +
               " Invalid count of MI.Second_Class.First_Class childs")
    check_simple(debug, ".public_var", "int public_var", "1",
                 "Simple", False, childs_list2[0])
    check_simple(debug, ".protected_var", "int protected_var",
                 "2", "Simple", False, childs_list2[1])
    check_simple(debug, ".private_var", "int private_var", "3",
                 "Simple", False, childs_list2[2])
    check_simple(debug, ".second_public_var", "int second_public_var", "4",
                 "Simple", False, childs_list1[1])
    check_simple(debug, ".second_protected_var", "int second_protected_var",
                 "5", "Simple", False, childs_list1[2])
    check_simple(debug, ".second_private_var", "int second_private_var", "6",
                 "Simple", False, childs_list1[3])
    check_simple(debug, "<Struct_As_Class >", "private Struct_As_Class", "",
                 "Class", False, childs_list[1])
    check_simple(debug, ".third_public_var", "int third_public_var", "7",
                 "Simple", False, childs_list[2])
    check_simple(debug, ".third_protected_var", "int third_protected_var", "8",
                 "Simple", False, childs_list[3])
    check_simple(debug, ".third_private_var", "int third_private_var", "9",
                 "Simple", False, childs_list[4])

    if "Darwin" in platform.system():
        debug.send("c")
        yield wait_until_not_busy(debug)

    debug.send("q")
    yield wait_idle()
