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

    debug.break_at_location(GPS.File("parse_c.c"), 3)
    debug.send("run")
    yield wait_until_not_busy(debug)

    debug.frame_up()

    gps_assert(debug.get_variable_by_name("Parse::Non_Existant_Variable")
               is None, True,
               "Non_Existant_Variable exists")

    check_simple(debug, "main::A", "int", "1", "Simple")
    check_simple(debug, "main::B", "float", "2", "Simple")
    check_simple(debug, "main::C", "char", "65 'A'", "Simple")
    check_simple(debug, "main::Sh", "short", "65", "Simple")
    check_simple(debug, "main::L", "long", "32", "Simple")
    check_simple(debug, "main::Uns", "unsigned int", "33", "Simple")
    check_simple(debug, "main::CC", "unsigned char", "66 'B'", "Simple")
    check_simple(debug, "main::UL", "unsigned long", "33", "Simple")
    check_simple(debug, "main::S", "char *",
                 r'^0x[0-9a-f]+ "abcd"', "Access", True)
    check_simple(debug, "main::S2", "char *",
                 r'^0x[0-9a-f]+ "ab\\nc"', "Access", True)
    check_simple(debug, "main::S3", "char *",
                 r'^0x[0-9a-f]+ "ab\[\\"c"', "Access", True)
    check_simple(debug, "main::Act", "int *", r'^0x[0-9a-f]+', "Access", True)
    check_simple(debug, "main::My_Enum_Variable",
                 "enum colors {Blue, Red, Green}", "Blue", "Enumeration")

    var = check_simple(debug, "main::T", "int [4]", "", "Array ( 0 ..  3)")
    children_list = var.children()
    gps_assert(len(children_list), 4, mode +
               " Invalid count of main::T children")
    check_simple(debug, "[0]", "int", "2", "Simple", False, children_list[0])
    check_simple(debug, "[1]", "int", "3", "Simple", False, children_list[1])
    check_simple(debug, "[2]", "int", "4", "Simple", False, children_list[2])
    check_simple(debug, "[3]", "int", "5", "Simple", False, children_list[3])

    check_simple(debug, "main::Ea", "", r"^0x[0-9a-f]+", "Access", True)

    var = check_simple(debug, "main::Aoa", "int *[3]", "", "Array ( 0 ..  2)")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode +
               " Invalid count of main::Aoa children")
    check_simple(debug, "[0]", "int *",
                 r"0x[0-9a-f]+", "Access", True, children_list[0])
    check_simple(debug, "[1]", "int *",
                 r"0x[0-9a-f]+", "Access", True, children_list[1])
    check_simple(debug, "[2]", "int *",
                 r"0x[0-9a-f]+", "Access", True, children_list[2])

    var = check_simple(debug, "main::U", "int [2][3]", "",
                       "Array ( 0 ..  1,  0 ..  2)")
    children_list = var.children()
    gps_assert(len(children_list), 6, mode +
               " Invalid count of main::U children")
    check_simple(debug, "[0,0]", "int", "2", "Simple", False, children_list[0])
    check_simple(debug, "[0,1]", "int", "3", "Simple", False, children_list[1])
    check_simple(debug, "[0,2]", "int", "4", "Simple", False, children_list[2])
    check_simple(debug, "[1,0]", "int", "5", "Simple", False, children_list[3])
    check_simple(debug, "[1,1]", "int", "6", "Simple", False, children_list[4])
    check_simple(debug, "[1,2]", "int", "7", "Simple", False, children_list[5])

    var = check_simple(debug, "main::A3d", "int [2][2][2]", "",
                       "Array ( 0 ..  1,  0 ..  1,  0 ..  1)")
    children_list = var.children()
    gps_assert(len(children_list), 8, mode +
               " Invalid count of main::A3d children")
    check_simple(debug, "[0,0,0]", "int",
                 "1", "Simple", False, children_list[0])
    check_simple(debug, "[0,0,1]", "int",
                 "2", "Simple", False, children_list[1])
    check_simple(debug, "[0,1,0]", "int",
                 "1", "Simple", False, children_list[2])
    check_simple(debug, "[0,1,1]", "int",
                 "2", "Simple", False, children_list[3])
    check_simple(debug, "[1,0,0]", "int",
                 "1", "Simple", False, children_list[4])
    check_simple(debug, "[1,0,1]", "int",
                 "2", "Simple", False, children_list[5])
    check_simple(debug, "[1,1,0]", "int",
                 "1", "Simple", False, children_list[6])
    check_simple(debug, "[1,1,1]", "int",
                 "2", "Simple", False, children_list[7])

    check_simple(debug, "main::Iaa", "int **", r"^0x[0-9a-f]+", "Access", True)

    var = check_simple(debug, "main::V", "struct My_Record ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 5, mode +
               " Invalid count of main::V children")
    check_simple(debug, ".i", "int i", "33", "Simple", False, children_list[0])
    check_simple(debug, ".field1", "struct My_Record",
                 r"^0x[0-9a-f]+", "Access", True, children_list[1])
    check_simple(debug, ".field2", "struct My_Record",
                 r'^0x[0-9a-f]+ "ab"', "Access", True, children_list[2])
    check_simple(debug, ".field3", "double field3",
                 "1", "Simple", False, children_list[3])
    check_simple(debug, ".field4", "struct My_Record",
                 "", "Array ( 0 ..  1,  0 ..  1)", False, children_list[4])
    childs_list1 = children_list[4].children()
    gps_assert(len(childs_list1), 4, mode +
               " Invalid count of main::V.field4 children")
    check_simple(debug, "[0,0]", "int", "1", "Simple", False, childs_list1[0])
    check_simple(debug, "[0,1]", "int", "2", "Simple", False, childs_list1[1])
    check_simple(debug, "[1,0]", "int", "3", "Simple", False, childs_list1[2])
    check_simple(debug, "[1,1]", "int", "4", "Simple", False, childs_list1[3])

    var = check_simple(debug, "main::Anonymous_Var", "struct ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 3, mode +
               " Invalid count of main::Anonymous_Var children")
    check_simple(debug, ".a", "int a", "1", "Simple", False, children_list[0])
    check_simple(debug, ".b", "int b", "2", "Simple", False, children_list[1])
    check_simple(debug, ".c", "struct {...}", "",
                 "Array ( 0 ..  1)", False, children_list[2])
    childs_list1 = children_list[2].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of main::Anonymous_Var.c children")
    check_simple(debug, "[0]", "int", "3", "Simple", False, childs_list1[0])
    check_simple(debug, "[1]", "int", "4", "Simple", False, childs_list1[1])

    var = check_simple(debug, "main::V2", "struct My_Record ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 5, mode +
               " Invalid count of main::V2 children")
    check_simple(debug, ".i", "int i", "33", "Simple", False, children_list[0])
    check_simple(debug, ".field1", "My_Record_Typedef",
                 r'^0x[0-9a-f]+', "Access", True, children_list[1])
    check_simple(debug, ".field2", "My_Record_Typedef",
                 r'^0x[0-9a-f]+ "ab"', "Access", True, children_list[2])
    check_simple(debug, ".field3", "double field3",
                 "1", "Simple", False, children_list[3])
    check_simple(debug, ".field4", "My_Record_Typedef",
                 "", "Array ( 0 ..  1,  0 ..  1)", False, children_list[4])
    childs_list1 = children_list[4].children()
    gps_assert(len(childs_list1), 4, mode +
               " Invalid count of main::V2.field4 children")
    check_simple(debug, "[0,0]", "int", "1", "Simple", False, childs_list1[0])
    check_simple(debug, "[0,1]", "int", "2", "Simple", False, childs_list1[1])
    check_simple(debug, "[1,0]", "int", "3", "Simple", False, childs_list1[2])
    check_simple(debug, "[1,1]", "int", "4", "Simple", False, childs_list1[3])

    var = check_simple(debug, "main::Mror",
                       "struct My_Record_Of_Record ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of main::Mror children")
    check_simple(debug, ".c", "struct Field1_Record ", "",
                 "Record", False, children_list[0])
    check_simple(debug, ".d", "int d", "2", "Simple", False, children_list[1])
    childs_list1 = children_list[0].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of main::Mror.c children")
    check_simple(debug, ".a", "int a", "1", "Simple", False, childs_list1[0])
    check_simple(debug, ".b", "struct Field1_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list1[1])

    var = check_simple(debug, "main::Mror2",
                       "struct My_Record_Of_Record2 ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 4, mode +
               " Invalid count of main::Mror2 children")
    check_simple(debug, ".c", "struct ", "", "Record", False, children_list[0])
    check_simple(debug, ".d", "int d", "2", "Simple", False, children_list[1])
    check_simple(debug, ".e", "int e", "3", "Simple", False, children_list[2])
    check_simple(debug, ".f", "int f", "4", "Simple", False, children_list[3])
    childs_list1 = children_list[0].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of main::Mror2.c children")
    check_simple(debug, ".a", "int a", "1", "Simple", False, childs_list1[0])
    check_simple(debug, ".b", "struct {...}",
                 r'^0x[0-9a-f]+', "Access", True, childs_list1[1])

    var = check_simple(debug, "main::mrou",
                       "struct My_Record_Of_Unions ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode +
               " Invalid count of main::Mror2 children")
    check_simple(debug, ".c", "union ", "", "Union", False, children_list[0])
    childs_list1 = children_list[0].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of main::mrou.c children")
    check_simple(debug, ".a", "int a", "1", "Simple", False, childs_list1[0])
    check_simple(debug, ".b", "float b",
                 "^1.4", "Simple", True, childs_list1[1])

    var = check_simple(debug, "main::mroe",
                       "struct My_Record_Of_Enum ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode +
               " Invalid count of main::mroe children")
    check_simple(debug, ".field", "enum colors {Blue, Red, Green}",
                 "Blue", "Enumeration", False, children_list[0])

    var = check_simple(debug, "main::Mrora", "struct My_Record_Of_Record [2]",
                       "", "Array ( 0 ..  1)")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of main::Mrora children")
    check_simple(debug, "[0]", "struct My_Record_Of_Record ", "",
                 "Record", False, children_list[0])
    childs_list1 = children_list[0].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of Mrora[0] children")
    check_simple(debug, ".c", "struct Field1_Record ", "",
                 "Record", False, childs_list1[0])
    check_simple(debug, ".d", "int d", "4", "Simple", False, childs_list1[1])
    childs_list2 = childs_list1[0].children()
    gps_assert(len(childs_list2), 2, mode +
               " Invalid count of Mrora[0].c children")
    check_simple(debug, ".a", "int a", "3", "Simple", False, childs_list2[0])
    check_simple(debug, ".b", "struct Field1_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list2[1])
    check_simple(debug, "[1]", "struct My_Record_Of_Record ", "",
                 "Record", False, children_list[1])
    childs_list1 = children_list[1].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of Mrora[1] children")
    check_simple(debug, ".c", "struct Field1_Record ", "",
                 "Record", False, childs_list1[0])
    check_simple(debug, ".d", "int d", "2", "Simple", False, childs_list1[1])
    childs_list2 = childs_list1[0].children()
    gps_assert(len(childs_list2), 2, mode +
               " Invalid count of Mrora[0].c children")
    check_simple(debug, ".a", "int a", "1", "Simple", False, childs_list2[0])
    check_simple(debug, ".b", "struct Field1_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list2[1])

    var = check_simple(debug, "main::Mrorpa",
                       "struct My_Record_Of_Record *[2]", "",
                       "Array ( 0 ..  1)")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of Mrorpa children")
    check_simple(debug, "[0]", "struct My_Record_Of_Record *",
                 r'^0x[0-9a-f]+', "Access",  True, children_list[0])
    check_simple(debug, "[1]", "struct My_Record_Of_Record *",
                 r'^0x[0-9a-f]+', "Access", True, children_list[1])

    var = check_simple(debug, "main::Uni", "union My_Union ", "", "Union")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Uni children")
    check_simple(debug, ".a", "int a", "1", "Simple", False, children_list[0])
    check_simple(debug, ".b", "float b",
                 r"^1.4", "Simple", True, children_list[1])

    var = check_simple(debug, "main::Uni2", "union My_Union2 ", "", "Union")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Uni2 children")
    check_simple(debug, ".a", "struct My_Record ", "",
                 "Record", False, children_list[0])
    childs_list1 = children_list[0].children()
    gps_assert(len(childs_list1), 5, mode +
               " Invalid count of Uni2.a children")
    check_simple(debug, ".i", "int i", "33", "Simple", False, childs_list1[0])
    check_simple(debug, ".field1", "struct My_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list1[1])
    check_simple(debug, ".field2", "struct My_Record",
                 r'^0x[0-9a-f]+ "ab"', "Access", True, childs_list1[2])
    check_simple(debug, ".field3", "double field3",
                 "1", "Simple", False, childs_list1[3])
    check_simple(debug, ".field4", "struct My_Record",
                 "", "Array ( 0 ..  1,  0 ..  1)", False, childs_list1[4])
    childs_list2 = childs_list1[4].children()
    gps_assert(len(childs_list2), 4, mode +
               " Invalid count of Uni2.a.field4 children")
    check_simple(debug, "[0,0]", "int", "1", "Simple", False, childs_list2[0])
    check_simple(debug, "[0,1]", "int", "2", "Simple", False, childs_list2[1])
    check_simple(debug, "[1,0]", "int", "3", "Simple", False, childs_list2[2])
    check_simple(debug, "[1,1]", "int", "4", "Simple", False, childs_list2[3])
    check_simple(debug, ".b", "int b", "33", "Simple", False, children_list[1])

    var = check_simple(debug, "main::Uni3", "union My_Union2 ", "", "Union")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Uni3 children")
    check_simple(debug, ".a", "struct My_Record ", "",
                 "Record", False, children_list[0])
    childs_list1 = children_list[0].children()
    gps_assert(len(childs_list1), 5, mode +
               " Invalid count of Uni2.a children")
    check_simple(debug, ".i", "int i", "2", "Simple", False, childs_list1[0])
    check_simple(debug, ".field1", "struct My_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list1[1])
    check_simple(debug, ".field2", "struct My_Record",
                 r'^0x[0-9a-f]+', "Access", True, childs_list1[2])
    check_simple(debug, ".field3", "double field3",
                 "0", "Simple", False, childs_list1[3])
    check_simple(debug, ".field4", "struct My_Record",
                 "", "Array ( 0 ..  1,  0 ..  1)", False, childs_list1[4])
    childs_list2 = childs_list1[4].children()
    gps_assert(len(childs_list2), 4, mode +
               " Invalid count of Uni3.a.field4 children")
    check_simple(debug, "[0,0]", "int", "0", "Simple", False, childs_list2[0])
    check_simple(debug, "[0,1]", "int", "0", "Simple", False, childs_list2[1])
    check_simple(debug, "[1,0]", "int", "0", "Simple", False, childs_list2[2])
    check_simple(debug, "[1,1]", "int", "0", "Simple", False, childs_list2[3])
    check_simple(debug, ".b", "int b", "2", "Simple", False, children_list[1])

    var = check_simple(debug, "main::Mrwu", "struct My_Record_With_Union ",
                       "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Mrwu children")
    check_simple(debug, ".field1", "union My_Union ", "",
                 "Union", False, children_list[0])
    childs_list1 = children_list[0].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of Mrwu.field1 children")
    check_simple(debug, ".a", "int a", "1", "Simple", False, childs_list1[0])
    check_simple(debug, ".b", "float b", r'^1.4',
                 "Simple", True, childs_list1[1])
    check_simple(debug, ".field2", "float field2",
                 r"^3.4", "Simple", True, children_list[1])

    check_simple(debug, "main::as", "void (*)()",
                 r'^0x[0-9a-f]+ \<foo\>', "Access", True)

    var = check_simple(debug, "main::asa", "void (*[2])()",
                       "", "Array ( 0 ..  1)")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of asa children")
    check_simple(debug, "[0]", "void (*)()",
                 r'^0x[0-9a-f]+ \<foo\>', "Access", True, children_list[0])
    check_simple(debug, "[1]", "void (*)()",
                 r'^0x[0-9a-f]+ \<foo\>', "Access", True, children_list[1])

    var = check_simple(debug, "main::Mrws",
                       "struct My_Record_With_Subprogram ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of Mrws children")
    check_simple(debug, ".field1", "struct My_Record_With_Subprogram",
                 r'^0x[0-9a-f]+ \<foo\>', "Access", True, children_list[0])
    check_simple(debug, ".field2", "int field2", "1",
                 "Simple", False, children_list[1])

    var = check_simple(debug, "main::Mrws2",
                       "struct My_Record_With_Subprogram2 ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of Mrws2 children")
    check_simple(debug, ".field1", "struct My_Record_With_Subprogram2",
                 "", "Array ( 0 ..  1)", False, children_list[0])
    childs_list1 = children_list[0].children()
    gps_assert(len(childs_list1), 2, mode +
               " Invalid count of Mrws2.field1 children")
    check_simple(debug, "[0]", "void (*", r'^0x[0-9a-f]+ \<foo\>',
                 "Access", True, childs_list1[0])
    check_simple(debug, "[1]", "void (*", r'^0x[0-9a-f]+ \<foo\>',
                 "Access", True, childs_list1[1])
    check_simple(debug, ".field2", "int field2", "1",
                 "Simple", False, children_list[1])

    var = check_simple(debug, "main::tv", "struct timeval ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode + " Invalid count of tv children")
    check_simple(debug, ".tv_sec", "long", "1", "Simple",
                 False, children_list[0])
    check_simple(debug, ".tv_usec", "long", "100000", "Simple",
                 False, children_list[1])

    var = check_simple(debug, "main::list",
                       "struct tree_common ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode + " Invalid count of list children")
    check_simple(debug, ".chain", "struct tree_common", r'^0x[0-9a-f]+',
                 "Access", True, children_list[0])

    var = check_simple(debug, "main::test_volatile",
                       "struct _test_volatile ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 1, mode +
               " Invalid count of test_volatile children")
    check_simple(debug, ".u", "union ", "", "Union", False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list1), 2, mode +
               " Invalid count of test_volatile.u children")
    check_simple(debug, ".x", "volatile struct ", "", "Record",
                 False, children_list1[0])
    children_list2 = children_list1[0].children()
    gps_assert(len(children_list), 1, mode +
               " Invalid count of test_volatile.u.x children")
    check_simple(debug, ".xx", "int xx", "12", "Simple",
                 False, children_list2[0])
    check_simple(debug, ".y", "int y", "12", "Simple",
                 False, children_list1[1])

    var = check_simple(debug, "(*main::Mrora)",
                       "struct My_Record_Of_Record ", "", "Record")
    children_list = var.children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of (*main::Mrora) children")
    check_simple(debug, ".c", "struct Field1_Record ", "", "Record",
                 False, children_list[0])
    children_list1 = children_list[0].children()
    gps_assert(len(children_list), 2, mode +
               " Invalid count of (*main::Mrora).c children")
    check_simple(debug, ".a", "int a", "3", "Simple",
                 False, children_list1[0])
    check_simple(debug, ".b", "struct Field1_Record", r'^0x[0-9a-f]+',
                 "Access", True, children_list1[1])
    check_simple(debug, ".d", "int d", "4", "Simple",
                 False, children_list[1])

    if "Darwin" in platform.system():
        debug.send("c")
        yield wait_until_not_busy(debug)

    debug.send("q")
    yield wait_idle()
