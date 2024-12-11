import platform
import GPS
from gs_utils.internal.utils import *
import re
from workflows import run_as_workflow
from workflows import promises


def check_variable(var, name, type, value, pattern=False):
    gps_assert(var.type_name, type, "Invalid type of " + name + " " + var.type_name)
    if pattern:
        val = var.simple_value
        gps_assert(
            re.search(value, val) is None, False, "Invalid value of " + name + " " + val
        )
    else:
        gps_assert(
            var.simple_value, value, "Invalid value of " + name + " " + var.simple_value
        )


@run_as_workflow
def get_children(var):
    if var is not None:
        promise = promises.DebuggerVariableWrapper(var)
        children = yield promise.children()
        if children.is_error:
            simple_error("get_children error:" + children.error_message)
        elif children.is_reject:
            simple_error("get_children rejected")

        yield children


@run_as_workflow
def check(promise, name, type, value, pattern=False):
    yield timeout(10)
    var = yield promise.get_variable_by_name(name)
    yield timeout(10)

    if var.is_valid:
        if var.data is None:
            simple_error(name + " is None")
        else:
            check_variable(var.data, name, type, value, pattern)

    elif var.is_error:
        simple_error(name + " error:" + var.error_message)

    else:
        simple_error(name + " rejected")

    yield var


@run_test_driver
def test_driver():
    # Wait the debugger console to be sure that debugger is started.
    yield wait_tasks()
    yield wait_for_mdi_child("Debugger Console parse_c" + dot_exe)
    yield wait_idle()

    p = promises.DebuggerWrapper(GPS.File("parse_c"))
    debug = p.get()
    yield wait_idle()
    yield wait_until_not_busy(debug)
    yield wait_idle()

    debug.break_at_location(GPS.File("parse_c.c"), 3)
    yield wait_DAP_server("setBreakpoints")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    debug.send("run")
    yield wait_DAP_server("stackTrace")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    debug.frame_up()
    yield wait_until_not_busy(debug)
    yield wait_idle()

    info = yield p.get_variable_by_name("Non_Existant_Variable")
    gps_assert(info.data is None, True, "Non_Existant_Variable")

    yield check(p, "A", "int", "1")
    yield check(p, "B", "float", "2")
    yield check(p, "C", "char", "65 'A'")
    yield check(p, "Sh", "short", "65")
    yield check(p, "L", "long", "32")
    yield check(p, "Uns", "unsigned int", "33")
    yield check(p, "CC", "unsigned char", "66 'B'")
    yield check(p, "UL", "unsigned long", "33")
    yield check(p, "S", "char *", r'^0x[0-9a-f]+ "abcd"', True)
    yield check(p, "S2", "char *", r'^0x[0-9a-f]+ "ab\\nc"', True)
    yield check(p, "S3", "char *", r'^0x[0-9a-f]+ "ab\[\\"c"', True)
    yield check(p, "Act", "int *", r"^0x[0-9a-f]+", True)
    yield check(p, "My_Enum_Variable", "enum colors", "Blue")

    var = yield check(p, "T", "int", "<optimized out>")

    yield check(p, "Ea", "int [0]", "")

    var = yield check(p, "Aoa", "int *[3]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of main::Aoa children")
    yield check_variable(children.data[0], "Aoa.[0]", "int *", r"0x[0-9a-f]+", True)
    yield check_variable(children.data[1], "Aoa.[1]", "int *", r"0x[0-9a-f]+", True)
    yield check_variable(children.data[2], "Aoa.[2]", "int *", r"0x[0-9a-f]+", True)

    var = yield check(p, "U", "int [2][3]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::U children")
    # U.[0]
    yield check_variable(children.data[0], "U.[0]", "int [3]", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of U.[0] children")
    yield check_variable(children1.data[0], "U.[0,0]", "int", "2")
    yield check_variable(children1.data[1], "U.[0,1]", "int", "3")
    yield check_variable(children1.data[2], "U.[0,2]", "int", "4")
    # U.[1]
    yield check_variable(children.data[1], "U.[1]", "int [3]", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 3, "Invalid count of U.[1] children")
    yield check_variable(children1.data[0], "U.[1,0]", "int", "5")
    yield check_variable(children1.data[1], "U.[1,1]", "int", "6")
    yield check_variable(children1.data[2], "U.[1,2]", "int", "7")

    var = yield check(p, "A3d", "int [2][2][2]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::A3d children")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of A3d.[0] children")
    # A3d.[0,0]
    yield check_variable(children1.data[0], "A3d.[0,0]", "int [2]", "")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of A3d.[0,0] children")
    yield check_variable(children2.data[0], "A3d.[0,0,0]", "int", "1")
    yield check_variable(children2.data[1], "A3d.[0,0,1]", "int", "2")
    # A3d.[0,1]
    yield check_variable(children1.data[1], "A3d.[0,1]", "int [2]", "")
    children2 = yield get_children(children1.data[1])
    gps_assert(len(children2.data), 2, "Invalid count of A3d.[0,1] children")
    yield check_variable(children2.data[0], "A3d.[0,1,0]", "int", "1")
    yield check_variable(children2.data[1], "A3d.[0,1,1]", "int", "2")

    yield check(p, "Iaa", "int **", r"^0x[0-9a-f]+", True)

    var = yield check(p, "V", "struct My_Record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 5, "Invalid count of main::V children")
    yield check_variable(children.data[0], "V.i", "int", "33")
    yield check_variable(children.data[1], "V.field1", "int *", r"^0x[0-9a-f]+", True)
    yield check_variable(children.data[2], "V.field2", "char *", r"^0x[0-9a-f]+", True)
    yield check_variable(children.data[3], "V.field3", "double", "1")
    yield check_variable(children.data[4], "V.field4", "int [2][2]", "")
    children1 = yield get_children(children.data[4])
    gps_assert(len(children1.data), 2, "Invalid count of V.field4 children")
    yield check_variable(children1.data[0], "V.field4[0]", "int [2]", "")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of V.field4[0] children")
    yield check_variable(children2.data[0], "V.field4[0,0]", "int", "1")
    yield check_variable(children2.data[1], "V.field4[0,1]", "int", "2")
    yield check_variable(children1.data[1], "V.field4[1]", "int [2]", "")
    children2 = yield get_children(children1.data[1])
    gps_assert(len(children2.data), 2, "Invalid count of V.field4[1] children")
    yield check_variable(children2.data[0], "V.field4[1,0]", "int", "3")
    yield check_variable(children2.data[1], "V.field4[1,1]", "int", "4")

    var = yield check(p, "Anonymous_Var", "struct {...}", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of main::Anonymous_Var children")
    yield check_variable(children.data[0], "Anonymous_Var.a", "int", "1")
    yield check_variable(children.data[1], "Anonymous_Var.b", "int", "2")
    yield check_variable(children.data[2], "Anonymous_Var.c", "int [2]", "")
    children1 = yield get_children(children.data[2])
    gps_assert(len(children1.data), 2, "Invalid count of Anonymous_Var.c children")
    yield check_variable(children1.data[0], "Anonymous_Var.c[0]", "int", "3")
    yield check_variable(children1.data[1], "Anonymous_Var.c[1]", "int", "4")

    var = yield check(p, "V2", "My_Record_Typedef", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 5, "Invalid count of main::V2 children")
    yield check_variable(children.data[0], "V2.i", "int", "33")
    yield check_variable(children.data[1], "V2.field1", "int *", r"^0x[0-9a-f]+", True)
    yield check_variable(children.data[2], "V2.field2", "char *", r"^0x[0-9a-f]+", True)
    yield check_variable(children.data[3], "V2.field3", "double", "1")
    yield check_variable(children.data[4], "V2.field4", "int [2][2]", "")
    children1 = yield get_children(children.data[4])
    gps_assert(len(children1.data), 2, "Invalid count of V2.field4 children")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of V2.field4[0] children")
    yield check_variable(children2.data[0], "V.field4[0,0]", "int", "1")
    yield check_variable(children2.data[1], "V.field4[0,1]", "int", "2")
    children2 = yield get_children(children1.data[1])
    gps_assert(len(children2.data), 2, "Invalid count of V2.field4[1] children")
    yield check_variable(children2.data[0], "V.field4[1,0]", "int", "3")
    yield check_variable(children2.data[1], "V.field4[1,1]", "int", "4")

    var = yield check(p, "Mror", "struct My_Record_Of_Record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::Mror children")
    yield check_variable(children.data[0], "Mror.c", "struct Field1_Record", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Mror.c children")
    yield check_variable(children1.data[0], "Mror.c.a", "int", "1")
    yield check_variable(children1.data[1], "Mror.c.b", "int *", r"^0x[0-9a-f]+", True)
    yield check_variable(children.data[1], "Mror.d", "int", "2")

    var = yield check(p, "Mror2", "struct My_Record_Of_Record2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 4, "Invalid count of main::Mror2 children")
    yield check_variable(children.data[0], "Mror2.c", "struct {...}", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Mror2.c children")
    yield check_variable(children1.data[0], "Mror2.c.a", "int", "1")
    yield check_variable(children1.data[1], "Mror2.c.b", "int *", r"^0x[0-9a-f]+", True)
    yield check_variable(children.data[1], "Mror2.d", "int", "2")
    yield check_variable(children.data[2], "Mror2.e", "int", "3")
    yield check_variable(children.data[3], "Mror2.f", "int", "4")

    var = yield check(p, "mrou", "struct My_Record_Of_Unions", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of main::mrou children")
    yield check_variable(children.data[0], "mrou.c", "union {...}", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of mrou.c children")
    yield check_variable(children1.data[0], "mrou.c.a", "int", "1")
    yield check_variable(children1.data[1], "mrou.c.b", "float", r"1.4[0-9,-]+", True)

    var = yield check(p, "mroe", "struct My_Record_Of_Enum", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of main::mroe children")
    yield check_variable(children.data[0], "mroe.field", "enum colors", "Blue")

    var = yield check(p, "Mrora", "struct My_Record_Of_Record [2]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::Mrora children")
    yield check_variable(children.data[0], "Mrora[0]", "struct My_Record_Of_Record", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Mrora[0] children")
    yield check_variable(children1.data[0], "Mrora[0].c", "struct Field1_Record", "")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of Mrora[0].c children")
    yield check_variable(children2.data[0], "Mrora[0].c.a", "int", "3")
    yield check_variable(
        children2.data[1], "Mrora[0].c.b", "int *", r"^0x[0-9a-f]+", True
    )
    yield check_variable(children1.data[1], "Mrora[0].d", "int", "4")
    yield check_variable(children.data[1], "Mrora[1]", "struct My_Record_Of_Record", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 2, "Invalid count of Mrora[1] children")
    yield check_variable(children1.data[0], "Mrora[1].c", "struct Field1_Record", "")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of Mrora[0].c children")
    yield check_variable(children2.data[0], "Mrora[0].c.a", "int", "1")
    yield check_variable(
        children2.data[1], "Mrora[0].c.b", "int *", r"^0x[0-9a-f]+", True
    )
    yield check_variable(children1.data[1], "Mrora[0].d", "int", "2")

    var = yield check(p, "Mrorpa", "struct My_Record_Of_Record *[2]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::Mrorpa children")
    yield check_variable(
        children.data[0],
        "Mrorpa[0]",
        "struct My_Record_Of_Record *",
        r"^0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children.data[1],
        "Mrorpa[1]",
        "struct My_Record_Of_Record *",
        r"^0x[0-9a-f]+",
        True,
    )

    var = yield check(p, "Uni", "union My_Union", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::Uni children")
    yield check_variable(children.data[0], "Uni.a", "int", "1")
    yield check_variable(children.data[1], "Uni.b", "float", r"1.4[0-9,-]+", True)

    var = yield check(p, "Uni2", "union My_Union2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::Uni2 children")
    yield check_variable(children.data[0], "Uni2.a", "struct My_Record", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 5, "Invalid count of Uni2.a children")
    yield check_variable(children1.data[0], "Uni2.a.i", "int", "33")
    yield check_variable(
        children1.data[1], "Uni2.a.field1", "int *", r"^0x[0-9a-f]+", True
    )
    yield check_variable(
        children1.data[2], "Uni2.a.field2", "char *", r'^0x[0-9a-f]+ "ab"', True
    )
    yield check_variable(children1.data[3], "Uni2.a.field3", "double", "1")
    yield check_variable(children1.data[4], "Uni2.a.field4", "int [2][2]", "")
    children2 = yield get_children(children1.data[4])
    gps_assert(len(children2.data), 2, "Invalid count of Uni2.a.field4 children")
    yield check_variable(children2.data[0], "Uni2.a.field4[0]", "int [2]", "")
    children3 = yield get_children(children2.data[0])
    gps_assert(len(children3.data), 2, "Invalid count of Uni2.a.field4[0] children")
    yield check_variable(children3.data[0], "Uni2.a.field4[0,0]", "int", "1")
    yield check_variable(children3.data[1], "Uni2.a.field4[0,1]", "int", "2")
    children3 = yield get_children(children2.data[1])
    gps_assert(len(children3.data), 2, "Invalid count of Uni2.a.field4[1] children")
    yield check_variable(children3.data[0], "Uni2.a.field4[1,0]", "int", "3")
    yield check_variable(children3.data[1], "Uni2.a.field4[1,1]", "int", "4")

    var = yield check(p, "Uni3", "union My_Union2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::Uni3 children")
    yield check_variable(children.data[0], "Uni3.a", "struct My_Record", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 5, "Invalid count of Uni3.a children")
    yield check_variable(children1.data[0], "Uni3.a.i", "int", "2")
    yield check_variable(
        children1.data[1], "Uni3.a.field1", "int *", r"^0x[0-9a-f]+", True
    )
    yield check_variable(
        children1.data[2], "Uni3.a.field2", "char *", r"^0x[0-9a-f]+", True
    )
    yield check_variable(children1.data[3], "Uni3.a.field3", "double", "0")
    yield check_variable(children1.data[4], "Uni3.a.field4", "int [2][2]", "")
    children2 = yield get_children(children1.data[4])
    gps_assert(len(children2.data), 2, "Invalid count of Uni3.a.field4 children")
    yield check_variable(children2.data[0], "Uni3.a.field4[0]", "int [2]", "")
    children3 = yield get_children(children2.data[0])
    gps_assert(len(children3.data), 2, "Invalid count of Uni3.a.field4[0] children")
    yield check_variable(children3.data[0], "Uni3.a.field4[0,0]", "int", "0")
    yield check_variable(children3.data[1], "Uni3.a.field4[0,1]", "int", "0")
    yield check_variable(children2.data[1], "Uni3.a.field4[1]", "int [2]", "")
    children3 = yield get_children(children2.data[1])
    gps_assert(len(children3.data), 2, "Invalid count of Uni3.a.field4[1] children")
    yield check_variable(children3.data[0], "Uni3.a.field4[1,0]", "int", "0")
    yield check_variable(children3.data[1], "Uni3.a.field4[1,1]", "int", "0")
    yield check_variable(children.data[1], "Uni3.b", "int", "2")

    var = yield check(p, "Mrwu", "struct My_Record_With_Union", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::Mrwu children")
    yield check_variable(children.data[0], "Mrwu.field1", "union My_Union", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Mrwu.field1 children")
    yield check_variable(children1.data[0], "Mrwu.field1.a", "int", "1")
    yield check_variable(
        children1.data[1], "Mrwu.field1.b", "float", r"1.4[0-9,-]+", True
    )
    yield check_variable(children.data[1], "Mrwu.field2", "float", r"3.4[0-9,-]+", True)

    yield check(p, "as", "void (*)()", r"^0x[0-9a-f]+ \<foo\>", True)

    var = yield check(p, "asa", "void (*[2])()", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::asa children")
    yield check_variable(
        children.data[0], "asa[0]", "void (*)()", r"^0x[0-9a-f]+ \<foo\>", True
    )
    yield check_variable(
        children.data[1], "asa[1]", "void (*)()", r"^0x[0-9a-f]+ \<foo\>", True
    )

    var = yield check(p, "Mrws", "struct My_Record_With_Subprogram", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::Mrws children")
    yield check_variable(
        children.data[0], "Mrws.field1", "void (*)()", r"^0x[0-9a-f]+ \<foo\>", True
    )
    yield check_variable(children.data[1], "Mrws.field2", "int", "1")

    var = yield check(p, "Mrws2", "struct My_Record_With_Subprogram2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of main::Mrws2 children")
    yield check_variable(children.data[0], "Mrws2.field1", "void (*[2])(int)", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Mrws2.field1 children")
    yield check_variable(
        children1.data[0],
        "Mrws2.field1[0]",
        "void (*)(int)",
        r"^0x[0-9a-f]+ \<foo\>",
        True,
    )
    yield check_variable(
        children1.data[1],
        "Mrws2.field1[1]",
        "void (*)(int)",
        r"^0x[0-9a-f]+ \<foo\>",
        True,
    )
    yield check_variable(children.data[1], "Mrws2.field2", "int", "1")

    var = yield check(p, "tv", "struct timeval", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of tv children")
    yield check_variable(children.data[0], "tv.tv_sec", "__time_t", "1")
    yield check_variable(children.data[1], "tv.tv_usec", "__time_t", "100000")

    var = yield check(p, "list", "struct tree_common", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of list children")
    yield check_variable(
        children.data[0], "list.chain", "union tree_node *", r"^0x[0-9a-f]+", True
    )

    var = yield check(p, "test_volatile", "struct _test_volatile", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of test_volatile children")
    yield check_variable(children.data[0], "test_volatile.u", "union {...}", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of test_volatile.u children")
    yield check_variable(
        children1.data[0], "test_volatile.u.x", "volatile struct {...}", ""
    )
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 1, "Invalid count of test_volatile.u.x children")
    yield check_variable(children2.data[0], "test_volatile.u.x.xx", "int", "12")

    GPS.execute_action("terminate all debuggers")
    yield wait_idle()
    GPS.exit(True)
