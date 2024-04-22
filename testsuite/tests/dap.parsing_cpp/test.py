import platform
import GPS
from gs_utils.internal.utils import *
import re
from workflows import run_as_workflow
from workflows import promises


def check_variable(var, name, type, value, pattern=False):

    gps_assert(var.type_name, type,
               "Invalid type of " + name +
               " " + var.type_name)
    if (pattern):
        val = var.simple_value
        gps_assert(re.search(value, val) is None, False,
                   "Invalid value of " + name + " " + val)
    else:
        gps_assert(var.simple_value, value,
                   "Invalid value of " + name +
                   " " + var.simple_value)

@run_as_workflow
def get_children(var):
    promise = promises.DebuggerVariableWrapper(var)
    children = yield promise.children()
    yield children

@run_as_workflow
def check(promise, name, type, value, pattern=False):

    yield timeout(5)
    var = yield promise.get_variable_by_name(name)
    yield timeout(5)

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

def check_simple(debug, name, type, value, description,
                 pattern=False, var=None):
    return var

@run_test_driver
def test_driver():
    # Wait for the DAP server to give us the sources of
    # the debugged executable.
    yield wait_DAP_server('loadedSources')

    p = promises.DebuggerWrapper(GPS.File("parse_cpp"))
    debug = GPS.Debugger.get()

    debug.break_at_location(GPS.File("parse_cpp.cc"), 10)
    yield wait_DAP_server("setBreakpoints")
    debug.send("run")
    yield wait_DAP_server('stackTrace')
    yield wait_until_not_busy(debug)

    debug.frame_up()
    yield wait_until_not_busy(debug)

    info = yield p.get_variable_by_name("Non_Existant_Variable")
    gps_assert(info.data is None,
               True, "Non_Existant_Variable")

    yield check(p, "A", "int", "1")
    yield check(p, "B", "float", "2")
    yield check(p, "C", "char", "65 'A'")
    yield check(p, "Sh", "short", "65")
    yield check(p, "L", "long", "32")
    yield check(p, "Uns", "unsigned int", "33")
    yield check(p, "UL", "unsigned long", "33")
    yield check(p, "S", "char *", '^0x[0-9a-f]+ "abcd"', True)
    yield check(p, "S2", "char *", r'^0x[0-9a-f]+ "ab\\nc"', True)
    yield check(p, "S3", "char *", r'^0x[0-9a-f]+ "ab\[\\"c"', True)
    yield check(p, "Act", "int *", '^0x[0-9a-f]+', True)
    yield check(p, "My_Enum_Variable", "enum {...}", "Blue")
    yield check(p, "T", "int", "<optimized out>")
    yield check(p, "Ea", "int [0]", "")

    var = yield check(p, "Aoa", "int *[3]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Aoa children")
    yield check_variable(children.data[0], "Aoa.[0]", "int *", r"0x[0-9a-f]+", True)
    yield check_variable(children.data[1], "Aoa.[1]", "int *", r"0x[0-9a-f]+", True)
    yield check_variable(children.data[2], "Aoa.[2]", "int *", r"0x[0-9a-f]+", True)

    var = yield check(p, "U", "int [2][3]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of U children")
    yield check_variable(children.data[0], "U.[0]", "int [3]", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of U.[0] children")
    yield check_variable(children1.data[0], "U.[0,0]", "int", "2")
    yield check_variable(children1.data[1], "U.[0,1]", "int", "3")
    yield check_variable(children1.data[2], "U.[0,2]", "int", "4")
    yield check_variable(children.data[1], "U.[1]", "int [3]", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 3, "Invalid count of U.[1] children")
    yield check_variable(children1.data[0], "U.[1,0]", "int", "5")
    yield check_variable(children1.data[1], "U.[1,1]", "int", "6")
    yield check_variable(children1.data[2], "U.[1,2]", "int", "7")

    var = yield check(p, "A3d", "int [2][2][2]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of A3d children")
    yield check_variable(children.data[0], "A3d.[0]", "int [2][2]", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of A3d.[0] children")
    yield check_variable(children1.data[0], "A3d.[0,0]", "int [2]", "")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of A3d.[0,0] children")
    yield check_variable(children2.data[0], "A3d.[0,0,0]", "int", "1")
    yield check_variable(children2.data[1], "A3d.[0,0,1]", "int", "2")
    yield check_variable(children.data[1], "A3d.[1]", "int [2][2]", "")

    yield check(p, "Iaa", "int **", "^0x[0-9a-f]+", True)

    var = yield check(p, "V", "My_Record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of V children")
    yield check_variable(children.data[0], "V.i", "int", "33")
    yield check_variable(children.data[1], "V.field1", "int *", r"^0x[0-9a-f]+", True)
    yield check_variable(children.data[2], "V.field2", "char *", r'^0x[0-9a-f]+ "ab"', True)

    var = yield check(p, "Anonymous_Var", "struct {...}", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Anonymous_Var children")
    yield check_variable(children.data[0], "Anonymous_Var.a", "int", "1")
    yield check_variable(children.data[1], "Anonymous_Var.b", "int", "2")

    var = yield check(p, "V2", "My_Record_Typedef", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of V2 children")
    yield check_variable(children.data[0], "V2.i", "int", "33")
    yield check_variable(children.data[1], "V2.field1", "int *", r'^0x[0-9a-f]+', True)
    yield check_variable(children.data[2], "V2.field2", "char *", r'^0x[0-9a-f]+ "ab"', True)

    var = yield check(p, "cl1", "CL", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 0, "Invalid count of cl1 children")

    var = yield check(p, "cl2", "CL2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of cl2 children")
    yield check_variable(children.data[0], "cl2.<CL2>", "int (**)(void)", r'^0x[0-9a-f]+', True)
    yield check_variable(children.data[1], "cl2.x", "int", "10")

    var = yield check(p, "cl3", "CL3", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of cl3 children")
    yield check_variable(children.data[0], "cl3.<CL2>", "CL2", "")
    yield check_variable(children.data[1], "cl3.y", "int", "11")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of cl3.<CL2> children")
    yield check_variable(children.data[0], "cl3.<CL2>", "CL2", "")
    yield check_variable(children1.data[1], "cl3.x", "int", "10")

    var = yield check(p, "cl4", "CL4", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of cl4 children")
    yield check_variable(children.data[0], "cl4.<CL2>", "CL2", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of cl4.<CL2> children")
    yield check_variable(children1.data[1], "cl4.x", "int", "10")

    var = yield check(p, "Mror", "My_Record_Of_Record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Mror children")
    yield check_variable(children.data[0], "Mror.c", "My_Record_Of_Record::Field1_Record", "")
    yield check_variable(children.data[1], "Mror.d", "int", "2")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Mror.c children")
    yield check_variable(children1.data[0], "Mror.c.a", "int", "1")
    yield check_variable(children1.data[1], "Mror.c.b", "int *", r'^0x[0-9a-f]+', True)

    var = yield check(p, "Mrora", "My_Record_Of_Record [2]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Mrora children")
    yield check_variable(children.data[0], "Mrora[0]", "My_Record_Of_Record", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Mrora[0] children")
    yield check_variable(children1.data[0], "Mrora[0].c", "My_Record_Of_Record::Field1_Record", "")
    yield check_variable(children1.data[1], "Mrora[0].d", "int", "4")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of Mrora[0].c children")
    yield check_variable(children2.data[0], "Mrora[0].c.a", "int", "3")
    yield check_variable(children2.data[1], "Mrora[0].c.B", "int *", r'^0x[0-9a-f]+', True)
    yield check_variable(children.data[1], "Mrora[1]", "My_Record_Of_Record", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 2, "Invalid count of Mrora[1] children")
    yield check_variable(children1.data[0], "Mrora[1].c", "My_Record_Of_Record::Field1_Record", "")
    yield check_variable(children1.data[1], "Mrora[1].d", "int", "2")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of Mrora[1].c children")
    yield check_variable(children2.data[0], "Mrora[1].c.a", "int", "1")
    yield check_variable(children2.data[1], "Mrora[1].c.B", "int *", r'^0x[0-9a-f]+', True)

    var = yield check(p, "Mrorpa", "My_Record_Of_Record *[2]", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Mrorpa children")
    yield check_variable(children.data[0], "Mrorpa[0]", "My_Record_Of_Record *", r'^0x[0-9a-f]+', True)
    yield check_variable(children.data[1], "Mrorpa[1]", "My_Record_Of_Record *", r'^0x[0-9a-f]+', True)

    var = yield check(p, "Uni", "My_Union", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Uni children")
    yield check_variable(children.data[0], "Uni.a", "int", "1")
    yield check_variable(children.data[1], "Uni.b", "float", r'^1.4[0-9,-]+', True)

    var = yield check(p, "Uni2", "My_Union2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Uni2 children")
    yield check_variable(children.data[0], "Uni2.a", "My_Record", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of Uni2.a children")
    yield check_variable(children1.data[0], "Uni2.a.i", "int", "33")
    yield check_variable(children1.data[1], "Uni2.a.field1", "int *", r'^0x[0-9a-f]+', True)
    yield check_variable(children1.data[2], "Uni2.a.field2", "char *", r'^0x[0-9a-f]+', True)

    var = yield check(p, "Uni3", "My_Union2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Uni3 children")
    yield check_variable(children.data[0], "Uni3.a", "My_Record", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of Uni3.a children")
    yield check_variable(children1.data[0], "Uni3.a.i", "int", "2")
    yield check_variable(children1.data[1], "Uni3.a.field1", "int *", r'^0x[0-9a-f]+', True)
    yield check_variable(children1.data[2], "Uni3.a.field2", "char *", r'^0x[0-9a-f]+', True)

    var = yield check(p, "Mrwu", "My_Record_With_Union", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Mrwu children")
    yield check_variable(children.data[0], "Uni3.a", "My_Union", "")
    yield check_variable(children.data[1], "Uni3.field1", "float", r'^3.4[0-9,-]+', True)
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Mrwu.a children")
    yield check_variable(children1.data[0], "Uni3.a.a", "int", "1")
    yield check_variable(children1.data[1], "Uni3.a.b", "float", r'^1.4[0-9,-]+', True)

    yield check(p, "as", "void (*)(void)", r'^0x[0-9a-f]+ \<foo\(\)\>', True)

    var = yield check(p, "asa", "void (*[2])(void)", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of asa children")
    yield check_variable(children.data[0], "asa[0]", "void (*)(void)", r'^0x[0-9a-f]+ \<foo\(\)\>', True)
    yield check_variable(children.data[1], "asa[1]", "void (*)(void)", r'^0x[0-9a-f]+ \<foo\(\)\>', True)

    var = yield check(p, "Mrws", "My_Record_With_Subprogram", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Mrws children")
    yield check_variable(children.data[0], "Mrws.field1", "void (*)(void)", r'^0x[0-9a-f]+ \<foo\(\)\>', True)
    yield check_variable(children.data[1], "Mrws.field2", "int", "1")

    var = yield check(p, "Mrws2", "My_Record_With_Subprogram2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Mrws2 children")
    yield check_variable(children.data[0], "Mrws2.field1", "void (*[2])(int)", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Mrws2.field1 children")
    yield check_variable(children1.data[0], "Mrws2.field1[0]", "void (*)(int)", r'^0x[0-9a-f]+ \<bar\(int\)\>', True)
    yield check_variable(children1.data[1], "Mrws2.field1[1]", "void (*)(int)", r'^0x[0-9a-f]+ \<bar\(int\)\>', True)
    yield check_variable(children.data[1], "Mrws2.field2", "int", "1")

    var = yield check(p, "tv", "timeval", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of tv children")
    yield check_variable(children.data[0], "tv.tv_sec", "__time_t", "1")
    yield check_variable(children.data[1], "tv.tv_usec", "__time_t", "100000")

    var = yield check(p, "list", "tree_common", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of list children")
    yield check_variable(children.data[0], "list.chain", "tree_node *", r'^0x[0-9a-f]+', True)

    var = yield check(p, "FC", "First_Class", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of FC children")
    yield check_variable(children.data[0], "FC.public_var", "int", "1")
    yield check_variable(children.data[1], "FC.protected_var", "int", "2")
    yield check_variable(children.data[2], "FC.private_var", "int", "3")

    var = yield check(p, "SC", "Second_Class", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 4, "Invalid count of SC children")
    yield check_variable(children.data[0], "SC.<First_Class>", "First_Class", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of SC.<First_Class> children")
    yield check_variable(children1.data[0], "SC.public_var", "int", "1")
    yield check_variable(children1.data[1], "SC.protected_var", "int", "2")
    yield check_variable(children1.data[2], "SC.private_var", "int", "3")
    yield check_variable(children.data[1], "SC.second_public_var", "int", "4")
    yield check_variable(children.data[2], "SC.second_protected_var", "int", "5")
    yield check_variable(children.data[3], "SC.second_private_var", "int", "6")

    var = yield check(p, "SAC", "Struct_As_Class", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of SAC children")
    yield check_variable(children.data[0], "SAC.struct_public_var", "int", "10")
    yield check_variable(children.data[1], "SAC.struct_private_var", "int", "11")

    var = yield check(p, "MI", "Multiple_Inheritance", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 5, "Invalid count of MI children")
    yield check_variable(children.data[0], "MI.<Second_Class>", "Second_Class", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 4, "Invalid count of MI.<Second_Class> children")
    yield check_variable(children1.data[0], "MI.<First_Class>", "First_Class", "")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 3, "Invalid count of MI.Second_Class.First_Class children")
    yield check_variable(children2.data[0], "MI.Second_Class.First_Class.public_var", "int", "1")
    yield check_variable(children2.data[1], "MI.Second_Class.First_Class.protected_var", "int", "2")
    yield check_variable(children2.data[2], "MI.Second_Class.First_Class.private_var", "int", "3")
    yield check_variable(children1.data[1], "MI.Second_Class.second_public_var", "int", "4")
    yield check_variable(children1.data[2], "MI.Second_Class.second_protected_var", "int", "5")
    yield check_variable(children1.data[3], "MI.Second_Class.second_private_var", "int", "6")
    yield check_variable(children.data[1], "MI.<Struct_As_Class>", "Struct_As_Class", "")
    yield check_variable(children.data[2], "MI.third_public_var", "int", "7")
    yield check_variable(children.data[3], "MI.third_protected_var", "int", "8")
    yield check_variable(children.data[4], "MI.third_protected_var", "int", "9")

    debug.send("q")
    yield wait_idle()
