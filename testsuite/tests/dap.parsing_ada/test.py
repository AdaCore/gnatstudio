import platform
import GPS
from gs_utils.internal.utils import *
import re
from workflows import run_as_workflow
from workflows import promises


def check_variable(var, name, type, value, pattern=False, type_pattern=False):
    if type_pattern:
        t = var.type_name
        gps_assert(
            re.search(type, t) is None,
            False,
            "Invalid type of " + name + " is:" + t + " expected:" + type,
        )
    else:
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
def check(promise, name, type, value, pattern=False, type_pattern=False):
    yield timeout(10)
    yield wait_until_not_busy(promise.get())
    var = yield promise.get_variable_by_name(name)
    yield timeout(10)

    if var.is_valid:
        if var.data is None:
            simple_error(name + " is None")
        else:
            check_variable(var.data, name, type, value, pattern, type_pattern)

    elif var.is_error:
        simple_error(name + " error:" + var.error_message)

    else:
        simple_error(name + " rejected")

    yield var


@run_as_workflow
def get_children(var):
    if var is not None:
        yield timeout(20)
        promise = promises.DebuggerVariableWrapper(var)
        children = yield promise.children()
        yield children


@run_test_driver
def test_driver():
    # Wait the debugger console to be sure that debugger is started.
    yield wait_tasks()
    yield wait_for_mdi_child("Debugger Console parse" + dot_exe)
    yield wait_idle()

    p = promises.DebuggerWrapper(GPS.File("parse"))
    debug = p.get()
    yield wait_until_not_busy(debug)
    yield wait_idle()

    debug.break_at_exception(False)
    yield wait_DAP_server("setExceptionBreakpoints")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    debug.send("cont")
    yield wait_DAP_server("stackTrace")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    debug.send("frame 7")
    yield wait_until_true(lambda: debug.current_line != 460)
    yield wait_until_not_busy(debug)
    yield wait_idle()

    info = yield p.get_variable_by_name("Non_Existant_Variable")
    gps_assert(info.data is None, True, "Non_Existant_Variable exists")

    yield check(p, "A", "integer", "1")
    yield check(p, "B", "float", "2.0")
    yield check(p, "C", "character", "65 'A'")
    yield check(p, "Sh", "short_integer", "65")
    yield check(p, "Ssh", "short_short_integer", "65")
    yield check(p, "S", "array (1 .. 4) of character", '"abcd"')
    yield check(p, "S2", "array (1 .. 4) of character", r'"ab["0a"]c"')
    yield check(p, "S3", "array (1 .. 5) of character", r'"ab[""c"')
    yield check(p, "S4", "array (1 .. 7) of character", r'"ab[""c""]"')
    yield check(p, "S4", "array (1 .. 7) of character", r'"ab[""c""]"')
    yield check(p, "Dur", "duration", "0.5")
    yield check(p, "R", "parse.my_range", "5")
    yield check(p, "M", "parse.my_mod", "8")
    yield check(p, "Act", "parse.access_type", r"0x[0-9a-f]+", True)
    yield check(p, "My_Enum_Variable", "parse.my_enum", "blue")

    var = yield check(p, "T", "parse.integer_array", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 4, "Invalid count of T children")
    yield check_variable(children.data[0], "T.1", "integer", "2")
    yield check_variable(children.data[1], "T.2", "integer", "3")
    yield check_variable(children.data[2], "T.3", "integer", "4")
    yield check_variable(children.data[3], "T.4", "integer", "5")

    var = yield check(p, "Ea", "parse.empty_array", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 0, "Invalid count of Ea children")

    var = yield check(p, "Ea2", "array (0 .. -1) of integer", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 0, "Invalid count of Ea2 children")

    var = yield check(p, "Aoa", "parse.array_of_access", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Aoa children")
    yield check_variable(
        children.data[0],
        "Aoa.4",
        "parse.access_type|access integer",
        r"0x[0-9a-f]+",
        True,
        True,
    )
    yield check_variable(
        children.data[1],
        "Aoa.5",
        "parse.access_type|access integer",
        r"0x[0-9a-f]+",
        True,
        True,
    )
    yield check_variable(
        children.data[2],
        "Aoa.6",
        "parse.access_type|access integer",
        r"0x[0-9a-f]+",
        True,
        True,
    )

    var = yield check(p, "Fiia", "parse.first_index_integer_array", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Fiia children")
    yield check_variable(children.data[0], "Fiia.24", "integer", "3")
    yield check_variable(children.data[1], "Fiia.25", "integer", "4")
    yield check_variable(children.data[2], "Fiia.26", "integer", "5")

    yield check(p, "Iaa", "parse.integer_array_access", r"0x[0-9a-f]+", "Access")

    var = yield check(p, "U", "parse.integer_array2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of U children")
    yield check_variable(children.data[0], "U.1", "array (1 .. 3) of integer", "")
    yield check_variable(children.data[1], "U.2", "array (1 .. 3) of integer", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of U(1,1) children")
    yield check_variable(children1.data[0], "U.1.1", "integer", "2")
    yield check_variable(children1.data[1], "U.1.2", "integer", "3")
    yield check_variable(children1.data[2], "U.1.3", "integer", "4")
    children2 = yield get_children(children.data[1])
    gps_assert(len(children2.data), 3, "Invalid count of U(1,2) children")
    yield check_variable(children2.data[0], "U.2.1", "integer", "5")
    yield check_variable(children2.data[1], "U.2.2", "integer", "6")
    yield check_variable(children2.data[2], "U.2.3", "integer", "7")

    var = yield check(p, "Enum_Array_Variable", "parse.enum_array", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Enum_Array_Variable children")
    yield check_variable(
        children.data[0], "Enum_Array_Variable.0", "parse.my_enum", "red"
    )
    yield check_variable(
        children.data[1], "Enum_Array_Variable.1", "parse.my_enum", "my_green"
    )
    yield check_variable(
        children.data[2], "Enum_Array_Variable.1", "parse.my_enum", "blue"
    )

    var = yield check(p, "Erm", "parse.enum_range_matrix", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Erm children")
    yield check_variable(
        children.data[0], "Erm(0)", "array (blue .. red) of integer", ""
    )
    children1 = yield get_children(children.data[0])
    yield check_variable(children1.data[0], "Erm(0,0)", "integer", "0")
    yield check_variable(children1.data[1], "Erm(0,1)", "integer", "0")
    yield check_variable(
        children.data[1], "Erm(1)", "array (blue .. red) of integer", ""
    )
    children2 = yield get_children(children.data[1])
    gps_assert(len(children2.data), 2, "Erm(1)", "array (blue .. red) of integer")
    yield check_variable(children2.data[0], "Erm(1,0)", "integer", "0")
    yield check_variable(children2.data[1], "Erm(1,1)", "integer", "0")

    var = yield check(p, "Negative_Array_Variable", "parse.negative_array", '"ABCDE"')
    children = yield get_children(var.data)
    gps_assert(
        len(children.data), 0, "Invalid count of Negative_Array_Variable children"
    )

    var = yield check(p, "Aa", "parse.array_of_array", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Aa children")
    # Aa(1)
    yield check_variable(
        children.data[0], "Aa(1)", "parse.first_index_integer_array", ""
    )
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of Aa.(1) children")
    yield check_variable(children1.data[0], "Aa.(1,24)", "integer", "3")
    yield check_variable(children1.data[1], "Aa.(1,25)", "integer", "4")
    yield check_variable(children1.data[2], "Aa.(1,26)", "integer", "5")
    # Aa(2)
    yield check_variable(
        children.data[1], "Aa(2)", "parse.first_index_integer_array", ""
    )
    children2 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 3, "Invalid count of Aa.(2) children")
    yield check_variable(children2.data[0], "Aa.(2,24)", "integer", "6")
    yield check_variable(children2.data[1], "Aa.(2,25)", "integer", "7")
    yield check_variable(children2.data[2], "Aa.(2,26)", "integer", "8")

    var = yield check(p, "A3d", "parse.array_3d", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of A3d children")
    # A3d(1)
    yield check_variable(
        children.data[0], "A3d(1)", "array (1 .. 2, 6 .. 7) of integer", ""
    )
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of A3d(1) children")
    # A3d(1,1)
    yield check_variable(children1.data[0], "A3d(1,1)", "array (6 .. 7) of integer", "")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of A3d(1,1) children")
    yield check_variable(children2.data[0], "Aa.(1,1,1)", "integer", "1")
    yield check_variable(children2.data[1], "Aa.(1,1,2)", "integer", "2")
    # A3d(1,2)
    yield check_variable(children1.data[1], "A3d(1,2)", "array (6 .. 7) of integer", "")
    children2 = yield get_children(children1.data[1])
    gps_assert(len(children2.data), 2, "Invalid count of A3d(1,2) children")
    yield check_variable(children2.data[0], "Aa.(1,2,1)", "integer", "1")
    yield check_variable(children2.data[1], "Aa.(1,2,2)", "integer", "2")
    # A3d(2)
    yield check_variable(
        children.data[1], "A3d(2)", "array (1 .. 2, 6 .. 7) of integer", ""
    )
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 2, "Invalid count of A3d(2) children")
    # A3d(2,1)
    yield check_variable(children1.data[0], "A3d(2,1)", "array (6 .. 7) of integer", "")
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of A3d(1,1) children")
    yield check_variable(children2.data[0], "Aa.(2,1,1)", "integer", "1")
    yield check_variable(children2.data[1], "Aa.(2,1,2)", "integer", "2")
    # A3d(2,2)
    yield check_variable(children1.data[1], "A3d(2,2)", "array (6 .. 7) of integer", "")
    children2 = yield get_children(children1.data[1])
    gps_assert(len(children2.data), 2, "Invalid count of A3d(1,2) children")
    yield check_variable(children2.data[0], "Aa.(2,2,1)", "integer", "1")
    yield check_variable(children2.data[1], "Aa.(2,2,2)", "integer", "2")

    var = yield check(p, "Aos", "parse.array_of_string", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Aos children")
    yield check_variable(children.data[0], "Aos(1)", "parse.string_access", '"ab"')
    yield check_variable(children.data[1], "Aos(2)", "parse.string_access", '"cd"')

    yield check(p, "Nr", "parse.null_record", "")

    var = yield check(p, "V", "parse.my_record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of V children")
    yield check_variable(
        children.data[0], "V.field1", "parse.access_type", r"^0x[0-9a-f]+", True
    )
    yield check_variable(
        children.data[1], "V.field2", "array (1 .. 2) of character", '"ab"'
    )

    yield check(p, "Mra", "parse.my_record_access", r"^0x[0-9a-f]+", True)

    var = yield check(p, "W", "array (0 .. 1) of parse.my_record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of W children")
    # W(0)
    yield check_variable(children.data[0], "W(0)", "parse.my_record", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of W.(0) children")
    yield check_variable(
        children1.data[0], "W(0).field1", "parse.access_type", r"^0x[0-9a-f]+", True
    )
    yield check_variable(
        children1.data[1], "W(0).field2", "array (1 .. 2) of character", '"ab"'
    )
    # W(1)
    yield check_variable(children.data[1], "W(1)", "parse.my_record", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 2, "Invalid count of W.(1) children")
    yield check_variable(
        children1.data[0], "W(1).field1", "parse.access_type", r"^0x[0-9a-f]+", True
    )
    yield check_variable(
        children1.data[1], "W(1).field2", "array (1 .. 2) of character", '"rt"'
    )

    var = yield check(p, "Rr", "parse.record_of_record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Rr children")
    yield check_variable(
        children.data[0], "Rr.casefield1", "parse.access_type", r"^0x[0-9a-f]+", True
    )
    yield check_variable(children.data[1], "Rr.field2", "parse.my_record", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 2, "Invalid count of Rr.field2 children")
    yield check_variable(
        children1.data[0],
        "Rr.field2.field1",
        "parse.access_type",
        r"^0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children1.data[1], "Rr.field2.field2", "array (1 .. 2) of character", '"ab"'
    )

    var = yield check(p, "Roa", "parse.record_of_array", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Roa children")
    # Roa.field1
    yield check_variable(children.data[0], "Roa.field1", "parse.integer_array", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 4, "Invalid count of Roa.field1 children")
    yield check_variable(children1.data[0], "Roa.field1(1)", "integer", "2")
    yield check_variable(children1.data[1], "Roa.field1(2)", "integer", "3")
    yield check_variable(children1.data[2], "Roa.field1(3)", "integer", "4")
    yield check_variable(children1.data[3], "Roa.field1(4)", "integer", "5")
    # Roa.field2
    yield check_variable(children.data[1], "Roa.field2", "parse.integer_array", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 4, "Invalid count of Roa.field2 children")
    yield check_variable(children1.data[0], "Roa.field2(1)", "integer", "2")
    yield check_variable(children1.data[1], "Roa.field2(2)", "integer", "3")
    yield check_variable(children1.data[2], "Roa.field2(3)", "integer", "4")
    yield check_variable(children1.data[3], "Roa.field2(4)", "integer", "5")
    yield check_variable(children.data[2], ".field3", "integer", "1234")

    var = yield check(p, "X", "parse.integer_array3", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 10, "Invalid count of X children")
    yield check_variable(children.data[0], "X(1)", "array (1 .. 20) of integer", "")
    yield check_variable(children.data[1], "X(2)", "array (1 .. 20) of integer", "")
    yield check_variable(children.data[2], "X(3)", "array (1 .. 20) of integer", "")
    yield check_variable(children.data[3], "X(4)", "array (1 .. 20) of integer", "")
    yield check_variable(children.data[4], "X(5)", "array (1 .. 20) of integer", "")
    yield check_variable(children.data[5], "X(6)", "array (1 .. 20) of integer", "")
    yield check_variable(children.data[6], "X(7)", "array (1 .. 20) of integer", "")
    yield check_variable(children.data[7], "X(8)", "array (1 .. 20) of integer", "")
    yield check_variable(children.data[8], "X(9)", "array (1 .. 20) of integer", "")
    yield check_variable(children.data[9], "X(10)", "array (1 .. 20) of integer", "")

    var = yield check(p, "Ar", "parse.array_of_record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Ar children")
    yield check_variable(children.data[0], "Ar(1)", "parse.my_record", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Ar.(1) children")
    yield check_variable(
        children1.data[0], "Ar.(1).field1", "parse.access_type", r"^0x[0-9a-f]+", True
    )
    yield check_variable(
        children1.data[1], "Ar.(1).field2", "array (1 .. 2) of character", '"ab"'
    )
    yield check_variable(children.data[1], "Ar(2)", "parse.my_record", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 2, "Invalid count of Ar.(2) children")
    yield check_variable(
        children1.data[0], "Ar.(2).field1", "parse.access_type", r"^0x[0-9a-f]+", True
    )
    yield check_variable(
        children1.data[1], "Ar.(2).field2", "array (1 .. 2) of character", '"cd"'
    )

    var = yield check(p, "Z", "parse.discriminants_record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Z children")
    yield check_variable(children.data[0], "Z.a", "integer", "1")
    yield check_variable(children.data[1], "Z.b", "boolean", "false")
    yield check_variable(children.data[2], "Z.c", "float", "2.0")

    yield check(p, "As", "parse.access_subprogram", r"^0x[0-9a-f]+", True)

    var = yield check(p, "Y", "parse.variable_record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Y children")
    yield check_variable(children.data[0], "Y.a", "boolean", "true")
    yield check_variable(children.data[1], "Y.<variant part>", "integer", "1")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 0, "Invalid count of Y.(var 1) children")

    var = yield check(p, "Y2", "parse.variable_record", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Y2 children")
    yield check_variable(children.data[0], "Y2.a", "boolean", "false")
    yield check_variable(children.data[1], "Y2.<variant part>", "float", "1.0")
    yield check_variable(children.data[2], "Y2.b", "integer", "2")

    var = yield check(p, "Tt", "parse.tagged_type", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Tt children")
    yield check_variable(
        children.data[0], "Tt.tag", "ada.tags.tag", r"^0x[0-9a-f]+", True
    )
    yield check_variable(children.data[1], "Tt.a", "integer", "2")
    yield check_variable(children.data[2], "Tt.a", "character", "67 " + "'" + "C" + "'")

    var = yield check(p, "Ctt2", "parse.child_tagged_type2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of Ctt2 children")
    yield check_variable(
        children.data[0], "Ctt2.<tagged_type>", "parse.tagged_type", ""
    )
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of Ctt2.<tagged_type> children")
    yield check_variable(
        children1.data[0],
        "Ctt2.<tagged_type>.tag",
        "ada.tags.tag",
        r"^0x[0-9a-f]+",
        True,
    )
    yield check_variable(children1.data[1], "Ctt2.<tagged_type>.a", "integer", "2")
    yield check_variable(
        children1.data[2], "Ctt2.<tagged_type>.b", "character", "67 " + "'" + "C" + "'"
    )

    var = yield check(p, "T_Ptr", "parse.t_ptr_type", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of T_Ptr children")
    yield check_variable(children.data[0], "T_Ptr(1)", "integer", "13")
    yield check_variable(children.data[1], "T_Ptr(2)", "integer", "17")

    var = yield check(p, "T_Ptr2", "parse.t_ptr_type", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of T_Ptr2 children")
    yield check_variable(children.data[0], "T_Ptr(2)", "integer", "13")
    yield check_variable(children.data[1], "T_Ptr(3)", "integer", "17")

    var = yield check(p, "Ba", "parse.big_array", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1000, "Invalid count of Ba children")
    yield check_variable(
        children.data[0],
        "Ba(1)",
        "parse.access_type|access integer",
        r"^0x[0-9a-f]+",
        True,
        True,
    )

    var = yield check(p, "Ba2", "parse.big_array2", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1000, "Invalid count of Ba2 children")
    yield check_variable(children.data[0], "Ba2(1)", "integer", "0")

    yield check(p, "RegExp", "array (1 .. 9) of character", r'"(\w)=(\w)"')
    yield check(p, "Null_Ptr", "parse.t_ptr_type", "")

    var = yield check(p, "Ra", "parse.record_array", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 10, "Invalid count of Ra children")
    # Ra(1)
    yield check_variable(children.data[0], "Ra(1)", "parse.point", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(1) children")
    yield check_variable(children1.data[0], "Ra(1).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(1).y", "integer", "0")
    # Ra(2)
    yield check_variable(children.data[1], "Ra(2)", "parse.point", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(2) children")
    yield check_variable(children1.data[0], "Ra(2).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(2).y", "integer", "0")
    # Ra(3)
    yield check_variable(children.data[2], "Ra(3)", "parse.point", "")
    children1 = yield get_children(children.data[2])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(3) children")
    yield check_variable(children1.data[0], "Ra(3).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(3).y", "integer", "0")
    # Ra(4)
    yield check_variable(children.data[3], "Ra(4)", "parse.point", "")
    children1 = yield get_children(children.data[3])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(4) children")
    yield check_variable(children1.data[0], "Ra(4).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(4).y", "integer", "0")
    # Ra(5)
    yield check_variable(children.data[4], "Ra(5)", "parse.point", "")
    children1 = yield get_children(children.data[4])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(5) children")
    yield check_variable(children1.data[0], "Ra(5).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(5).y", "integer", "0")
    # Ra(6)
    yield check_variable(children.data[5], "Ra(6)", "parse.point", "")
    children1 = yield get_children(children.data[5])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(6) children")
    yield check_variable(children1.data[0], "Ra(6).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(6).y", "integer", "0")
    # Ra(7)
    yield check_variable(children.data[6], "Ra(7)", "parse.point", "")
    children1 = yield get_children(children.data[6])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(7) children")
    yield check_variable(children1.data[0], "Ra(7).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(7).y", "integer", "0")
    # Ra(8)
    yield check_variable(children.data[7], "Ra(8)", "parse.point", "")
    children1 = yield get_children(children.data[7])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(8) children")
    yield check_variable(children1.data[0], "Ra(8).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(8).y", "integer", "0")
    # Ra(9)
    yield check_variable(children.data[8], "Ra(9)", "parse.point", "")
    children1 = yield get_children(children.data[8])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(9) children")
    yield check_variable(children1.data[0], "Ra(9).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(9).y", "integer", "0")
    # Ra(10)
    yield check_variable(children.data[9], "Ra(10)", "parse.point", "")
    children1 = yield get_children(children.data[9])
    gps_assert(len(children1.data), 2, "Invalid count of Ra.(10) children")
    yield check_variable(children1.data[0], "Ra(10).x", "integer", "0")
    yield check_variable(children1.data[1], "Ra(10).y", "integer", "0")

    var = yield check(p, "Nvp", "parse.null_variant_part", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of Nvp children")
    yield check_variable(children.data[0], "Nvp.discr", "integer", "3")

    var = yield check(p, "This", "parse.object", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of This children")
    yield check_variable(children.data[0], "This.attributes", "parse.attribute_arr", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of This.attributes children")
    # .attributes(0)
    yield check_variable(
        children1.data[0],
        "This.attributes(0)",
        "array (channel_1 .. channel_2) of parse.pointer",
        "",
    )
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 2, "Invalid count of This.attributes(0) children")
    yield check_variable(
        children2.data[0],
        "This.attributes(0,0)",
        "parse.pointer",
        r"^0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children2.data[1],
        "This.attributes(0,1)",
        "parse.pointer",
        r"^0x[0-9a-f]+",
        True,
    )
    # .attributes(1)
    yield check_variable(
        children1.data[1],
        "This.attributes(1)",
        "array (channel_1 .. channel_2) of parse.pointer",
        "",
    )
    children2 = yield get_children(children1.data[1])
    gps_assert(len(children2.data), 2, "Invalid count of This.attributes(1) children")
    yield check_variable(
        children2.data[0],
        "This.attributes(1,0)",
        "parse.pointer",
        r"^0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children2.data[1],
        "This.attributes(1,1)",
        "parse.pointer",
        r"^0x[0-9a-f]+",
        True,
    )

    var = yield check(p, "Scr", "parse.screen_image_type", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of Scr children")
    yield check_variable(
        children.data[0], "Scr.new_screen_image", "parse.screen_image_type_0", ""
    )
    children1 = yield get_children(children.data[0])
    gps_assert(
        len(children1.data), 14, "Invalid count of Scr.new_screen_image children"
    )
    yield check_variable(
        children1.data[0], "Scr.new_screen_image(1)", "parse.line_buffer_type", ""
    )
    children2 = yield get_children(children1.data[0])
    gps_assert(
        len(children2.data), 24, "Invalid count of Scr.new_screen_image(1) children"
    )
    yield check_variable(
        children2.data[0], "Scr.new_screen_image(1,1)", "parse.screen_element_type", ""
    )
    children3 = yield get_children(children2.data[0])
    gps_assert(
        len(children3.data), 4, "Invalid count of Scr.new_screen_image(1,1) children"
    )
    yield check_variable(
        children3.data[0], "Scr.new_screen_image(1,1).char", "character", "32 ' '"
    )
    yield check_variable(
        children3.data[1], "Scr.new_screen_image(1,1).color", "parse.color_type", "cyan"
    )
    yield check_variable(
        children3.data[2], "Scr.new_screen_image(1,1).font", "parse.font_type", "small"
    )
    yield check_variable(
        children3.data[3], "Scr.new_screen_image(1,1).reverse_video", "boolean", "true"
    )

    var = yield check(p, "More_Fruits", "parse.fruits_array_type", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of More_Fruits children")
    yield check_variable(children.data[0], "More_Fruits(0)", "parse.fruit", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 2, "Invalid count of More_Fruits.(0) children")
    yield check_variable(
        children1.data[0], "More_Fruits(0).choice", "parse.choice_type", "apple"
    )
    yield check_variable(children1.data[1], "More_Fruits(0).abc", "integer", "20")
    yield check_variable(children.data[1], "More_Fruits(1)", "parse.fruit", "")

    yield wait_until_not_busy(debug)
    yield wait_idle()
    debug.send("frame 6")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    var = yield check(
        p, "Args", "array (1 .. 3) of parse.tn_9305_014.string_access", ""
    )
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Args children")
    yield check_variable(
        children.data[0],
        "Args(1)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children.data[1],
        "Args(2)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children.data[2],
        "Args(3)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )

    var = yield check(
        p, "Args2", "array (1 .. 3, 1 .. 3) of parse.tn_9305_014.string_access", ""
    )
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of Args2 children")
    # Args2(1)
    yield check_variable(
        children.data[0],
        "Args2(1)",
        "array (1 .. 3) of parse.tn_9305_014.string_access",
        "",
    )
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of Args2(1) children")
    yield check_variable(
        children1.data[0],
        "Args2(1,1)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children1.data[1],
        "Args2(1,2)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children1.data[2],
        "Args2(1,3)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    # Args2(2)
    yield check_variable(
        children.data[1],
        "Args2(2)",
        "array (1 .. 3) of parse.tn_9305_014.string_access",
        "",
    )
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 3, "Invalid count of Args2(1) children")
    yield check_variable(
        children1.data[0],
        "Args2(1,1)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children1.data[1],
        "Args2(1,2)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children1.data[2],
        "Args2(1,3)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    # Args2(3)
    yield check_variable(
        children.data[2],
        "Args2(3)",
        "array (1 .. 3) of parse.tn_9305_014.string_access",
        "",
    )
    children1 = yield get_children(children.data[2])
    gps_assert(len(children1.data), 3, "Invalid count of Args2(1) children")
    yield check_variable(
        children1.data[0],
        "Args2(1,1)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children1.data[1],
        "Args2(1,2)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )
    yield check_variable(
        children1.data[2],
        "Args2(1,3)",
        "parse.tn_9305_014.string_access",
        r"0x[0-9a-f]+",
        True,
    )

    yield wait_until_not_busy(debug)
    yield wait_idle()
    debug.send("cont")
    yield wait_DAP_server("stackTrace")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    debug.send("frame 6")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    var = yield check(p, "Ut", "parse.union_type", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Ut children")
    yield check_variable(children.data[0], "Ut.b", "integer", "3")
    yield check_variable(children.data[1], "Ut.c", "float", r"^4.2", True)

    var = yield check(p, "A", "integer", "5")

    var = yield check(p, "T", "<ref> parse.tagged_type", "(a => 2, b => 67 'C')")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of T children")
    yield check_variable(children.data[0], "T(1)", "parse.tagged_type", "")
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 3, "Invalid count of T.tagged_type children")
    yield check_variable(
        children1.data[0], "T'tag", "ada.tags.tag", r"0x[0-9a-f]+", True
    )
    yield check_variable(children1.data[1], "T.a", "integer", "2")
    yield check_variable(children1.data[2], "T.b", "character", "67 'C'")

    if platform.system().lower() != "windows":
        var = yield check(p, "R", "parse.my_record", "")

        children = yield get_children(var.data)
        gps_assert(len(children.data), 2, "Invalid count of R children")
        yield check_variable(
            children.data[0], "R.field1", "parse.access_type", r"0x[0-9a-f]+", True
        )
        yield check_variable(
            children.data[1], "R.field2", "array (1 .. 2) of character", '"ab"'
        )

    var = yield check(
        p, "Ustring", "ada.strings.unbounded.unbounded_string", '"not_set"'
    )

    var = yield check(p, "Asu_Test", "parse.bar.matrix_map", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Asu_Test children")
    yield check_variable(
        children.data[0],
        "Asu_Test(1)",
        "array (1 .. 1) of ada.strings.unbounded.unbounded_string",
        "",
    )
    children1 = yield get_children(children.data[0])
    gps_assert(len(children1.data), 1, "Invalid count of Asu_Test(1) children")
    yield check_variable(
        children1.data[0],
        "Asu_Test(1,1)",
        "ada.strings.unbounded.unbounded_string",
        '"not_set"',
    )
    yield check_variable(
        children.data[1],
        "Asu_Test(2)",
        "array (1 .. 1) of ada.strings.unbounded.unbounded_string",
        "",
    )
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 1, "Invalid count of Asu_Test(2) children")
    yield check_variable(
        children1.data[0],
        "Asu_Test(2,1)",
        "ada.strings.unbounded.unbounded_string",
        '"not_set"',
    )

    var = yield check(p, "Asu_Test2", "parse.bar.matrix_map_instance", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of Asu_Test2 children")
    yield check_variable(
        children.data[0], "Asu_Test2'tag", "ada.tags.tag", r"0x[0-9a-f]+", True
    )
    yield check_variable(children.data[1], "Asu_Test2.map", "parse.bar.matrix_map", "")
    children1 = yield get_children(children.data[1])
    gps_assert(len(children1.data), 2, "Invalid count of Asu_Test2.map children")
    yield check_variable(
        children1.data[0],
        "Asu_Test2.map(1)",
        "array (1 .. 1) of ada.strings.unbounded.unbounded_string",
        "",
    )
    children2 = yield get_children(children1.data[0])
    gps_assert(len(children2.data), 1, "Invalid count of Asu_Test2.map(1) children")
    yield check_variable(
        children2.data[0],
        "Asu_Test2.map(1,1)",
        "ada.strings.unbounded.unbounded_string",
        '"not_set"',
    )
    yield check_variable(
        children1.data[1],
        "Asu_Test2.map(2)",
        "array (1 .. 1) of ada.strings.unbounded.unbounded_string",
        "",
    )
    children2 = yield get_children(children1.data[1])
    gps_assert(len(children2.data), 1, "Invalid count of Asu_Test2.map(2) children")
    yield check_variable(
        children2.data[0],
        "Asu_Test2.map(2,1)",
        "ada.strings.unbounded.unbounded_string",
        '"not_set"',
    )

    var = yield check(p, "NBI_N", "parse.value_type", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 1, "Invalid count of NBI_N children")
    yield check_variable(
        children.data[0], "NBI_N.var", "parse.value_var_type", "v_null"
    )

    var = yield check(p, "NBI_B", "parse.value_type", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of NBI_B children")
    yield check_variable(
        children.data[0], "NBI_B.var", "parse.value_var_type", "v_boolean"
    )
    yield check_variable(children.data[1], "NBI_B.boolean_value", "boolean", "true")

    var = yield check(p, "NBI_I", "parse.value_type", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 2, "Invalid count of NBI_I children")
    yield check_variable(
        children.data[0], "NBI_I.var", "parse.value_var_type", "v_integer"
    )
    yield check_variable(children.data[1], "NBI_I.integer_value", "integer", "18")

    var = yield check(p, "AP", "parse.access_procedure", r"0x[0-9a-f]+", True)

    var = yield check(p, "AF", "parse.access_function", r"0x[0-9a-f]+", True)

    info = yield p.get_variable_by_name("AF.all")
    gps_assert(info.data is None, True, "AF.all exists")

    var = yield check(p, "RAF", "parse.record_access_function", "")
    children = yield get_children(var.data)
    gps_assert(len(children.data), 3, "Invalid count of RAF children")
    yield check_variable(children.data[0], "RAF.i", "integer", "0")
    yield check_variable(
        children.data[1], "RAF.af", "parse.access_function", r"0x[0-9a-f]+", True
    )
    yield check_variable(children.data[2], "RAF.i2", "integer", "0")

    yield wait_until_not_busy(debug)
    yield wait_idle()
    debug.send("b swap")
    yield wait_until_not_busy(debug)
    yield wait_idle()
    debug.send("cont")
    yield wait_DAP_server("stackTrace")
    yield wait_until_not_busy(debug)
    yield wait_idle()

    var = yield check(p, "Word", "string", '"qeaLfjb"')

    GPS.execute_action("terminate all debuggers")
    yield wait_idle()
    GPS.exit(True)
