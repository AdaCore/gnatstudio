#!/usr/bin/python
# -*- coding: utf-8 -*-

import GPS
import difflib
import inspect


SUCCESS = 0
FAILURE = 1
NOT_RUN = 99
XFAIL = 100   # expected failure
exit_status = SUCCESS


def diff(left, right):
    """
    Pretty print a diff between two elements.
    """
    try:
        return "\n".join(difflib.unified_diff(left, right))
    except TypeError:
        # We might not be able to compare the two lists, specially if
        # they are not lists of strings. In that case returne a representation
        # of the two objects.
        return "left: %s\nright: %s\n" % (left, right)


def pretty_print(str, maxwidth=100):
    """
    Pretty print a string to fit in maxwidth columns,
    """
    result = ''
    col = 1
    for s in range(len(str)):
        result += str[s]
        if str[s] == ',':
            col = 1
            result += '\n'
        else:
            col = col + 1
            if col == maxwidth:
                col = 1
                result += '\n'
    return result


def __show_error(msg, quiet=False):
    """
    Display the error message on stdout, and possibly add the backtrace.
    :param bool quiet: whether to show a traceback
    """
    global exit_status
    exit_status = FAILURE

    if not quiet:
        msg += '\n'
        for s in inspect.stack():
            msg += '\n  at %s:%s:%s' % (s[1], s[2], s[3])
        msg += '\n in directory ' + GPS.pwd()

    GPS.Logger('TESTSUITE').log(msg)


def display_error(left=None, right=None, comp=None, msg="", quiet=False):
    """
    Display an error showing the difference between left and right.

    :param str comp: a string to display the comparison operator.
    :param bool quiet: whether to show a traceback
    """
    if left is not None and right is not None and comp is not None:
        if isinstance(left, list) and len(left) > 10:
            msg += diff(left, right)
        else:
            left_lns = str(left).splitlines()
            right_lns = str(right).splitlines()
            if len(left_lns) > 10:
                msg += diff(left_lns, right_lns)
            else:
                msg += '\n%s\n%s\n%s' % (
                    pretty_print("%s" % (left, )),
                    comp,
                    pretty_print('%s' % (right, )))

    __show_error(msg, quiet=quiet)


def gps_assert(left, right, msg='Error in test script', quiet=False):
    """
    Ensure that left==right, or display an error
    dialog in GPS.
    :param bool quiet: whether to show a traceback
    :return bool: whether the test passes
    """
    if left != right:
        display_error(left, right, '!=', msg, quiet)
        return False
    return True


def gps_assert_is_instance(inst, klass, msg='', quiet=False):
    """
    Ensure inst is of the given type
    """
    if not isinstance(inst, klass):
        display_error("%s" % inst, "%s" % klass,
                      "is not an instance of", msg, quiet)
        return False
    return True


def gps_assert_list(left, right, msg, quiet=0):
    """
    Compare two lists and display an error if they do not match
    :return bool: whether the test passes
    """
    if left != right:
        error = ""
        for line, value in enumerate(left):
            if value != right[line]:
                error += "-%3d: %s\n" % (line, value)
                error += "+%3d: %s\n" % (line, right[line])
                break
        display_error(error, "", "!=", msg, quiet)
        return False
    return True


def compare_menus(refmenu, menu):
    """
    Compare two menu descriptions, with minimal diff.
    :return: a string, the diff
    """
    dots = unichr(8230)
    result = ""
    ref = 0
    men = 0
    while ref < len(refmenu):
        if men < len(menu):
            if menu[men] != refmenu[ref]:
                result += "First diff at index %s\nref=%s\ngot=%s\n" % (
                    ref, refmenu[ref], menu[men])
                break
        men = men + 1
        ref = ref + 1

    for i, v in enumerate(refmenu):
        if i >= len(menu):
            break
        ustr = menu[i].decode("utf-8")
        idx = ustr.find(dots)
        if idx != -1:
            ustr = ustr[:idx] + u"..." + ustr[idx + 1:]
        item = ustr.encode("utf-8")
        if v != item:
            result += "FAILED item [%s] differs from expected value [%s]\n" % (
                item, v)
        else:
            result += "OK item [%s]\n" % (v, )

    if len(menu) != len(refmenu):
        result += ("length of menu differs from expected\n" +
                   "expected: %s\nmenu:     %s\n") % (refmenu, menu)

    return result


def gps_assert_flat_menu(expected, actual, msg='Incorrect menu'):
    """Compare a menu (in general contextual), but shows a smaller
       diff when there are differences.
       :param expected: a list of all the items, starting with their depth
          as in:
             [' 1 - item1', ' 2 - subitem1', ...]
       :return bool: whether the test passes
    """

    diff = compare_menus(refmenu=expected, menu=actual)
    if diff[0] == 0:
        __show_error(diff[1], quiet=False)
        return False
    return True


def gps_assert_menu(expected, actual, msg='Incorrect menu'):
    """Compare a menu (in general contextual), but shows a smaller
       diff when there are differences.
       :param expected: a list of list for the items, as in:
           ['item1', ['subitem1'], 'item2', ...]
       :return bool: whether the test passes
    """

    def diff_list(a, b):
        result = ''
        while a and b:
            e1 = a.pop(0)
            e2 = b.pop(0)
            if isinstance(e1, list):
                if isinstance(e2, list):
                    result += '[' + diff_list(e1, e2) + ']'
                else:
                    result += '\n- ' + str(e1)
                    result += '\n+ ' + str(e2)
            elif isinstance(e2, list):
                result += '\n- ' + str(e1)
                result += '\n+ ' + str(e2)
            elif e1 == e2:
                result += '.'
            else:
                result += '\n- ' + str(e1) + '\n+ ' + str(e2) + '\n'

        if a:
            result += '\n- '.join([str(e) for e in a])
        if b:
            result += '\n+ '.join([str(e) for e in b])

        return result

    if expected != actual:
        __show_error(diff_list(expected, actual), quiet=False)
        return False
    return True


def gps_not_null(left, msg='Error in test script', quiet=False):
    """
    Error if left compares to False/None/empty string/empty list
    :return bool: whether the test passes
    """
    if not left:
        display_error(left, '', 'should compare to false', msg, quiet)
        return False
    return True


def gps_not_assert(left, right, msg='Error in test script', quiet=False):
    """Ensure that left != right"""
    if left == right:
        display_error(left, right, '==', msg, quiet)
        return False
    return True


def clang_assert(left, right, msg='Error in test script', quiet=False):
    """
    Special assert procedure for clang tests, that will log a few things
    relevant to the error happening, namely errors and include paths for
    libclang
    """
    import compiler_paths
    from os import path

    if not gps_assert(left, right, msg, quiet):
        f = GPS.EditorBuffer.get().file()
        logger = GPS.Logger("TESTSUITE")
        logger.log("Clang assert failed")
        clang_tu = GPS.Libclang.get_translation_unit(f)

        logger.log("Begin logging clang diagnostics")
        for i, d in enumerate(clang_tu.diagnostics):
            logger.log("    Diagnostic {}: {}".format(i, d))
        logger.log("End logging clang diagnostics")

        logger.log("Begin logging clang include paths")

        # Pass the testsuite logger to get_compiler_search_paths, so that we
        # also get all the logging
        paths = compiler_paths.get_compiler_search_paths(
            f.project().name(), f.language(), logger=logger, use_cache=False
        )

        for p in paths:
            logger.log("    Path : {}".format(p))
            if not path.isdir(p):
                logger.log("    WARNING: This path is not valid (path.isdir)")
        logger.log("End logging clang include paths")
        return False
    return True
