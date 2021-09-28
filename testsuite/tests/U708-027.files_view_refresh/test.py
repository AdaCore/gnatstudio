"""
This test checks expanded status of files view after refresh
"""
import GPS
from gs_utils.internal.utils import \
    gps_assert, run_test_driver, wait_idle, \
    get_widget_by_name, dump_tree_model, wait_tasks
from workflows.promises import timeout
from gs_utils.internal.tree import Tree
import os

expected = ['src',
             ['d1',['x1', ['f1.ads'],'x2'],
              'd2',
              'd3',['f4.ads'],
             'main.adb']]

def dump_expanded(tree, column, iter=None):
    """
    Return the model's contents for a specific column and expanded rows
    :param int column: the column to dump.
    """
    value = []
    model = tree.get_model()

    if not iter:
        iter = model

    for row in iter:
        value.append(row[column])
        iter = row.iterchildren()

        if iter and tree.row_expanded(row.path):
            result = dump_expanded(tree, column, iter)
            if result != []:
                value.append(result)

    return value

@run_test_driver
def run_test():
    GPS.execute_action("open Files")
    buf = GPS.EditorBuffer.get(GPS.File('main.adb'))
    yield wait_idle()
    entry = get_widget_by_name("Files_View_Directory")
    entry.set_text(os.path.join (GPS.Project.root().file().directory(), "src"))
    yield wait_tasks()

    fview = get_widget_by_name("File Explorer Tree")
    t=Tree(get_widget_by_name("File Explorer Tree"))
    t.expand_by_name(column=1, value='src')
    t.expand_by_name(column=1, value='d1')
    t.expand_by_name(column=1, value='d3')
    yield timeout(100)
    t.expand_by_name(column=1, value='x1')
    yield timeout(100)

    d = dump_expanded(fview, 1)
    gps_assert(d, expected, "Wrong contents of the files view")

    GPS.execute_action("refresh files view")
    yield wait_idle()
    yield timeout(100)

    d = dump_expanded(fview, 1)
    gps_assert(d, expected, "Wrong contents of the files view")
