# Test hierarchical directories representation in the Project view.

from gs_utils.internal.utils import run_test_driver, gps_assert, \
    wait_tasks, wait_idle, Project_View, dump_tree_model, find_in_tree, \
    get_widget_by_name
import os

expected =  ['Hello',
  ['src',
  ['main.adb',
  'ui.ads'],
  '.',
  'A',
  ['src',
  ['a.adb',
  'a.ads'],
  '.'],
  'B',
  ['src',
  [os.path.join('src', '1'),
  ['class_definition.ads'],
  os.path.join('src', '2'),
  [os.path.join('src', '2', 'in'),
  ['parent1.ads'],
  'lib.ads'],
  os.path.join('src', '3'),
  ['parent2.ads'],
  'b.adb',
  'b.ads'],
  'src1',
  ['aaa.ads'],
  '.']]]

filtered =  ['Hello',
  ['B',
  ['src',
  [os.path.join('src','2'),
  ['lib.ads']],
  '.']]]


@run_test_driver
def driver():
    GPS.Preference("explorer-hierarchical-directories").set("True")
    yield wait_tasks()

    prj_view = Project_View()
    yield prj_view.open_and_yield()

    tree = prj_view.dialog
    path = find_in_tree(tree, column=1, key='A')
    tree.expand_row(path, open_all=False)
    path = find_in_tree(tree, column=1, key='B')
    tree.expand_row(path, open_all=False)
    yield wait_idle()

    d1 = dump_tree_model(tree.get_model(), 1)
    gps_assert(d1, expected, "Wrong hierarchi")

    filt = get_widget_by_name("Project Explorer Filter")
    filt.set_text("lib")
    yield wait_tasks()

    d2 = dump_tree_model(tree.get_model(), 1)
    gps_assert(d2,filtered,"Wrong filtered")
