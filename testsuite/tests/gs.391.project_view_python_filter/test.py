"""
Test the GPS.ProjectViewFilters class.
"""

from GPS import *
from gs_utils.internal.utils import *

FILES = ["bar.ads", "bar.adb", "foo.ads", "foo.adb", "foo_bar.ads", "foo_bar.adb"]

EXPECTED_LIST = ["filter1", "second_filter"]


def expected_for_filters(func_list):
    visibles = FILES
    for func in func_list:
        if func:
            visibles = [f for f in visibles if func(GPS.File(f))]
    return ["Test", [".", visibles, "."]]


def only_adb(file):
    return file.path.endswith("adb")


def only_ads(file):
    return file.path.endswith("ads")


def has_foo(file):
    return "foo" in file.path


@run_test_driver
def run_test():
    view = GPS.MDI.get("Project")
    tree = get_widget_by_name("Project Explorer Tree", view.pywidget())

    def test_filter(tree, func_list):
        scenario = "+".join([func.__name__ for func in func_list])

        # Add all the filters
        for func in func_list:
            GPS.ProjectViewFilters.add_filter(func.__name__, func)

        # Refilter the Project view
        GPS.Hook("project_view_changed").run()
        yield wait_idle()

        gps_assert(
            dump_tree_model(tree.get_model(), 1),
            expected_for_filters(func_list),
            "Issue after applying filter %s" % scenario,
        )

        tree = get_widget_by_name("Project Explorer Tree", view.pywidget())

        # Remove all the filters
        for func in func_list:
            GPS.ProjectViewFilters.remove_filter(func.__name__)

        # Refilter the Project view
        GPS.Hook("project_view_changed").run()
        yield wait_idle()

        gps_assert(
            dump_tree_model(tree.get_model(), 1),
            expected_for_filters([]),
            "Issue after removing filter %s" % scenario,
        )

    yield test_filter(tree, [only_adb])
    yield test_filter(tree, [only_ads])
    yield test_filter(tree, [has_foo])
    yield test_filter(tree, [has_foo, only_adb])

    gps_assert(
        GPS.ProjectViewFilters.list_filters(), [], "The filter list should be empty"
    )

    GPS.ProjectViewFilters.add_filter("filter1", only_adb)
    GPS.ProjectViewFilters.add_filter("second_filter", only_adb)
    gps_assert(
        sorted(GPS.ProjectViewFilters.list_filters()),
        EXPECTED_LIST,
        "Missing filters in the list",
    )

    for name in EXPECTED_LIST:
        GPS.ProjectViewFilters.remove_filter(name)
    gps_assert(
        GPS.ProjectViewFilters.list_filters(),
        [],
        "All filters should be cleaned using list_filters",
    )
