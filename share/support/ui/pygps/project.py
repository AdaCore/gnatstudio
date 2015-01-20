"""This module gives access to all project-related interfaces in GPS"""


try:
    import pygps
    import pygps.tree
    from gi.repository import Gtk
    import GPS

    def open_project_wizard(on_open, *args, **kwargs):
        """
        Open the project wizard asynchronously, and call
        on_open (wizard, *args, **kwargs)
        """

        pygps.open_menu("/Project/New...", on_open, [], args, kwargs)

    def open_project_properties(on_open, *args, **kwargs):
        """
        Open the project properties editor asynchronously, and call
        on_open (dialog, treeview, *args, **kwargs),
        where treeview is the widget found on the left of the project
        properties dialog, so that you can easily change to a specific page
        inside the notebook:
            from pygps.tree import select_in_tree
            def on_pp (dialog, treeview):
               select_in_tree (treeview, 3, "Switches/Ada")
            open_project_properties (on_pp)
        """

        pygps.open_menu(
            "/Project/Properties...", on_open,
            ["Project Properties Tree"],
            args, kwargs, 2000)


except ImportError:
    pass
