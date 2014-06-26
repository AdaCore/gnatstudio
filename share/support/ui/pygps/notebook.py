""" This module provides a higher-level interface to notebooks.
    It relies on the pygobject package.
"""


try:
    from gi.repository import Gtk, GObject
    import pygps

    def switch_notebook_page(notebook, label):
        """Switch to the page with the given label in the notebook.
           Returns the page number that was selected.
           -1 is returned if no such page exists."""

        page = 0
        while notebook.get_nth_page(page):
            p = notebook.get_nth_page(page)
            if get_notebook_page_label(notebook, p).get_label() == label:
                notebook.set_current_page(page)
                pygps.process_all_events()
                return page
            page = page + 1

        return -1

    def get_notebook_page_label(notebook, page):
        """Return the label of a page in a notebook.
           Page is an integer, the index of the page in the notebook"""

        return pygps.get_widgets_by_type \
            (Gtk.Label, notebook.get_tab_label(page))[0]

    def get_notebook_pages(notebook):
        """Return the list of all visible pages for the notebook"""

        pages = []
        for p in range(0, notebook.get_n_pages()):
            page = notebook.get_nth_page(p)
            if page.get_visible():
                pages.append(page)
        return pages

    def get_notebook_page_labels_text(notebook):
        """Return a list containing the labels of all visible pages for the
           notebook"""

        return [get_notebook_page_label(notebook, p).get_label()
                for p in get_notebook_pages(notebook)]

except ImportError:
    pass
