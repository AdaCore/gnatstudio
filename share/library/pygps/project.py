"""This module gives access to all project-related interfaces in GPS"""



try:
  import gtk, gobject
  import pygps

  def open_project_wizard (on_open, *args, **kwargs):
    """Open the project wizard asynchronously, and call
       on_open (wizard, *args, **kwargs)"""

    pygps.open_menu ("/Project/New...", on_open, [], args, kwargs)

  def open_project_properties (on_open, *args, **kwargs):
    """Open the project properties editor asynchronously, and call
       on_open (dialog, notebook, *args, **kwargs),
       where notebook is the notebook found in the project properties
       dialog, so that you can easily change to a specific page
       inside the notebook:
           import pygps.notebook
           def on_pp (dialog, notebook):
              page = switch_notebook_page (notebook, "Switches")
           open_project_properties (on_pp)
    """

    pygps.open_menu ("/Project/Edit Project Properties", on_open,
               ["Project Properties Notebook"],
               args, kwargs)

except ImportError:
  pass
