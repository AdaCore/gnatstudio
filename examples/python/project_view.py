# This script changes the label of the project view and set it to the current
# project file path. Upon a project changed, the title is properly updated.

from GPS import Project, MDI, Timeout, Hook


def update_project_view_title(t):
    new_name = Project.root().file().name()
    new_short_name = Project.root().name()
    view = MDI.get("Project View")

    if view is not None:
        view.rename(new_name, new_short_name)
        t.remove()


def on_project_changed(h):
    Timeout(100, update_project_view_title)


Hook("project_changed").add(on_project_changed)
