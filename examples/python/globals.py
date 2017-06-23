"""This script shows how to compute the list of global entities in a whole
   application, in a project or in a source file"""

from GPS import Project, File


def global_entities(where=None):
    """Return all global entities in where, which should either be an instance
       of GPS.File, GPS.Project, or None. In the latter case, all global
       entities in the application are returned"""
    result = []
    if not where:
        for p in Project.root().dependencies(recursive=True):
            result.extend(global_entities(p))

    elif isinstance(where, Project):
        for s in where.sources():
            result.extend(global_entities(s))

    elif isinstance(where, File):
        for e in where.entities():
            if e.attributes()["global"]:
                result.append(e)
    return result
