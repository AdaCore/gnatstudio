import GPS
import lal
from gps_utils import interactive


def flag_storage_of_project_type(source_file):
    """Flag all dangerous storage of a Project_Type in the given file"""
    c = lal.libadalang.AnalysisContext()
    a = c.get_from_file(source_file.path)

    # for safety, in case libadalang cannot process a file
    if not a.root:
        GPS.Console().write(
            "libadalang could not process {}\n".format(source_file))
        return

    # Find all record definitions in the unit
    for record in a.root.findall(lal.libadalang.RecordTypeDef):

        # ... look at all the fields in these records
        for field in record.findall(lal.libadalang.ComponentDecl):

            # if the component def contains "Project_Type" flag the location
            if 'Project_Type' in field.f_component_def.text:
                GPS.Message("Dangerous storing of Project_Type",
                            source_file,
                            int(field.sloc_range.start.line),
                            field.sloc_range.start.column,
                            "warning: you should not store a Project_Type")


def initialize_project_plugin():
    """Called automatically when gps.gpr is loaded"""
    # Create a specific menu under Analyze/GPS to flag

    if not GPS.Action("find project types stored").exists():
        @interactive(category="General", name="find project types stored",
                     menu="/Analyze/GPS/Flag stored Project Types")
        def find_project_types_stored():
            all_ada_sources = filter(lambda x: x.language().lower() == 'ada',
                                     GPS.Project.root().sources(
                                         recursive=True))

            for source_file in all_ada_sources:
                flag_storage_of_project_type(source_file)


def finalize_project_plugin():
    """Called automatically when gps.gpr is unloaded"""
    pass
