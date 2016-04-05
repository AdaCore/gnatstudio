"""
If this plugin is enabled, provides automatic setting of Ada reformatting
preferences based on gnatpp switches as set in the project file.

For example, if gnatpp switch -i4 is set, then the
Editor/Ada/Default indentation preference is set to 4

Note that some gnatpp switches have no direct equivalence (and vice-versa),
in which case they are ignored.
"""


import GPS


def set_pref(f, name, val):
    GPS.Preference(name).set(val, False)
    GPS.Logger("gnatpp").log("convert " + f + " into " + name + "=" + str(val))


def project_recomputed(hook_name):
    s = GPS.Project.root().get_attribute_as_list(
        "default_switches", package="pretty_printer", index="ada")

    if s == []:
        GPS.Logger("gnatpp").log("no gnatpp switches, exit")
        return

    compact_layout = True
    indent_level = 3
    GPS.Preference("Ada-Auto-Indentation").set("Extended", False)
    GPS.Preference("Ada-Casing-Policy").set("On_The_Fly", False)
    GPS.Preference("Ada-Format-Operators").set(True, False)

    for f in s:
        if f == "-A0":
            set_pref(f, "Ada-Align-On-Colons", False)
        elif f == "-A1":
            set_pref(f, "Ada-Align-On-Colons", True)
        elif f == "-A4":
            set_pref(f, "Ada-Align-On-Arrows", True)
        elif f[0:2] == "-i":
            indent_level = int(f[2:])
            set_pref(f, "Ada-Indent-Level", indent_level)
        elif f[0:3] == "-cl":
            set_pref(f, "Ada-Continuation-Level", int(f[3:]))
        elif f == "-kL":
            set_pref(f, "Ada-Reserved-Casing", "Lower")
        elif f == "-kU":
            set_pref(f, "Ada-Reserved-Casing", "Upper")
        elif f == "-nD":
            set_pref(f, "Ada-Ident-Casing", "Unchanged")
        elif f == "-nU":
            set_pref(f, "Ada-Ident-Casing", "Upper")
        elif f == "-nL":
            set_pref(f, "Ada-Ident-Casing", "Lower")
        elif f == "-nM":
            set_pref(f, "Ada-Ident-Casing", "Smart_Mixed")
        elif f == "-c0":
            set_pref(f, "Ada-Indent-Comments", False)
        elif f == "-c4":
            set_pref(f, "Ada-Indent-Comments", True)
        elif f == "-l3":
            compact_layout = False
        elif f[0:2] == "-M":
            set_pref(f, "Src-Editor-Highlight-Column", int(f[2:]))
        else:
            GPS.Logger("gnatpp").log("ignore switch " + f)

    if not compact_layout:
        set_pref("-l3", "Ada-Record-Level", indent_level)

GPS.Hook("project_view_changed").add(project_recomputed)
