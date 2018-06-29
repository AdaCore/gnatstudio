import GPS


class Project_Template_Object:
    def __init__(self):
        pass

    def on_apply(self):
        GPS.EditorBuffer.get(GPS.File("README"))


def get_object():
    return Project_Template_Object()
