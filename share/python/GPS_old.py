
## This module provides compatibility with older versions of GPS
## Simply import it in your own script if some of the recent changes can no
## longer be executed with python.
## As much as possible, you should update your scripts, though


import GPS
class Preference:
    def get(name):
       return GPS.Preference(name).get()
    get = staticmethod(get)

    def set(name, value):
       GPS.Preference(name).set (value)
    set = staticmethod(set)

class Entity(object):
    """Provides backward compatibility with older versions of GPS.
To use, build an instance of GPS.Entity, as usual, and then an instance of this method:
    ent = GPS.Entity ("name", GPS.File ("file"))
    GPS_old.Entity (ent).decl_file() == ent.declaration().file()
    ent.calls ()
"""

    def __init__(self, gps_entity):
       self.gps_entity = gps_entity 
    def decl_file(self):
       return self.gps_entity.declaration().file()
    def decl_line(self):
       return self.gps_entity.declaration().line()
    def decl_column(self):
       return self.gps_entity.declaration().column()

