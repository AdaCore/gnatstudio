
## This module provides compatibility with older versions of GPS


import GPS
class Preference:
    def get(name):
       return GPS.Preference(name).get()
    get = staticmethod(get)

    def set(name, value):
       GPS.Preference(name).set (value)
    set = staticmethod(set)

