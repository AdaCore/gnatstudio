"""This file provides enhanced support for editing CSS files
"""
import GPS


class CSSLanguage(GPS.Language):

    def __init__(self):
        pass


class CSSSupport(object):

    def __init__(self):
        GPS.Language.register(CSSLanguage(),
                              name="CSS",
                              body_suffix=".css",
                              spec_suffix="",
                              obj_suffix="")


CSSSupport()
