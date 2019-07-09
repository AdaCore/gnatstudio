""" Initial support for Why3 task language """

import GPS
from constructs import INDENTATION_NONE


class Why3Lang(GPS.Language):
    def __init__(self):
        pass


GPS.Language.register(Why3Lang(), "why", ".why", "", "", INDENTATION_NONE)
