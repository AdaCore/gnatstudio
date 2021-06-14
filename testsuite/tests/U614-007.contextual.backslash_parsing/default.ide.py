from gs_utils import *


def generate_contextual_label(context):
    return "Something\/Something"


@interactive("General", name="do something",
             contextual=generate_contextual_label, static_path="Custom/%c")
def do_something():
    return
