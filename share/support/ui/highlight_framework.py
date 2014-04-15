from highlight_engine import Struct, HighlighterModule, Highlighter
from functools import partial
import GPS

##############################
# Highlight classes creation #
##############################


def simple(regexp_string, tag="", matchall=True, **kwargs):
    """
    Return a simple matcher for a regexp string
    @type regexp_string: str
    @type tag: str
    """
    return Struct(kind="simple", re=regexp_string, tag=tag,
                  matchall=matchall, **kwargs)


def words(words_list, **kwargs):
    """
    Return a matcher for a list of words
    @type words_list: str|list[str]
    """
    if type(words_list) is list:
        words_list = "|".join(words_list)

    return simple(r"\b(?:{0})\b".format(words_list), **kwargs)


def region(start_re=None, end_re=None, name="", tag="", highlighter=(),
           matchall=True, **kwargs):
    """
    Return a matcher for a region, which can contain a whole specific
    highlighter
    @type start_re: string
    @type end_re: string
    """
    return Struct(kind="region", start_re=start_re, end_re=end_re,
                  name=name, tag=tag, highlighter_spec=highlighter,
                  matchall=matchall, **kwargs)


def region_template(*args, **kwargs):
    return partial(region, *args, **kwargs)


def region_ref(name):
    return Struct(kind="region_ref", region_ref_name=name)


def new_style(lang, name, foreground_color, prio=20):
    style_id = "{0}_{1}".format(lang, name)
    pref = GPS.Preference("Editor/{0}/{1}".format(lang, name))
    doc = "Color for '{0}'".format(name)
    pref.create(doc, "color", doc, foreground_color)
    pref.tag = None
    HighlighterModule.preferences[style_id] = pref

    return Struct(
        style_id=style_id,
        name=name,
        prio=prio,
        pref=pref
    )


def register_highlighter(language, *args, **kwargs):
    highlighter = Highlighter(*args, **kwargs)
    HighlighterModule.highlighters[language] = highlighter
