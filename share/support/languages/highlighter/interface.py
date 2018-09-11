"""
The mechanism here described allows any user to add syntax highlighting to GPS
for any language in a declarative domain specific language.

Tutorial: Add support for python highlighting in GPS
----------------------------------------------------

In this short tutorial, we will walk through the steps needed to create a
small plugin for GPS that will allow it to highlight python code.

The idea of the whole API is for the user to declare matchings in a
declarative way, specifying the matcher via a classic regular expression
syntax, and taking the appropriate action depending on the kind of the
matcher. There are basically two types of matches:

* Simple matchers will just apply a tag to the matched text region. This
  will be useful to highlight keywords or number expressions in source,
  for example.

* Region matchers will change the set of matchers to the one specified in
  the region definition. That way, you can do more complex highlighters in
  which some simple matchers will work only in some context.

In addition to that, you have a set of helpers that will simplify common
patterns based on those two primitives, or make some additional things
possible. See the full API doc below for more details.

**IMPORTANT NOTE**: As you will see, the way you register an highlighter is by
specifying the language it applies to in the call to register_highlighter. If
you want to highlight a language that is not yet known to GPS, you have to
register a new language. The way to do that is detailled in the
:ref:`Adding_support_for_new_languages` section.

First step, creating a dumb highlighter
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As a first step, we will just create an highlighter that highlights the
self symbol in python, as a simple hello world.::

    from highlighter.common import *

    register_highlighter(
        language="python",
        spec=(
            # Match self
            simple("self", tag=tag_keyword),
        )
    )

As we can see, the first step to register a new highlighter is to call the
:func:`register_highlighter` function, giving the name of the language and
the spec of the language as parameters.

The spec parameter is a tuple of matchers. In this case we have only one,
a simple matcher, as described above, which will match the "self" regexp,
and apply the "keyword" tag everytime it matches.

The tag parameter is the name of the tag that will be used to highlight matches
. GPS has a number of built-in tags for highlighting, that are all defined in
the :py:mod:`highlighter.common` module. They may not be sufficient, so the
user has the possibility of creating new styles, a capability that we will talk
about later on.

Second step, discovering our first helper
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Highlighting just self is a good first step, but we would like to be a
little more pervasive in our highlighting of keywords. Fortunately for us,
python has a way to dynamically get all the language's keywords, by doing::

    from keywords import kwlist

By combining that with the :func:`words` helper, we can easily create a
matcher for every python keyword::

    register_highlighter(
        language="python",
        spec=(
            # Match keywords
            words(kwlist, tag="keyword"),
        )
    )

The :func:`words` helper just creates a simple matcher for a list of words.
:code:`words(["a", "b", "c"], tag="foo")` is equivalent to :code:`simple(
"a|b|c", tag="foo")`.

Third step, highlighting strings literals in a clever way
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Next, we're gonna want to highlight some literals. Let's start by strings,
because they are hard and interresting. A string is a literal that starts
with  a " or a ' character, and ends with the same character, but one needs
to be  careful because there are several corner cases:

* If an escaped string delimiter occurs in the string (\" in a " string for
  example), it should *not* end the string !
* In python, strings delimited by " or ' are single line strings. It means
  that the match needs to be terminated at the end of the line
* BUT, if the last character of the line is a backslash, the string actually
  continues on the next line !

Additionally, some editors are nice enough to highlight escaped chars in a
specific colors inside string literals. Since we want our highlighter to be
cutting edge, we will add this requirement to the list. Here is the
region declaration for this problem, for the case of single quoted strings::

    string_region = region(
        r"'", r"'|[^\\]$", tag="string",
        highlighter=(
            simple(r"\\.", tag=tag_string_escapes),
        )
    )

Here are the important points:

* The first parameter is the regular expression delimiting the beginning of
  the region, in this case a simple quote.

* The second parameter is the regular expression delimiting the end of the
  region, in this case, either a simple quote, either an end of line anchor
  ($). This way, a string will be terminated after a new line.

* The way both line continuations and escaped quotes are handled is actually
  very simple: The simple matcher declared inside the region's highlighter
  will match any character preceded by a backslash, including newlines. An
  important point to understand is that, when inside a region, the matcher
  for ending the region has the lowest priority of all. In this case,
  it means the simple matcher will consume both quotes and new lines if they
  are preceded by a backslash, and so they won't be available for the ending
  matcher anymore.

Creating custom style tags
^^^^^^^^^^^^^^^^^^^^^^^^^^

If the style tags predefined in the :py:mod:`highlighter.common` module are not
enough, you can define new ones with the :func:`new_style` function.

When you define a new style via this function, a corresponding preference will
be created in the GPS preferences, so that the user can change the color later.

The tag_string_escapes common tag is defined with this function this way::

    tag_string_escapes = new_style(lang="General", name="string_escapes",
                                   foreground_colors=('#875162', '#DA7495'))

The first parameter is the name of the language for which this applies, or
"General" if this can potentially apply to several languages. This will be used
by GPS to choose which preference category will be used for the corresponding
preference.

The second parameter is the name of the style.

The third parameter is the colors that will be used by default for this style.
The first color is the one used for light themes, the second color is the one
used for dark themes.

Going further
^^^^^^^^^^^^^

All the details of the engine are not yet documented, but if while creating
your highlighter you find yourself stuck, don't hesitate to look at the C or
Python highlighters, in the c_highlighter and python_highlighter modules that
are shipped with your version of GPS. Those are complete real world examples
that are used by GPS to highlight files in those languages.

API Documentation
-----------------

"""
from functools import partial
import GPS
import re


##############################
# Highlight classes creation #
##############################

def search_for_capturing_groups(regexp_string):
    """
    Return a list of matches for capturing groups in a regular expression.

    :param str regexp_string: The regular expression we want to analyze.
    """
    return re.findall(r"[^\\]\((?!\?:)", regexp_string)


def simple(regexp_string, tag):
    """
    Return a simple matcher for a regexp string.
    Raises an exception if capturing groups are present in the
    regular expression (not supported by the engine).

    :param str regexp_string: The regular expression for this matcher
    :rtype: SimpleMatcher
    """
    if search_for_capturing_groups(regexp_string):
        raise Exception("""Capturing groups are not supported.
Please use non-capturing groups when defining regular expressions.""")
    from highlighter.engine import SimpleMatcher
    return SimpleMatcher(tag, regexp_string)


def words(words_list, **kwargs):
    """
    Return a matcher for a list of words

    :param words_list: The list of words, either as a string of "|"
       separated words, or as a list of strings.
    :type words_list: str|list[str]
    :rtype: SimpleMatcher
    """
    if type(words_list) is list:
        words_list = "|".join(words_list)

    return simple(r"\b(?:{0})\b".format(words_list), **kwargs)


def region(start_re, end_re, tag=None, name="", highlighter=(),
           matchall=True, igncase=False):
    """
    Return a matcher for a region, which can contain a whole specific
    highlighter

    :param string start_re: The regexp used to match the start of the region
    :param string end_re: The regexp used to match the end of the region
    :param highlighter.engine.Style tag: The Tag which will be used to
      highlight the whole region. Beware, if you plan to apply other tags to
      elements inside the region, they must have an higher priority than this
      one !
    :rtype: RegionMatcher
    """
    from highlighter.engine import RegionMatcher
    return RegionMatcher(tag, start_re, end_re, highlighter,
                         matchall, name, igncase=igncase)


def region_template(*args, **kwargs):
    """
    Used to partially construct a region, if you want to define for example,
    several regions having the same sub highlighter and tag, but not the
    same start and end regular expressions.

    :param args: Positional params to pass to region
    :param kwargs: Keyword params to pass to region
    :return: A partially constructed region
    """
    return partial(region, *args, **kwargs)


def region_ref(name):
    """
    Used to reference a region that already exists. The main and only use
    for this is to define recursive regions, eg. region that can occur
    inside themselves or inside their own sub regions. See  the tutorial for a
    concrete use case.

    The returned region reference will behave exactly the same as the
    original region inside the highlighter.

    :param name: The name of the region.
    :rtype: RegionRef
    """
    from highlighter.engine import RegionRef
    return RegionRef(name)


def new_style(lang, name, label, doc, foreground_colors,
              background_colors=("transparent", "transparent"),
              font_style="default", prio=-1):
    """
    Creates a new style to apply when a matcher successfully matches a
    portion of text. A style is the conflation of

    - An editor tag with corresponding text style
    - A user preference that will be added to the corresponding language page

    :param string lang: The language for which this style will be applicable
      . This is used to automatically store the preference associated with
      this style in the right preferences subcategory.

    :param string name: The name of the style, used to identify it.

    :param string label: The label that will be shown in the preferences
      dialog for this style.

    :param string doc: The documentation that will be shown in the preferences
      dialog for this style.

    :param foreground_colors: The foreground colors of the style, expressed as
      a tuple of two CSS-like strings, for example ("#224488", "#FF6677"). The
      first color is used for light themes, the second is used for dark themes
    :type foreground_colors: string, string

    :param  background_colors: The background colors of the style.
    :type background_colors: string, string

    :param string font_style: : The style of the font, one of "default",
          "normal", "bold", "italic" or "bold_italic"

    :param prio: The priority of the style. This determines which style will
      prevail if two styles are applied to the same portion of text. See
      :func:`Highlighter.region`
      -1 means default priority: tags added last have precedence.

    :rtype: highlighter.engine.Style
    """
    try:
        from highlighter.engine import Style, HighlighterModule
        import theme_handling
        from theme_handling import Color, transparent

        dark_bg_color = None
        light_bg_color = None

        if background_colors[0] == "transparent":
            light_bg_color = transparent
        else:
            light_bg_color = Color(background_colors[0])

        if background_colors[1] == "transparent":
            dark_bg_color = transparent
        else:
            dark_bg_color = Color(background_colors[1])

        style_id = "{0}_{1}".format(lang, name)
        pref_name = "Editor/Fonts & Colors:{0}/{1}".format(lang, name)
        pref = GPS.Preference(pref_name)
        pref.create_style(label, doc,
                          foreground_colors[0],
                          light_bg_color.to_rgba_string(),
                          font_style)

        theme_handling.variant_prefs[style_id] = pref_name
        theme_handling.common_dark[style_id] = (font_style.upper(),
                                                Color(foreground_colors[1]),
                                                dark_bg_color)

        theme_handling.common_light[style_id] = (font_style.upper(),
                                                 Color(foreground_colors[0]),
                                                 light_bg_color)
        pref.tag = None
        HighlighterModule.preferences[style_id] = pref
        return Style(style_id, prio, pref)
    except Exception:
        raise  # TODO: remove this exception handler, used for doc framework


def existing_style(pref_name, name="", prio=-1):
    """
    Creates a new style to apply when a matcher succeeds, using an existing
    style as a basis. This probably should not be used directly, but one
    should use one of the existing styles declared in
    :func:`Highlighter.common`

    :param string pref_name: The name of the preference to bind to the style
    :param string name: The name of the style, used for the underlying gtk tag
    :param int prio: The priority of the style compared to others. Higher
      priority styles will take precedence over lower priority ones.
      -1 means default priority: tags added last have precedence.
    :rtype: highlighter.engine.Style
    """
    try:
        from highlighter.engine import Style, HighlighterModule
        style_id = "{0}_hl".format(name if name else pref_name)
        pref = GPS.Preference(pref_name)
        pref.tag = None
        HighlighterModule.preferences[style_id] = pref

        return Style(style_id, prio, pref)
    except Exception:
        pass  # TODO: remove this exception handler, used for doc framework


def register_highlighter(language, spec, igncase=False):
    """
    Used to register the declaration of an highlighter. See the tutorial for
    more information

    :param string language: The language to be used as a filter for the
       highlighter.
    :param tuple spec: The spec of the highlighter.
    """
    from highlighter.engine import Highlighter, HighlighterModule
    HighlighterModule.highlighters[language] = Highlighter(spec, igncase)
