"""
The mechanism here described allows any user to add syntax highlighting to GPS
for any language in a declarative domain specific language.

Tutorial: Add support for python highlighting in GPS
----------------------------------------------------

In this short tutorial, we will walk through the steps needed to create a
small plug-in for GPS that will allow it to highlight python code.

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

First step, creating a dumb highlighter
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As a first step, we will just create an highlighter that highlights the
self symbol in python, as a simple hello world.::

    register_highlighter(
        language="python",
        spec=(
            # Match self
            simple("self", tag="keyword"),
        )
    )

As we can see, the first step to register a new highlighter is to call the
:func:`register_highlighter` function, giving the name of the language and
the spec of the language as parameters.

The spec parameter is a tuple of matchers. In this case we have only one,
a simple matcher, as described above, which will match the "self" regexp,
and apply the "keyword" tag everytime it matches.

The style parameter is the name of the style you want to refer to. GPS has
a number of built-in styles, but they may not be sufficient, so the user
has the possibility of creating new styles, a capability that will will talk
about later on.

The built-in styles are: "keyword", "type", "block", "comment", "string",
"number".

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

API Documentation
-----------------

"""
from functools import partial

from highlighter.engine import Style, HighlighterModule, Highlighter, \
    RegionMatcher, RegionRef, SimpleMatcher
import GPS
import colorschemes


##############################
# Highlight classes creation #
##############################


def simple(regexp_string, tag):
    """
    Return a simple matcher for a regexp string

    :param str regexp_string: The regular expression for this matcher
    :rtype: SimpleMatcher
    """
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
           matchall=True):
    """
    Return a matcher for a region, which can contain a whole specific
    highlighter

    :param string start_re: The regexp used to match the start of the region
    :param string end_re: The regexp used to match the end of the region
    :param Style tag: The Tag which will be used to highlight the whole
      region. Beware, if you plan to apply other tags to elements inside the
      region, they must have an higher priority than this one !
    :rtype: RegionMatcher
    """
    return RegionMatcher(tag, start_re, end_re, highlighter, matchall, name)


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
    return RegionRef(name)


def new_style(lang, name, foreground_colors,
              background_colors=("white",  "white"),
              font_style="default", prio=20):
    """
    Creates a new style to apply when a matcher successfully matches a
    portion of text.

    :param string lang: The language for which this style will be applicable
      . This is used to automatically store the preference associated with
      this style in the right preferences subcategory.

    :param string name: The name of the style, as will be shown in the
      preferences.

    :param (string, string) foreground_colors: The foreground color of the
      style, expressed as a CSS-like string, for example "#FF6677".

    :param (string, string) background_colors: The background color of the
      style.

    :param string font_style: : The style of the font, one of "default",
          "normal", "bold", "italic" or "bold_italic"

    :param prio: The priority of the style. This determines which style will
      prevail if two styles are applied to the same portion of text. See
      :func:`Highlighter.region`

    :rtype: Style
    """
    style_id = "{0}_{1}".format(lang, name)
    pref_name = "Editor/{0}/{1}".format(lang, name)
    pref = GPS.Preference(pref_name)
    doc = "Style for '{0}'".format(name)
    pref.create_style(doc, doc, foreground_colors[0], background_colors[0],
                      font_style)

    colorschemes.dark_common[pref_name] = (font_style.upper(),
                                           foreground_colors[1],
                                           background_colors[1])

    colorschemes.light_common[pref_name] = (font_style.upper(),
                                            foreground_colors[0],
                                            background_colors[0])
    pref.tag = None
    HighlighterModule.preferences[style_id] = pref
    return Style(style_id, prio, pref)


def existing_style(pref_name, name="", prio=20):
    """
    Creates a new style to apply when a matcher succeeds, using an existing
    style as a basis. This probably should not be used directly, but one
    should use one of the existing styles declared in
    :func:`Highlighter.common`

    :param string pref_name: The name of the preference to bind to the style
    :param string name: The name of the style, used for the underlying gtk tag
    :param int prio: The priority of the style compared to others. Higher
      priority styles will take precedence over lower priority ones.
    :rtype: Style
    """
    style_id = "{0}_hl".format(name if name else pref_name)
    pref = GPS.Preference(pref_name)
    pref.tag = None
    HighlighterModule.preferences[style_id] = pref

    return Style(style_id, prio, pref)


def register_highlighter(language, spec):
    """
    Used to register the declaration of an highlighter. See the tutorial for
    more information

    :param string language: The language to be used as a filter for the
       highlighter.
    :param tuple spec: The spec of the highlighter.
    """
    HighlighterModule.highlighters[language] = Highlighter(spec)
