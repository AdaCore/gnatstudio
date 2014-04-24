"""
This package is the public interface to the highlighting engine of GPS. The
public interface here described allows any user to add syntax highlighting
for any language in a declarative DSL.

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

"""
from functools import partial

from highlighter.engine import Style, HighlighterModule, Highlighter, \
    RegionMatcher, RegionRef, SimpleMatcher
import GPS


##############################
# Highlight classes creation #
##############################


def simple(regexp_string, tag=None):
    """
    Return a simple matcher for a regexp string
    :type regexp_string: str
    :type tag: Style
    """
    return SimpleMatcher(tag, regexp_string)


def words(words_list, **kwargs):
    """
    Return a matcher for a list of words
    @type words_list: str|list[str]
    """
    if type(words_list) is list:
        words_list = "|".join(words_list)

    return simple(r"\b(?:{0})\b".format(words_list), **kwargs)


def region(start_re, end_re, tag=None, name="", highlighter=(),
           matchall=True):
    """
    Return a matcher for a region, which can contain a whole specific
    highlighter
    :type start_re: string
    :type end_re: string
    :type tag: Style
    """
    return RegionMatcher(tag, start_re, end_re, highlighter, matchall, name)


def region_template(*args, **kwargs):
    return partial(region, *args, **kwargs)


def region_ref(name):
    return RegionRef(name)


def new_style(lang, name, foreground_color, background_color="white",
              font_style="default", prio=20):
    style_id = "{0}_{1}".format(lang, name)
    pref = GPS.Preference("Editor/{0}/{1}".format(lang, name))
    doc = "Style for '{0}'".format(name)
    pref.create_style(doc, doc, foreground_color, background_color, font_style)
    pref.tag = None
    HighlighterModule.preferences[style_id] = pref
    return Style(style_id, prio, pref)


def existing_style(pref_name, name="", prio=20):
    """
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


def register_highlighter(language, *args, **kwargs):
    highlighter = Highlighter(*args, **kwargs)
    HighlighterModule.highlighters[language] = highlighter
