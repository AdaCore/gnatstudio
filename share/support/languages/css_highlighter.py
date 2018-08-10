from highlighter.common import (
    region_template, tag_string, hl_inside_strings, region, tag_comment,
    hl_comment_notes, simple, tag_keyword, tag_number, tag_type, words,
    tag_default, register_highlighter)


#################################################
# Keywords, properties and default values lists #
#################################################

properties_list = ["align-content", "align-items", "align-self",
                   "animation-delay", "animation-direction",
                   "animation-duration", "animation-fill-mode",
                   "animation-iteration-count", "animation-name",
                   "animation-play-state", "animation-timing-function",
                   "animation", "backface-visibility", "background-attachment",
                   "background-clip", "background-color", "background-image",
                   "background-origin", "background-position",
                   "background-repeat", "background-size", "background",
                   "border-bottom-color", "border-bottom-left-radius",
                   "border-bottom-right-radius", "border-bottom-style",
                   "border-bottom-width", "border-bottom", "border-collapse",
                   "border-color", "border-image-outset",
                   "border-image-repeat", "border-image-slice",
                   "border-image-source", "border-image-width",
                   "border-image", "border-left-color",
                   "border-left-style", "border-left-width", "border-left",
                   "border-radius", "border-right-color",
                   "border-right-style", "border-right-width",
                   "border-right", "border-spacing",
                   "border-style", "border-top-color",
                   "border-top-left-radius", "border-top-right-radius",
                   "border-top-style", "border-top-width",
                   "border-top", "border-width",
                   "border", "bottom", "box-shadow", "box-sizing",
                   "caption-side", "clear", "clip", "color", "column-count",
                   "column-fill", "column-gap", "column-rule",
                   "column-rule-color", "column-rule-style",
                   "column-rule-width", "column-span",  "column-width",
                   "columns", "content", "counter-increment", "counter-reset",
                   "cursor", "direction", "display", "empty-cells",
                   "filter", "flex-basis", "flex-direction", "flex-flow",
                   "flex-grow", "flex-shrink", "flex-wrap", "flex",
                   "float", "@font-face", "font-family", "font-size",
                   "font-size-adjust", "font-stretch", "font-style",
                   "font-variant", "font-weight", "font",
                   "hanging-punctuation", "height", "justify-content",
                   "@keyframes", "left",  "letter-spacing",
                   "line-height", "list-style-image",
                   "list-style-position", "list-style-type", "list-style",
                   "margin-bottom", "margin-left", "margin-right",
                   "margin-top", "margin", "max-height", "max-width",
                   "@media", "min-height", "min-width", "nav-down",
                   "nav-index", "nav-left", "nav-right",
                   "nav-up", "opacity", "order",
                   "outline-color", "outline-offset", "outline-style",
                   "outline-width", "outline", "overflow-x", "overflow-y",
                   "overflow", "padding-bottom", "padding-left",
                   "padding-right", "padding-top", "padding",
                   "page-break-after", "page-break-before",
                   "page-break-inside",   "perspective", "perspective-origin",
                   "position", "quotes", "resize", "right", "tab-size",
                   "table-layout",  "text-align-last", "text-align",
                   "text-decoration-color", "text-decoration-line",
                   "text-decoration-style", "text-decoration", "text-indent",
                   "text-justify",
                   "text-overflow", "text-shadow", "text-transform",
                   "top", "transform-origin", "transform-style",
                   "transform", "transition-delay", "transition-duration",
                   "transition-property", "transition-timing-function",
                   "transition", "unicode-bidi", "vertical-align",
                   "visibility", "white-space", "width",
                   "word-break", "word-spacing", "word-wrap", "z-index"]


html_elements_list = ["a", "abbr", "acronym", "abbr", "address", "applet",
                      "embed", "object", "area", "article", "aside",
                      "audio", "b", "base", "basefont", "bdi", "bdo",
                      "big", "blockquote", "body", "br", "button",
                      "canvas", "caption", "center", "cite", "code", "col",
                      "colgroup", "colgroup", "datalist", "dd", "del",
                      "details", "dfn", "dialog", "dir", "ul", "div", "dl",
                      "dt", "em", "embed", "fieldset", "figcaption", "figure",
                      "figure", "font", "footer", "form", "frame", "frameset",
                      "h1", "h6", "head", "header", "hr", "html", "i",
                      "iframe", "img", "input", "ins",
                      "kbd", "keygen", "label", "input", "legend",
                      "fieldset", "li", "link", "main", "map", "mark",
                      "menu", "menuitem", "meta", "meter", "nav",
                      "noframes",  "noscript", "object", "ol", "optgroup",
                      "option",  "output", "p", "param", "pre", "progress",
                      "q", "rp", "rt", "ruby", "s", "samp", "script",
                      "section", "select", "small", "source", "video",
                      "audio", "span", "strike", "del", "s", "strong", "style",
                      "sub", "summary", "details", "sup", "table", "tbody",
                      "td", "textarea", "tfoot", "th", "thead", "time",
                      "title", "tr", "track", "video", "audio", "tt",
                      "u", "ul", "var", "video", "wbr"]

length_units_list = "|".join(["em", "ex", "%", "px", "cm", "mm", "in", "pt",
                              "pc", "ch", "rem", "vh", "vw", "vmin", "vmax"])

border_types_list = ["none", "hidden", "dotted", "dashed", "solid", "groove",
                     "ridge", "inset", "outset", "initial", "inherit"]

color_names_list = ["aliceblue", "antiquewhite", "aqua", "aquamarine",
                    "azure", "beige", "bisque", "black", "blanchedalmond",
                    "blue", "blueviolet", "brown", "burlywood", "cadetblue",
                    "chartreuse", "chocolate", "coral", "cornflowerblue",
                    "cornsilk", "crimson", "cyan", "darkblue", "darkcyan",
                    "darkgoldenrod", "darkgray", "darkgreen", "darkkhaki",
                    "darkmagenta", "darkolivegreen", "darkorange",
                    "darkorchid", "darkred", "darksalmon",
                    "darkseagreen", "darkslateblue", "darkslategray",
                    "darkturquoise", "darkviolet", "deeppink",
                    "deepskyblue", "dimgray", "dodgerblue", "firebrick",
                    "floralwhite", "forestgreen", "fuchsia", "gainsboro",
                    "ghostwhite", "gold", "goldenrod", "gray", "green",
                    "greenyellow", "honeydew", "hotpink", "indianred",
                    "indigo", "ivory", "khaki", "lavender", "lavenderblush",
                    "lawngreen", "lemonchiffon", "lightblue", "lightcoral",
                    "lightcyan", "lightgoldenrodyellow", "lightgray",
                    "lightgreen", "lightpink", "lightsalmon", "lightseagreen",
                    "lightskyblue", "lightslategray", "lightsteelblue",
                    "lightyellow", "lime", "limegreen", "linen",
                    "magenta", "maroon", "mediumaquamarine", "mediumblue",
                    "mediumorchid", "mediumpurple", "mediumseagreen",
                    "mediumslateblue", "mediumspringgreen", "mediumturquoise",
                    "mediumvioletred", "midnightblue", "mintcream",
                    "mistyrose", "moccasin", "navajowhite",
                    "navy", "oldlace", "olive", "olivedrab", "orange",
                    "orangered", "orchid", "palegoldenrod", "palegreen",
                    "paleturquoise",  "palevioletred", "papayawhip",
                    "peachpuff", "peru", "pink", "plum", "powderblue",
                    "purple", "rebeccapurple", "red", "rosybrown",
                    "royalblue", "saddlebrown", "salmon", "sandybrown",
                    "seagreen", "seashell", "sienna", "silver", "skyblue",
                    "slateblue", "slategray", "snow", "springgreen",
                    "steelblue", "tan", "teal", "thistle", "tomato",
                    "turquoise", "transparent", "violet", "wheat", "white",
                    "whitesmoke", "yellow", "yellowgreen"]

#######################
# Matcher definitions #
#######################

string_template = region_template(tag=tag_string,
                                  highlighter=(hl_inside_strings,))

simple_quoted_strings = string_template(r"'", r"'",
                                        name="simple_quoted_strings")
double_quoted_strings = string_template(r'"', r'"',
                                        name="double_quoted_strings")

comments = region(r"/\*", r"\*/",
                  tag=tag_comment,
                  highlighter=(hl_comment_notes,))

number_literals = simple(r"\b[0-9]*\.?[0-9]+\b", tag=tag_number)

colors = simple(r"#[0-9-a-f-A-F]{3}(?:[0-9-a-f-A-F]{3})?\b", tag=tag_number)

length_units = simple(r"{0}".format(length_units_list), tag=tag_keyword)

length_values = region(r"\b[0-9]", r"\b", tag=tag_number,
                       highlighter=(length_units,))

properties = words(properties_list, tag=tag_type)

html_elements = words(html_elements_list, tag=tag_keyword)

color_names = words(color_names_list, tag=tag_string)

border_types = words(border_types_list, tag=tag_type)

class_or_id_identifiers = simple(r"(?:\.|#)(?:\w|-)+",
                                 tag=tag_type)

gtk_variables = simple(r"@(?:\w|-)+", tag=tag_default)

property_assignment = region(r":", r";",
                             tag=tag_default,
                             highlighter=(color_names,
                                          border_types,
                                          colors,
                                          number_literals,
                                          length_values,
                                          comments,
                                          simple_quoted_strings,
                                          double_quoted_strings,
                                          gtk_variables))

blocks = region(r"\{", r"\}",
                tag=tag_default,
                highlighter=(properties, property_assignment, comments))

#######################
# General highlighter #
#######################

register_highlighter(
    language="css",
    spec=(html_elements,
          number_literals,
          colors,
          class_or_id_identifiers,
          blocks,
          comments,
          simple_quoted_strings,
          double_quoted_strings))
