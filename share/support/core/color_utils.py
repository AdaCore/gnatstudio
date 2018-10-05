"""Color handling.

The purpose of this module is to provide an utility Color class, which
is to be shared between plugins. The main usage currenly is to compute
an overlay color based on another color.
"""


class ColorError(Exception):
    pass


class Color(object):
    """Represent a Color."""

    def __init__(self, strcol):
        """Initialize a color.

        :param strcol: either a string representing a color in one of the
            following formats: "#RGB", "#RRGGBB", "rgb(int,int,int)",
            "color name" or a triple of floats between 0.0 and 1.0
        """
        if isinstance(strcol, tuple):
            if len(strcol) != 3 or \
                    not 0.0 <= strcol[0] <= 1.0 or \
                    not 0.0 <= strcol[1] <= 1.0 or \
                    not 0.0 <= strcol[2] <= 1.0:
                raise ColorError('invalid color: %s' % strcol)
            self.col = strcol
        else:
            # Ignore leading or trailing spaces and ensure that matching is
            # case insensitive
            norm_str_col = strcol.strip().lower()
            self.col = None

            if norm_str_col in self.colors:
                # check for color name
                norm_str_col = self.colors[norm_str_col]

            try:
                if norm_str_col.startswith("#"):
                    if len(norm_str_col) == 4:
                        # #RGB case
                        self.col = (int(norm_str_col[1], 16) / 15.0,
                                    int(norm_str_col[2], 16) / 15.0,
                                    int(norm_str_col[3], 16) / 15.0)

                    elif len(norm_str_col) == 6:
                        # #RRGGBB case
                        self.col = (int(norm_str_col[0:2], 16) / 255.0,
                                    int(norm_str_col[2:4], 16) / 255.0,
                                    int(norm_str_col[4:6], 16) / 255.0)

                elif norm_str_col.startswith("rgb(") and strcol.endswith(")"):
                    # rgb(%r,%g,%b) case
                    self.col = [int(c.strip()) / 255.0
                                for c in norm_str_col[4:-1].split(",")]
                    if len(self.col) != 3:
                        raise ColorError('Invalid color string: %s' % strcol)
            except Exception:
                self.col = None

            if self.col is None:
                raise ColorError("Invalid color string: %s" % strcol)

    @property
    def hex(self):
        """Hexadecimal representation of the color."""
        return "#" + "".join(["%02X" % (c * 255) for c in self.col])

    @property
    def rgb(self):
        """RGB representation of the color."""
        return "rgb(%s)" % ",".join([str(int(c * 255)) for c in self.col])

    @property
    def luminance(self):
        """Square of the perceived luminance."""
        return self.col[0] ** 2 * 0.241 + \
            self.col[1] ** 2 * 0.691 + \
            self.col[2] ** 2 * 0.068

    def shade(self, amount):
        """Get a darker version of a given color.

        :param amount: amount by which the luminance is reduced. If the amount
            is too big the return value will be black
        :type amount: float
        :return: a Color instance
        :rtype: Color
        """
        return Color(tuple((max(c - amount, 0) for c in self.col)))

    def lighten(self, amount):
        """Get a lighter version of a given color.

        :param amount: amount by which the luminance is incremented. If the
            amount is too big the return value will be white
        :type amount: float
        :return: a Color instance
        :rtype: Color
        """
        return Color(tuple((min(c + amount, 1) for c in self.col)))

    def shade_or_lighten(self, amount):
        """Compute a suitable overlay color.

        If the color is "dark" return a lighten version. If the
        color is "light" return darker version of the color.

        :param amount: amount by which the luminance is modified. If the amount
            is too big the return value will be black or white.
        :type amount: float
        :return: a new color
        :rtype: Color
        """
        # The square of the luminance of the "50%" grey
        grey50_luminance = 130.0 ** 2 / 255.0 ** 2

        if self.luminance < grey50_luminance:
            return self.lighten(amount)
        else:
            return self.shade(amount)

    def __repr__(self):
        return "<Color : {0}>".format(self.to_hex())

    def __str__(self):
        return self.to_hex()

    colors = {
        "aliceblue": 	"#F0F8FF",
        "antiquewhite": 	"#FAEBD7",
        "aqua": 	"#00FFFF",
        "aquamarine": 	"#7FFFD4",
        "azure": 	"#F0FFFF",
        "beige": 	"#F5F5DC",
        "bisque": 	"#FFE4C4",
        "black": 	"#000000",
        "blanchedalmond": 	"#FFEBCD",
        "blue": 	"#0000FF",
        "blueviolet": 	"#8A2BE2",
        "brown": 	"#A52A2A",
        "burlywood": 	"#DEB887",
        "cadetblue": 	"#5F9EA0",
        "chartreuse": 	"#7FFF00",
        "chocolate": 	"#D2691E",
        "coral": 	"#FF7F50",
        "cornflowerblue": 	"#6495ED",
        "cornsilk": 	"#FFF8DC",
        "crimson": 	"#DC143C",
        "cyan": 	"#00FFFF",
        "darkblue": 	"#00008B",
        "darkcyan": 	"#008B8B",
        "darkgoldenrod": 	"#B8860B",
        "darkgray": 	"#A9A9A9",
        "darkgreen": 	"#006400",
        "darkkhaki": 	"#BDB76B",
        "darkmagenta": 	"#8B008B",
        "darkolivegreen": 	"#556B2F",
        "darkorange": 	"#FF8C00",
        "darkorchid": 	"#9932CC",
        "darkred": 	"#8B0000",
        "darksalmon": 	"#E9967A",
        "darkseagreen": 	"#8FBC8F",
        "darkslateblue": 	"#483D8B",
        "darkslategray": 	"#2F4F4F",
        "darkturquoise": 	"#00CED1",
        "darkviolet": 	"#9400D3",
        "deeppink": 	"#FF1493",
        "deepskyblue": 	"#00BFFF",
        "dimgray": 	"#696969",
        "dimgrey": 	"#696969",
        "dodgerblue": 	"#1E90FF",
        "firebrick": 	"#B22222",
        "floralwhite": 	"#FFFAF0",
        "forestgreen": 	"#228B22",
        "fuchsia": 	"#FF00FF",
        "gainsboro": 	"#DCDCDC",
        "ghostwhite": 	"#F8F8FF",
        "gold": 	"#FFD700",
        "goldenrod": 	"#DAA520",
        "gray": 	"#808080",
        "green": 	"#008000",
        "greenyellow": 	"#ADFF2F",
        "honeydew": 	"#F0FFF0",
        "hotpink": 	"#FF69B4",
        "ivory": 	"#FFFFF0",
        "khaki": 	"#F0E68C",
        "lavender": 	"#E6E6FA",
        "lavenderblush": 	"#FFF0F5",
        "lawngreen": 	"#7CFC00",
        "lemonchiffon": 	"#FFFACD",
        "lightblue": 	"#ADD8E6",
        "lightcoral": 	"#F08080",
        "lightcyan": 	"#E0FFFF",
        "lightgoldenrodyellow": 	"#FAFAD2",
        "lightgray": 	"#D3D3D3",
        "lightgreen": 	"#90EE90",
        "lightpink": 	"#FFB6C1",
        "lightsalmon": 	"#FFA07A",
        "lightseagreen": 	"#20B2AA",
        "lightskyblue": 	"#87CEFA",
        "lightslategray": 	"#778899",
        "lightsteelblue": 	"#B0C4DE",
        "lightyellow": 	"#FFFFE0",
        "lime": 	"#00FF00",
        "limegreen": 	"#32CD32",
        "linen": 	"#FAF0E6",
        "magenta": 	"#FF00FF",
        "maroon": 	"#800000",
        "mediumaquamarine": 	"#66CDAA",
        "mediumblue": 	"#0000CD",
        "mediumorchid": 	"#BA55D3",
        "mediumpurple": 	"#9370DB",
        "mediumseagreen": 	"#3CB371",
        "mediumslateblue": 	"#7B68EE",
        "mediumspringgreen": 	"#00FA9A",
        "mediumturquoise": 	"#48D1CC",
        "mediumvioletred": 	"#C71585",
        "midnightblue": 	"#191970",
        "mintcream": 	"#F5FFFA",
        "mistyrose": 	"#FFE4E1",
        "moccasin": 	"#FFE4B5",
        "navajowhite": 	"#FFDEAD",
        "navy": 	"#000080",
        "oldlace": 	"#FDF5E6",
        "olive": 	"#808000",
        "olivedrab": 	"#6B8E23",
        "orange": 	"#FFA500",
        "orangered": 	"#FF4500",
        "orchid": 	"#DA70D6",
        "palegoldenrod": 	"#EEE8AA",
        "palegreen": 	"#98FB98",
        "paleturquoise": 	"#AFEEEE",
        "palevioletred": 	"#DB7093",
        "papayawhip": 	"#FFEFD5",
        "peachpuff": 	"#FFDAB9",
        "peru": 	"#CD853F",
        "pink": 	"#FFC0CB",
        "plum": 	"#DDA0DD",
        "powderblue": 	"#B0E0E6",
        "purple": 	"#800080",
        "red": 	"#FF0000",
        "rosybrown": 	"#BC8F8F",
        "royalblue": 	"#4169E1",
        "saddlebrown": 	"#8B4513",
        "salmon": 	"#FA8072",
        "sandybrown": 	"#F4A460",
        "seagreen": 	"#2E8B57",
        "seashell": 	"#FFF5EE",
        "sienna": 	"#A0522D",
        "silver": 	"#C0C0C0",
        "skyblue": 	"#87CEEB",
        "slateblue": 	"#6A5ACD",
        "slategray": 	"#708090",
        "snow": 	"#FFFAFA",
        "springgreen": 	"#00FF7F",
        "steelblue": 	"#4682B4",
        "tan": 	"#D2B48C",
        "teal": 	"#008080",
        "thistle": 	"#D8BFD8",
        "tomato": 	"#FF6347",
        "turquoise": 	"#40E0D0",
        "violet": 	"#EE82EE",
        "wheat": 	"#F5DEB3",
        "white": 	"#FFFFFF",
        "whitesmoke": 	"#F5F5F5",
        "yellow": 	"#FFFF00",
        "yellowgreen": 	"#9ACD32",
    }
