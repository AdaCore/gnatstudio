import re

"""
The purpose of this module is to provide an utility Color class, which
is to be shared between plug-ins. It is able to parse most CSS color formats,
to modify them internally, and to give back an hex / rgb(r,g,b) representation.
"""


def parse_color(strcol):
    # Constructor for string case
    # Hexadecimal format
    if strcol.startswith("#"):
        col = strcol[1:]

        # #RGB case
        if len(col) == 3:
            return tuple((int(c, 16) / 15.0 for c in col))

        # #RRGGBB case
        elif len(col) == 6:
            return tuple([int(col[a:a + 2], 16) / 255.0
                          for a in [0, 2, 4]])

    # rgb(%r,%g,%b) format
    elif strcol.startswith("rgb"):
        return tuple([
            int(n) / 255.0
            for n in re.findall(r"\((.+?)\)", strcol)[0].split(",")
        ])

    # strcol is a color name
    elif strcol.lower() in Color.colors:
        return parse_color(Color.colors[strcol])
    else:
        raise Exception("Can't parse color")


class Color(object):

    def __init__(self, strcol):
        # Constructor for tuple case
        if type(strcol) == tuple:
            self.col = strcol
        else:
            self.col = parse_color(strcol)

    def to_hex(self):
        return "#" + "".join(["%02X" % (c * 255) for c in self.col])

    def to_rgb(self):
        return "rgb(%s)" % ",".join([str(int(c * 255)) for c in self.col])

    def shade(self, amount):
        return Color(tuple((max(c - amount, 0) for c in self.col)))

    def lighten(self, amount):
        return Color(tuple((min(c + amount, 1) for c in self.col)))

    def shade_or_lighten(self, amount):
        square = lambda x: x * x
        t = 130.0 * 130.0 / (255.0 * 255.0)
        m = (square(self.col[0]) * 0.241 +
             square(self.col[1]) * 0.691 +
             square(self.col[2]) * 0.068)
        return (self.lighten if m < t else self.shade)(amount)

    def __repr__(self):
        return "<Color : {0}>".format(self.to_hex())

    def __str__(self):
        return self.to_hex()

Color.colors = {
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
