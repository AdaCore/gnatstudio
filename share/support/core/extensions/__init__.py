__author__ = 'amiard'
from extensions.private.xml import X
from GPS import *
import GPS

#########################################
# Decorators and auto submodules import #
#########################################


def extend_gps(kls):
    """
    :type kls: type
    """
    class_name = kls.__name__
    gps_class = getattr(GPS, class_name, None)

    if not gps_class:
        setattr(GPS, class_name, kls)
    else:
        for name, method in kls.__dict__.iteritems():
            if (hasattr(method, "override_gps_method")
                    or not hasattr(gps_class, name)):
                setattr(gps_class, name, method)

    return kls


def override_gps_method(method):
    method.override_gps_method = True
    return method


######################
# General extensions #
######################

@extend_gps
class EditorBuffer(object):
    def insert(self, loc_or_text, text=None):
        """
        Inserts some text in the buffer.

        :param EditorLocation loc_or_text: Either where to insert the text,
            or the text to insert in the buffer
        :type loc_or_text: string|EditorLocation

        :param string text: If the first passed parameter was a location,
            this is the text to be inserted. Else, it can be ignored.
        :type text: string|None

        .. seealso:: :func:`GPS.EditorBuffer.delete`
        """
        if isinstance(loc_or_text, EditorLocation):
            assert isinstance(text, str) or isinstance(text, unicode)
            self._insert_at_location(loc_or_text, text)
        else:
            text = loc_or_text
            assert isinstance(text, str) or isinstance(text, unicode)
            self._insert_at_location(self.current_view().cursor(),
                                     loc_or_text)


@extend_gps
class Icon(object):
    def __init__(self, icon_id, label, path, alt_menu=None,
                 alt_small_toolbar=None, alt_large_toolbar=None,
                 alt_local_toolbar=None, alt_button=None, alt_dnd=None,
                 alt_dialog=None):
        """
        :param string icon_id: The id of the icon
        :param string label: The label of the icon
        :param string path: The path of the default icon file
        :param string alt_menu: The path for the alternate menu icon file,
            typically in a 16x16 format.
        :param string alt_small_toolbar: The path for the alternate small
            toolbar icon file, typically in a 18x18 format.
        :param string alt_large_toolbar: The path for the alternate large
            toolbar icon file, typically in a 24x24 format.
        :param string alt_local_toolbar: The path for the alternate local
            toolbar icon file, typically in a 12x12 format.
        :param string alt_button: The path for the alternate button icon
            file, typically in a 20x20 format.
        :param string alt_dnd: The path for the alternate drag&drop
            operation icon file, typically in a 32x32 format.
        :param string alt_dialog: The path for the main image in a dialog,
            typically 48x48 pixels.
        """

        alts = [(alt_menu, "Icon_Size_Menu"),
                (alt_small_toolbar, "Icon_Size_Small_Toolbar"),
                (alt_large_toolbar, "Icon_Size_Large_Toolbar"),
                (alt_local_toolbar, "Icon_Size_Local_Toolbar"),
                (alt_button, "Icon_Size_Button"),
                (alt_dnd, "Icon_Size_Dnd"),
                (alt_dialog, "Icon_Size_Dialog")]

        x = X("icon", id=icon_id, file=path, label=label)
        self.x = x.children(X("alternate", file=a, size=s) for a, s in alts
                            if a is not None)
        GPS.parse_xml(X("GPS", X("stock", self.x)).with_header())


@extend_gps
class Language(object):

    @override_gps_method
    def __init__(self):
        """
        This constructor is provided to prevent the initialisation of any
        object of the Language class, because it is abstract. The
        consequence of this is that subclassers of Language must reimplement
        __init__ to avoid having an exception raised at instance construction
        time
        """
        raise NotImplementedError
