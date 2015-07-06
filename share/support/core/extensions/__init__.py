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
            if not hasattr(gps_class, name):
                setattr(gps_class, name, method)
            elif hasattr(method, "override_gps_method"):
                # Save the gps internal method (can be useful to call it in the
                # new implementation for example)
                original_method = getattr(gps_class, name)
                backup_name = "_internal_" + name
                setattr(gps_class, backup_name, original_method)

                # Add the new method
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
        if isinstance(loc_or_text, GPS.EditorLocation):
            assert isinstance(text, str) or isinstance(text, unicode)
            self._insert_at_location(loc_or_text, text)
        else:
            text = loc_or_text
            assert isinstance(text, str) or isinstance(text, unicode)
            self._insert_at_location(self.current_view().cursor(),
                                     loc_or_text)

    def entity_under_cursor(self):
        """
        Shortcut to return a `GPS.Entity` instance corresponding to the entity
        under cursor

        :rtype: :class:`GPS.Entity`
        """
        return self.main_cursor().location().entity()


@extend_gps
class Cursor(object):
    def location(self):
        """
        Returns the cursor's location
        :rtype: :class:`GPS.EditorLocation`
        """
        return self.mark().location()


@extend_gps
class EditorLocation(object):
    def get_word(self):
        """
        This will return the word that contains this location, if there
        is one, the empty string otherwise. This is a shortcut method that uses
        the inside_word, starts_word and ends_word methods of
        `GPS.EditorLocation`.

        :returns: A tuple (word, start location, end location)
        :rtype: (str, :class:`GPS.EditorLocation`, :class:`GPS.EditorLocation`)
        """

        word = ""
        start_loc = None
        end_loc = None

        if self.inside_word():
            start_loc = self
            while not start_loc.starts_word():
                start_loc = start_loc.forward_char(-1)

            end_loc = self
            while not end_loc.ends_word():
                end_loc = end_loc.forward_char()

            word = self.buffer().get_chars(start_loc, end_loc).strip()
            word = word.decode("utf8")  # make unicode-string

        return word, start_loc, end_loc

    def entity(self):
        """
        Shortcut to return a `GPS.Entity` instance at the given location

        :rtype: :func:`GPS.Entity`
        """
        word, start_loc, end_loc = self.get_word()
        return GPS.Entity(word, self.buffer().file(),
                          start_loc.line(), start_loc.column())


@extend_gps
class BuildTarget(object):

    @override_gps_method
    def __init__(self, name):
        self.target_name = name
        self._internal___init__(name)

    @override_gps_method
    def execute(self, main_name='', file=None, force=False,
                extra_args='', build_mode='', synchronous=True,
                directory='', quiet=False, on_exit=None):

        import workflows as wf

        if self.target_name in wf.workflows_target_name_set:
            idt = (self.target_name, main_name)
            if idt in wf.exit_handlers_table:
                GPS.Logger("BUILDTARGET").log(
                    "Workflow {} already in execution".format(idt)
                )
                return
            wf.exit_handlers_table[idt] = on_exit
            on_exit = None

        # Call the internal execute with given parameters
        self._internal_execute(
            main_name, file, force, extra_args, build_mode,
            synchronous, directory, quiet, on_exit
        )


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
