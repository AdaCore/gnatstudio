""" This plugin provides interactive help for the GPS API using the
    "help" command in the Python and Shell consoles.
"""
import inspect
import GPS

default_help = help


def helpdoc(gps_class_name):
    import GNATStudio_doc

    items = gps_class_name.split(".")

    parent = GNATStudio_doc

    for item in items:
        if hasattr(parent, item):
            parent = getattr(parent, item)

    return parent.__doc__

    # no class found
    return "Could not find documentation for GPS class %s" % gps_class_name


def help(item):
    # If this item belongs to the GPS module, extract its documentation
    # from the module GPS_doc
    # Because of the bindings shenanigans if inspect can't resolve the module
    # try to search in GPS_doc
    module = inspect.getmodule(item)
    if module is None or module == GPS:
        class_name = None

        if inspect.isclass(item):
            class_name = item.__name__
        elif inspect.isclass(item):
            class_name = item.__class__.__name__
        else:
            # item might be a method of a class. No choice here but to iterate
            # over all classes in GPS and attempt to find the parent.
            item_name = item.__name__

            for class_name in dir(GPS):
                klass = getattr(GPS, class_name)
                if inspect.isclass(klass):
                    if hasattr(klass, item_name) and getattr(klass, item_name) == item:
                        class_name = klass.__name__ + "." + item_name
                        break

        if class_name:
            print(helpdoc(class_name))
            return

    # fallback on the default help function
    default_help(item)
