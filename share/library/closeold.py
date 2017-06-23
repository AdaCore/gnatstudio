"""
Automatically close old editors when a new one is opened.

This script can be used to keep only a limited number of editors
open at all times. It will automatically close the least recently
accessed editors when you open a new one.
"""

import GPS
from gps_utils import hook, interactive


maxeditors = GPS.Preference('Plugins/closeold/maxeditors')
maxeditors.create(
    'Maximum number of editors',
    'integer',
    'Automatically close least recently accessed editors when more'
    'than this number of editors are opened',
    8)


def __close_editors_if_needed(keep=set()):
    """
    If too many editors are opened, close one.

    :param set keep: a set of files that should not be closed, even if
       they are not pinned.
    """
    opened = GPS.EditorBuffer.list()
    toclose = len(opened) - maxeditors.get()

    for ed in reversed(opened):
        if toclose <= 0:
            break

        if (ed.file() not in keep and
                not ed.is_modified() and
                not getattr(ed, 'pinned', False)):

            GPS.Console().write(
                'Automatically closing %s\n' % ed.file().path)
            ed.close(force=False)
            toclose -= 1


@hook('file_edited')
def __on_file_edited(file):
    """
    A new file has just been opened.
    """
    __close_editors_if_needed(keep=set([file]))


@interactive(category='MDI', name='Pin Editor', filter='Source editor',
             menu='/File/(Un)pin Editor', before='Close',
             key='alt-p')
def __pin_file():
    """
    Prevent a file from being closed automatically.
    """
    ed = GPS.EditorBuffer.get(GPS.current_context().file(), open=False)
    if ed:
        ed.pinned = not getattr(ed, 'pinned', False)
        for v in ed.views():
            t = v.title().replace('^', '')
            if ed.pinned:
                GPS.MDI.get_by_child(v).rename('^%s' % t)
            else:
                GPS.MDI.get_by_child(v).rename(t)
