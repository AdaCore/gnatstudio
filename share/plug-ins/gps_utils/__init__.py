## This package contains various subprograms that wrap functions
## exported by GPS to make it easier to write plugins

from GPS import *

def save_excursion (f, args=[], kwargs=dict(), undo_group=True):
   """Save current buffer, cursor position and selection and execute f.
      (args and kwargs) are passed as arguments to f. They indicate that any
      number of parameters (named or unamed) can be passed in the usual way
      to save_excursion, and they will be transparently passed on to f.
      If undo_group is True, then all actions performed by f will be grouped
      so that the user needs perform only one single undo to restore previous
      start.
      Then restore the context as it was before, even in the case of abnormal
      exit.
      Example of use:
         def my_subprogram ():
            def do_work ():
               do actual work here
            save_excursion (do_work)
   """

   mdi    = MDI.current ()
   buffer = EditorBuffer.get ()
   view   = buffer.current_view ()
   cursor = view.cursor ()
   start  = buffer.selection_start ().create_mark ()
   end    = buffer.selection_end ().create_mark ()

   if undo_group: buffer.start_undo_group ()

   try:
     apply (f, args, kwargs)

   finally:
     if undo_group: buffer.finish_undo_group ()
     try:
        # View might have been destroyed
        mdi.raise_window ()
        view.goto (cursor)
     except:
        # In this case use the next view available if any
        view = buffer.current_view ()
        if not view: return

     if start.location () != end.location ():
        buffer.select (start.location (), end.location ())
     start.delete ()
     end.delete ()

