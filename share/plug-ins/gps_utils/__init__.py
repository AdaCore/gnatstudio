## This package contains various subprograms that wrap functions
## exported by GPS to make it easier to write plugins

from GPS import *
import types

def save_dir (fn):
   """Saves the current directory before executing the instrumented
      function, and restore it on exit. This is a python decorator which
      should be used as
          @save_dir
          def my_function ():
              ,,,
   """

   def do_work (*args, **kwargs):
      saved = pwd ()
      try:
         apply (fn, args, kwargs)
      finally:
         cd (saved)
   do_work.__name__ = fn.__name__   # Reset name
   do_work.__doc__ = fn.__doc__
   return do_work

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
      See also the with_save_excursion decorator below for cases when you
      need to apply save_excursion to a whole function.
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
     else:
        buffer.current_view ().goto (start.location ())
     start.delete ()
     end.delete ()

def with_save_excursion (fn):
   """A decorator with the same behavior as save_excursion.
      To use it, simply add @with_save_excursion before the definition of
      the function. This ensures that the current context will be restored
      when the function terminates
      example:
          @with_save_excursion
          def my_function():
            pass"""

   def do_work (*args, **kwargs):
      save_excursion (fn, args=args, kwargs=kwargs)
   do_work.__name__ = fn.__name__   # Reset name for interactive ()
   do_work.__doc__ = fn.__doc__
   return do_work

def make_interactive (callback, category="General", filter="", menu="", key="",
                      contextual="", name="", before="", after=""):
   """Declare a new GPS action (an interactive function, in Emacs talk),
      associated with an optional menu and default key. The description of
      the action is automatically taken from the documentation of the
      python function. Likewise the name of the action is taken from the
      name of the python function, unless specified with _name_.

      _callback_ is a python function that requires no argument, although it
      can have optional arguments (none will be set when this is called from
      the menu or the key shortcut). Alternatively, _callback_ can also be
      a class: when the user executes the action, a new instance of the class
      is created, so it is expected that the work is done in the __init__ of
      the class. This is in particular useful for classes that derive from
      CommandWindow.

      _menu_ is the name of a menu to associate with the action. It will be
      placed within its parent just before the item referenced as _before_,
      or after the item referenced as _after_"""

   if isinstance(callback, types.TypeType):  # Do we have a class ?
       cb = callback
       def do():
           cb()   # Create new instance
       do.__doc__ = callback.__doc__
       callback = do

   a = Action (name or callback.__name__)
   a. create (callback, filter=filter, category=category,
              description=callback.__doc__)
   if menu:
      if before:
         a.menu (menu, add_before=True, ref=before)
      else:
         a.menu (menu, add_before=False, ref=after)
   if contextual: a.contextual (contextual)
   if key:
      if menu:
         # Bind the key to the menu so that it is visible to the user
         Action (menu).key (key)
      else:
         a.key (key)

class interactive:
   """A decorator with the same behavior as make_interactive().
      This can be used to easily associate a function with an interactive
      action, menu or key, so that a user can conveniently call it.
      Example:
         @interactive ("Editor", menu="/Edit/Foo")
         def my_function ():
           pass"""

   def __init__ (self, category="General", filter="", menu="", key="",
                 contextual="", name="", before="", after=""):
       self.filter = filter
       self.category = category
       self.menu = menu
       self.key = key
       self.name = name
       self.contextual = contextual
       self.before = before
       self.after = after
   def __call__ (self, fn):
       make_interactive (fn, filter=self.filter, category=self.category,
                         menu=self.menu, key=self.key, after=self.after,
                         before=self.before,
                         contextual=self.contextual, name=self.name)
       return fn

############################################################
## Some predefined filters
## These are filters that can be used when creating new menus, contextual
## menus or actions
############################################################

def in_ada_file (context):
   """Returns True if the focus is currently inside an Ada editor"""
   if not hasattr (context, "in_ada_file"):
      buffer = EditorBuffer.get (open=False)
      context.in_ada_file =  buffer \
         and MDI.current() == MDI.get_by_child (buffer.current_view()) \
         and buffer.file ().language ().lower () == "ada"
   return context.in_ada_file

def is_writable (context):
   """Returns True if the focus is currently inside a writable editor"""
   if not hasattr (context, "is_writable"):
      buffer = EditorBuffer.get (open=False)
      context.is_writable =  buffer and not buffer.is_read_only ()
   return context.is_writable

def in_xml_file (context):
   """Returns True if the focus is in an XML editor"""
   if not hasattr (context, "in_xml_file"):
      buffer = EditorBuffer.get (open=False)
      context.in_xml_file = \
         MDI.current() == MDI.get_by_child (buffer.current_view()) \
         and buffer.file ().language ().lower () in ["xml", "html"]
   return context.in_xml_file
