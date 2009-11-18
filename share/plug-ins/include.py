"""Provides support for C/C++ #include directives in hyper mode:
   #include <xxx.h>
"""



import GPS

# Callback for #include <xxx> URLs
def view_include(url):
  try:
    GPS.MDI.get_by_child (
      GPS.EditorBuffer.get (GPS.File (url)).current_view()).raise_window()
  except:
    pass

# Register a highlighter to #include
GPS.EditorHighlighter (
  r'#include[\s]+[<"](.*)[>"]', view_include, 1, view_include)
