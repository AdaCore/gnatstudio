"""Provides support for some common URLs via the hyper mode in GPS:
   - http://
   - https://
   - file://
   - file:\\
"""



import GPS

# Define an action
def view_html(url):
    GPS.HTML.browse (url)

def view_file(url):
    GPS.MDI.get_by_child (
      GPS.EditorBuffer.get (GPS.File (url[7:])).current_view()).raise_window()

# Register a highlighter to launch a browser on http(s):// URLs
GPS.EditorHighlighter ("http(s)?://[^\s:,]*", view_html, 0, view_html)

# Register a highlighter to open a file on file:// URLs
GPS.EditorHighlighter (r'file:[\\/][\\/][^\s:,]*', view_file, 0, view_file)
