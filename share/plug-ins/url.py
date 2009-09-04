"""Provides support for some common URLs via the hyper mode in GPS:
   - http://
   - https://
   - file://
   - file:\\
"""



import GPS

# Define an action
def view_url(url):
  if url.startswith ("file"):
    GPS.MDI.get_by_child (
      GPS.EditorBuffer.get (GPS.File (url[7:])).current_view()).raise_window()
  else:
    GPS.HTML.browse (url)

# Register a highlighter to URLs
GPS.EditorHighlighter (
  r'(file:[\\/][\\/][^\s]*|http(s)?://[^\s:,]*)', view_url, 0, view_url)
