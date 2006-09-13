"""Enhanced documentation for python and GPS shell commands

This package provides support for:
 - storing the documentation for any Python entity in a separate XML file.
   This allows dynamic modification, easier
   translation, and saves some memory since it doesn't need to be kept
   in memory at all times, and once per language in addition.

 - Dynamically generating HTML file to document python entities. In
   particular, a predefined menu is added for the GPS extensions
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)


############################################################################
## No user customization below this line
############################################################################

import GPS, pydoc, os, inspect, pydoc, sys, re
from string import rstrip, lstrip, expandtabs

# The following classes provide a nice way to display HTML documentation.
# They are mostly useful for the GPS module, although they are expected to
# work on other modules as well

class DocGenerator:
   def __init__ (self):
      self.work_dir = "/tmp/pythondoc"

   def special_entries (self, module_name):
      """Return a list of tuples (name, url, generator) for special entries
         to add to the classes index for the module"""

      return [('Module description', 'file://' + self.filename_for_module (module_name), DocGenerator.doc_module),
              ('Global routines', 'file://' + self.filename_for_global (module_name), DocGenerator.doc_global_routines)]

   def classesindex (self, module_name, classes):
      """Return the index of classes for the module, in HTML format"""

      output = "<h2>Module <strong>" + module_name + \
        "</strong></h2><div id='classesIndex'><ul>\n"

      for s in self.special_entries (module_name):
         output += " <li><i><a href='" + s[1] + "'>" + s[0] + "</a></i></li>\n"

      for c in classes:
         output += "  <li><a href='file://" + self.filename_for_class (c[0]) + "'>" + c[0] + "</a></li>\n"
      return output + "</ul></div>\n"

   def classcontents (self, name, object):
      """Return an HTML description of the class contents"""

      methods = inspect.getmembers (object, inspect.isroutine)
      output  = "<h2>Methods in <strong>" + name + "</strong></h2>\n<div id='classMethods'><ul>\n"
      for m in methods:
         if self.is_visible_entity (m[0]):
            output += " <li><a href='#" + self.anchor_name (m[0]) + "'>" + m[0] + "</a></li>\n";
      return output + "</ul></div>\n"

   def global_routines (self, name, routines):
      """Return an HTML list of the global routines"""
      output = "<h2>Global routines in <strong>" + name + "</strong></h2>\n<div id='classMethods'><ul>\n"
      for r in routines:
          if self.is_visible_entity (r[0]):
              output += "  <li><a href='#" + self.anchor_name (r[0]) + "'>" + r[0]
      return output + "</ul></div>\n"

   def setup_links (self, module_name, text):
      """Replace all links like module_name.<...> by hyperlinks"""

      pattern = re.compile (module_name + r'\.(\w+)(\.(\w+))?')
      here = 0
      result = []
      linked_module = "<a href='file://" + self.filename_for_module (module_name) + "'>" + module_name + "</a>."

      while True:
         match = pattern.search (text, here)
         if not match: break
         start, end = match.span ()
         result.append (text[here:start])

         name1, foo, name2 = match.groups()
         if foo:
            result.append (linked_module + "<a href='file://" + self.filename_for_class (name1) + "'>" + name1 \
              + "</a>.<a href='file://" + self.filename_for_class (name1) + "#" + self.anchor_name (name2) + "'>" + name2 + "</a>")
         else:
            result.append (linked_module + "<a href='file://" + self.filename_for_class (name1) + "'>" + name1 + "</a>")

         here = end
      result.append (text[here:])
      return ''.join (result)

   def get_documentation (self, object):
      """Return the documentation for the object. This can easily be overriden if
         the documentation is dynamic for instance (a result of a method's call"""
      return inspect.getdoc (object) or inspect.getcomments (object)

   def get_formated_documentation (self, module_name, object):
      """Same as get_documentation, except new lines that should be protected
         are also protected in the output of get_formated_documentation. We
         cannot replace all < or > symbols, since doc already contains the HTML
         formating which we need to preserve"""
      doc = self.get_documentation (object) or ""
      doc = "&amp;".join (doc.split ('&'))
      doc = "&lt; ".join (doc.split ('< '))
      doc = "&lt;".join (doc.split ('<<'))
      doc = "&lt;,".join (doc.split ('<, '))
      doc = "&gt; ".join (doc.split ('> '))
      doc = "&gt;".join (doc.split ('>>'))
      doc = "&gt;,".join (doc.split ('>,'))
      doc = "<p>\n".join (doc.split ("\n\n"))

      return self.setup_links (module_name, doc)

   def full_name (self, object):
      """Return the fully qualified name of object"""
      module = inspect.getmodule (object)
      if module == None:
         module = ""
      else:
         module = module.__name__ + "."

      try:
         c = object.im_class.__name__ + "."
      except:
         c = ""

      return module + c + object.__name__

   def documentation (self, module_name, object, document_members = 1):
      """Return the HTML documentation for the object"""

      doc = self.get_formated_documentation (module_name, object)
      output = "<div class='documentation'><h1>" + object.__name__ + \
          "</h1>\n<table class='description'>" + doc

      if inspect.isclass (object):
        mro     = inspect.getmro (object)
        if len (mro) > 1:
           output += "<tr><td colspan='3' class='title'>Inheritance tree</td></tr>" \
              + "<tr><td colspan='3'><ul>\n"
           for base in mro:
               output += "  <li>" + self.full_name (base) + "</li>\n"
           output += "</ul></td></tr>"

      output += "</table>\n"

      if document_members == 1:
         attrs   = inspect.classify_class_attrs (object)
         methods = inspect.getmembers (object, inspect.isroutine)
         for m in methods:
           for a in attrs:
             if a[0] == m[0]:
                output += "<a name='" + self.anchor_name (m[0]) + "'/>\n" + \
                   "<table class='description'><caption>" + m[0]

                if a[2] != object:
                   output += " (inherited " + a[1] + " from " \
                     + self.full_name (a[2]) \
                     + ")</caption></table>"
                else:
                   if a[1] == 'static method':
                      output += " (static method)"
                   elif a[1] == 'class method':
                      output += " (class method)"
                   output += "</caption>" \
                      + self.get_formated_documentation (module_name, m[1]) \
                      + "</table>\n"

      return output + "</div>\n"

   def is_visible_entity (self, name):
      """Whether the entity is visible and should be documented"""
      if name in ['__builtins__', '__doc__', '__file__', '__path__',
                  '__module__', '__name__']: return 0
      # Private names are hidden, but special names are displayed.
      if name.startswith('__') and name.endswith('__'): return 1
      return not name.startswith('_')

   def html_header (self, module_name):
      """Return the HTML header to use for generated files"""

      return "<html><head><title>Module " + module_name + \
           "</title><style type='text/css'>\n" + \
           self.style_sheet () + "\n</style></head><body>\n"

   def style_sheet (self):
      """Return the style sheet to use for the generated files"""

      return """
body              { background-color: white;
                    width: 1000px;
                    font-family: tahoma, helvetica; }
div.menu          { width: 200px;
                    top:   8px;
                    float: left;
                    padding: 0px;
                    margin: 0px;
                    margin-left: 3px; }
div.menu h2       { background: #369;
                    color: white;
                    font-size: small;
                    margin: 1em 0 0 0;
                    padding: 2px; }
div.menu div      { border: 1px solid #369;
                    border-top-width: 0px;
                    padding: 5px;
                    background: #EEE;
                    font-size: small;
                    margin-bottom: 15px; }
div.menu ul       { color: #369;
                    margin: 3px 0px 0px 20px;
                    padding: 0px }
div.menu li       { margin-top: 3px; }
div.documentation { position: relative;
                    top: 8px;
                    margin-left: 210px;
                    height: 100% }
div.documentation h1 { font-size: x-large;
                       text-align: center }
div.documentation h2 { font-size: large;
                       font-weight: bold;
                       background-color: #369;
                       color: white;
                       text-align: center; }
table.description            { border: 1px solid black;
                               width: 100% }
table.description caption     { background-color: #369;
                                width: 100%;
                                font-size: large;
                                font-weight: bold;
                                margin-top: 20px;
                                color: white;
                                white-space: pre;
                                text-align: center; }
table.description .title   { background-color: #dddddd; }
table.description td.header   { font-style: italic; }
table.description td.example { font-family: "Courier New", "Courier";
                               background-color: #cccccc; }
table.description .name    { font-family: "Courier New", "Courier";
                             font-weight: bold; }
table.description .return  { font-style: italic; }
table.description .default { font-family:   "Courier New", "Courier"; }
table.description td.seeAlso { font-family: "Courier New", "Courier"; }
table.description .obsolescent { color: red }
.example                       { white-space: pre }
"""

   def html_footer (self):
      """Return the HTML footer to use for generated files"""

      return """</body></html>"""

   def filename_for_class (self, class_name):
      """Return the file name that contains the documentation for class_name"""

      return self.work_dir + "/" + class_name.lower() + ".html"

   def filename_for_global (self, module_name):
      """Return the file name to use to document the global routines"""

      return self.work_dir + "/" + module_name.lower() + "_Globals.html"

   def filename_for_module (self, module_name):
      """Return the file name to use for the module description"""
      return self.work_dir + "/" + module_name.lower() + "_Module.html";

   def anchor_name (self, method_name):
      """Return the name of the anchor used for that method"""

      return method_name.lower()

   def doc_global_routines (self, module, classes):
      """Generate the documentation for the global routines"""
      routines = inspect.getmembers (module, inspect.isroutine)
      f = file (self.filename_for_global (module.__name__), "w")
      f.write (self.html_header (module.__name__))
      f.write ("<div class='menu'>\n")
      f.write (self.classesindex (module.__name__, classes))
      f.write (self.global_routines (module.__name__, routines))
      f.write ("</div><div class='documentation'>\n")
      for r in routines:
         if self.is_visible_entity (r[0]):
            f.write ("<a name='" + self.anchor_name (r[0]) + "'/>\n" + \
               "<table class='description'><caption>" + r[0] + "</caption>\n" \
               + self.get_formated_documentation (module.__name__, r[1]) \
               + "</table>")
      f.write ("</div>")
      f.write (self.html_footer ())
      f.close ()

   def doc_module (self, module, classes):
      """Generate the documentation for the module itself"""
      classes  = inspect.getmembers (module, inspect.isclass)
      f = file (self.filename_for_module (module.__name__), "w")
      f.write (self.html_header(module.__name__))
      f.write ("<div class='menu'>\n")
      f.write (self.classesindex (module.__name__, classes))
      f.write ("</div>\n")
      f.write (self.documentation (module.__name__, module, 0))
      f.write (self.html_footer())
      f.close()

   def doc_classes (self, module, classes):
      """Generate the documentation for all classes in the module"""
      for c in classes:
         f = file (self.filename_for_class (c[0]), "w")
         f.write (self.html_header(module.__name__))
         f.write ("<div class='menu'>\n")
         f.write (self.classesindex (module.__name__, classes))
         f.write (self.classcontents (c[0], c[1]))
         f.write ("</div>\n")
         f.write (self.documentation (module.__name__, c[1]))
         f.write (self.html_footer())
         f.close ()

   def html_documentation (self, module):
      try: os.mkdir (self.work_dir)
      except:pass

      classes  = inspect.getmembers (module, inspect.isclass)
      self.doc_classes (module, classes)
      for e in self.special_entries (module.__name__):
         e[2] (self, module, classes)

      return self.filename_for_module (module.__name__)

class GPSDocGenerator (DocGenerator):
    """Class specifically setup to dynamically create the documentation for GPS's exported objects"""

    def __init__ (self, work_dir):
        self.work_dir = work_dir
        self.wrapper  = Help_Wrapper ()

    def get_documentation (self, object):
        Help_Wrapper.set_current_class (object)
        return self.wrapper.getdoc_from_gps (object, as_html=1)

    def filename_for_hooks (self, module_name):
        """Return the file name to use for the hooks description"""
        return self.work_dir + "/" + module_name.lower() + "_Hooks.html"

    def doc_hooks (self, module, classes):
        """Generate the documentation for hooks"""
        f = file (self.filename_for_hooks (module.__name__), "w")
        f.write (self.html_header (module.__name__))
        f.write ("<div class='menu'>\n")
        f.write (self.classesindex (module.__name__, classes))
        f.write ("</div><div class='documentation'><table class='description'>\n")
        f.write (self.get_formated_documentation (module.__name__, "@hooks_list@"))

        f.write ("</table>")
        list = GPS.Hook.list()
        list.sort()
        for hook in list:
          f.write ("<table class='description'>\n")
          f.write ("  <caption>" + hook + "</caption>\n")
          f.write (self.get_formated_documentation (module.__name__, "@hook@ " + hook))
          f.write ("</table>")

        f.write ("</div>\n" + self.html_footer ())
        f.close ()

    def special_entries (self, module_name):
        return DocGenerator.special_entries (self, module_name) + \
            [('Hooks', 'file://' + self.filename_for_hooks (module_name),
               GPSDocGenerator.doc_hooks)]

#####################################################################
##  No user-callable function below this point.
##  The following functions and classes are called implicitely whenever
##  you do a help() command, and override the behavior of pydoc.py
#####################################################################

def subst_spaces(matchobj):
  len=matchobj.end() - matchobj.start()
  return '\n' + ('&nbsp;' * len)

def replace_spaces (text):
  return re.sub (re.compile (r'\n[ \t]+', re.MULTILINE), subst_spaces, text)

## These two wrappers are used to make sure that the call to getdoc() knows
## about the class name of the entity, otherwise it is lost in pydoc functions
## and thus we will not be able to find in the XML file the reference for the
## entity

class XMLTextDoc (pydoc.TextDoc):
   def document(self, object, name=None, *args):
       Help_Wrapper.set_current_class (object)
       return pydoc.TextDoc.document(self, object, name, *args)

class XMLHtmlRepr (pydoc.HTMLRepr):
   def escape (self, text):
       text = "&amp;".join (text.split ('&'))
       text = "&lt; ".join (text.split ('< '))
       text = "&lt;,".join (text.split ('<, '))
       text = "&gt; ".join (text.split ('> '))
       text = "&gt;,".join (text.split ('>,'))
       return text

class XMLHtmlDoc (pydoc.HTMLDoc):
    pydoc.HTMLDoc._repr_instance = XMLHtmlRepr()
    pydoc.HTMLDoc.escape = pydoc.HTMLDoc._repr_instance.escape
    pydoc.HTMLDoc.repr   = pydoc.HTMLDoc._repr_instance.repr

    def page (self, title, contents):
       return '''
<!doctype html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html><head><title>%s</title>
<style>
body         { background-color:#f0f0f8;
               font-family: Tahoma, helvetica, arial;
               width: 1000px;  }
table.description            { border: 1px solid black;
                               width: 100%% }
table.description td.title   { background-color: #dddddd;
                               text-align: center; }
table.description td.example { font-family: "Courier New", "Courier";
                               background-color: #cccccc; }
table.description .name    { font-family: "Courier New", "Courier";
                        font-weight: bold; }
table.description .return  { font-style: italic; }
table.description .default { font-family: "Courier New", "Courier"; }
table.description td.seeAlso { font-family: "Courier New", "Courier"; }
table.description .obsolescent { color: red }
</style>
</head><body bgcolor="#f0f0f8">
%s
</body></html>''' % (title, contents)

    ## Rename this preformat if we want to have narrower paragraphs. Otherwise,
    ## all blank spaces are non breakable, which is ugly
    def preformat2 (self, text):
       text = self.escape (expandtabs (text))
       return pydoc.replace (text, '\n\n', '\n \n', '\n\n', '\n \n',
                             '\n', '<br>\n')

    def preformat (self, text):
       text = self.escape (expandtabs (text))
       return pydoc.replace (replace_spaces (text), '\n\n', '\n \n',
                             '\n\n', '\n \n',
                             '\n', '<br>\n')

    def document(self, object, name=None, *args):
       Help_Wrapper.set_current_class (object)
       return pydoc.HTMLDoc.document (self, object, name, *args)

class Help_Wrapper:
   current_class = ""
   def __init__(self):
      self.doc = GPS.Help()

   def set_current_class(object):
      try:
         Help_Wrapper.current_class = object.im_class.__name__ + "."
      except:
         Help_Wrapper.current_class = ""
   set_current_class = staticmethod (set_current_class)

   def getdoc_from_gps (self, object, as_html):
      try:

         ## If we directly have a string, use it as the name to look up in the
         ## XML file

         if type (object) == type (""):
            return self.doc.getdoc (object, 1)
      except:
         return ""

      try:
         try:
            ## The __doc__ string for static methods is the fully qualified
            ## name of the entity -- if it was exported from GPS
            doc=object.__doc__
            if doc:
               static_doc = self.doc.getdoc (doc, 1)
               if static_doc:
                  return static_doc
               return doc
         except:
            pass

         try:
            module=object.__module__
            if module == None:
               ## Work around for limitation in older versions of pythons: no
               ## module could be associated to functions exported from C
	       if `sys.version_info[0]`+'.'+`sys.version_info[1]`+'.'+`sys.version_info[2]` < "2.3.3":
                   module="GPS."
               else:
                   module=""
            else:
               module = module + '.'
         except:
            module=""

         ## Special case for classes
         if object.__name__ + '.' == Help_Wrapper.current_class:
            name = module + object.__name__
         else:
            name = module + Help_Wrapper.current_class + object.__name__

         return self.doc.getdoc (name, as_html)
      except:
         return __oldgetdoc__(object)

   def getdoc (self, object):
       """Same as getdoc_from_gps, but the result is encapsulated in a <table>"""
       return "<table class='description'>" \
         + self.getdoc_from_gps (object, as_html=1) + "</table>"

   def getdoc_nohtml (self, object):
       return self.getdoc_from_gps (object, as_html=0)

def writedoc(thing, forceload=0):
   """Wrapper around pydoc.writedoc to limit the number of times an XML file
      is parsed"""
   inspect.getdoc = Help_Wrapper().getdoc_nohtml
   __oldwritedoc__ (thing, forceload)
   inspect.getdoc = __oldgetdoc__

def help (request=None):
   """Wrapper around pydoc.help to limit the number of times an XML file
      is parsed"""
   inspect.getdoc = Help_Wrapper().getdoc_nohtml
   __oldhelp__(request)
   inspect.getdoc = __oldgetdoc__


## Override standard methods to call our own

__oldwritedoc__     = pydoc.writedoc
__oldhelp__         = pydoc.help
__oldgetdoc__       = inspect.getdoc
pydoc.help          = help
pydoc.writedoc      = writedoc
pydoc.text          = XMLTextDoc()
pydoc.html          = XMLHtmlDoc()


####################################################################
## User accessible methods
####################################################################

docgen = GPSDocGenerator (GPS.get_home_dir() + "generated_doc")

def generate_doc (entity):
  """Generate the documentation for a python entity dynamically.
     Return the name of the HTML file that was created.
     The documentation is generated in the object directory of the root
     project"""
  GPS.set_busy()

  ## Generate the documentation for our own module
  name = docgen.html_documentation (entity)

  ## These comment lines are for use through pydoc
  #home_dir = GPS.get_home_dir()
  #name = home_dir + os.sep + entity.__name__ + ".html"
  #if not os.path.isfile (name) or os.stat (name).st_mtime < os.stat (GPS.Help().file()).st_mtime:
  #   cwd = os.getcwd()
  #   os.chdir (home_dir)
  #   pydoc.writedoc (entity)
  #   os.chdir (cwd)

  GPS.unset_busy()
  return name

def browse_doc (entity):
  """Open a browser for the documentation relative to the specified entity"""
  ## Use a hook, so that users can substitute their internal browser if they wish
  GPS.Hook ("html_action_hook").run (generate_doc (entity), 1, "")

def on_gps_started (hook_name):
  """Creates the menus for this package"""
  GPS.parse_xml("""
  <documentation_file>
       <shell lang="python">python_doc.browse_doc(GPS)</shell>
       <descr>GPS'extensions for Python</descr>
       <menu before="About">/Help/Python extensions</menu>
       <category>Scripts</category>
    </documentation_file>""")

GPS.Hook ("gps_started").add (on_gps_started)


