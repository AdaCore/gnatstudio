## This package provides support for:
##  - storing the documentation for any Python entity in a separate XML file.
##    This allows dynamia modification (post it for instance), easier
##    translation, and saves some memory since it doesn't need to be kept
##    in memory at all times, and once per language in addition.
##
##  - Dynamically generating HTML file to document python entities. In
##    particular, a predefined menu is added for the GPS extensions

import GPS, pydoc, os, inspect, pydoc, sys
from string import rstrip, lstrip

def generate_doc (entity):
  """Generate the documentation for a python entity dynamically.
     Return the name of the HTML file that was created.
     The documentation is generated in the object directory of the root
     project"""
  home_dir = GPS.get_home_dir()
  name = home_dir + os.sep + entity.__name__ + ".html"
  if not os.path.isfile (name) or os.stat (name).st_mtime < os.stat (GPS.Help().file()).st_mtime:
     GPS.set_busy()
     cwd = os.getcwd()
     os.chdir (home_dir)
     pydoc.writedoc (entity)
     os.chdir (cwd)
     GPS.unset_busy()
  return name


def browse_doc (entity):
  """Open a browser for the documentation relative to the specified entity"""
  ## Use a hook, so that users can substitute their internal browser if they wish
  GPS.Hook ("html_action_hook").run (GPS.File (generate_doc (entity)), 1, "")

## Create a default menu for the python documentation
GPS.parse_xml("""
  <documentation_file>
       <shell lang="python">python_doc.browse_doc(GPS)</shell>
       <descr>GPS'extensions for Python</descr>
       <menu before="About">/Help/Python extensions</menu>
       <category>Scripts</category>
    </documentation_file>""")

#####################################################################
##  No user-callable function below this point.
##  The following functions and classes are called implicitely whenever
##  you do a help() command
#####################################################################

## These two wrappers are used to make sure that the call to getdoc() knows
## about the class name of the entity, otherwise it is lost in pydoc functions
## and thus we will not be able to find in the XML file the reference for the
## entity

class XMLTextDoc (pydoc.TextDoc):
   def document(self, object, name=None, *args):
       Help_Wrapper.set_current_class (object)
       return pydoc.TextDoc.document(self, object, name, *args)

class XMLHtmlDoc (pydoc.HTMLDoc):
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

   def getdoc (self, object): 
      try:
         try:
            ## The __doc__ string for static methods is the fully qualified
            ## name of the entity -- if it was exported from GPS
            doc=object.__doc__
            if doc:
               static_doc = self.doc.getdoc (doc)
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

         return self.doc.getdoc (name)
      except:
         return __oldgetdoc__(object)

def writedoc(thing, forceload=0):
   """Wrapper around pydoc.writedoc to limit the number of times an XML file
      is parsed"""
   inspect.getdoc = Help_Wrapper().getdoc
   __oldwritedoc__ (thing, forceload)
   inspect.getdoc = __oldgetdoc__

def help (request=None):
   """Wrapper around pydoc.help to limit the number of times an XML file
      is parsed"""
   inspect.getdoc = Help_Wrapper().getdoc
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

