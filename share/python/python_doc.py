## This package provides support for:
##  - storing the documentation for any Python entity in a separate XML file.
##    This allows dynamia modification (post it for instance), easier
##    translation, and saves some memory since it doesn't need to be kept
##    in memory at all times, and once per language in addition.
##
##  - Dynamically generating HTML file to document python entities. In
##    particular, a predefined menu is added for the GPS extensions

import GPS, pydoc, os, inspect, pydoc
from xml.dom import minidom
from string import rstrip, lstrip


def generate_doc (entity):
  """Generate the documentation for a python entity dynamically.
     Return the name of the HTML file that was created.
     The documentation is generated in the object directory of the root
     project"""
  obj_dir = GPS.Project.root().object_dirs (recursive=False) [0]
  cwd = os.getcwd()
  os.chdir (obj_dir)
  pydoc.writedoc (entity)
  os.chdir (cwd)
  return obj_dir + "/" + entity.__name__ + ".html"


def browse_doc (entity):
  """Open a browser for the documentation relative to the specified entity"""
  GPS.HTML.browse (generate_doc (entity))

## Create a default menu for the python documentation
GPS.parse_xml("""
  <documentation_file>
       <shell lang="python">python_doc.browse_doc(GPS)</shell>
       <descr>GPS'extensions for Python</descr>
       <menu>/Help/Python extensions</menu>
       <category>Scripts</category>
    </documentation_file>""")


#####################################################################
##  No user-callable function below this point.
##  The following functions and classes are called implicitely whenever
##  you do a help() command
#####################################################################

def xml_get_full_string (node):
  """Concatenate all the text children of node into one single string"""
  str="" 
  for c in node.childNodes:
    if c.nodeType == node.TEXT_NODE:
        str=str + c.data
  return lstrip(str, '\n')

class XMLGetDoc:
  """Gives the possibility to store the help for any Python entity in a
     separate XML file, which is parsed on the fly whenever the documentation
     for an entity needs to be printed"""
  lastclass=""
  xmldoc=None

  def __init__ (self):
     self.xmldoc=minidom.parse(GPS.get_system_dir() + "share/gps/shell_commands.xml")

  def getdoc(self, object):
    """Read the documentation for object in the XML file"""
    try:
       module=object.__module__
    except:
       module=""

    try:
       klass=object.im_class.__name__
    except:
       klass=XMLGetDoc.lastclass


    try:
      for node in self.xmldoc.getElementsByTagName("shell_doc"):
         attrs=node.attributes
         if attrs != None:
           if attrs.has_key ("module"):
              matched=attrs["module"].value==module
           else:
              matched=module==""

           if attrs.has_key ("class"):
              matched=matched and attrs["class"].value==klass
           else:
              matched=matched and klass==""

           if attrs.has_key ("name"):
              matched=matched and attrs["name"].value==object.__name__
           else:
              matched=matched and object.__name__==""

           if matched:
              doc=""
              for child in node.getElementsByTagName("param"):
                 doc=doc + '\n' + child.attributes["name"].value + ':\t\t';
                 if child.attributes.has_key("default"):
                    doc = doc + "(default=\""
                    if child.attributes.has_key("default"):
                       doc = doc + child.attributes["default"].value
                    doc=doc + "\") "

                 doc = doc + xml_get_full_string (child)
                 
              for child in node.getElementsByTagName("return"):
                 doc = doc + '\n' + "returns\t\t" + xml_get_full_string (child)
       
              for child in node.getElementsByTagName("description"):
                 doc = doc + '\n' + xml_get_full_string (child)

              for child in node.getElementsByTagName("see_also"):
                 doc = doc + '\nSee also: '
                 if child.attributes.has_key ("module"):
                    doc = doc + child.attributes["module"].value + "."
                 if child.attributes.has_key ("class"):
                    doc = doc + child.attributes["class"].value + "."
                 doc = doc + child.attributes["name"].value + "()"

              for child in node.getElementsByTagName("example"):
                 if child.attributes.has_key("lang") \
                    and child.attributes["lang"].value == "python" :
                    doc = doc + '\n\nExample:\n' + xml_get_full_string(child)

              return doc
      return __oldgetdoc__(object)
    except:
      return __oldgetdoc__(object)



## These two wrappers are used to make sure that the call to getdoc() knows
## about the class name of the entity, otherwise it is lost in pydoc functions
## and thus we will not be able to find in the XML file the reference for the
## entity

class XMLTextDoc (pydoc.TextDoc):
   def docroutine (self, object, name=None, mod=None, cl=None):
       try:
          XMLGetDoc_Wrapper.current_class = object.im_class.__name__
       except:
          XMLGetDoc_Wrapper.current_class = ""
       return pydoc.TextDoc.docroutine (self, object, name, mod, cl)

class XMLHtmlDoc (pydoc.HTMLDoc):
    def docroutine(self, object, name=None, mod=None,
                   funcs={}, classes={}, methods={}, cl=None):
       try:
          XMLGetDoc_Wrapper.current_class = object.im_class.__name__
       except:
          XMLGetDoc_Wrapper.current_class = ""
       return pydoc.HTMLDoc.docroutine (self, object, name, mod, funcs, classes, methods, cl)


class XMLGetDoc_Wrapper:
   current_class = ""
   def __init__(self):
      self.doc = GPS.Help()
   def getdoc (self, object): 
      try:
         try:
            module=object.__module__ + '.'
            if module == None:
               ## Work around for limitation in older versions of pythons: no
               ## module could be associated to functions exported from C
	       if sys.version_info[0]+'.'+sys.version_info[1]+'.'+sys.version_info[2] < "2.3.3":
                   module="GPS."
               else:
                   module=""
         except:
            module=""

         if XMLGetDoc_Wrapper.current_class == "":
            klass = ""
         else:
            klass = XMLGetDoc_Wrapper.current_class + '.'

         return self.doc.getdoc (module+klass+object.__name__)
      except:
         return __oldgetdoc__(object)

   def __del__(self):
      self.doc.reset()

def writedoc(thing, forceload=0):
   """Wrapper around pydoc.writedoc to limit the number of times an XML file
      is parsed"""
   inspect.getdoc = XMLGetDoc_Wrapper().getdoc
   __oldwritedoc__ (thing, forceload)
   inspect.getdoc = __oldgetdoc__

def help (request=None):
   """Wrapper around pydoc.help to limit the number of times an XML file
      is parsed"""
   inspect.getdoc = XMLGetDoc_Wrapper().getdoc
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

