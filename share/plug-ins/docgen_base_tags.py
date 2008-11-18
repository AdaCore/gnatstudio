"""
Handling of documentation tags. This script can be used as is or as a basis for
 your own documentation tags.

The list of custom tags is:
- description
- summary
- parameter (attribute "name" is expected)
- exception
- seealso
- c_version
- group (this builds a custom index from all <group>A Group</group> tags)

"""

import GPS, re, os

class ParameterTagHandler (GPS.DocgenTagHandler):
   """Handling for <parameter name="param">Description of the paramter</parameter>"""
   def __init__ (self):
      GPS.DocgenTagHandler.__init__ (self, "parameter", on_match = self.on_match)

   def on_match (self, docgen, attrs, value, entity_name, entity_href):
      res = re.split ('name=\"([^"]*)\"', attrs)
      if len (res) > 1:
         return """<div class="parameter"><span class="name">%s :</span>%s</div>""" % (res[1], value)
      else:
         return """<div class="parameter" %s>%s</div>""" % (attrs, value)

class CVersionTagHandler (GPS.DocgenTagHandler):
   """Handling for <c_version>xx.yy</c_version>"""
   def __init__ (self):
      GPS.DocgenTagHandler.__init__ (self, "c_version", on_match = self.on_match)

   def on_match (self, docgen, attrs, value, entity_name, entity_href):
      return """<p>Binding from C File version <b>%s</b></p>""" % (value)

class GroupTagHandler (GPS.DocgenTagHandler):
   """Handling for <group>A Group of packages</group>"""
   def __init__ (self):
      GPS.DocgenTagHandler.__init__ \
        (self, "group",
         on_match=self.on_match,
         on_start=self.on_start,
         on_exit=self.on_exit)
      self.groups = {}

   def on_start (self, docgen):
      self.groups = {}

   def on_match (self, docgen, attrs, value, entity_name, entity_href):
      if not self.groups.has_key (value):
         self.groups[value] = []
      self.groups[value].append ("""<a href="%s">%s</a>""" % (entity_href,entity_name));
      return " "

   def on_exit (self, docgen):
      if len (self.groups) == 0:
         return

      # first print the right-side box containing the group index
      content=""
      content += """<div class='default' id='rightSide'>"""
      content += """<div id='rightSideInside'>"""
      content += """<div id='Index'>"""
      content += """<h2>Index</h2>"""
      content += """<ul>"""
      n = 0
      for group in sorted(self.groups.keys()):
         content += """<li><a href="#%d">%s</a></li>""" % (n, group)
         n += 1
      content += """</ul></div></div></div>"""

      content += """<div class="title">Widget Groups</div>"""
      n = 0
      for group in sorted(self.groups.keys()):
         content += """<p><a name="%d"></a><h1>%s</h1>\n""" % (n, group)
         n += 1
         for href in sorted(self.groups[group]):
            content += "%s<br/>\n" % (href)
         content += "</p>\n"

      if content != "":
         docgen.generate_index_file ("Widget groups", "groups.html", content);

def on_gps_start (hook):
   # use default tag handling for description, summary, exception and seealso
   GPS.Docgen.register_css (GPS.get_system_dir() + "share/gps/docgen2/support/tags.css");
   GPS.Docgen.register_tag_handler (GPS.DocgenTagHandler ("description"))
   GPS.Docgen.register_tag_handler (GPS.DocgenTagHandler ("summary"))
   GPS.Docgen.register_tag_handler (GPS.DocgenTagHandler ("exception"))
   GPS.Docgen.register_tag_handler (GPS.DocgenTagHandler ("seealso"))
   # register <parameter>, <c_version> and <group> tags.
   GPS.Docgen.register_tag_handler (ParameterTagHandler ())
   GPS.Docgen.register_tag_handler (CVersionTagHandler ())
   GPS.Docgen.register_tag_handler (GroupTagHandler ())

GPS.Hook ("gps_started").add (on_gps_start)
