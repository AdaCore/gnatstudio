"""
Handling of custom tags. This script can be used as is or as a basis for
 your own documentation tags.

The list of custom tags is:
- description
- summary
- parameter (attribute "name" is expected)
- exception
- seealso
- c_version
- group (this builds a custom index from all <group>A Group</group> tags)
- screenshot (this builds a custom index with all screenshots, and also
   display the actual image instead of the tag).
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

class ScreenshotTagHandler (GPS.DocgenTagHandler):
   """Handling for screenshots"""
   def __init__ (self):
      GPS.DocgenTagHandler.__init__ (
        self, "screenshot",
        on_match = self.on_match,
        on_start=self.on_start,
        on_exit=self.on_exit)

   def on_start (self, docgen):
      self.pictureslist = {}

   def on_match (self, docgen, attrs, value, entity_name, entity_href):
      file = docgen.get_current_file()
      dir = file.project().file().directory()+"../../share/doc/gtkada/gtkada_rm/"

      try:
         os.stat(dir+value)
         pict = value
      except:
         try:
            os.stat(dir+value+".png")
            pict = value + ".png"
         except:
            try:
               os.stat(dir+value+".jpg")
               pict = value + ".jpg"
            except:
               GPS.Console ("Messages").write ("could not find screenshot %s\n" % (value))
               return ""

      img = """<img src="%s%s" alt="%s" style="border: 0px;"/>""" % (dir, pict, pict)

      self.pictureslist[entity_name] = [entity_href, img]
      return """</div>
        <div class='profile'>
          <h3>Screenshot</h3>
          %s
        </div>
        <div class='comment'>""" % (img)

   def on_exit (self, docgen):
      content=""

      for pict in sorted(self.pictureslist.keys()):
         content += """
            <div class='subprograms'>
              <div class='class'>
                <h3>%s</h3>
                <div class='comment'>
                  <a href="%s">%s</a>
                </div>
              </div>
            </div>""" % (pict, self.pictureslist[pict][0], self.pictureslist[pict][1])

      if content != "":
         docgen.generate_index_file ("Widget Screenshots", "screenshots.html", content);

class GroupTagHandler (GPS.DocgenTagHandler):
   """Handling for <group>A Group of packages</group>"""
   def __init__ (self):
      GPS.DocgenTagHandler.__init__ \
        (self, "group",
         on_match=self.on_match,
         on_start=self.on_start,
         on_exit=self.on_exit)

   def on_start (self, docgen):
      self.groups = {}

   def on_match (self, docgen, attrs, value, entity_name, entity_href):
      if not self.groups.has_key (value):
         self.groups[value] = []
      self.groups[value].append ("""<a href="%s">%s</a>""" % (entity_href,entity_name));
      return " "

   def on_exit (self, docgen):
      content=""

      for group in sorted(self.groups.keys()):
         content += "<p><h1>%s</h1>\n" % (group)
         for href in self.groups[group]:
            content += "%s<br/>\n" % (href)
         content += "</p>\n"

      if content != "":
         docgen.generate_index_file ("Widget groups", "groups.html", content);

def on_gps_start (hook):
   # use default tag handling for description, summary, exception and seealso
   GPS.Docgen.register_tag_handler (GPS.DocgenTagHandler ("description"))
   GPS.Docgen.register_tag_handler (GPS.DocgenTagHandler ("summary"))
   GPS.Docgen.register_tag_handler (GPS.DocgenTagHandler ("exception"))
   GPS.Docgen.register_tag_handler (GPS.DocgenTagHandler ("seealso"))
   # register <parameter>, <c_version> and <group> tags.
   GPS.Docgen.register_tag_handler (ParameterTagHandler ())
   GPS.Docgen.register_tag_handler (CVersionTagHandler ())
   GPS.Docgen.register_tag_handler (GroupTagHandler ())
   GPS.Docgen.register_tag_handler (ScreenshotTagHandler ())

GPS.Hook ("gps_started").add (on_gps_start)
