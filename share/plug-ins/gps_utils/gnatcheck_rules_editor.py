import GPS, os, os.path, re, string, pygtk, traceback, os_utils, sys
import sys, gps_utils.gnat_rules
from gps_utils.switches import *
from gps_utils.gnatcheck_default import *
from xml.dom import minidom
from xml.dom import *

pygtk.require('2.0')
import gobject, gtk

def getText(nodelist):
    rc = ""
    for node in nodelist:
        if node.nodeType == node.TEXT_NODE:
            rc = rc + node.data
    return rc

class Category:
   def __init__ (self, defaultName):
      self.name = defaultName
      self.parent = None
      self.categories = []
      self.rules = []
      self.xml = ""

   def ReadXml (self, node):
      newname = str (node.getAttribute ("name"))
      if newname != "":
         self.name = newname

      child = node.firstChild
      while child != None:
         if child.nodeType == Node.ELEMENT_NODE:
            # if tip is not defined as attribute, it might be defined as a child
            tip = str (child.getAttribute ("tip"))
            if tip == "":
               children = child.getElementsByTagName ("tip")
               if children.length == 1:
                  tip = getText (children)

            # ??? Workaround issues in gnatcheck output I424-029
            minval = child.getAttribute ("min")
            maxval = child.getAttribute ("max")
            defaultval = child.getAttribute ("default")

            if minval == 1 and defaultval == 1:
               minval = 0
               defaultval = 0
            separator = child.getAttribute ("separator")
            if separator == "":
               separator = ":"

            if child.localName == "category":
               newcat = Category("")
               newcat.ReadXml (child)
               self.AddCategory (newcat)
            elif child.localName == "check":
               self.AddRule (
                 Check (str (child.getAttribute ("switch")),
                        re.sub ("^[+]", "-", str (child.getAttribute ("switch"))),
                        str (child.getAttribute ("label")),
                        tip,
                        False,
                        False))
            elif child.localName == "spin":
               self.AddRule(
                 Spin (str (child.getAttribute ("switch")),
                       re.sub ("^[+]", "-", str (child.getAttribute ("switch"))),
                       str (child.getAttribute ("label")),
                       tip,
                       separator,
                       defaultval,
                       minval,
                       maxval,
                       False))
            elif child.localName == "field":
               self.AddRule(
                 Field (str (child.getAttribute ("switch")),
                        str (child.getAttribute ("switch-off")),
                        str (child.getAttribute ("label")),
                        tip,
                        separator,
                        defaultval,
                        False))
         child = child.nextSibling

   def AddCategory (self, cat):
      cat.parent = self
      self.categories.append (cat)

   def AddRule (self, rule):
      self.rules.append (rule)

   def Parent (self):
      return self.parent

   def IsEmpty (self):
      if len (self.rules) > 0:
         return False
      for c in self.categories:
         if not c.IsEmpty():
            return False
      return True

   def Xml (self, num):
      xml = ""

      if num != "":
        xml += """<popup label="%s %s">""" % (num, self.name)

      # Add subcategories
      subnum = 0
      for cat in self.categories:
         if not cat.IsEmpty ():
            subnum += 1
            xml += cat.Xml (num + str (subnum) + ".")

      # Add rules widgets
      for rule in self.rules:
         xml += rule.Xml (1, 1)

      if num != "":
         xml += "</popup>"
      return xml

# workaround on visibility issues with Python and nested methods.
class Namespace:
   pass

def get_supported_rules (gnatCmd):
   ns = Namespace()
   ns.msg=""

   ns.warnings_list = []
   ns.warnings_rules_analysis = False
   ns.style_rules_analysis = False
   ns.all_warnings_exception_list = ""

   # Verify we have the correct gnatcheck executable
   # First get gnatcheck rules
   process = GPS.Process (gnatCmd + "check -hx", remote_server = "Tools_Server")
   xmlstring = re.sub ("gnatcheck: No existing file to process.*", "", process.get_result())
   try:
      dom = minidom.parseString (xmlstring)
   except:
      GPS.Console("Messages").write ("Warning: the gnatcheck module could not retrieve the gnatcheck rules. Using the default ones.\n")
      dom = minidom.parseString (gnatcheck_default)
   roots = dom.getElementsByTagName ("gnatcheck")

   # Build the switches from the dom tree
   cat = Category ("Main")
   cat.ReadXml (roots.item (0))

   # verify the we had a correct execution of gnat check
   if cat.IsEmpty():
      GPS.Console ("Messages").write ("Error: Gnatcheck not found, the gnatcheck module is disabled\n")
      return []

   # Then retrieve warnings/style/restriction checks from gnatmake
   warningsCat = Category ("GNAT Warnings")
   gps_utils.gnat_rules.EnsureInitialized()
   rules = gps_utils.gnat_rules.gnatmakeproc.get_warnings_list()
   for r in rules:
     r.switch = re.sub("-gnatw","+RWarnings:", r.switch)
     r.switchoff = re.sub("-gnatw","+RWarnings:", r.switchoff)
     r.label = re.sub("-gnatw","+RWarnings:", r.label)
     r.tip = re.sub("-gnatw","+RWarnings:", r.tip)
     for dep in r.dependencies:
       dep[0] = re.sub("-gnatw","+RWarnings:", dep[0])
     warningsCat.AddRule (r)
   cat.AddCategory (warningsCat)

   return cat

def deactivate (widg):
   """Utility function to deactivate a widget. Used as callback in foreach loop"""
   widg.set_sensitive(False)
def activate (widg):
   """Utility function to activate a widget. Used as callback in foreach loop"""
   widg.set_sensitive(True)

class rulesEditor(gtk.Dialog):
   """Dialog used to edit the coding standard file."""

   def __init__ (self, maincat, defaultfile):
      # call parent __init__
      gtk.Dialog.__init__ (self, title="Coding Standard editor", parent=GPS.MDI.current().pywidget().get_toplevel(), flags=gtk.DIALOG_MODAL, buttons=None)
      self.set_default_size (600, 400)
      self.set_name ("CodingStandardEditor")

      # main cat contains the main Category object corresponding to extracted rules from gnatcheck -hx
      self.main_cat = maincat
      self.warnings_list = []
      self.warnings_widgets = []
      # additional switches that might be defined in the coding standard file
      self.additional_switches = []

      # Selection of a coding standard file
      hbox = gtk.HBox()
      hbox.show()
      self.vbox.pack_start (hbox, False, False, 0)

      label=gtk.Label("coding standard file:")
      label.show()
      hbox.pack_start (label, False, False, 0)

      hbox = gtk.HBox()
      hbox.show()
      self.vbox.pack_start (hbox, False, False, 0)

      self.fileEntry = gtk.Entry()
      # Connect callbacks on the file entry modifications
      self.fileEntry.set_editable(True)
      self.fileEntry.show()
      if None != defaultfile:
         self.fileEntry.set_text (defaultfile.name())
      self.fileEntry.connect ('changed', self.on_file_entry_changed)
      hbox.pack_start (self.fileEntry, True, True, 0)

      button=gtk.Button ('Browse')
      button.connect ('clicked', self.on_coding_standard_file_browse)
      button.show()
      hbox.pack_start (button, False, False, 0)

      # rules container
      self.switchvbox = gtk.VBox()
      self.switchvbox.show()
      self.vbox.pack_start (self.switchvbox, True, True, 0);

      # Check box for optional rules file edition after exit
      hbox = gtk.HBox()
      hbox.show()
      label = gtk.Label("")
      label.hide()
      hbox.pack_start (label, True, True, 0)
      self.vbox.pack_start (hbox, False, False, 0)
      self.open_file_after_exit_check = gtk.CheckButton("Open rules file after exit")
      self.open_file_after_exit_check.show()
      hbox.pack_end (self.open_file_after_exit_check, False, False, 0)

      label=gtk.Label()
      label.set_markup("<span weight='bold' size='large'>Coding standard rules</span>")
      label.show()
      self.switchvbox.pack_start (label, False, False, 0)

      self.tips = gtk.Tooltips()
      xml = self.main_cat.Xml ("")
      xml = str ("""<?xml version="1.0"?><tool name="Coding_Standard" lines="1" columns="1">%s</tool>""" % (xml))
      self.SwitchesChooser = GPS.SwitchesChooser ("Gnatcheck", xml);
      self.switchvbox.pack_start (self.SwitchesChooser.pywidget())
      self.show_all()

      # Save - Cancel buttons
      self.saveButton=gtk.Button ('Save')
      self.saveButton.connect ('clicked', self.on_save)
      self.saveButton.show()
      self.action_area.pack_start(self.saveButton, True, True, 0)

      self.cancelButton=gtk.Button ('Cancel')
      self.cancelButton.connect ('clicked', self.on_cancel)
      self.cancelButton.show()
      self.action_area.pack_start(self.cancelButton, True, True, 0)

      self.on_file_entry_changed()

   def get_filename (self):
      return GPS.File (self.fileEntry.get_text())

   def on_file_entry_changed (self, *args):
      """Callback when the file entry changed"""
      name = self.fileEntry.get_text()
      if name == "":
        self.tips.disable()
        self.switchvbox.foreach (deactivate)
        self.saveButton.set_sensitive(False)
      else:
        self.tips.enable()
        self.switchvbox.foreach (activate)
        self.saveButton.set_sensitive(True)
        if os.path.isfile (name):
           f = open (name, "r")
           content = f.read()
           f.close ()
           self.parse (content)

   def parse (self, content):
      """Parse the content of a coding standard file, and apply the values to the editor"""
      content2 = re.sub (r'\-\-.*\n','',content)
      if content2 != content:
        msg = "Warning: the selected file contains comments.\nThese will be removed if the coding standard file is saved from the graphical editor\n"
        dialog = gtk.MessageDialog (self,
                                    gtk.DIALOG_MODAL,
                                    gtk.MESSAGE_WARNING,
                                    gtk.BUTTONS_OK,
                                    msg)
        dialog.run()
        dialog.destroy()
      content = re.sub ('\n',' ',content2)
      self.SwitchesChooser.set_cmd_line (content)

   def check_all (self, value):
      """Change all check states for the switches to 'value'"""
      for elem in self.rules_widgets:
         if elem[2] != "":
            if value:
               elem[0].set_value(1)
            else:
               elem[0].set_value(0)
         else:
            elem[0].set_active(value)

   def on_coding_standard_file_browse (self, *args):
      """Callback to coding standard 'Browse' button"""
      file = GPS.MDI.file_selector ()
      if file.name() != "":
         self.fileEntry.set_text (file.name())

   def on_cancel (self, *args):
      """Callback to 'Cancel' button"""
      self.response(gtk.RESPONSE_NONE)

   def on_save (self, *args):
      """Callback to 'Save' button"""
      file = self.get_filename()
      f = open (file.name(), "w")
      content = self.SwitchesChooser.get_cmd_line ()
      content = re.sub (" +", "\n", content)
      f.write (content)
      f.close ()
      if self.open_file_after_exit_check.get_active():
        GPS.EditorBuffer.get(file)
      self.response(gtk.RESPONSE_NONE)

