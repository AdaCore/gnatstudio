## This package uses an external browser to display help, instead of GPS's
## internal browser
## This can be configured through several preferences

import GPS, re

GPS.parse_xml ("""
   <preference name="html-external-browser-name"
               page="Helpers"
               default="mozilla %p://%u#%a"
               tip="Name of the browser to use to display HTML pages. This should include arguments. %u will be substituted by the URL, and %a by the name of the anchor, and %p by the name of the protocol (http, file,...)"
               label="External browser"
               type="string" />
   <preference name="html-external-browser-policy"
               page="Helpers"
               tip="When the external browser should be used to display HTML pages"
               default="0"
               label="External Browser Policy"
               type="choices" >
        <choice>Never</choice>
        <choice>Http pages only</choice>
        <choice>Always</choice>
   </preference>
""")

def open_html (hook_name, file, enable_navigation, anchor):
   browser = GPS.Preference ("html-external-browser-name").get()
   policy  = GPS.Preference ("html-external-browser-policy").get()

   if re.search ("://", file.name()):
      protocol, url = file.name().split ("://")
   else:
      protocol = "file"
      url = file.name()

   if url == "": protocol, url = "file", protocol

   if policy == "Always" \
      or (policy == "Http pages only" and protocol == "http"):
      browser = browser.replace ("%u", url)
      browser = browser.replace ("%a", anchor)
      browser = browser.replace ("%p", protocol)
      GPS.Process (browser)
      return 1
   else:
      return 0

GPS.Hook ("html_action_hook").add (open_html)
