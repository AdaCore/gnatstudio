import GPS
import re
import os
import sys
import string

####################
# Global variables #
####################

wtxtcl_sync_pattern="^wtxtcl> "  # Sync pattern used to retrieve commands output
wtxtcl_error_pattern=re.compile ("Error")
selected_tgtsvr = ""  # Target server selected ("" means no tgtsvr selected)
# The following variables .... XXX
cmd_buffer_size   = 32                      # buffer size
cmd_cyclic_buffer = range (cmd_buffer_size) # commands buffer
cmd_current       = 0  # index of the slot containing the next wtxtcl command
cmd_first_empty   = 0  # first empty slot in the buffer
cmd_in_progress   = 1  # if set to 1 then a wtxtcl command is in progress

error_no_connection = "You are not connected to target server"
debug=1
wtxtcl_wrapper_activated=True
tgtsvr_init = 0 
target_server_menus = 0 
tgtsvr_list_entry = 0 
gps_toolbar = 0
wtxtcl_session = 0 
Before_Exit_Hook = 0 
status_out = 0
wtxtcl_command = "wtxtcl"

#######################
# Utilities functions #
#######################

def message_box (str):
   if debug == 1:
       log (str)
   else:
       GPS.MDI.dialog (str)

def log (str):
   global debug
   global debug_out
   if debug == 1:
      s = sys._getframe (1).f_code.co_name
      s = string.ljust (s, 30)
      debug_out.write ("[" + s + "]: "  + str + "\n")

# Convert string representing a list returned by a wtxtcl command to a python
# list. Ex: if we get the following string:
#     a b {c d}
# the result of this function will be
#     ["a", "b", ["c", "d"]]
def string_to_list (str):
   result="["
   previous=""
   for i in str:
       if i == ' ':
          if previous != ' ':
             if previous != '}':
                 result=result + '\"'
             result=result + ','
       elif i == '{':
          if previous == '\n':
               result=result + ','
          result=result + '['
       elif i == '}':
          if previous != '}':
               result = result + '\"'
          if previous == '{':
               result = result + '\"'
          result=result + ']'
       elif i == '\n':
          if previous != '}' and previous != '\n' and previous != '':
               result = result + '\"'
       else:
          if previous == '{' or previous == ' ':
               result = result + '\"'
          elif previous == '\n':
               result = result + ','
          elif previous == '':
               result = result + '\"'
          result=result + i
       if previous != '' or i != '\n': 
          previous=i
   result = result + "]"
   result=eval(result)
   return result

# convert a string returned by a wtxtcl command to a dictionary
# Ex: if we get the following string:
#     {a b c} {d e f}
# the returned dictionary will be
#     { 'a' : ['b', 'c'], 'd' : ['e', 'f'] }
def string_to_dict (str):
   list=string_to_list (str)
   result = {}
   for i in list:
       result [i[0]] = i [1:]
   return result

#######################################
# wtxtcl commands scheduler functions #
#######################################

# If there is no command in progress and that the buffer is not empty then
# execute the next command in the buffer. Otherwise do nothing
def process_next_command ():
   global cmd_cyclic_buffer
   global cmd_first_empty
   global cmd_buffer_size
   global cmd_current
   global cmd_in_progress
   global wtxtcl_sync_pattern
   global wtxtcl_session
   global wtxtcl_wrapper_activated
   log ("cmd_in_progress: " + str (cmd_in_progress) + "(" + str ((cmd_first_empty - cmd_current) % cmd_buffer_size) + " in buffer)")
   if cmd_in_progress == 0 and cmd_first_empty != cmd_current:
      cmd_in_progress = 1
      log ("get command lock")
      # No command in queue so we can execute it
      cmd = cmd_cyclic_buffer [cmd_current][0]
      action = cmd_cyclic_buffer [cmd_current][1]

      log ("Send command: " + cmd + ",callback: " + action.__name__)
      wtxtcl_session.expect (wtxtcl_sync_pattern, action)
      if wtxtcl_wrapper_activated == True:
            cmd = "catch { puts [ " + cmd + " ] } result; if { $result != \"\" } { puts \"Error $result\" }; puts  \"wtxtcl> \""
            wtxtcl_session.send (cmd)
            log ("real command sent :" + cmd)
      else:
            wtxtcl_session.send (cmd)
      cmd_current = (cmd_current + 1) % cmd_buffer_size

# Default callback
def default_command_callback (pid, match_src, until_match_str):
   global cmd_in_progress
   log ("sync on prompt")
   command_terminated ()

# Add a new command in the wtxtcl commands buffer
def send_command (cmd, action = default_command_callback):
   # Queue the command
   global cmd_cyclic_buffer
   global cmd_first_empty
   global cmd_buffer_size
   global wtxtcl_session

   log ("queue command : " + cmd)
   cmd_cyclic_buffer [cmd_first_empty] = [cmd,action]
   cmd_first_empty = (cmd_first_empty + 1) % cmd_buffer_size
   process_next_command ()

# Command that must be called by all callbacks related to wtxtcl interaction
def command_terminated ():
   global cmd_in_progress
   log ("release command lock")
   cmd_in_progress = 0     # Release the lock
   process_next_command () # Check for pending commands

#######################################
# Target servers management functions #
#######################################

# Get the list of target servers registered in the registry
def get_tgtsvr_list ():
   send_command ("wtxInfoQ", get_tgtsvr_list_callback)

def get_tgtsvr_list_callback (pid, match_str, until_match_str):
   global tgtsvr_list
   global tgtsvr_list_entry
   log ("enter get_tgtsvr_list_callback")
   log (until_match_str)
   # Get the dictionary containing list of services registered in the registry
   tgtsvr_list=string_to_dict (until_match_str)

   # Update drop-down menu that contains the list of available target servers
   tgtsvr_list_entry.clear ()
   tgtsvr_list_entry.add ("Not Connected")
   for i in tgtsvr_list.keys():
        if tgtsvr_list[i][0] == "tgtsvr":
            tgtsvr_list_entry.add (i)

   # release the command lock
   command_terminated ()

# Show current target server status
def show_tgtsvr_status (id):
   global error_no_connection
   if selected_tgtsvr != "":
      send_command ("wtxTsInfoGet", show_tgtsvr_status_callback)
   else:
      message_box (error_no_connection)

def show_tgtsvr_status_callback (pid, match_str, until_match_str):
   global selected_tgtsvr, cmd_in_progress, wtxtcl_error_pattern
   until_match_str=string.strip (until_match_str) 
   log ("output for status" + until_match_str)
   result = wtxtcl_error_pattern.search (until_match_str)
   log ("match error pattern result: " + str (result))
   if result != None:
      status_msg = until_match_str
      message_box (status_msg)
      tgtsvr_list_entry.set_text ("Not Connected")
   else:
       tgtsvr_status=string_to_list (until_match_str)
       status_msg="Name   : " + selected_tgtsvr + "\n"
       status_msg=status_msg + "Version: " + tgtsvr_status[13] + "\n"
       status_msg=status_msg + "Status : " + tgtsvr_status[11] + "\n"
       status_msg=status_msg + "Runtime:"
       for i in tgtsvr_status[3]:
          status_msg=status_msg + " " + i
       status_msg=status_msg + "\n"
       status_msg=status_msg + "Agent  : " + tgtsvr_status[14] + "\n"
       status_msg=status_msg + "CPU    : " + "\n"
       status_msg=status_msg + "BSP    :"
       for i in tgtsvr_status[4]:
          status_msg=status_msg + " " + i
       status_msg=status_msg + "\n"
       status_msg=status_msg + "Memory : " + tgtsvr_status[6][1] + "\n"
       status_msg=status_msg + "Link   :"
       for i in tgtsvr_status[0]:
          status_msg=status_msg + " " + i
       status_msg=status_msg + "\n"
       status_msg=status_msg + "User   : " + tgtsvr_status[8] + "\n"
       status_msg=status_msg + "Start  :"
       for i in tgtsvr_status[9]:
          status_msg=status_msg + " " + i
       status_msg=status_msg + "\n"
       status_msg=status_msg + "Last   :"
       for i in tgtsvr_status[10]:
          status_msg=status_msg + " " + i
       status_msg=status_msg + "\n"
       status_msg=status_msg + "Attached Tools:\n"
       for i in tgtsvr_status[16:]:
          status_msg=status_msg + "    " + i[1] + "(" + i[4] + ")\n"
       message_box (status_msg)
   command_terminated ()

# Callback that check if the wtxToolAttach was successful
def tgtsvr_attach_callback (pid, match_src, until_match_str):
   global cmd_in_progress
   global selected_tgtsvr
   global wtxtcl_error_pattern
   global tgtsvr_list_entry
   log ("enter tgtsvr_attach_callback")
   temp=string.strip (until_match_str)
   log (temp)
   if temp != selected_tgtsvr:
         message_box ("Connection to " + selected_tgtsvr + " failed")
         log ("Error message")
         tgtsvr_list_entry.set_text ("Not Connected")
   else:
         print "Attached to ",selected_tgtsvr
         set_tgtsvr_menus_sensitive (True)
   command_terminated ()

# Select a target server in the list and attach to it
def select_tgtsvr (entry_id, id):
   global selected_tgtsvr
   if selected_tgtsvr != "":
      # If we were attached to another target server, detach from it
      print "Detach from " + selected_tgtsvr
      send_command ("wtxToolDetach")
      selected_tgtsvr=""
   if id != "Not Connected":
      # Attach to the target server selected
      selected_tgtsvr=id
      send_command ("wtxToolAttach " + id, tgtsvr_attach_callback)
   else:
      set_tgtsvr_menus_sensitive (False)

# Unregister the current target server
def tgtsvr_unregister (menu_id):
   global selected_tgtsvr
   global tgtsvr_list_entry
   id=selected_tgtsvr
   tgtsvr_list_entry.set_text ("Not Connected")
   #select_tgtsvr ("", "Not Connected")
   send_command (cmd = "wtxUnregister " + id)
   get_tgtsvr_list ()

# Reboot the target associated with the current target server
def tgtsvr_reset (menu_id):
   global selected_tgtsvr, error_no_connection, tgtsvr_list_entry
   if selected_tgtsvr != "":
      print "Reset " + selected_tgtsvr
      send_command (cmd ="wtxTargetReset")
      tgtsvr_list_entry.set_text ("Not Connected")
      #select_tgtsvr ("", "Not Connected")
      get_tgtsvr_list ()
   else:
      message_box (error_no_connection)

# Kill the current target server
def tgtsvr_kill (menu_id):
   global selected_tgtsvr, tgtsvr_list_entry
   if selected_tgtsvr != "":
      print "Kill target server " + selected_tgtsvr
      send_command (cmd = "wtxTsKill destroy")
      tgtsvr_list_entry.set_text ("Not Connected")
      #select_tgtsvr ("", "Not Connected")
      get_tgtsvr_list ()
   else:
      message_box (error_no_connection)

def tgtsvr_lock (menu_id):
   send_command (cmd = "wtxTsLock")

def tgtsvr_unlock (menu_id):
   send_command (cmd = "wtxTsUnlock")

# Finalize the wtxtcl session (this function is called when we exit GPS)
def finalize_wtxtcl (arg):
   global wtxtcl_session
   print "Detach from target servers"
   wtxtcl_session.send ("wtxToolDetach -all")
   print "Kill wtxtcl"
   wtxtcl_session.kill ()
   return 1

def get_selected_tgtsvr ():
   global selected_tgtsvr
   print selected_tgtsvr

def set_tgtsvr_menus_sensitive (bool):
   global target_server_menus
   log ("set availability of target server menus to " + str (bool))
   for i in target_server_menus:
       i.set_sensitive (bool)

def wtxtcl_has_died_callback (pid, match_src, until_match_str):
   message_box ("Can not load integration because wtxtcl died")

def gdb_connect_to_target (hook_name):
   if selected_tgtsvr != "":
       gdb_session = GPS.Debugger ()
       gdb_session.send ("target wtx " + selected_tgtsvr)

def initialization_callback (pid, match_src, until_match_str):
   global  tgtsvr_init, target_server_menus, tgtsvr_list_entry, gps_toolbar, wtxtcl_session, Before_Exit_Hook, status_out
   if tgtsvr_init == 0:
      log ("create target server menus")
      target_server_menus = ( 
       GPS.Menu.create ("Tools/Target Server/Status", show_tgtsvr_status),
       GPS.Menu.create ("Tools/Target Server/Reboot", tgtsvr_reset),
       GPS.Menu.create ("Tools/Target Server/Kill", tgtsvr_kill),
       GPS.Menu.create ("Tools/Target Server/Unregister", tgtsvr_unregister),
       GPS.Menu.create ("Tools/Target Server/Reserve", tgtsvr_lock),
       GPS.Menu.create ("Tools/Target Server/Unreserver", tgtsvr_unlock))
 
      set_tgtsvr_menus_sensitive (False)
      log (str (wtxtcl_session))
      gps_toolbar=GPS.Toolbar ()
      tgtsvr_list_entry=gps_toolbar.append ("tgtsvr_combo", "target server", select_tgtsvr)
      #gps_toolbar.append (tgtsvr_list_entry, "")
      get_tgtsvr_list ()
 
      # Ensure that the wtxtcl session is closed before exiting from GPS
      Before_Exit_Hook = GPS.Hook ("before_exit_action_hook")
      Before_Exit_Hook.add (finalize_wtxtcl)

      # Hook for gdb
      gdb_hook = GPS.Hook ("debugger_started")
      gdb_hook.add (gdb_connect_to_target)

      # Add buttons
      #GPS.parse_xml ("""
      #   <action name=start_windsh>
      #       <shell lang="python">tgtsvr.get_selected_tgtsvr ()</shell>
      #       <external>xterm -e windsh %1</external>
      #   </action>
      #   <button action="start_windsh">
      #     <title>Launchx WindSh</title>
      #     <pixmap>/alexandria.a/home/roche/.gps/python_startup/windsh.png</pixmap>
      #   </button>
      #   """)
      status_out = GPS.Console (1)
      status_out.set_console ("Messages")
      sys.stdout = status_out
      tgtsvr_init = 1
      command_terminated ()
   else:
      log ("initialization already done")


##############################
# Initialize actions for GPS #
##############################


WIND_BASE=os.getenv ("WIND_BASE")
WIND_HOST_TYPE=os.getenv ("WIND_HOST_TYPE")

if (WIND_BASE == None) | (WIND_HOST_TYPE == None):
   print "Tornado integration not loaded"
else:
   if WIND_HOST_TYPE == "sun4-solaris2":
          wtxtcl_command="wtxtcl"
          wtxtcl_wrapper_activated=False
          external_cmd_prefix="xterm -e "
   if WIND_HOST_TYPE == "x86-win32":
          wtxtcl_command="ssh oslo wtxtcl"
          wtxtcl_wrapper_activated=True
          external_command_prefix = "cmd /c"
 
   print ("Host: " + WIND_HOST_TYPE + ", commands wrapper: " + str (wtxtcl_wrapper_activated)) 
   if debug == 1:
          debug_out = GPS.Console (1)
          debug_out.set_console ("TgtSvr Debug") 
   if WIND_HOST_TYPE == "x86-win32" or WIND_HOST_TYPE == "sun4-solaris2":
      wtxtcl_session=GPS.Process (wtxtcl_command, wtxtcl_sync_pattern, initialization_callback, wtxtcl_has_died_callback)
      if wtxtcl_wrapper_activated == True:
            wtxtcl_session.send ("puts \"wtxtcl> \"")
   else:
      print ("WIND_HOST_TYPE should be equal to \"sun4-solaris2\" or \"x86-win32\"")
