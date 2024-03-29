"""
Defines the protocols used when communicating with remote hosts.

The GPS remote facility (/Tools/View/Remote) allows you to easily
work (eg build and compile) on remote hosts. The communication with
these remote hosts can be done through a number of protocols, which
are defined in this package.
"""

import GPS

XML = r"""<?xml version="1.0"?>
<GNAT_Studio>

  <!-- SSH -->

  <!-- tty allocation required for some platforms like solaris -->
  <remote_connection_config name="ssh">
    <start_command use_pipes="true">ssh</start_command>
    <start_command_common_args>-t -t -Y -C %U %h %C</start_command_common_args>
    <start_command_user_args>-l %u</start_command_user_args>
    <send_interrupt>~.</send_interrupt>
    <extra_ptrn auto_answer="true" answer="yes">^.*continue connecting \(yes/no\)\? *</extra_ptrn>
  </remote_connection_config>

  <!-- TELNET -->

  <!-- rsh will ignore the shell start command, as remote commands are not
       supported by this tool
    -->
  <remote_connection_config name="telnet">
    <start_command use_pipes="false">telnet</start_command>
    <start_command_common_args>%U %h</start_command_common_args>
    <start_command_user_args>-l %u</start_command_user_args>
  </remote_connection_config>

  <!-- PLINK SSH -->

  <remote_connection_config name="plink ssh">
    <start_command use_pipes="false">plink</start_command>
    <start_command_common_args>-t -C -ssh %U %h %C</start_command_common_args>
    <start_command_user_args>-l %u</start_command_user_args>
    <extra_ptrn auto_answer="true" answer="y">^Store key in cache\? \(y/n\) *</extra_ptrn>
  </remote_connection_config>

  <!-- PLINK RSH -->

  <remote_connection_config name="plink rsh">
    <start_command use_pipes="false">plink</start_command>
    <start_command_common_args>-rlogin %U %h %C</start_command_common_args>
    <start_command_user_args>-l %u</start_command_user_args>
  </remote_connection_config>

  <!-- PLINK TELNET -->

  <remote_connection_config name="plink telnet">
    <start_command use_pipes="false">plink</start_command>
    <start_command_common_args>-telnet %h %C</start_command_common_args>
    <start_command_user_args></start_command_user_args>
  </remote_connection_config>

  <!-- SH shell -->

  <remote_shell_config name="sh">
    <start_command>sh -i</start_command>
    <!-- use default generic_prompt -->
    <gps_prompt>^---GPSPROMPT--#$</gps_prompt>
    <filesystem>unix</filesystem>
    <init_commands>
      <cmd>PS1=---GPSPROMPT--#</cmd>
      <cmd>COLUMNS=2048</cmd>
      <cmd>LANG=C</cmd>
      <cmd>export LANG COLUMNS</cmd>
      <cmd>unalias ls</cmd>
    </init_commands>
    <exit_commands>
      <cmd>exit</cmd>
    </exit_commands>
    <cd_command>cd %d</cd_command>
    <get_status_command>echo $?</get_status_command>
    <get_status_ptrn>^([0-9]+)\s*$</get_status_ptrn>
    <no_echo_command>stty -echo</no_echo_command>
  </remote_shell_config>

  <!-- BASH shell -->

  <remote_shell_config name="bash">
    <start_command>bash --login -i</start_command>
    <!-- use default generic_prompt -->
    <gps_prompt>^---GPSPROMPT--#$</gps_prompt>
    <filesystem>unix</filesystem>
    <init_commands>
      <cmd>export PS1=---GPSPROMPT--#</cmd>
      <cmd>unset PROMPT_COMMAND</cmd>
      <cmd>export COLUMNS=2048</cmd>
      <cmd>export LANG=C</cmd>
      <cmd>unalias ls</cmd>
    </init_commands>
    <exit_commands>
      <cmd>exit</cmd>
    </exit_commands>
    <cd_command>cd %d</cd_command>
    <get_status_command>echo $?</get_status_command>
    <get_status_ptrn>^([0-9]+)\s*$</get_status_ptrn>
    <no_echo_command>stty -echo</no_echo_command>
  </remote_shell_config>

  <!-- CSH shell -->

  <remote_shell_config name="csh">
    <start_command>csh -i</start_command>
    <!-- use default generic_prompt -->
    <gps_prompt>^---GPSPROMPT--#$</gps_prompt>
    <filesystem>unix</filesystem>
    <init_commands>
      <cmd>set prompt=---GPSPROMPT--#</cmd>
      <cmd>setenv COLUMNS 2048</cmd>
      <cmd>setenv LANG C</cmd>
      <cmd>unalias ls</cmd>
    </init_commands>
    <exit_commands>
      <cmd>exit</cmd>
    </exit_commands>
    <cd_command>cd %d</cd_command>
    <get_status_command>echo $status</get_status_command>
    <get_status_ptrn>^([0-9]+)\s*$</get_status_ptrn>
    <no_echo_command>stty -echo</no_echo_command>
  </remote_shell_config>

  <!-- TCSH shell -->

  <remote_shell_config name="tcsh">
    <start_command>tcsh -i</start_command>
    <!-- use default generic_prompt -->
    <gps_prompt>^---GPSPROMPT--#$</gps_prompt>
    <filesystem>unix</filesystem>
    <init_commands>
      <cmd>set prompt=---GPSPROMPT--#</cmd>
      <cmd>setenv COLUMNS 2048</cmd>
      <cmd>setenv LANG C</cmd>
      <cmd>unalias ls</cmd>
    </init_commands>
    <exit_commands>
      <cmd>exit</cmd>
    </exit_commands>
    <cd_command>cd %d</cd_command>
    <get_status_command>echo $status</get_status_command>
    <get_status_ptrn>^([0-9]+)\s*$</get_status_ptrn>
    <no_echo_command>stty -echo</no_echo_command>
  </remote_shell_config>

  <!-- CMD.EXE shell -->

  <remote_shell_config name="cmd.exe">
    <start_command>cmd.exe</start_command>
    <!-- use default generic_prompt -->
    <gps_prompt>^---GPSPROMPT--#$</gps_prompt>
    <filesystem>windows</filesystem>
    <init_commands>
      <cmd>PROMPT ---GPSPROMPT--#</cmd>
    </init_commands>
    <exit_commands>
      <cmd>exit</cmd>
    </exit_commands>
    <cd_command>cd %d</cd_command>
    <get_status_command>echo %errorlevel%</get_status_command>
    <get_status_ptrn>^([0-9]+)\s*$</get_status_ptrn>
  </remote_shell_config>

  <!-- RSYNC CONFIGURATION -->

  <remote_sync_config name="rsync">
    <arguments>-az --delete --progress --exclude *.o --exclude *.a --exclude .xrefs</arguments>
  </remote_sync_config>
</GNAT_Studio>
"""

GPS.parse_xml(XML)
