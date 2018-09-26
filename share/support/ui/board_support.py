"""
This plugin creates buttons on the toolbar to conveniently
flash and debug programs on external boards.

Three tool suites are currently supported by GPS to flash/debug
a specific board:

. st-util/st-flash (for STM32 family boards only)

   These utilities are included in recent Windows-based versions of
   GNAT for the arm-eabi targets, or can be downloaded from
   https://github.com/texane/stlink and built.

   To use st-util/st-flash in order to debug/flash a specific board, set
   the IDE'Connection_Tool project attribute to 'st-util'. These two
   utilities need to be in the PATH in order to use them from GPS.

   Note that the USB driver for these utility programs
   must be installed in order for them to operate correctly, but this
   plugin is not concerned with that aspect.

. OpenOCD (for possibly any board supporting JTAG connections )

   OpenOCD (Open On-Chip Debugger) is open-source software that interfaces with
   a hardware debugger's JTAG port. It can be retrieved on the OpenOCD official
   website: http://openocd.org/getting-openocd

   To use OpenOCD in order to debug/flash a specific board, set the
   IDE'Connection_Tool project attribute to 'openocd'.

   In addition, OpenOCD needs a board-specific configuration file
   in order to interact with a given board. OpenOCD comes with a
   set of default configuration files that can generally be found in
   the '/usr/local/share/openocd/scripts/board' directory. Set the
   IDE'Configuration_File project attribute to choose the configuration file
   to use with OpenOCD for your project.

. pyOCD (using CMSIS-DAP)

   pyOCD is an Open Source python 2.7 based library for programming and
   debugging ARM Cortex-M microcontrollers using CMSIS-DAP. Linux, OSX and
   Windows are supported.

   To use pyOCD in order to debug/flash a specific board, set the
   IDE'Connection_Tool project attribute to 'pyocd'.

   Note that pyOCD will need permissions to talk with the board so it's better
   to add a UDEV rules saying that the device is accessible for non-privileged
   users.
"""

import GPS
from modules import Module
from target_connector import TargetConnector
from gps_utils.internal.dialogs import Project_Properties_Editor
import workflows
import workflows.promises as promises


class BoardLoader(Module):

    # The list of debug build targets created by this plugin
    __debug_build_targets = []

    # The list of flash build targets created by this plugin
    __flash_build_targets = []

    # The target on which we want to debug/flash.
    # Retrieved from the Target project attribute.
    __target = None

    # The tool spawned to interface with the device when debugging.
    # Retrieved from the IDE'Communication_Tool project attribute.
    __connection_tool = None

    # The optional configuration file used to configure the connection
    # tool.
    # Retrieved from the IDE'Communication_Config_File project attribute.
    __config_file = None

    # The build target associated with the connection tool
    __connector = None

    # The ProcessWrapper instance used to spawn the connection tool
    # and the associated promises
    __connection = None

    # Name or IP address of the target we want to connect.
    # Retrieved from the IDE'Program_Host project attribute.
    __remote_target = None

    # Protocol used to connect to the target.
    # Retrieved from the IDE'Communication_Protocol project attribute.
    __remote_protocol = None

    # The address where we should load the executable. Retrieved using objdump.
    __load_address = None

    # Set to True if a workflow is still being processed.
    # This is used to avoid launching more than one workflow at the time.
    __is_busy = False

    def __is_non_native_project(self, prj):
        """
        Used to know if the project is set for a native target or a
        BB/cross target.m
        """

        return self.__target != "" and self.__target != "native"

    @staticmethod
    def __display_message(msg, mode="text"):
        """
        Display the given message in the GPS Messages windows.
        Use the 'error' mode for to display warning/error messages.
        """

        GPS.Console("Messages").write(msg + "\n", mode=mode)

    def __error_exit(self, msg=""):
        """
        Display the given error message and reset the workflows
        """

        self.__display_message(msg, mode="error")
        self.__display_message("[workflow stopped]", mode="error")

        self.__reset_all()

    def __update_build_targets_visibility(self, active):
        """
        Update the Build & Run, Build & Debug and the plugin's associated
        Build Targets depending on ``active``.
        """

        build_run_target = GPS.BuildTarget("Build & Run")
        build_debug_target = GPS.BuildTarget("Build & Debug")

        if active:
            build_run_target.hide()
            build_debug_target.hide()
            for target in self.__debug_build_targets:
                target.show()

            if self.__flashing_tool:
                for target in self.__flash_build_targets:
                    target.show()
            else:
                for target in self.__flash_build_targets:
                    target.hide()
        else:
            build_run_target.show()
            build_debug_target.show()
            for target in self.__debug_build_targets:
                target.hide()
            for target in self.__flash_build_targets:
                target.hide()

    @workflows.run_as_workflow
    def __open_remote_project_properties(self, text):
        """
        Open the Project Properties editor and go to the page
        defining the remote settings used to debug on a board.
        """

        editor = Project_Properties_Editor()
        yield editor.open_and_yield(wait_scan=False)
        yield editor.select("Embedded")

    def __verify_settings(self, for_debug=False):
        """
        Verify that the settings have correctly been set in order to flash,
        and, if for_debug is True, to debug a remote target.


        Return True if the settings are correctly set, False otherwise.
        """

        console = GPS.Console("Messages")
        message_header = ("Can't debug on board:" if for_debug
                          else "Can't flash the board:")
        result = True

        if self.__connection_tool == "openocd" and not self.__config_file:
            console.write(("%s no configuration file specified. "
                           "Please set the "
                           % (message_header)),
                          mode="error")
            console.insert_link("IDE'Connection_Config_File",
                                self.__open_remote_project_properties)
            console.write(" project attribute\n",
                          mode="error")
            result = False

        if for_debug and not self.__remote_target:
            console.write(("%s no remote target specified. Please set the "
                           % (message_header)),
                          mode="error")
            console.insert_link("IDE'Protocol_Host",
                                self.__open_remote_project_properties)
            console.write(" project attribute\n",
                          mode="error")
            result = False

        if for_debug and not self.__remote_protocol:
            console.write(("%s no remote protocol specified. Please set the "
                           % (message_header)),
                          mode="error")
            console.insert_link("IDE'Communication_Protocol",
                                self.__open_remote_project_properties)
            console.write(" project attribute\n",
                          mode="error")
            result = False

        return result

    def __get_flashing_command_line(self, binary):
        """
        Get the command line used to invoke the currently set flashing tool.
        The flashing tool is deduced from the IDE'Connection_Tool (i.e: use
        'st-flash' if IDE'Connection_Tool is set to 'st-util'.).
        """

        cmd = [self.__flashing_tool]
        args = []

        if self.__flashing_tool == "openocd":
            # Replace backslashes by forward slashes.
            # This is used to support OpenOCD on Windows.
            binary = binary.replace('\\', '/')
            args = ["-f", self.__config_file, "-c",
                    "program %s verify reset exit %s"
                    % (binary, self.__load_address)]

        elif self.__flashing_tool == "st-flash":
            args = ["--reset", "write", binary, self.__load_address]

        elif self.__flashing_tool == "pyocd-flashtool":
            args = ["-a", self.__load_address, binary]

        return cmd + args

    def __get_flashing_complete_regexp(self):
        """
        Get the regexp used to detect when the flashing tool has successfully
        flashed the board.
        This regexp depends on the currently used flashing tool.
        """

        if self.__flashing_tool == "st-flash":
            return "Flash written and verified! jolly good!"
        elif self.__flashing_tool == "openocd":
            return "Verified OK"
        elif self.__flashing_tool == "pyocd-flashtool":
            return "INFO:root:Programmed \d+ bytes \(\d+ pages\) " \
              "at [\d\.]+ kB\/s|^No operation performed"
        else:
            return ""

    def __update_settings(self, project):
        """
        Update the settings used to flash/debug by retrieving the related
        project attributes.
        """

        self.__target = project.target

        self.__remote_target = project.get_attribute_as_string(
            package="IDE",
            attribute="Program_Host")

        self.__remote_protocol = project.get_attribute_as_string(
            package="IDE",
            attribute="Communication_Protocol")

        self.__connection_tool = project.get_attribute_as_string(
            package="IDE",
            attribute="Connection_Tool").lower()

        # Retrieve the configuration file only if we are using OpenOCD and
        # set the flashing tool according to the connection tool.
        if self.__connection_tool == "openocd":
            self.__flashing_tool = "openocd"
            self.__config_file = project.get_attribute_as_string(
                package="IDE",
                attribute="Connection_Config_File")
        elif self.__connection_tool == "st-util":
            self.__flashing_tool = "st-flash"
            self.__config_file = ""
        elif self.__connection_tool == "pyocd":
            self.__flashing_tool = "pyocd-flashtool"
            self.__config_file = ""
        else:
            self.__flashing_tool = self.__connection_tool
            self.__config_file = ""

    def __get_connection_command_line(self):
        """
        Get the command line used to invoke the currently set connection tool
        (e.g: the target address/port used to interact with GDB).
        Semihosting should work for all tools.
        """

        cmd = []
        args = []

        if self.__connection_tool == "pyocd":
            cmd = ["pyocd-gdbserver"]
        else:
            cmd = [self.__connection_tool]

        gdb_port = self.__remote_target.split(':')[-1]

        if self.__connection_tool == "openocd":
            # Semihosting command can only be used after the `init` command.
            args = ["-f", self.__config_file, "-c", "gdb_port %s" % (gdb_port),
                    "-c init", "-c arm semihosting enable"]
        elif self.__connection_tool == "st-util":
            has_semihosting = False
            semihosting_switch = "--semihosting"

            args = ["-p", gdb_port]

            # Add semihosting support if it's supported by the used st-util
            try:
                process = GPS.Process(["st-util", '--help'])
                output = process.get_result()
                has_semihosting = semihosting_switch in output
            except Exception:
                has_semihosting = False

            if has_semihosting:
                args += [semihosting_switch]
        elif self.__connection_tool == "pyocd":
            args = ["-S", "-p %s" % (gdb_port), "--semihosting"]

        return cmd + args

    def __get_connection_detection_regexp(self):
        """
        Get the regexp used to detect when the target connector is successfully
        connected to the board.
        This regexp depends on the currently used connection tool.
        OpenOCD output is not constant and depends on the interface used for
        debugging, when it displays the number of hardware breakpoints, we
        can assume that the connection is established.
        For pyOCD detection, we dont know if we have a system-wide or just
        user specific installation, we just match whatever is between the
        two colons.
        """
        if self.__connection_tool == "st-util":
            return "Listening at"
        elif self.__connection_tool == "openocd":
            return ".cpu: hardware has"
        elif self.__connection_tool == "pyocd":
            return "INFO:.+:GDB server started at port:\d+"
        else:
            return ""

    def __reset_all(self, manager_delete=True, connection_delete=True):
        """ Reset the workflows """

        if self.__connection is not None and connection_delete:
            self.__display_message("Resetting the connection")

            # Close the connection
            self.__connection.terminate()
            self.__connection = None

            # Kill the task attached to the connection tool  if it still there
            for i in GPS.Task.list():
                if self.__connection_tool in i.name():
                    i.interrupt()
                elif self.__connection_tool == "pyocd" and \
                        i.name() == "pyocd-gdbserver":
                    i.interrupt()

        # Workflow has been reset and any existing connection has been killed:
        # we are not busy anymore.
        self.__is_busy = False

    def __create_targets_lazily(self):
        """
        Create all the build targets needed to flash/debug a board. Here is
        the list of these build targets:
          . 'Flash to Board' and 'Flash <current file> to Board' build targets
          . 'Debug on Board' and 'Debug <current file> on Board' build targets
          . TargetConnector build target (created from IDE'Connection_Tool)

        This method is called each time the project changes.
        """

        project = GPS.Project.root()

        # Update the settings used for flash/debug
        self.__update_settings(project)

        # Check if it's a project for non-native targets
        active = self.__is_non_native_project(project)

        # Remove the previous Target Connector build target since the
        # connection tool and/or its arguments may have changed.
        if self.__connector:
            self.__connector.remove()

        # Create the Target Connector build target if a connection tool
        # has been specified in the project.
        if active and self.__connection_tool:
            cmd = self.__get_connection_command_line()
            self.__connector = TargetConnector(
                tool_name=cmd[0],
                default_args=cmd[1:])

        # Create the build targets needed in order to flash/debug the board
        # if not created yet.
        if not self.__debug_build_targets:
            workflows.create_target_from_workflow(
                parent_menu='/Build/Bareboard/Debug on Board/',
                target_name="Debug on Board",
                workflow_name="debug-on-board",
                workflow=self.__debug_wf,
                icon_name="gps-boardloading-debug-symbolic")
            self.__debug_build_targets.append(
                GPS.BuildTarget("Debug on Board"))

            workflows.create_target_from_workflow(
                parent_menu='/Build/Bareboard/',
                target_name="Debug <current file> on Board",
                workflow_name="debug-current-on-board",
                workflow=self.__debug_wf,
                icon_name="gps-boardloading-debug-symbolic",
                in_toolbar=False,
                main_arg="%fp")
            self.__debug_build_targets.append(
                GPS.BuildTarget("Debug <current file> on Board"))

        if not self.__flash_build_targets:
            workflows.create_target_from_workflow(
                parent_menu='/Build/Bareboard/Flash to Board/',
                target_name="Flash to Board",
                workflow_name="flash-to-board",
                workflow=self.__flash_wf,
                icon_name="gps-boardloading-flash-symbolic")
            self.__flash_build_targets.append(
                GPS.BuildTarget("Flash to Board"))

            workflows.create_target_from_workflow(
                parent_menu='/Build/Bareboard/',
                target_name="Flash <current file> to Board",
                workflow_name="flash-current-to-board",
                workflow=self.__flash_wf,
                icon_name="gps-boardloading-flash-symbolic",
                in_toolbar=False,
                main_arg="%fp")
            self.__flash_build_targets.append(
                GPS.BuildTarget("Flash <current file> to Board"))

        # Show/Hide the build targets accordingly
        self.__update_build_targets_visibility(active)

    ###############################
    # The following are workflows #
    ###############################

    def __flash_wf(self, main_name):
        """Workflow to build and flash the program on the board.
        """

        # Return with a warning message if we are still processing a previously
        # launched workflow.
        if self.__is_busy:
            self.__display_message(
                msg="Warning: 'Flash to Board' already being executed",
                mode="error")
            return

        self.__is_busy = True

        # Check if we have a main to flash
        if main_name is None:
            self.__error_exit(msg="Could not find the name of the main.")
            return

        # Build the executable
        builder = promises.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(main_name)
        if r0 is not 0:
            self.__reset_all()
            return

        # Check that the settings are correctly set to flash the board
        success = self.__verify_settings()
        if not success:
            self.__error_exit(msg="Could not flash the board.")
            return

        # Get the executable path
        exe = GPS.File(main_name).executable_path.path

        # Retrieve the load address of the executable with objdump
        self.__display_message("Retrieving the load address.")
        cmd = ["%s-objdump" % (self.__target), exe, "-h"]
        self.__display_message(' '.join(cmd))

        try:
            con = promises.ProcessWrapper(cmd)
        except Exception:
            self.__error_exit("Could not launch executable %s" % (cmd[0]))
            return

        # We want to match the line after
        # the Algn to get the first load address.
        output = yield con.wait_until_match("Algn\n.+")
        if output is None:
            self.__error_exit("%s returned an error." % (cmd[0]))
            return

        # We know that the first LMA is at index 5.
        self.__load_address = "0x%s" % (output.split()[5])
        self.__display_message("Load address is: %s" % (self.__load_address))

        # Create the flashable binary with objcopy
        self.__display_message("Creating the binary (flashable) image.")
        binary = exe + ".bin"
        cmd = ["%s-objcopy" % (self.__target), "-O", "binary", exe, binary]
        self.__display_message(' '.join(cmd))

        try:
            con = promises.ProcessWrapper(cmd)
        except Exception:
            self.__error_exit("Could not launch executable %s." % (cmd[0]))
            return

        status, output = yield con.wait_until_terminate()
        if status != 0:
            #  Show output in the Messages view on error
            self.__display_message(output)
            self.__error_exit("%s returned an error." % (cmd[0]))
            return

        # Flash the binary and wait until it completes
        cmd = self.__get_flashing_command_line(binary)
        self.__display_message("Flashing image to board...")
        self.__display_message(' '.join(cmd))

        try:
            con = promises.ProcessWrapper(cmd)
            con.lines.subscribe(self.__display_message)
            output = yield con.wait_until_match(
                self.__get_flashing_complete_regexp(),
                120000)
            if output is None:
                self.__error_exit(msg="Could not flash the executable.")
                con.terminate()
                return
        except Exception:
            self.__error_exit("Could not connect to the board.")
            return

        self.__display_message(("Flashing complete. "
                                "You may need to reset (or cycle power)."))

        # Not busy anymore
        self.__is_busy = False

    def __debug_wf(self, main_name):
        """
        Workflow to build, flash and debug the program on the real board.
        """

        # Return with a warning message if we are still processing a previously
        # launched workflow.
        if self.__is_busy:
            self.__display_message(("Warning: 'Debug on Board' "
                                    "already being executed"),
                                   mode="error")
            return

        # Reset the connection if still alive
        self.__reset_all()

        # Tell GPS that we can't run another workflow until we finish
        # the one that is currently running.
        self.__is_busy = True

        # Check if we have a main to debug
        if main_name is None:
            self.__error_exit(msg="Main not specified")
            return

        # Build the executable
        builder = promises.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(main_name)
        if r0 is not 0:
            self.__reset_all()
            return

        # Check that the settings are correctly set to debug on board
        success = self.__verify_settings(for_debug=True)
        if not success:
            self.__error_exit(msg="Could not connect to the board.")
            return

        # Switch directly to the "Debug" perspective so that the
        # connection tool console is still visible when spawning the debugger.
        GPS.MDI.load_perspective("Debug")

        # Launch the connection tool, if any
        if self.__connector:
            # Launch the connection tool with its associated console
            cmd = self.__connector.get_command_line()
            self.__display_message("Launching: %s" % (' '.join(cmd)))
            try:
                self.__connection = promises.ProcessWrapper(cmd)
                self.__connection.lines.subscribe(self.__display_message)
                output = yield self.__connection.wait_until_match(
                    self.__get_connection_detection_regexp(),
                    120000)
                if output is None:
                    self.__error_exit(msg="Could not connect to the board.")
                    return
            except Exception:
                self.__error_exit("Could not connect to the board.")
                return

        # Spawn the debugger on the executable and load it
        self.__display_message("Launching debugger.")
        exe = GPS.File(main_name).executable_path
        debugger_promise = promises.DebuggerWrapper(exe)

        # Load the executable
        yield debugger_promise.wait_and_send(
            cmd='load "%s"' % (exe),
            block=True)

        # Reset the board
        yield debugger_promise.wait_and_send(
            cmd="monitor reset halt",
            block=True)

        # Not busy anymore
        self.__is_busy = False

    def setup(self):
        """
        When setting up the module, create the associated Build Targets.
        """

        GPS.Hook("debugger_terminated").add(self.debugger_terminated)
        self.__create_targets_lazily()

    def project_view_changed(self):
        """
        Try to create the build targets when the project changes.
        """

        self.__create_targets_lazily()

    def debugger_terminated(self, hookname, debugger):
        """
        When debugger terminates, terminate the connection.
        """

        self.__reset_all()
