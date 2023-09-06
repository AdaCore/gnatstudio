# Simple plugin to show how to send commands to the debugger on startup


def on_debugger_started(name, debugger):
    """This is called when the debugger starts"""

    # Right now let's send the command "show version"
    debugger.send("show version", show_in_console=True)


def debugger_commands_override(hook, debugger, command):
    if command == "next":
        return debugger.send("show version", False)
    else:
        return ""


def initialize_project_plugin():
    """This is called automatically when the project is loaded"""

    # Schedule the function "on_debugger_started" to be called each time
    # the debugger starts
    GPS.Hook("debugger_started").add(on_debugger_started)
    GPS.Hook("debugger_command_action_hook").add(debugger_commands_override)


def finalize_project_plugin():
    """This is called automatically when another project is loaded"""
