# This script automatically assigns the scenario variable "target" upon
# assignment of the build server.

import GPS

my_linux_server_nickname = "foo"


def server_changed(hook, server_type, nickname):
    if server_type == "BUILD_SERVER":
        if nickname == "my_linux_server_nickname":
            GPS.Project.set_scenario_variable("target", "Linux-remote")
        else:
            GPS.Project.set_scenario_variable("target", "Windows")


GPS.Hook("server_config_hook").add(server_changed)
