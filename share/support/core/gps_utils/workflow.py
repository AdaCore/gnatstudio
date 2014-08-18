"""
   Driver execute while generating mono-chain program
   from generators (the parameter), and send execution result
   (how the promise is fullfilled) back to the generator.
"""

import sys
import GPS
from gps_utils import promises

registered_workflows = {}


def run_registered_workflows(workflow_name, workflow_arg=None):
    # find workflow in registed table provided the workflow name
    # and then run it with the driver
    try:
        wf = registered_workflows[workflow_name]()
        driver(wf, workflow_arg)
    except KeyError:
        GPS.Console("Messages").write(
            "\nError: Workflow name not registered.\n")


def driver(w, workflow_arg=None):
    def go(gowith=None):
        try:
            # if there's feedback from previous event, tell the generator
            if gowith is not None:
                    p = w.send(gowith)
            # otherwise just goto the next step
            else:
                p = w.next()

            # if promise instance is got from workflow, "then" it
            if isinstance(p, promises.Promise):
                p.then(go)
            # otherwise, continue to the next object by go()
            else:
                if p == "give_me_arg":
                    print "driver: got your message asking arg, give you"
                    go(workflow_arg)
                else:
                    go()
        # when hits the end, exit
        except StopIteration:
            # print sys.exc_info()[0]
            return
    go()


# The following are decorators for workflows(generators)
def make_action_from_workflow(name, category="General",
                              description="", criteria=None):
    """
       Decorator that wrap a workflow and generate an action

       name: string, name of the returning action
       category: category as action object required
       description: string, description of the result action
       criteria: function that returns boolean, workflow is executed only when
                 it returns True

       The input of the decorator = expecting argument for returning wrap
       = a workflow = a generator that can be driven by driver()
    """
    # the wrapper function to be returned by decorator
    def wrap(workflow):
        # create action with given name
        action = GPS.Action(name)

        def drive():
            if (criteria is None) or criteria():
                w = workflow()
                driver(w)
            else:
                GPS.Console("Messages").write("Criteria doesn't meet.\n")

        # modify the action
        action.create(drive, "", category, description)

        return action

    return wrap


def make_button_for_action(button_name, button_id):
    """
       Decorator that wraps an action and make a button for it on the toolbar

       button_name: name of the button
       button_id: id of the button (kept by GPS)

       The input of the decorator = expecting argument for returning wrap
       = an action
    """
    def wrap(action):
        # create on_click function for the button from the action
        def on_click(button):
            action.execute_if_possible()

        # create the button and append it to the toolbar
        b = GPS.Button(button_id, button_name, on_click)
        GPS.Toolbar().append(b)

        return action

    return wrap


def create_target_from_workflow(target_name, workflow_name, workflow,
                                icon_name="gtk-print"):
    """
    Create a Target under the category Workflow from a workflow --
    to be feed by user.
    By building this target, the workflow is driven (executed).
    * target_name and workflow_name are all strings
    """

    # going to store the feeded workflow in a global variable
    global registered_workflows

    registered_workflows[workflow_name] = workflow

    xml1 = """
<target model="python" category="Workflow" name="%s">
<in-toolbar>TRUE</in-toolbar>
<icon>%s</icon>
<launch-mode>MANUALLY</launch-mode>
<read-only>TRUE</read-only>
<target-type>main</target-type>
<command-line>
    <arg>gps_utils.workflow.run_registered_workflows("%s", "</arg>
    """ % (target_name, icon_name, workflow_name)

    xml2 = """
    <arg>%T</arg>
    <arg>")</arg>
</command-line>
</target>
    """
    XML = xml1+xml2
    GPS.parse_xml(XML)
