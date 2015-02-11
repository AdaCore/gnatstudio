""" This module defines a framework for writing workflows.

A workflow is a Python generator which can be used to execute
a chain of asynchronous actions, while retaining a sequential
structure.
"""

import sys
import GPS
import workflows.promises as promises
import types

# A table of all registered workflows
registered_workflows = {}


def run_registered_workflows(workflow_name, *args, **kwargs):
    """ Find workflow and run it with the driver.
    """
    try:
        wf = registered_workflows[workflow_name](*args, **kwargs)
        driver(wf)
    except KeyError:
        GPS.Console("Messages").write(
            "\nError: Workflow name not registered.\n")


def driver(gen_inst):
    """
    This is the main driver for workflows. You can pass your worklow (which is
    a python generator instance ) to it and it will execute it.

    From a worklow, you can yield two types of objects:

    - You can yield promises. Those will be chained so that when the promise
      resolves, the execution of the workflow is resumed, and any eventual
      result of the promise will be passed as result to the yield call.

    - You can yield other generators, in which case the driver will take care
      of consuming (executing) them, and then resume the execution of the
      current generator
    """
    gen_stack = [gen_inst]

    def internal(return_val=None):
        el = None
        while gen_stack:
            gen = gen_stack[0]
            try:
                # if there's feedback from previous event, tell the generator
                if return_val is not None:
                    el = gen.send(return_val)
                    return_val = None
                # otherwise just goto the next step
                else:
                    el = gen.next()

                if isinstance(el, types.GeneratorType):
                    gen_stack.insert(0, el)
                else:
                    break

            # indicates the end of the generator: exit cleanly
            except StopIteration:
                gen_stack.pop(0)

        if el is None:
            return

        # if promise instance is got from workflow, "then" it
        if isinstance(el, promises.Promise):
            el.then(internal)
        # otherwise, continue to the next object by internal()
        else:
            internal()

    internal()


# The following are decorators for workflows(generators)
def make_action_from_workflow(name, category="General",
                              description="", criteria=None):
    """
       Decorator that creates a GPS action from a workflow.

       name: string, name of the returning action
       category: category as action object required
       description: string, description of the result action
       criteria: function that returns boolean:
          the workflow is executed only when it returns True

       The input of the decorator is a workflow.
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


def make_button_for_action(button_name, icon_name):
    """
       Decorator that wraps an action and make a button for it on the toolbar

       :param button_name: name of the button
       :param icon_name: the icon to use.

       The input of the decorator = expecting argument for returning wrap
       = an action
    """
    def wrap(action):
        # create on_click function for the button from the action
        def on_click(button):
            action.execute_if_possible()

        # create the button and append it to the toolbar
        b = GPS.Button(icon_name, button_name, on_click)
        GPS.Toolbar().append(b)
        return action

    return wrap


def create_target_from_workflow(target_name, workflow_name, workflow,
                                icon_name="gps-print-symbolic"):
    """
    Create a Target under the category Workflow from a given workflow.
    Executing this target runs the workflow.

    target_name and workflow_name are strings
    """

    # going to store the feeded workflow in a global variable
    global registered_workflows

    registered_workflows[workflow_name] = workflow

    xml1 = """
<target model="python" category="Workflow" name="%s">
<in-toolbar>TRUE</in-toolbar>
<iconname>%s</iconname>
<launch-mode>MANUALLY</launch-mode>
<read-only>TRUE</read-only>
<do-not-save>TRUE</do-not-save>
<target-type>main</target-type>
<command-line>
    <arg>workflows.run_registered_workflows("%s", "</arg>
    """ % (target_name, icon_name, workflow_name)

    xml2 = """
    <arg>%T</arg>
    <arg>")</arg>
</command-line>
</target>"""

    XML = xml1 + xml2
    GPS.parse_xml(XML)
