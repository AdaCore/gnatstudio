#!/usr/bin/env python

# Parse a .rst file to extract documentation for some of the GNAT switches.

doc = 'gnat/building_executable_programs_with_gnat.rst'

import re

try:
    input = open(doc)
except IOError:
    print('%s not found' % doc)
    exit()


class Switch(object):
    INVALID = ['-gnaty', '-gnatV', '-gnatw{xxx}', '-gnaty-', '-gnaty+']
    # Switches that appear in the documentation, but that we do not want to
    # document.

    BOLD = re.compile('\*(.+?)\*')
    SAMP = re.compile(':switch:`(.+?)`')
    COMMENT = re.compile('\.\. .*$')

    def __init__(self, name):
        self.name = name
        self.short = ''
        self.full = ''
        self.in_code_block = False

    def __cleanup_before_add(self, descr):
        """
        Cleanup part of a description string
        """
        if not self.in_code_block:
            descr = descr.strip()

        descr = Switch.BOLD.sub(r'\1', descr)
        descr = Switch.SAMP.sub(r'\1', descr)
        descr = descr.replace("<", "&lt;").replace(">", "&gt;")

        if '.. code-block:: ada' in descr:
            self.in_code_block = True
            return ''
        else:
            descr = Switch.COMMENT.sub('', descr)
            if self.in_code_block:
                return descr
            elif descr:
                return descr + ' '
            else:
                return '\n'

    def add_short_descr(self, descr):
        self.short += self.__cleanup_before_add(descr)

    def add_full_descr(self, descr):
        self.full += self.__cleanup_before_add(descr)

    def display(self):
        if (self.name.startswith('-gnatw') or
            self.name.startswith('-gnatV') or
            self.name.startswith('-gnaty')) \
           and self.name not in self.INVALID:

            print("    '%s': [" % self.name)
            print('         """%s""",' % self.short.strip())
            print('         """\n%s\n"""],' % self.full.strip())

        self.name = ''   # Prevent outputing the same switch several times


STATE_NOT_STARTED = 0         # analysis not started yet
STATE_SHORT_DESCR = 1         # analyzing the short description
STATE_FULL_DESCR = 2          # analyzing the full description
state = STATE_NOT_STARTED
switch = Switch('')   # The current switch

print('switches_comments={')

new_switch = re.compile('^:switch:`-')  # start of line for new switch

for line in input.readlines():
    is_new_switch = new_switch.match(line)
    if is_new_switch:
        name = line[9:-2]
        switch.display()
        switch = Switch(name)
        state = STATE_SHORT_DESCR

    elif state == STATE_FULL_DESCR and line.startswith('.. '):
        switch.display()   # Display the previous switch if needed
        state = STATE_NOT_STARTED

    elif state == STATE_SHORT_DESCR:
        if line.strip() == '':
            state = STATE_FULL_DESCR
        else:
            switch.add_short_descr(line)

    elif state == STATE_FULL_DESCR:
        switch.add_full_descr(line)

switch.display()
print('}')
