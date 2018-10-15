"""This file provides support to display VCS diff in the editor."""

import GPS
import collections
from constructs import CAT_PACKAGE, CAT_PROCEDURE, INDENTATION_NONE, \
    VISIBILITY_PUBLIC

FILE_SECTION = "diff "
LOC_SECTION = "@@"
DIFF_CATEGORY = "Diff category"


class DiffLanguage(GPS.Language):

    def __init__(self):
        self.offsets = []

    def make_tuple(self, line, col):
        return line, col, self.offsets[line - 1] + col

    def get_loc_descr(self, string):
        """The format is @@ loc_descr @@ code"""
        return string.split(LOC_SECTION)[1]

    def create_construct(self, clist, is_file, text, start_line,
                         end_line, col_end):
        """
        :parameter is_file: |Boolean| indicate if we are adding a file or a
        diff section in the construct tree.
        :parameter text: |str| the content of the line
        """
        if is_file:
            type = CAT_PACKAGE
            name = text[len(FILE_SECTION):]
            entity_col = len(FILE_SECTION) + 1
        else:
            type = CAT_PROCEDURE
            name = self.get_loc_descr(text).strip()
            entity_col = len(LOC_SECTION) + 2

        clist.add_construct(type,
                            False,
                            VISIBILITY_PUBLIC,
                            name,
                            "",
                            self.make_tuple(start_line + 1, 1),
                            self.make_tuple(end_line, col_end),
                            self.make_tuple(start_line + 1, entity_col))

    def pairwise(self, lst):
        """ yield item i and item i+1 in lst. e.g.
        (lst[0], lst[1]), (lst[1], lst[2]), ..., (lst[-1], None)
        """
        if not lst:
            return
        for i in range(len(lst)-1):
            yield lst[i], lst[i+1]
        yield lst[-1], None

    def parse_constructs(self, constructs_list, gps_file, string):
        lines = string.splitlines()
        self.offsets = [0 for _ in lines]
        for i in range(1, len(lines)):
            self.offsets[i] = (self.offsets[i - 1] +
                               len(lines[i - 1]) + 1)

        # Key: |int| start line of file section
        # Elem: list of |int| start lines of code sections
        blocks = collections.OrderedDict()

        # Parse the file and create message
        for ix in range(0, len(lines)):
            if lines[ix].startswith(FILE_SECTION):
                blocks[ix] = []
            elif lines[ix].startswith(LOC_SECTION):
                blocks[list(blocks.keys())[-1]].append(ix)

        def _internal_parse(start_file, end_file):
            for start_code, end_code in self.pairwise(blocks[start_file]):
                if end_code:
                    self.create_construct(constructs_list,
                                          False,
                                          lines[start_code],
                                          start_code,
                                          end_code,
                                          len(lines[end_code]))
                else:
                    self.create_construct(constructs_list,
                                          False,
                                          lines[start_code],
                                          start_code,
                                          end_file,
                                          len(lines[end_file - 1]))
            self.create_construct(constructs_list,
                                  True,
                                  lines[start_file],
                                  start_file,
                                  end_file,
                                  len(lines[end_file - 1]))
        # Fill the construct_list
        for start_file, end_file in self.pairwise(list(blocks.keys())):
            if end_file:
                _internal_parse(start_file, end_file)
            else:
                _internal_parse(start_file, len(lines) - 1)


GPS.Language.register(DiffLanguage(), "Diff",
                      ".diff", "", "", INDENTATION_NONE)
