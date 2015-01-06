import unittest
import support
from support import GNATCOLL_TestCase, chdir, pathsep, requires_windows
import os
import re


class Test(GNATCOLL_TestCase):
    @chdir("prj1")
    def test_prj(self):
        self.gprbuild()
        self.runexec("obj/test_projects", "")

    @chdir("K302-015")
    def test_K302_015(self):
        self.gprbuild("iter.gpr")
        self.runexec("obj/iter", "test.out", sorted=True)

    @chdir("K712-015")
    def test_K712_015(self):
        self.gprbuild("testit.gpr")
        self.runexec("test_it", "test.out")

    @chdir("K926-026")
    def test_K926_026(self):
        self.gprbuild("prj.gpr")
        self.runexec("main", "")

    @chdir("KB30-016")
    def test_KB30_016(self):
        self.gprbuild("testit.gpr")
        self.runexec("test_it", "")

    @chdir("L801-020")
    def test_L801_020(self):
        self.gprbuild("default.gpr")
        self.runexec("obj/test_projects", "")

    @chdir("c_sources")
    def test_c_sources(self):
        self.gprbuild("default.gpr")
        self.runexec("obj/test_c", "test.out")

    @chdir("M320-003")
    def test_M320_003(self):
        # We want the same error messages in gprbuild and GNATCOLL
        # (GNATCOLL used to drop the missing withs)
        self.gprbuild("xxxx.gpr")
        self.runexec("main", "test.out")

    @chdir("N728-016")
    def test_N728_016(self):
        # Message about runtime not being found should be passed
        # to error traceback
        self.gprbuild("gnatprove.gpr")
        self.runexec("main", "")

    @chdir("objects")
    def test_objects(self):
        # Test that we can correctly find the location of ALI files
        # in the presence of extending projects
        self.runexec(["sh", "test.cmd"], "")

    @chdir("objects2")
    def test_objects2(self):
        # Test that we can correctly find the location of ALI files
        # when using xref_subdirs
        self.runexec(["sh", "test.cmd"], "")

    @chdir("aggregate")
    def test_aggregate(self):
        # Test that we can load aggregate project, get sorce files
        # and iterate through project hierarchy
        self.gprbuild("default.gpr")
        self.runexec("main", "")

    @chdir("aggregate_info")
    def test_aggregate_info(self):
        # Test that we can get File_Info via Info_Set and results
        # are correct
        self.gprbuild("default.gpr")
        self.runexec("main", "")

    @requires_windows
    @chdir("aggregate_info_win")
    def test_aggregate_info_win(self):
        # Test on mixed casing passed to Info
        self.gprbuild("default.gpr")
        self.runexec("main", "")

    @chdir("aggregate_create")
    def test_aggregate_create(self):
        # Test on Create for aggregate projects
        self.gprbuild("aggr.gpr")
        self.gprbuild("default.gpr")
        self.runexec("main", "")

    @chdir("aggregate_ali")
    def test_aggregate_ali(self):
        # Test that ALI files with same base names but from different
        # aggregated projects do not exclude each other
        self.gprbuild("p2_2.gpr")
        self.gprbuild("p2.gpr")
        self.gprbuild("p1.gpr")
        self.gprbuild("default.gpr")
        self.runexec("main", "")

    @support.requires_not_windows
    @chdir("N918-040")
    def test_bareboard(self):
        self.create_fake_bb_compiler('install', 'arm-eabi', '6.1.0w', '3.4.6')
        self.gprbuild('main_prj')

        m = re.search('for Target use "(.*?)"', open('auto.cgpr').read())
        target = m.group(1)

        def customFilter(actual):
            return actual.replace(target, "<native>")

        self.runexec(['sh', 'test.sh'], 'test.out', customFilter=customFilter)

    @requires_windows
    @chdir("L908-006")
    def test_L908_006(self):
        # Test that backslashes are replaced by slashes when changing attrs
        self.gprbuild("default.gpr")
        self.runexec("main", "")
