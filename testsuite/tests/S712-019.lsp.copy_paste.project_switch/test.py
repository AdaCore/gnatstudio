from gps_utils.internal.utils import run_test_driver, gps_assert


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.select(b.at(2, 1), b.at(4, 12))
    GPS.execute_action("Copy to Clipboard")

    GPS.Project.load("q.gpr")
    b = GPS.EditorBuffer.get(GPS.File("p.ads"))
    b.current_view().goto(b.at(2, 1))
    GPS.execute_action("Paste from Clipboard")

    gps_assert(b.get_chars(), """package p is
--  bla
--  bla bla
--  bla bla
end p;
""", "paste did not work after switching projects")
