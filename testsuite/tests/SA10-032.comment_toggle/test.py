from gps_utils.internal.utils import run_test_driver, gps_assert


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    v = b.current_view()

    # test chaining 5 single-line toggles on code

    v.goto(b.at(5, 4))
    for j in range(5):
        GPS.execute_action("toggle comment")

    gps_assert(b.get_chars(b.at(5, 1), b.at(9, 14)),
               """   --  if True then
   --     if True then
   --        null;
   --     end if;
   --  end if;""",
               "issue with chaining toggles (comment, Ada)")

    # test chaining 5 single-line toggles on commented code

    v.goto(b.at(5, 4))
    for j in range(5):
        GPS.execute_action("toggle comment")
    gps_assert(b.get_chars(b.at(5, 1), b.at(9, 10)),
               """   if True then
      if True then
         null;
      end if;
   end if;""",
               "issue with chaining toggles (uncomment, Ada)")

    b.select(b.at(5, 11), b.at(9, 8))
    GPS.execute_action("toggle comment")
    gps_assert(b.get_chars(b.at(5, 1), b.at(9, 14)),
               """   --  if True then
   --     if True then
   --        null;
   --     end if;
   --  end if;""",
               "issue with multi-line toggle (comment, Ada)")

    b = GPS.EditorBuffer.get(GPS.File("t.c"))
    v = b.current_view()

    # test selection toggle on C code

    b.select(b.at(10, 1), b.at(12, 4))
    GPS.execute_action("toggle comment")
    gps_assert(b.get_chars(b.at(10, 1), b.at(12, 28)),
               """  /* for (i=0;i<100;i++){ */
  /*   for(;fork(););     */
  /* }                    */""",
               "issue with multi-line toggle (comment, C)")

    # verify that the selection bounds are cleverly moved after toggle
    gps_assert((b.selection_start(), b.selection_end()),
               (b.at(10, 1), b.at(12, 7)),
               "selection not moved correctly after toggle to comment")
    GPS.execute_action("toggle comment")
    gps_assert((b.selection_start(), b.selection_end()),
               (b.at(10, 1), b.at(12, 4)),
               "selection not moved correctly after toggle to uncomment")
