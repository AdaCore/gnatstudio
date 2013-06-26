Enhancements to editors
-----------------------

Aliases expansion directly in editora :feature:`(GPS -- 2013-06-01 -- M411-022)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The aliases expansion feature now works directly in-editor, without presenting
the user a dialog for aliases with parameters. The user progresses through the
fields directly in the editor via the Ctrl+Tab shortcut. Visual feedback is
provided for the current field via highlighting.

Smart-completion
~~~~~~~~~~~~~~~~

Smart-completion on aliases :feature:`(GPS -- 2013-04-11 -- H603-006)`
.......................................................................

Aliases can now be completed and expanded by the regular smart-completion
mechanism.

.. figure:: completion_on_alias.*
   :width: 400pt
   :class: screenshot

   Smart completion for aliases


Visual indication of visibility :feature:`(GPS -- 2013-04-16 -- H605-006)`
..........................................................................

In the smart completion popup, entities that are not visible in the current
scope are now displayed in grey.


Completion for return blocks :feature:`(GPS -- 2013-02-04 -- M129-002)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add "return ... end return;" statement support for complete block completion

Indentation
~~~~~~~~~~~

Improved indentation of conditional expressions :feature:`(GPS -- 2013-05-17 -- M424-019)`
..........................................................................................

Ada 2012 If-expressions and case-expressions are better handled by the
indentation engine and in particular extra indentation levels are used for
each case branch.

Improved indentation of parenthesized expressions :feature:`(GPS -- 2013-06-20 -- M524-041)`
............................................................................................

Extra spaces following an open parenthesis is now taken into account when
indenting Ada code.


Refill automatically computes paragraphs :feature:`(GPS -- 2013-01-11 -- M111-033)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The menu /Edit/Selection/Refill will now either apply to the current
selection if it exists (as before), or automatically compute the extents
of the current paragraph to reformat (instead of only applying to the
current line).

.. figure:: refill.*
   :width: 400pt
   :class: screenshot

   Refill computes the bounds of the paragraph


Preserve location in new views :feature:`(GPS -- 2013-06-06 -- M124-022)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When creating a new view for an editor (for instance through drag-and-drop) the
new view is now displayed at the same location as the original editor, rather
than on the first line.


Transluscent highlighting colors :feature:`(GPS -- 2013-03-29 -- M329-013)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is now possible to specify translucent colors for error messages,
current lines,... in the editors. This is a convenient solution so that
the current line also shows whether it has an associated error.
