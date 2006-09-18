-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  <description>
--  This package implements a typical wizard widget, that helps guide the
--  user through a series of simple operations. Some typical examples are
--  the installation interfaces that ask for a directory, then the list of
--  packages to install, ...
--
--  You can set up as many pages as you want that will be displayed one by
--  one for the user to act upon them.
--
--  This widget will handle automatically the previous and next buttons, as
--  well as the finish buttons that appears when the user is on the last page
--  and that replaces the next button then.
--
--  A table of contents can be associated with this widget, and will be
--  displayed on the left. The currently displayed page is highlighted
--  automatically. This helps the user get a global idea of the process.
--  If you do not specify a table of contents, nothing is displayed.
--  </description>

with Gtk.Button;
with Gtk.Widget;
with Gtk.Style;
with Gtk.Label;
with GPS.Kernel;
with GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;

with Logo_Boxes;

package Wizards is

   type Wizard_Record is abstract new Logo_Boxes.Logo_Box_Record with private;
   type Wizard is access all Wizard_Record'Class;
   --  A wizard is a special kind of container that shows one of its children
   --  at a time, and provides navigation between them through Previous, Next
   --  and Apply buttons.
   --  The wizard is implemented as a dialog. You can therefore call Run to
   --  display the dialog and let the user interact with it. The return value
   --  of Run indicates whether the user pressed Finish (Gtk_Response_Apply)
   --  or cancel (Gtk_Response_Cancel).

   ------------------
   -- Wizard pages --
   ------------------

   type Wizard_Page_Record is abstract tagged private;
   type Wizard_Page is access all Wizard_Page_Record'Class;
   --  A page in the wizard

   type Wizard_Pages_Array is array (Positive range <>) of Wizard_Page;
   type Wizard_Pages_Array_Access is access Wizard_Pages_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Wizard_Pages_Array, Wizard_Pages_Array_Access);

   function Create_Content
     (Page : access Wizard_Page_Record;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
     is abstract;
   --  Return the widget to display when the page is active.
   --  This method can create the content of the page lazily. However, you'll
   --  need to take into account when the wizard is completed that not all
   --  pages might have been created.
   --  Wiz is the wizard in which the page is included.

   function Get_Content
     (Page : access Wizard_Page_Record'Class) return Gtk.Widget.Gtk_Widget;
   --  Return the widget that was created by Create_Content, or null if the
   --  latter was never called

   procedure Update_Page (Page : access Wizard_Page_Record);
   --  Called just before displaying this page (including the first time, just
   --  after Create_Content was called).
   --  The goal is to refresh the page based on the contents of other pages.
   --  The default is to do nothing.

   function Next_Page
     (Page : access Wizard_Page_Record;
      Wiz  : access Wizard_Record'Class) return Wizard_Page;
   --  Return the page to display when the user presses "Next" after this one.
   --  The new page must have been added to the wizard.
   --  The default behavior is to return null, which lets the wizard compute
   --  the next page by itself.
   --  Overriding this method can be used to have a non linear transition in
   --  the pages, for instance display a new set of pages if the user has
   --  pressed a toggle button in the first page.
   --        1 -> 2 -> 3 -> 4 -> 5 -> 6
   --              \-> 3bis ---/  (if the user selects an option on page 2)

   function Is_Complete
     (Page : access Wizard_Page_Record) return String;
   --  Return an error message to display for the current page.
   --  It should return the empty string "" if all fields on the page have a
   --  valid value, and should return the message to display at the top of the
   --  wizard otherwise.
   --  By default, the Finish button is activated when all pages return the
   --  empty string from this function.
   --  A user cannot move forward when a page is not complete. Therefore, you
   --  can do data validation in this function, and return a message if some
   --  data is incorrect. However, it is your responsability to change the
   --  sensitivity of the Next button once the content of the page is valid,
   --  through a call to Update_Buttons_Sensitivity. This is generally done by
   --  connecting to the "changed" signal on all fields and reset the button
   --  sensitivity from there.
   --  By default, this returns the empty string (no error).
   --  If a field is empty or is incorrect, but should be set, you should do a
   --  Grab_Focus on it.
   --  Be aware that this function might be called even before calling
   --  Create_Content if Lazy_Creation was set to True when it was added.

   procedure On_Destroy (Page : access Wizard_Page_Record);
   --  Called when the page is destroyed. This function should free internal
   --  data stored for the page (but need not take care of the GUI).

   -------------
   -- Wizards --
   -------------

   procedure Initialize
     (Wiz       : access Wizard_Record'Class;
      Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title     : String;
      Show_Toc  : Boolean := True);
   --  Initialize a new wizard.
   --  If Show_Toc is true, a column is displayed on the left of the wizard
   --  that shows the list of pages that have been registered for this wizard.
   --  Display the wizard by using the standard Gtk.Dialog.Run or a derivative
   --  of it.

   procedure Add_Page
     (Wiz           : access Wizard_Record;
      Page          : access Wizard_Page_Record'Class;
      Description   : String;
      Toc           : String := "";
      Lazy_Creation : Boolean := False);
   --  Add a new page in the wizard.
   --  This description will be displayed at the top of the page, and the
   --  Toc will be displayed on the left area if this area exists in this
   --  wizard.
   --  By default, Toc will take the same value as Description.
   --  If Lazy_Creation is True, then the page is only created the first time
   --  it is displayed, which saves startup time. However, this means that all
   --  subprograms like Is_Complete and Perform_Finish must take into account
   --  the fact that this page might not have been created at all if the user
   --  pressed Finished

   procedure Display_Message
     (Wiz      : access Wizard_Record;
      Msg      : String;
      As_Error : Boolean := False);
   --  Display a message in the wizard. It should be called by the
   --  Is_Complete procedure above.
   --  If As_Error is true, the message is displayed as an error message.
   --  An empty message should be displayed to hide the message area.

   function Can_Complete (Wiz : access Wizard_Record) return Boolean;
   --  Return whether the Finish button should be activated.
   --  By default, this checkes whether all the pages in the wizard return
   --  True for their Is_Complete function.

   procedure Update_Buttons_Sensitivity
     (Wiz : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Recompute whether the Previous, Next,... buttons should be clickable
   --  by the user. This is done by checking the current page's Is_Complete
   --  Status.
   --  You should connect this as a callback for all the changes in the
   --  fields of your page is you want to provide on-the-fly validation.

   procedure Perform_Finish (Wiz : access Wizard_Record) is abstract;
   --  Called when the user has pressed the Finish button.
   --  It is the reponsability of the wizard to ask the page for any action to
   --  be performed.
   --  Pressing Finish will in any case close the wizard after this function
   --  has been called.

   procedure Remove_Pages
     (Wiz   : access Wizard_Record;
      After : access Wizard_Page_Record'Class);
   --  Remove all pages after, but not including, After.

   function Get_Pages
     (Wiz : access Wizard_Record) return Wizard_Pages_Array_Access;
   --  Return the list of pages for this wizard.
   --  The returned array must not be modified.

   function Get_Kernel
     (Wiz : access Wizard_Record) return GPS.Kernel.Kernel_Handle;
   --  Return the kernel instance stored in Wiz

private
   type Wizard_Page_Record is abstract tagged record
      Toc           : Gtk.Label.Gtk_Label;
      Title         : GNAT.OS_Lib.String_Access;
      Content       : Gtk.Widget.Gtk_Widget;
      Was_Complete  : Boolean;
   end record;

   type Wizard_Record is abstract new Logo_Boxes.Logo_Box_Record with record
      Cancel          : Gtk.Button.Gtk_Button;
      Finish          : Gtk.Button.Gtk_Button;
      Next            : Gtk.Button.Gtk_Button;
      Previous        : Gtk.Button.Gtk_Button;

      Current_Page    : Positive;
      Pages           : Wizard_Pages_Array_Access;
      Highlight_Style : Gtk.Style.Gtk_Style;
      Normal_Style    : Gtk.Style.Gtk_Style;
      Kernel          : GPS.Kernel.Kernel_Handle;
   end record;
end Wizards;
