-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
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
--
--  The default when the user presses previous or next is to display the
--  page that was entered next in the wizard. However, you can override this
--  behavior by connecting the "clicked" signal to the Next_Button and
--  Previous_Button, and by calling Set_Page yourself. Remember to also
--  call
--     Emit_Stop_By_Name (Next_Button (Wiz), "clicked");
--  on the button so that the default behavior is deactivated.
--
--  This allows you to have behaviors like:
--     1  ->  2  ->  3  ->  4  ->  5  -> 6
--            \            /
--             \-> 7 -> 8 /  (if the user selects a specific option on page 2)
--
--  </description>

with Wizard_Window_Pkg;
with Gtk.Button;
with Gtk.Widget;
with Gtk.Style;
with Gdk.Pixmap;
with Gdk.Bitmap;
with Glide_Kernel;

package Wizards is

   type Wizard_Record is new Wizard_Window_Pkg.Wizard_Window_Record
     with private;
   type Wizard is access all Wizard_Record'Class;

   procedure Gtk_New
     (Wiz : out Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Title : String;
      Bg : String;
      Num_Pages : Positive);
   --  Create a new wizard.
   --  Bg is the color to use for the background of the table of contents.
   --  Num_Pages is the number of pages. When the last one is displayed, the
   --  Next button is replaced by the Finished button.

   procedure Initialize
     (Wiz       : access Wizard_Record'Class;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Title     : String;
      Bg        : String;
      Num_Pages : Positive);
   --  Internal function used to create the new widget

   procedure Set_Toc
     (Wiz      : access Wizard_Record;
      Page_Num : Positive;
      Toc      : String := "";
      Level    : Integer := 1);
   --  Set the table-of-contents entry to be used for the Page_Num-th page in
   --  the wizard.
   --  Whereas the pages themselves can be created lazily only before they
   --  actually need to be displayed, you should always set the table of
   --  content entries before displaying the widget.
   --  Level is the level of indentation for this new entry. 1 is the
   --  top-level, 2 is indented by one, ...
   --  ???  Sub-levels can be hidden later on.
   --  Page numbers start at 1.

   procedure Set_Page
     (Wiz   : access Wizard_Record;
      Page_Num : Positive;
      Page  : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set the widget to use when displaying the Page_Num-th page in the
   --  wizard.
   --  Note: This procedure can be called directly from the callback
   --  "switch_page", in case some of the pages take a long time to create and
   --  you want the wizard to start as fast as possible.

   function Get_Nth_Page
     (Wiz : access Wizard_Record; Page_Num : Positive)
      return Gtk.Widget.Gtk_Widget;
   --  Return the widget displayed for the Page_Num-th page.
   --  null is return if this widget hasn't been set yet.

   procedure Add_Logo
     (Wiz    : access Wizard_Record;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   --  Add a pixmap in the lower-left corner of the wizard

   procedure Set_Current_Page (Wiz : access Wizard_Record; Num : Positive);
   --  Change the page currently displayed.

   procedure Set_Wizard_Title (Wiz : access Wizard_Record; Title : String);
   --  Set the current title for the wizard.
   --  You should probably change this every time a new page is displayed
   --  by the user.

   function Get_Current_Page (Wiz : access Wizard_Record) return Positive;
   --  Return the index of the current page.

   function Cancel_Button
     (Wiz : access Wizard_Record) return Gtk.Button.Gtk_Button;
   --  Return the button used for "cancel".
   --  Wiz will be destroyed as the last callback on "clicked" for this
   --  button.

   function Previous_Button
     (Wiz : access Wizard_Record) return Gtk.Button.Gtk_Button;
   --  Return the button used for "previous"

   function Next_Button
     (Wiz : access Wizard_Record) return Gtk.Button.Gtk_Button;
   --  Return the button used for "next"

   function Finish_Button
     (Wiz : access Wizard_Record) return Gtk.Button.Gtk_Button;
   --  Return the button used for "finish".
   --  This button can be clicked by the user only when on the last page, and
   --  will not be visible for other pages.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "switch_page"
   --    procedure Handler
   --      (Wiz : access Wizard_Record'Class; Page_Num : Guint);
   --
   --    Notify the user when a new page is about to be displayed. Page_Num is
   --    the number of the page that will be displayed.
   --  </signals>

private

   type Widget_Array is array (Positive range <>) of Gtk.Widget.Gtk_Widget;
   type Widget_Array_Access is access Widget_Array;

   type Wizard_Record is new Wizard_Window_Pkg.Wizard_Window_Record with record
      Toc             : Widget_Array_Access;
      Current_Page    : Positive;
      Pages           : Widget_Array_Access;
      Highlight_Style : Gtk.Style.Gtk_Style;
      Normal_Style    : Gtk.Style.Gtk_Style;
      Has_Toc         : Boolean := False;
   end record;

   pragma Inline (Previous_Button);
   pragma Inline (Next_Button);
   pragma Inline (Cancel_Button);
   pragma Inline (Get_Current_Page);
   pragma Inline (Finish_Button);
end Wizards;
