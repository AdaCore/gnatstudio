-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
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

--  This package provides a widget to edit the list of languages. It provides a
--  number of check buttons, one per known language, which the user can select.

with Gtk.Check_Button;
with Gtk.Frame;
with Gtk.Widget;
with Glide_Kernel;
with GNAT.OS_Lib;
with Projects;

package Languages_Lists is

   type Languages_List_Record is new Gtk.Frame.Gtk_Frame_Record with private;
   type Languages_List is access all Languages_List_Record'Class;

   procedure Gtk_New
     (List           : out Languages_List;
      Kernel         : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project        : Projects.Project_Type);
   --  Create a new languages editor.
   --  If Edit_Compilers is true, List is setup to also edit the languages

   function Get_Languages
     (List : access Languages_List_Record) return GNAT.OS_Lib.Argument_List;
   --  Return the list of languages selected by the user. The returned array
   --  must be freed by the caller.

   function Is_Selected
     (List : access Languages_List_Record; Language : String)
      return Boolean;
   --  Return True if Language is currently selected in List

   function Get_Check_Button
     (List : access Languages_List_Record; Language : String)
      return Gtk.Check_Button.Gtk_Check_Button;
   --  Return the check button representing Language

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following signals can be emitted:
   --
   --  - "changed"
   --    procedure Handler (List : access Languages_List_Record'Class);
   --
   --    Emitted when the list of selected languages has changed
   --  </signals>


private
   type Widget_Array is array (Natural range <>) of Gtk.Widget.Gtk_Widget;
   type Widget_Array_Access is access Widget_Array;

   type Languages_List_Record is new Gtk.Frame.Gtk_Frame_Record with record
      Kernel    : Glide_Kernel.Kernel_Handle;
      Languages : Widget_Array_Access;
   end record;

end Languages_Lists;
