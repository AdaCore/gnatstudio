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
--  This package implements a new widget to interactively edit the switches
--  for the GNAT tools (currently supported are gnatmake, gcc, gnatbind and
--  gnatlink).
--  A GUI is provided for the more common switches, but the user can always
--  edit them through an interactive command line.
--  </description>

--  This file extends and replaces the GLADE-generated unit
--  Switches_Editor_Pkg.

with Gtk.Widget;
with GNAT.OS_Lib;
with Switches_Editor_Pkg; use Switches_Editor_Pkg;

package Switches_Editors is

   type Switches_Edit_Record is new Switches_Editor_Record with private;
   type Switches_Edit is access all Switches_Edit_Record'Class;

   type Page_Filter is mod 2 ** 4;
   Gnatmake_Page : constant Page_Filter := 2 ** 0;
   Compiler_Page : constant Page_Filter := 2 ** 1;
   Binder_Page   : constant Page_Filter := 2 ** 2;
   Linker_Page   : constant Page_Filter := 2 ** 3;
   All_Pages     : constant Page_Filter :=
     Gnatmake_Page or Compiler_Page or Binder_Page or Linker_Page;

   type Tool_Names is (Gnatmake, Compiler, Binder, Linker);

   procedure Gtk_New (Editor : out Switches_Edit);
   --  Create a new switches editor

   procedure Destroy_Pages
     (Editor : access Switches_Edit_Record; Pages : Page_Filter);
   --  Destroy specific pages in the editor, and remove them from the display.
   --  The pages are completely removed from memory, but it isn't possible
   --  to recreate them later on.

   function Get_Window
     (Editor : access Switches_Edit_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the window to use to insert the editor in a parent container.
   --  You should not use Editor itself, which is a top-level window.
   --  Likewise, you shouldn't call Show_All on the editor itself, but rather
   --  on the window.

   ------------------------
   -- Access to switches --
   ------------------------

   function Get_Switches
     (Editor : access Switches_Edit_Record; Tool : Tool_Names)
      return GNAT.OS_Lib.Argument_List;
   --  Return the switches set in the editor for one of the specific tools.
   --  It is your responsability to free the strings (see Free below).

   procedure Free (Switches : in out GNAT.OS_Lib.Argument_List);
   procedure Free (Switches : in out GNAT.OS_Lib.Argument_List_Access);
   --  Free all the strings in Switches, and Switches itself when applicable.

   procedure Set_Switches
     (Editor   : access Switches_Edit_Record;
      Tool     : Tool_Names;
      Switches : GNAT.OS_Lib.Argument_List);
   --  Set the initial value for the switches, for a specific tool

   procedure Filter_Switches
     (Editor   : access Switches_Edit_Record;
      Tool     : Tool_Names;
      Switches : in out GNAT.OS_Lib.Argument_List);
   --  Remove from Switches all the ones that can be set directly from
   --  the GUI. As a result, on exit Switches will only contain non-null
   --  values for the switches that were set manually by the user, and that
   --  don't have GUI equivalents

   ---------------------------
   -- Callbacks for the GUI --
   ---------------------------
   --  The subprograms below shouldn't be used directly, and are only meant
   --  as callbacks for the graphical user interface.

   procedure Update_Cmdline
     (Editor : access Switches_Edit_Record; Tool : Tool_Names);
   --  Update the command lines found at the bottom of the page for Tool.
   --  This adds the switches set through the buttons, and keeps the switches
   --  added by the user.

   procedure Update_Gui_From_Cmdline
     (Editor : access Switches_Edit_Record; Tool : Tool_Names);
   --  Update the GUI from the contents of the command line for Tool.
   --  This is called every time the user has inserted new switches in the
   --  command line, so that we can keep the GUI and the command line coherent

private
   type Switches_Edit_Record is new Switches_Editor_Record with record
      Pages : Page_Filter := All_Pages;

      Block_Refresh : Boolean := False;
      --  If this is True, then clicking on the toggle buttons in the
      --  editor will not refresh the command lines. This is required so that
      --  people can edit the command lines manually.
   end record;

end Switches_Editors;
