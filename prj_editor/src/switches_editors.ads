-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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
with Glib.Object;
with Glide_Kernel;
with Switches_Editor_Pkg; use Switches_Editor_Pkg;
with Prj;
with Prj.Tree;
with Prj_API;

package Switches_Editors is

   type Switches_Edit_Record is new Switches_Editor_Record with private;
   type Switches_Edit is access all Switches_Edit_Record'Class;

   type Tool_Names is
     (Gnatmake, Ada_Compiler, C_Compiler, Cpp_Compiler,
      Pretty_Printer, Binder, Linker);

   procedure Gtk_New (Editor : out Switches_Edit);
   --  Create a new switches editor

   procedure Set_Visible_Pages
     (Editor : access Switches_Edit_Record;
      Languages : GNAT.OS_Lib.Argument_List);
   --  Set the visible pages based on the specific languages

   function Get_Window
     (Editor : access Switches_Edit_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the window to use to insert the editor in a parent container.
   --
   --  From the moment this function is called, you should no longer use
   --  Editor itself, but only the result of Get_Window, unless you want to
   --  put the editor back in Editor.
   --  Likewise, you shouldn't call Show_All on the editor itself, but rather
   --  on the window.

   function From_Window
     (Window : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Switches_Edit;
   --  Return the switches editor from the window.

   procedure Set_Page
     (Editor : access Switches_Edit_Record; Tool : Tool_Names);
   --  Show a specific page of the editor.

   ------------------------
   -- Access to switches --
   ------------------------

   function Get_Switches
     (Editor : access Switches_Edit_Record; Tool : Tool_Names)
      return GNAT.OS_Lib.Argument_List;
   --  Return the switches set in the editor for one of the specific tools.
   --  It is your responsability to free the strings (see Free below).

   procedure Set_Switches
     (Editor   : access Switches_Edit_Record;
      Tool     : Tool_Names;
      Switches : GNAT.OS_Lib.Argument_List);
   --  Set the initial value for the switches, for a specific tool

   procedure Set_Switches
     (Editor : access Switches_Edit_Record; Project_View : Prj.Project_Id);
   --  Set the initial value for the switches, based on the contents
   --  of Project_View. If a page doesn't exist in Editor, it will not be
   --  automatically created.

   function Generate_Project
     (Switches     : access Switches_Edit_Record'Class;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project      : Prj.Tree.Project_Node_Id;
      Project_View : Prj.Project_Id;
      Files        : GNAT.OS_Lib.Argument_List)
      return Prj_API.Project_Node_Array;
   --  Generate the information in Project to represent the status of Switches.
   --  The list of modified projects is returned (there can be several in case
   --  one of the modified packages was a renaming of another package).
   --  Project_View can be No_Project, in which case the return value will
   --  always be non empty, after modification of the project.

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

   -----------------------------------------------------
   -- Editing switches for a specific file or project --
   -----------------------------------------------------
   --  The subprograms below are convenience subprogram to edit some specific
   --  switches. They provide a higher-level framework over the standard
   --  switches editor.

   procedure Edit_Switches
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access);
   --  Callback suitable for a contextual menu item.  If the file name is the
   --  empty string, then the default switches for the project are edited,
   --  otherwise the switches for the specific file are edited.

   procedure Edit_Switches_For_Context
     (Context       : Glide_Kernel.Selection_Context_Access;
      Force_Default : Boolean := False);
   --  Same as Edit_Switches, but if Force_Default is True it always edit the
   --  default switches, even if there is a file information in Context.

   procedure Edit_Switches_For_Files
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project      : Prj.Tree.Project_Node_Id;
      Project_View : Prj.Project_Id;
      Files        : GNAT.OS_Lib.Argument_List);
   --  Edit the switches for a list of files. All the files will be assigned
   --  the same switches.
   --  If there are no files in Files, the default switches are edited.

private
   type Page_Filter is mod 2 ** 32;
   Gnatmake_Page       : constant Page_Filter := 2 ** 0;
   Ada_Page            : constant Page_Filter := 2 ** 1;
   C_Page              : constant Page_Filter := 2 ** 2;
   Cpp_Page            : constant Page_Filter := 2 ** 3;
   Pretty_Printer_Page : constant Page_Filter := 2 ** 4;
   Binder_Page         : constant Page_Filter := 2 ** 5;
   Linker_Page         : constant Page_Filter := 2 ** 6;
   All_Pages           : constant Page_Filter :=
     Gnatmake_Page or Ada_Page or C_Page or Cpp_Page or
     Pretty_Printer_Page or Binder_Page or Linker_Page;

   type Switches_Edit_Record is new Switches_Editor_Record with record
      Kernel          : Glide_Kernel.Kernel_Handle;
      Project_View    : Prj.Project_Id;
      Pages           : Page_Filter := All_Pages;
      Files           : GNAT.OS_Lib.Argument_List_Access;

      Block_Refresh   : Boolean := False;
      --  If this is True, then clicking on the toggle buttons in the
      --  editor will not refresh the command lines. This is required so that
      --  people can edit the command lines manually.

      Prev_Make_Debug : Boolean := False;
      --  Store the previous value of Make_Debug flag.
   end record;

end Switches_Editors;
