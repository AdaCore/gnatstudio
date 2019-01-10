------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNAT.Strings;

with Glib.Object;               use Glib.Object;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;

with GNATCOLL.Projects;         use GNATCOLL.Projects;
with Dialog_Utils;              use Dialog_Utils;
with GPS.Kernel;                use GPS.Kernel;
with Project_Viewers;           use Project_Viewers;
with Toolchains;                use Toolchains;

package Toolchains_Editor is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register script commands in GPS

   type Languages_Page_Record is new Project_Editor_Page_Record
     (Flags => Multiple_Projects or Multiple_Scenarios)
     with private;
   type Languages_Page_Access is access all Languages_Page_Record'Class;
   overriding procedure Initialize
     (Self         : not null access Languages_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project);
   overriding function Edit_Project
     (Self               : not null access Languages_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   --  Create the "Languages" page for the project properties

   procedure When_Languages_Change
     (Self     : not null access Languages_Page_Record;
      Data     : access GObject_Record'Class;
      Callback : Cb_GObject_UTF8_String_Void);
   --  When the list of selected languages changes, calls the procedure and
   --  pass it Data.

   function Get_Languages
     (Self : not null access Languages_Page_Record)
      return GNAT.Strings.String_List_Access;
   --  Return the list of languages selected by the user

   type Toolchain_Page_Record is new Project_Editor_Page_Record
     (Flags => Multiple_Projects or Multiple_Scenarios)
   with private;
   type Toolchain_Page is access all Toolchain_Page_Record'Class;
   overriding procedure Initialize
     (Self         : not null access Toolchain_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project);
   overriding function Edit_Project
     (Self               : not null access Toolchain_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding function Is_Visible
     (Self               : not null access Toolchain_Page_Record;
      Languages          : GNAT.Strings.String_List) return Boolean;
   overriding procedure Destroy (Self : in out Toolchain_Page_Record);
   --  Create the "Toolchain" page for the project properties

private
   type Languages_Page_Record is new Project_Editor_Page_Record
     (Flags => Multiple_Projects or Multiple_Scenarios)
   with record
      Kernel          : access Kernel_Handle_Record'Class;
      Languages       : Gtk_Tree_View;
      Lang_Model      : Gtk_Tree_Store;
      Toggle_Renderer : Gtk_Cell_Renderer_Toggle;
   end record;

   type Compilers_Scan_Status is (Not_Launched, Scanning, Complete, Failed);
   --  Type representing the different statuses when scanning the compilers
   --  available on the user's host.

   type Toolchain_Page_Record is new Project_Editor_Page_Record
     (Flags => Multiple_Projects or Multiple_Scenarios)
   with record
      Kernel            : access Kernel_Handle_Record'Class;
      Toolchains_Tree   : Gtk_Tree_View;
      Model             : Gtk_Tree_Store;
      Toolchains_View   : Dialog_View_With_Button_Box;
      Details_View      : Dialog_View;
      Toolchain         : Toolchains.Toolchain := Toolchains.Null_Toolchain;
      Scan_Status       : Compilers_Scan_Status := Not_Launched;
      Read_Only         : Boolean := False;
      Edited_Prj        : GNATCOLL.Projects.Project_Type;

      Languages_Cache   : GNAT.Strings.String_List_Access;
      --  A cache so that we can easily reset the displayed tools
   end record;

end Toolchains_Editor;
