------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

--  This package provides a view that displays the memory usage of an
--  executable. This view is only activated if the '--print-memory-usage'
--  switch is given to the linker when building the executable with the
--  'Build All' or 'Build Main' Build Targets.
--
--  See the Memory_Usage_Views.Linker_Parser package for more information
--  about the way this switch is enabled/disabled and how the linker's output
--  is parsed when this switch is present.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with GNATCOLL.VFS;                           use GNATCOLL.VFS;

with Gtkada.Tree_View;                       use Gtkada.Tree_View;
with Gtk.Label;                              use Gtk.Label;
with Gtk.Menu;
with Gtk.Scrolled_Window;                    use Gtk.Scrolled_Window;
with Gtk.Toolbar;
with Gtk.Tree_Model;                         use Gtk.Tree_Model;
with Gtk.Tree_Store;                         use Gtk.Tree_Store;
with Gtk.Tree_View_Column;                   use Gtk.Tree_View_Column;
with Gtk.Widget;                             use Gtk.Widget;
with Gtkada.MDI;

with Generic_Views;                          use Generic_Views;
with GPS.Kernel;                             use GPS.Kernel;
with GPS.Kernel.MDI;                         use GPS.Kernel.MDI;
with GPS.Search;                             use GPS.Search;

package Memory_Usage_Views is

   type Memory_Usage_View_Record is new Generic_Views.View_Record with private;
   type Memory_Usage_View is access all Memory_Usage_View_Record'Class;
   --  Type representing the memory usage view.

   type Memory_Region_Description is private;
   --  Type representing a memory region

   type Memory_Section_Description is private;
   --  Type representing a memory section

   type Module_Description is private;
   --  Type representing a module description.
   --  A module represents a file based split of ressources consumed by a given
   --  object file for a particular section (e.g: ressources consumed by
   --  the main unit's object file for the .text section).

private

   type Module_Description is record
      Obj_File : Virtual_File;
      Lib_File : Virtual_File;
      Origin   : Unbounded_String;
      Size     : Integer;
   end record;

   package Module_Description_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Module_Description, "=");

   type Memory_Section_Description is record
      Name    : Unbounded_String;
      Origin  : Unbounded_String;
      Length  : Integer;
      Modules : Module_Description_Lists.List;
   end record;

   package Memory_Section_Description_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Memory_Section_Description,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   type Memory_Region_Description is record
      Name      : Unbounded_String;
      Origin    : Unbounded_String;
      Length    : Integer;
      Used_Size : Integer;
      Sections  : Memory_Section_Description_Maps.Map;
   end record;

   function "<" (Left, Right : Memory_Region_Description) return Boolean;

   package Memory_Region_Description_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Memory_Region_Description,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   type Memory_Usage_Tree_View_Record is new Tree_View_Record with record
      Pattern : Search_Pattern_Access;
   end record;
   type Memory_Usage_Tree_View is
     access all Memory_Usage_Tree_View_Record'Class;
   overriding function Is_Visible
     (Self       : not null access Memory_Usage_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean;

   function Get_ID
     (Self : not null access Memory_Usage_Tree_View_Record'Class;
      Row  : Gtk_Tree_Iter) return String;

   package Expansions is new Expansion_Support
     (Tree_Record => Memory_Usage_Tree_View_Record,
      Id          => String,
      Get_Id      => Get_ID,
      Hash        => Ada.Strings.Hash,
      "="         => "=");

   type Memory_Usage_View_Record is new Generic_Views.View_Record with record
      Scrolled          : Gtk_Scrolled_Window;
      Memory_Tree       : Memory_Usage_Tree_View;
      Memory_Tree_Model : Gtk_Tree_Store;
      Col_Addresses     : Gtk_Tree_View_Column;
      No_Data_Label     : Gtk_Label;
   end record;
   overriding procedure Create_Menu
     (View    : not null access Memory_Usage_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Create_Toolbar
     (View    : not null access Memory_Usage_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Memory_Usage_View_Record;
      Pattern : in out GPS.Search.Search_Pattern_Access);

   function Initialize
     (Self : access Memory_Usage_View_Record'Class) return Gtk_Widget;
   --  Initialize the memory usage view widget

   procedure On_Init
     (Self : not null access Memory_Usage_View_Record'Class);
   --  Called when creating the view.
   --  Used to connect to the Preferences_Changed hook.

   procedure Refresh
     (Self           : access Memory_Usage_View_Record'Class;
      Memory_Regions : Memory_Region_Description_Maps.Map);
   --  Refresh the given memory usage view to display the given memory usage
   --  data.

   package Memory_Usage_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name               => "Memory_Usage_Views",
      View_Name                 => "Memory Usage",
      Formal_View_Record        => Memory_Usage_View_Record,
      Formal_MDI_Child          => GPS_MDI_Child_Record,
      Local_Config              => True,
      Initialize                => Initialize,
      Areas                     => Gtkada.MDI.Sides_Only,
      Position                  => Gtkada.MDI.Position_Left);
   --  Instantiation of the Generic_Views.Simple_Views package with
   --  the parameters we want for our memory usage views.

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);

end Memory_Usage_Views;
