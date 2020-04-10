------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2004-2020, AdaCore                     --
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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNATCOLL.VFS;     use GNATCOLL.VFS;
with GNATCOLL.Xref;    use GNATCOLL.Xref;
with GPS.Kernel;       use GPS.Kernel;
with Gtk.Tree_Model;   use Gtk.Tree_Model;
with Gtk.Widget;       use Gtk.Widget;
with Gtkada.Tree_View;
with Language;         use Language;

package Outline_View is

   Outline_Error : exception;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   ---------------------
   -- Tooltip Factory --
   ---------------------

   type Outline_Tooltip_Factory_Type is
     access function
       (Kernel      : not null access Kernel_Handle_Record'Class;
        File        : GNATCOLL.VFS.Virtual_File;
        Entity_Name : String;
        Line        : Integer;
        Column      : Visible_Column) return Gtk_Widget;
   --  Type representing a tooltip factory for the Outline view.

   procedure Set_Outline_Tooltip_Factory
     (Tooltip_Factory : not null Outline_Tooltip_Factory_Type);
   --  Set the tooltip factory to use when hovering on the Outline.

   procedure Set_Outline_Tooltips_Synchronous (Synchronous : Boolean);
   --  Set it to True when tooltips created via the factory are ready to be
   --  shown immediately, False otherwise.

   ----------------------
   -- Provider Factory --
   ----------------------

   type Outline_Provider is interface;
   type Outline_Provider_Access is access all Outline_Provider'Class;

   procedure Start_Fill
     (Self : access Outline_Provider; File : Virtual_File) is abstract;
   --  Fill the Outline view: depending of the provider this can be done
   --  asynchronously.

   procedure Stop_Fill (Self : access Outline_Provider) is abstract;
   --  If the provider is asynchronously filling the view => stop it.
   --  Do nothing otherwise.

   function Support_Language
     (Self : access Outline_Provider;
      Lang : Language_Access)
      return Boolean is abstract;
   --  Ask the provider if Lang is supported

   procedure Set_Default_Provider (Provider : Outline_Provider_Access);
   --  Set the provider used for language non supported by the LSP

   procedure Set_LSP_Provider (Provider : Outline_Provider_Access);
   --  Set the provider using LSP requests

   --------------------------
   -- Utility For Provider --
   --------------------------

   type Outline_Model is limited private;
   type Outline_Model_Access is access Outline_Model;

   type Insertion_Movement is (Up, Down, Stay);
   --  Up   : insert as a parent
   --  Down : insert as a child
   --  Stay : insert as a brother

   function Get_Outline_Model
     (Kernel  : Kernel_Handle;
      File    : Virtual_File;
      Default : Boolean := False)
      return Outline_Model_Access;
   --  Retrieve the model, return null if the File is not currently displayed
   --  in Outline and Outline_Error if the view is closed.
   --  Set Default to True, if the call is coming from the Default_Provider

   procedure Add_Row
     (Self           : Outline_Model_Access;
      Name           : String;
      Profile        : String;
      Category       : Language_Category;
      Is_Declaration : Boolean;
      Visibility     : Construct_Visibility;
      Def_Line       : Integer;
      Def_Col        : Integer;
      End_Line       : Integer;
      Id             : String;
      Visible        : out Boolean);
   --  Add a Row in the Outline Model. The parent is defined using the last
   --  added row and the preferences.
   --  Name           : the name of the Entity which will be added
   --  Profile        : the Entity's profile (can be empty)
   --  Category       : the Entity's category
   --  Is_Declaration : True if the Entity is a declaration
   --  Visibility     : the Entity's visibility
   --  Def_Line       : the line containing the Entity's Defining_Name
   --                   (this can be different from Start_Line)
   --  Def_Column     : the column containing the Entity's Defining_Name
   --                   (if set to 0, the Entity will not be highlighted
   --                    when selected via the Outline)
   --  End_Line       : the line where the Entity's block finishes
   --  Id             : the Entity's Id, this can be used to select
   --                   its row via the python API (can be empty)
   --  Visible        : True is returned if Entity is visible with the
   --                   current preferences. This should be used to prevent
   --                   adding unecessary nodes (I.E. the Entity's children)

   procedure Move_Cursor
     (Self     : Outline_Model_Access;
      Movement : Insertion_Movement);
   --  Change the cursor pointing to the last added row.

   type Computing_Status is (Failed, Stopped, Succeeded);
   --  Failed will be returned when the Outline can't be computed (when the
   --  request is rejected by the server).
   --  Stopped will be returned when the request is outdated (the context has
   --  changed or the current editor has been modified).
   --  Succeeded will be returned when the Oultine has been properly filled.
   procedure Finished_Computing
     (Kernel : Kernel_Handle;
      Status : Computing_Status := Succeeded);
   --  Must be called by a provider when all the nodes have been added.

   procedure Clear_Outline_Model (Self : Outline_Model_Access);
   --  Clear the Outline model, this is needed for asynchronous result.

   procedure Free (Self : in out Outline_Model_Access);
   --  Free the model, this does not clear or affect the view.

private
   function Identity
     (Self : Language_Category)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Language_Category'Pos (Self)));
   package Category_To_Iter_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Language_Category,
      Element_Type    => Gtk_Tree_Iter,
      Hash            => Identity,
      Equivalent_Keys => "=");
   --  Map used for the preferences Group_By_Categories

   function Get_Id
     (Self : not null access Gtkada.Tree_View.Tree_View_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return String;
   package Expansion is new Gtkada.Tree_View.Expansion_Support
     (Tree_Record        => Gtkada.Tree_View.Tree_View_Record,
      Id                 => String,
      Get_Id             => Get_Id,
      Hash               => Ada.Strings.Hash);

   type Tree_Filter is record
      Show_Profile      : Boolean;
      Sort_Alphabetical : Boolean;
      Sort_Category     : Boolean;
      Editor_Link       : Boolean;
      Show_Decls        : Boolean;
      Show_Types        : Boolean;
      Show_Field        : Boolean;
      Show_Tasks        : Boolean;
      Show_Objects      : Boolean;
      Show_With         : Boolean;
      Flat_View         : Boolean;
      Group_By_Category : Boolean;
   end record;

   type Outline_Model is limited record
      Model        : Expansion.Detached_Model;
      Current_Path : Gtk_Tree_Path;
      Category_Map : Category_To_Iter_Map.Map;
      Filter       : Tree_Filter;
   end record;

end Outline_View;
