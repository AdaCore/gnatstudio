------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
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

--  <description>
--  This package provides a module to customize properties for projects.
--  It basically gives access to various aspects of project files, like
--  attributes of project, their types and indexes, etc,
--  List of attributes split to sections and pages as they presented in
--  GPS project editor.
--  </description>

with GNAT.Strings;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.Projects;         use GNATCOLL.Projects;

with GPS.Core_Kernels;          use GPS.Core_Kernels;
with GPS.Customizable_Modules;  use GPS.Customizable_Modules;

with XML_Utils;                 use XML_Utils;

package GPS.Project_Properties is

   type Boolean_Array is array (Natural range <>) of Boolean;
   type Boolean_List is access Boolean_Array;

   ----------------------------
   -- Attribute descriptions --
   ----------------------------

   type Attribute_As is (Attribute_As_String,
                         Attribute_As_Filename,
                         Attribute_As_Unit,
                         Attribute_As_Directory,
                         Attribute_As_Static_List,
                         Attribute_As_Dynamic_List);
   type File_Filter is (Filter_None,
                        Filter_From_Project,
                        Filter_From_Extended,
                        Filter_From_All_Projects);

   type Attribute_Type (Typ : Attribute_As := Attribute_As_String) is
   record
      case Typ is
         when Attribute_As_String
            | Attribute_As_Filename
            | Attribute_As_Unit
            | Attribute_As_Directory   =>
            Default           : GNAT.Strings.String_Access;
            Filter            : File_Filter := Filter_None;
            Allow_Empty       : Boolean := True;
         when Attribute_As_Static_List =>
            Static_Allows_Any_String : Boolean := False;
            Static_List       : GNAT.Strings.String_List_Access;
            Static_Default    : Boolean_List;
         when Attribute_As_Dynamic_List =>
            Dynamic_Allows_Any_String : Boolean := False;
            Dynamic_List_Lang : GNAT.Strings.String_Access;
            Dynamic_List_Cmd  : GNAT.Strings.String_Access;
            Dynamic_Default   : GNAT.Strings.String_Access;
      end case;
   end record;

   type Indexed_Attribute_Type is record
      Typ         : Attribute_Type;
      Index_Value : GNAT.Strings.String_Access;  --  null for the general case
   end record;

   type Indexed_Attribute_Type_Array
     is array (Natural range <>) of Indexed_Attribute_Type;
   type Indexed_Attribute_Type_List is access Indexed_Attribute_Type_Array;

   type Attribute_Description (Indexed : Boolean := False) is tagged limited
   record
      Name                 : GNAT.Strings.String_Access;
      Pkg                  : GNAT.Strings.String_Access;
      Description          : GNAT.Strings.String_Access;
      Label                : GNAT.Strings.String_Access;
      Hide_In              : GNAT.Strings.String_Access;
      Is_List              : Boolean := False;
      Ordered_List         : Boolean := False;
      Omit_If_Default      : Boolean := True;
      Base_Name_Only       : Boolean := False;
      Case_Sensitive_Index : Boolean := False;

      Disable_If_Not_Set   : Boolean := False;
      --  If True, the project attribute needs to be explicitly specified by
      --  the user, or the editor is greyed out (a check button is also shown
      --  to allow the edition of the attribute)

      Disable              : GNAT.Strings.String_Access;
      --  Space-separated list of attributes that are disabled when this
      --  attribute is set. This assumes that Disable_If_Not_Set is True,
      --  otherwise nothing happens.

      case Indexed is
         when True =>
            Index_Attribute : GNAT.Strings.String_Access;
            Index_Package   : GNAT.Strings.String_Access;
            Index_Types     : Indexed_Attribute_Type_List;
         when False =>
            Non_Index_Type  : Attribute_Type;
      end case;
   end record;

   function Attribute_Name (Attr : Attribute_Description) return String;
   --  Return a string suitable for display that describes the attribute

   function Get_Default_Value
     (Attr          : access Attribute_Description'Class;
      Index         : String) return String;
   function Get_Default_Value
     (Kernel        : access Core_Kernel_Record'Class;
      Attr          : access Attribute_Description'Class;
      Index         : String := "") return String_List_Access;
   --  Get default value as specified in the attribute definition
   --  This is used to reflect the value the attribute has if not specified
   --  in the project.

   function Get_Value_From_Project
     (Project       : Project_Type;
      Attr          : access Attribute_Description'Class;
      Index         : String) return String;
   function Get_Value_From_Project
     (Kernel        : access Core_Kernel_Record'Class;
      Project       : Project_Type;
      Attr          : access Attribute_Description'Class;
      Index         : String := "") return String_List_Access;
   --  Get value in the current Project, if such project exists.
   --  This is used to detect changes in the edited project.

   function Attribute_Exists
     (Attr            : access Attribute_Description'Class;
      Project         : Project_Type;
      Attribute_Index : String := "") return Boolean;
   --  Return True if Attr was explicitly defined in Project

   function Is_Any_String
     (Attr  : access Attribute_Description'Class;
      Index : String) return Boolean;
   --  Whether, for the index Index, Attr behaves like a free-form string.
   --  False is returned for special types like lists, filenames,...

   function Get_Attribute_Type_From_Description
     (Attr : access Attribute_Description'Class; Index : String)
      return Attribute_Type;
   --  Return the type to use for an attribute given its index. This properly
   --  handles the case where the attribute isn't indexed

   type List_Attribute_Callback is access procedure
     (Value : String; Is_Default : Boolean);
   --  Is_Default is set to true if Value is set as a default value for the
   --  attribute in the XML file

   procedure For_Each_Item_In_List
     (Kernel   : access Core_Kernel_Record'Class;
      Attr     : Attribute_Type;
      Callback : List_Attribute_Callback);
   --  Calls Callback for each possible value of the attribute

   type Attribute_Description_Access is access all Attribute_Description'Class;

   type Attribute_Description_Array
     is array (Natural range <>) of Attribute_Description_Access;
   type Attribute_Description_List is access Attribute_Description_Array;

   type Attribute_Page_Section is record
      Name       : GNAT.Strings.String_Access;  --  "" for unnamed sections
      Attributes : Attribute_Description_List;
   end record;

   type Attribute_Page_Section_Array
     is array (Natural range <>) of Attribute_Page_Section;
   type Attribute_Page_Section_List is access Attribute_Page_Section_Array;

   type Attribute_Page is record
      Name     : GNAT.Strings.String_Access;
      Sections : Attribute_Page_Section_List;
   end record;

   type Attribute_Page_Array is array (Natural range <>) of Attribute_Page;
   type Attribute_Page_List is access Attribute_Page_Array;

   -----------------------
   -- Properties module --
   -----------------------

   type Base_Properties_Module (Kernel : access Core_Kernel_Record'Class) is
     new Customizable_Module_Record with private;

   function Pages (Self : Base_Properties_Module) return Attribute_Page_List;
   --  Return list of pages of attributes known to the module

   function Get_Attribute_Type_From_Name
     (Module : access Base_Properties_Module;
      Pkg    : String;
      Name   : String) return Attribute_Description_Access;
   --  Find the description of an attribute given its package and name

   function New_Attribute_Description
     (Module  : access Base_Properties_Module;
      Indexed : Boolean)
      return Attribute_Description_Access;
   --  Create new empty Attribute_Description (Indexed) record.
   --  Derived modules could ovveride it to return extended version of
   --  description

private

   type Base_Properties_Module (Kernel : access Core_Kernel_Record'Class) is
     new Customizable_Module_Record with
   record
      Pages  : Attribute_Page_List;
   end record;

   overriding procedure Destroy (Module : in out Base_Properties_Module);
   overriding procedure Customize
     (Module : access Base_Properties_Module;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

end GPS.Project_Properties;
