-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005-2007, AdaCore              --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides file-specific properties, optionaly persistent
--
--  Example of use:
--     declare
--        Int   : Integer_Property;
--        Found : Boolean;
--        Prop  : Property_Access;
--     begin
--        Get_Property (Int, File, "dummy", Found);
--        if Found then
--           Put_Line (Int.Value'Img);
--        end if;
--
--        Prop := new Integer_Property'(Value => 232);
--        Set_Property (File, "dummy", Prop, Persistent => True);
--     end;

with GNAT.Strings;
with Glib.Xml_Int;
with Projects;

package GPS.Kernel.Properties is

   ----------------
   -- Properties --
   ----------------

   type Property_Record is abstract tagged null record;
   type Property_Access is access all Property_Record'Class;
   --  A general property that can be associated with a file.
   --  Such properties can be marked as persistent, that is they will exist
   --  from one session of GPS to the next, transparently.

   procedure Save
     (Property : access Property_Record;
      Node     : in out Glib.Xml_Int.Node_Ptr) is abstract;
   --  Save the property to an XML node.
   --  The Node has already been created, and its name must not be changed.
   --  Attributes can be added if needed, though.
   --  In the end, the XML file will contain something like:
   --     <properties file="...">
   --        <property name="...">save1</property>
   --        <property name="...">save2</property>
   --     </properties>
   --  where "save1" and "save2" are set by Save.

   procedure Load
     (Property : in out Property_Record; From : Glib.Xml_Int.Node_Ptr)
     is abstract;
   --  Load a property from an XML node.
   --  From has been found automatically by GPS based on the property node. If
   --  it doesn't match the type expected by Property, it is likely because two
   --  properties have the same name. In this case, an error message should be
   --  written in the console.
   --  You mustn't keep references to the Node_Ptr, which will be destroyed
   --  after the call to Load

   procedure Destroy (Property : in out Property_Record);
   --  Free the memory occupied by the property. You should always call the
   --  parent's Destroy handler.

   -------------------------------------------
   -- Associating properties with any index --
   -------------------------------------------

   procedure Set_Property
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Index_Name  : String;
      Index_Value : String;
      Name        : String;
      Property    : access Property_Record'Class;
      Persistent  : Boolean := False);
   --  Associate a given property with Index, so that it can be queried
   --  later through Get_File_Property.
   --  If Persistent is True, the property will be preserved from one
   --  session of GPS to the next.
   --  Property names are case sensitive.

   procedure Get_Property
     (Property    : out Property_Record'Class;
      Index_Name  : String;
      Index_Value : String;
      Name        : String;
      Found       : out Boolean);
   --  Return the given named property associated with Index.
   --  Found is set to False if there is no such property.
   --  Property names are case sensitive.

   procedure Remove_Property
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Index_Name  : String;
      Index_Value : String;
      Name        : String);
   --  Remove the named property (persistent or not) associated with Index.

   ---------------------------------------
   -- Associating properties with files --
   ---------------------------------------

   procedure Set_Property
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False);
   --  Associate a given property with File, so that it can be queries later
   --  through Get_File_Property.
   --  If Persistent is True, the property will be preserved from one session
   --  of GPS to the next.
   --  Property names are case sensitive.

   procedure Get_Property
     (Property : out Property_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Name     : String;
      Found    : out Boolean);
   --  Return the given named property associated with File.
   --  Found is set to False if there is no such property.
   --  Property names are case sensitive.

   procedure Remove_Property
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Name     : String);
   --  Remove the named property (persistent or not) from the file.

   ------------------------------------------
   -- Associating properties with projects --
   ------------------------------------------

   procedure Set_Property
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project    : Projects.Project_Type;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False);
   --  Associate a given property with File, so that it can be queries later
   --  through Get_File_Property.
   --  If Persistent is True, the property will be preserved from one session
   --  of GPS to the next.
   --  Property names are case sensitive.

   procedure Get_Property
     (Property : out Property_Record'Class;
      Project  : Projects.Project_Type;
      Name     : String;
      Found    : out Boolean);
   --  Return the given named property associated with File.
   --  Found is set to False if there is no such property.
   --  Property names are case sensitive.

   procedure Remove_Property
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project  : Projects.Project_Type;
      Name     : String);
   --  Remove the named property (persistent or not) from the file.

   -----------------------------
   -- Specific property types --
   -----------------------------
   --  These are provided for convenience

   type Integer_Property is new Property_Record with record
      Value : Integer;
   end record;
   type Integer_Property_Access is access all Integer_Property'Class;

   type String_Property is new Property_Record with record
      Value : GNAT.Strings.String_Access;
   end record;
   type String_Property_Access is access all String_Property'Class;

   type Boolean_Property is new Property_Record with record
      Value : Boolean;
   end record;
   type Boolean_Property_Access is access all Boolean_Property'Class;

   procedure Set_Language_From_File
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : GNATCOLL.VFS.Virtual_File;
      Language : String := "");
   --  Override the language to be used for the specific filename. This doesn't
   --  change the project itself, just the properties associated with the file.
   --  If Language is set to "", then the language will be guessed from the
   --  project.

   -----------------------------------------
   -- Saving and restoring all properties --
   -----------------------------------------

   procedure Save_Persistent_Properties
     (Kernel : access Kernel_Handle_Record'Class);
   --  Save all persistent properties for all files in the current project.

   procedure Restore_Persistent_Properties
     (Kernel : access Kernel_Handle_Record'Class);
   --  Restore persistent properties for the files in the current project.
   --  This subprogram should only be called by the kernel itself.

   procedure Reset_Properties
     (Kernel : access Kernel_Handle_Record'Class);
   --  Clear the properties cache. No property will be available after this
   --  call.
   --  This subprogram should only be called by the kernel itself.

   -------------
   -- Scripts --
   -------------

   procedure Register_Script_Commands
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register the script commands associated with this module

private
   procedure Destroy (Property : in out String_Property);
   procedure Save
     (Property : access String_Property; Node : in out Glib.Xml_Int.Node_Ptr);
   procedure Load
     (Property : in out String_Property; From : Glib.Xml_Int.Node_Ptr);
   --  See inherited documentation

   procedure Save
     (Property : access Integer_Property; Node : in out Glib.Xml_Int.Node_Ptr);
   procedure Load
     (Property : in out Integer_Property; From : Glib.Xml_Int.Node_Ptr);
   --  See inherited documentation

   procedure Save
     (Property : access Boolean_Property; Node : in out Glib.Xml_Int.Node_Ptr);
   procedure Load
     (Property : in out Boolean_Property; From : Glib.Xml_Int.Node_Ptr);
   --  See inherited documentation

end GPS.Kernel.Properties;
