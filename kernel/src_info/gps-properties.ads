------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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
with XML_Utils;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with String_Hash;

package GPS.Properties is

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
      Node     : in out XML_Utils.Node_Ptr) is abstract;
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
     (Property : in out Property_Record; From : XML_Utils.Node_Ptr)
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

   procedure Get_Property
     (Property    : out Property_Record'Class;
      Index_Name  : String;
      Index_Value : String;
      Name        : String;
      Found       : out Boolean);
   --  Return the given named property associated with Index.
   --  null is returned if there is no such property.
   --  Property names are case sensitive.
   --
   --  This function might be dangereous in some cases. See for instance the
   --  following scenario:
   --      Prop : String_Property_Access := new ... (with a string_access)
   --      Set_Property (Index, Prop);
   --      ...
   --      Prop2 : String_Property;
   --      Get_Property (Prop2, Index)
   --      Set_Property (Index, new String_Property'(Prop2));
   --          the last call would first call Destroy on the previous value of
   --          the property associated with Index, ie would free the memory
   --          used by the embedded string_access. Therefore when we set the
   --          property it now points to deallocated memory
   --
   --  We cannot just return an access to Property_Record'Class, because the
   --  actual type of the property the user expects must be known in advance
   --  for the case where the value has only been read from XML and not
   --  converted to Ada yet.

   ---------------------------------------
   -- Associating properties with files --
   ---------------------------------------

   procedure Get_Property
     (Property : out Property_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Name     : String;
      Found    : out Boolean);
   --  Return the given named property associated with File.
   --  Found is set to False if there is no such property.
   --  Property names are case sensitive.

   ------------------------------------------
   -- Associating properties with projects --
   ------------------------------------------

   procedure Get_Property
     (Property : out Property_Record'Class;
      Project  : GNATCOLL.Projects.Project_Type;
      Name     : String;
      Found    : out Boolean);
   --  Return the given named property associated with File.
   --  Found is set to False if there is no such property.
   --  Property names are case sensitive.

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

   ---------------------------
   -- Convenience functions --
   ---------------------------

   function To_String
     (File : GNATCOLL.VFS.Virtual_File) return String;
   --  Return the file name associated with File

   function To_String (Prj : GNATCOLL.Projects.Project_Type) return String;
   --  Return the project's path

   -------------------------------
   --  Move to private part ???
   -------------------------------

   Sep : constant String := "@@";
   --  Separator between resource name and property name in the key of htables

   type Property_Description is record
      Value      : Property_Access;
      --  The actual value of the property. This is still null if the property
      --  hasn't been parsed yet (and then Unparsed it not null and contains
      --  the value read from the XML file). This parsing is implemented
      --  lazily so that we do not have to register the property types in
      --  advance, and they are only needed when actually reading a property

      Unparsed   : XML_Utils.Node_Ptr;
      Persistent : Boolean;
   end record;
   type Property_Description_Access is access Property_Description;
   --  The description of a property

   procedure Free (Description : in out Property_Description_Access);

   package Properties_Hash is new String_Hash
     (Data_Type      => Property_Description_Access,
      Free_Data      => Free,
      Null_Ptr       => null,
      Case_Sensitive => True);

   All_Properties : Properties_Hash.String_Hash_Table.Instance;
   --  Global variable storing all the current properties for the current
   --  project.
   --  Indexes a made of both the Resource_Key (ie the file or project name for
   --  instance) and the name of property we want for that resource, with the
   --  following format:
   --       Resource_Key & "@@" & Property_Name
   --  To avoid ambiguities, property names should not contain "@@".
   --
   --  ??? It would be nicer to store this in the kernel but:
   --    - it really doesn't provide anything in addition (no more task safe
   --      in any case)
   --    - It would require an extra Kernel parameter to Set_Property
   --      and Get_Property, thus making the API harder to use.
   --  For now, we'll live with this global variable.

private
   overriding procedure Destroy (Property : in out String_Property);
   overriding procedure Save
     (Property : access String_Property; Node : in out XML_Utils.Node_Ptr);
   overriding procedure Load
     (Property : in out String_Property; From : XML_Utils.Node_Ptr);
   --  See inherited documentation

   overriding procedure Save
     (Property : access Integer_Property; Node : in out XML_Utils.Node_Ptr);
   overriding procedure Load
     (Property : in out Integer_Property; From : XML_Utils.Node_Ptr);
   --  See inherited documentation

   overriding procedure Save
     (Property : access Boolean_Property; Node : in out XML_Utils.Node_Ptr);
   overriding procedure Load
     (Property : in out Boolean_Property; From : XML_Utils.Node_Ptr);
   --  See inherited documentation

end GPS.Properties;
