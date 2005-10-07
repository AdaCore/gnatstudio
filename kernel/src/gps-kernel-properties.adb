-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
--                              AdaCore                              --
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

with Ada.Unchecked_Deallocation;
with File_Utils;                 use File_Utils;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with Glib.Xml_Int;               use Glib.Xml_Int;
with Projects;                   use Projects;
with String_Hash;
with Traces;                     use Traces;
with VFS;                        use VFS;

package body GPS.Kernel.Properties is
   Me : constant Debug_Handle := Create ("Properties");

   type Property_Description is record
      Value      : Property_Access;
      --  The actual value of the property. This is still null if the property
      --  hasn't been parsed yet (and then Unparsed it not null and contains
      --  the value read from the XML file). This parsing is implemented
      --  lazily so that we do not have to register the property types in
      --  advance, and they are only needed when actually reading a property

      Unparsed   : Node_Ptr;
      Persistent : Boolean;
   end record;
   No_Description : constant Property_Description := (null, null, False);
   --  The description of a property

   procedure Free (Description : in out Property_Description);

   package Properties_Description_Hash is new String_Hash
     (Data_Type      => Property_Description,
      Free_Data      => Free,
      Null_Ptr       => No_Description,
      Case_Sensitive => True);
   use Properties_Description_Hash.String_Hash_Table;

   type Properties_Description_HTable is
     access all Properties_Description_Hash.String_Hash_Table.HTable;

   procedure Free (Hash : in out Properties_Description_HTable);

   package Properties_Hash is new String_Hash
     (Data_Type      => Properties_Description_HTable,
      Free_Data      => Free,
      Null_Ptr       => null,
      Case_Sensitive => Filenames_Are_Case_Sensitive);
   use Properties_Hash.String_Hash_Table;

   All_Properties : Properties_Hash.String_Hash_Table.HTable;
   --  Global variable storing all the current properties for the current
   --  project.
   --  ??? It would be nicer to store this in the kernel but:
   --    - it really doesn't provide anything in addition (no more task safe
   --      in any case)
   --    - It would require an extra Kernel parameter to Set_File_Property
   --      and Get_File_Property, thus making the API harder to use.
   --  For now, we'll leave with this global variable.

   function Get_Properties_Filename
     (Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the filename to use when saving the persistent properties for the
   --  current project

   procedure Set_Resource_Property
     (Resource_Key  : String;
      Resource_Kind : String;
      Name          : String;
      Property      : Property_Description);
   --  Set property for any kind of resource. This is the internal
   --  implementation of Set_*_Property

   ----------
   -- Free --
   ----------

   procedure Free (Description : in out Property_Description) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Property_Record'Class, Property_Access);
   begin
      if Description.Value /= null then
         Destroy (Description.Value.all);
         Unchecked_Free (Description.Value);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Hash : in out Properties_Description_HTable) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Properties_Description_Hash.String_Hash_Table.HTable,
         Properties_Description_HTable);
   begin
      if Hash /= null then
         Reset (Hash.all);
         Unchecked_Free (Hash);
      end if;
   end Free;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Property : in out Property_Record) is
      pragma Unreferenced (Property);
   begin
      null;
   end Destroy;

   ----------
   -- Save --
   ----------

   function Save
     (Property : access String_Property) return Glib.Xml_Int.Node_Ptr is
   begin
      if Property.Value /= null then
         return new Node'
           (Tag        => new String'("value"),
            Attributes => null,
            Value      => new String'(Property.Value.all),
            Parent     => null,
            Child      => null,
            Next       => null,
            Specific_Data => 1);
      else
         return null;
      end if;
   end Save;

   ----------
   -- Save --
   ----------

   function Save
     (Property : access Integer_Property) return Glib.Xml_Int.Node_Ptr is
   begin
      return new Node'
        (Tag        => new String'("value"),
         Attributes => null,
         Value      => new String'(Integer'Image (Property.Value)),
         Parent     => null,
         Child      => null,
         Next       => null,
         Specific_Data => 1);
   end Save;

   ----------
   -- Save --
   ----------

   function Save
     (Property : access Boolean_Property) return Glib.Xml_Int.Node_Ptr is
   begin
      return new Node'
        (Tag        => new String'("value"),
         Attributes => null,
         Value      => new String'(Boolean'Image (Property.Value)),
         Parent     => null,
         Child      => null,
         Next       => null,
         Specific_Data => 1);
   end Save;

   ----------
   -- Load --
   ----------

   procedure Load
     (Property : in out String_Property; From : Glib.Xml_Int.Node_Ptr) is
   begin
      if From.Value /= null then
         Property.Value := new String'(From.Value.all);
      else
         Property.Value := null;
      end if;
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load
     (Property : in out Integer_Property; From : Glib.Xml_Int.Node_Ptr) is
   begin
      Property.Value := Integer'Value (From.Value.all);
   exception
      when Constraint_Error =>
         Property.Value := 0;
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load
     (Property : in out Boolean_Property; From : Glib.Xml_Int.Node_Ptr) is
   begin
      Property.Value := Boolean'Value (From.Value.all);
   exception
      when Constraint_Error =>
         Property.Value := True;
   end Load;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Property : in out String_Property) is
   begin
      Free (Property.Value);
   end Destroy;

   ---------------------------
   -- Set_Resource_Property --
   ---------------------------

   procedure Set_Resource_Property
     (Resource_Key  : String;
      Resource_Kind : String;
      Name          : String;
      Property      : Property_Description)
   is
      pragma Unreferenced (Resource_Kind);
      --  Not used yet, the idea is to have various attributes in the XML file
      --  depending on the resource type

      Hash : Properties_Description_HTable;
   begin
      Hash := Get (All_Properties, Resource_Key);
      if Hash = null then
         Hash := new Properties_Description_Hash.String_Hash_Table.HTable;
         Set (All_Properties, Resource_Key, Hash);
      end if;

      Set (Hash.all, Name, Property);
   end Set_Resource_Property;

   -----------------------
   -- Set_File_Property --
   -----------------------

   procedure Set_File_Property
     (File       : VFS.Virtual_File;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False) is
   begin
      Set_Resource_Property
        (Full_Name (File).all, "file", Name,
         Property_Description'(Value      => Property_Access (Property),
                               Unparsed   => null,
                               Persistent => Persistent));
   end Set_File_Property;

   -----------------------
   -- Get_File_Property --
   -----------------------

   procedure Get_File_Property
     (Property : out Property_Record'Class;
      File     : VFS.Virtual_File;
      Name     : String;
      Found    : out Boolean)
   is
      Hash : Properties_Description_HTable;
      Descr : Property_Description;
   begin
      Found := False;
      Hash := Get (All_Properties, Full_Name (File).all);
      if Hash /= null then
         Descr := Get (Hash.all, Name);
         if Descr /= No_Description then
            if Descr.Value = null then
               Load (Property, Descr.Unparsed);
               Descr.Value := new Property_Record'Class'(Property);
            end if;

            Property := Descr.Value.all;
            Found := True;
         end if;
      end if;

   exception
      when others =>
         Found := False;
   end Get_File_Property;

   -----------------------------
   -- Get_Properties_Filename --
   -----------------------------

   function Get_Properties_Filename
     (Kernel : access Kernel_Handle_Record'Class) return String is
   begin
      --  We could use the .gps directory, but that would mean we have to keep
      --  in memory information for files that do not belong to the current
      --  project.
      --  return Get_Hom_Dir (Kernel) & "properties.xml";

      return Object_Path (Get_Project (Kernel), Recursive => False)
        & Directory_Separator & ".gps_properties.xml";
   end Get_Properties_Filename;

   --------------------------------
   -- Save_Persistent_Properties --
   --------------------------------

   procedure Save_Persistent_Properties
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Filename : constant String := Get_Properties_Filename (Kernel);
      Iter  : Properties_Hash.String_Hash_Table.Iterator;
      Iter2 : Properties_Description_Hash.String_Hash_Table.Iterator;
      Hash  : Properties_Description_HTable;
      Root, File, Prop : Node_Ptr;
      Descr : Property_Description;
   begin
      Trace (Me, "Saving " & Filename);
      Root := new Node'
        (Tag        => new String'("persistent_properties"),
         Attributes => null,
         Value      => null,
         Parent     => null,
         Child      => null,
         Next       => null,
         Specific_Data => 1);

      Get_First (All_Properties, Iter);
      loop
         Hash := Get_Element (Iter);
         exit when Hash = null;

         File := new Node'
           (Tag        => new String'("properties"),
            Attributes => new String'("file='" & Get_Key (Iter) & "'"),
            Value      => null,
            Parent     => null,
            Child      => null,
            Next       => null,
            Specific_Data => 1);
         Add_Child (Root, File);

         Get_First (Hash.all, Iter2);
         loop
            Descr := Get_Element (Iter2);
            exit when Descr = No_Description;

            if Descr.Persistent then
               Prop := new Node'
                 (Tag        => new String'("property"),
                  Attributes => new String'("name='" & Get_Key (Iter2) & "'"),
                  Value      => null,
                  Parent     => null,
                  Child      => null,
                  Next       => null,
                  Specific_Data => 1);
               Add_Child (File, Prop);

               if Descr.Value = null then
                  Add_Child (Prop, Descr.Unparsed);
               else
                  Add_Child (Prop, Save (Descr.Value));
               end if;
            end if;

            Get_Next (Hash.all, Iter2);
         end loop;

         Get_Next (All_Properties, Iter);
      end loop;

      Print (Root, Filename);
      Reset (All_Properties);
   end Save_Persistent_Properties;

   -----------------------------------
   -- Restore_Persistent_Properties --
   -----------------------------------

   procedure Restore_Persistent_Properties
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Filename : constant String := Get_Properties_Filename (Kernel);
      Root, File, Prop : Node_Ptr;
   begin
      Trace (Me, "Loading " & Filename);
      if Is_Readable_File (Filename) then
         Root := Parse (Filename);
         File := Root.Child;
         while File /= null loop
            Prop := File.Child;
            while Prop /= null loop
               Set_Resource_Property
                 (Resource_Key  => Get_Attribute (File, "file"),
                  Resource_Kind => "file",
                  Name          => Get_Attribute (Prop, "name"),
                  Property      => (Value    => null,
                                    Unparsed => Prop.Child,
                                    Persistent    => True));
               Prop.Child := null; --  Since we are deleting the tree afterward
               Prop := Prop.Next;
            end loop;

            File := File.Next;
         end loop;
         Free (Root);
      end if;
   end Restore_Persistent_Properties;

end GPS.Kernel.Properties;
