------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Fixed;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with XML_Utils;                  use XML_Utils;
with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

package body GPS.Kernel.Properties is

   use Properties_Hash.String_Hash_Table;

   Me : constant Trace_Handle := Create ("Properties");

   function Get_Properties_Filename
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File;
   --  Return the filename to use when saving the persistent properties for the
   --  current project

   procedure Set_Resource_Property
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Resource_Key  : String;
      Resource_Kind : String;
      Name          : String;
      Property      : Property_Description;
      Save_File     : Boolean := True);
   --  Set property for any kind of resource. This is the internal
   --  implementation of Set_*_Property

   procedure Remove_Resource_Property
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Resource_Key  : String;
      Resource_Kind : String;
      Name          : String);
   --  Remove property for any kind of resource

   procedure File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles script commands for properties

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles script commands for properties

   procedure Properties_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles script commands for properties

   ---------------------------
   -- Set_Resource_Property --
   ---------------------------

   procedure Set_Resource_Property
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Resource_Key  : String;
      Resource_Kind : String;
      Name          : String;
      Property      : Property_Description;
      Save_File     : Boolean := True)
   is
      pragma Unreferenced (Resource_Kind);
      --  Not used yet, the idea is to have various attributes in the XML file
      --  depending on the resource type

   begin
      Set (All_Properties, Resource_Key & Sep & Name,
           new Property_Description'(Property));

      if Save_File and then Property.Persistent then
         Save_Persistent_Properties (Kernel);
      end if;
   end Set_Resource_Property;

   ------------------------------
   -- Remove_Resource_Property --
   ------------------------------

   procedure Remove_Resource_Property
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Resource_Key  : String;
      Resource_Kind : String;
      Name          : String)
   is
      pragma Unreferenced (Resource_Kind);
   begin
      Remove (All_Properties, Resource_Key & Sep & Name);
      Save_Persistent_Properties (Kernel);
   end Remove_Resource_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Index_Name  : String;
      Index_Value : String;
      Name        : String;
      Property    : access Property_Record'Class;
      Persistent  : Boolean := False) is
   begin
      Set_Resource_Property
        (Kernel,
         Index_Value, Index_Name, Name,
         Property_Description'(Value      => Property_Access (Property),
                               Unparsed   => null,
                               Persistent => Persistent));
   end Set_Property;

   ---------------------
   -- Remove_Property --
   ---------------------

   procedure Remove_Property
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Index_Name  : String;
      Index_Value : String;
      Name        : String) is
   begin
      Remove_Resource_Property (Kernel, Index_Value, Index_Name, Name);
   end Remove_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False) is
   begin
      Set_Property
        (Kernel, "file", To_String (File), Name, Property, Persistent);
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project    : Project_Type;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False) is
   begin
      Set_Property
        (Kernel, "project", To_String (Project), Name, Property, Persistent);
   end Set_Property;

   ---------------------
   -- Remove_Property --
   ---------------------

   procedure Remove_Property
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Name     : String) is
   begin
      Remove_Property (Kernel, "file", To_String (File), Name);
   end Remove_Property;

   ---------------------
   -- Remove_Property --
   ---------------------

   procedure Remove_Property
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project  : Project_Type;
      Name     : String) is
   begin
      Remove_Property (Kernel, "project", To_String (Project), Name);
   end Remove_Property;

   -----------------------------
   -- Get_Properties_Filename --
   -----------------------------

   function Get_Properties_Filename
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File is
   begin
      --  We could use the .gps directory, but that would mean we have to keep
      --  in memory information for files that do not belong to the current
      --  project.

      return Create_From_Dir (Get_Home_Dir (Kernel), "properties.xml");
   end Get_Properties_Filename;

   --------------------------------
   -- Save_Persistent_Properties --
   --------------------------------

   procedure Save_Persistent_Properties
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Filename : constant Virtual_File :=
                   Get_Properties_Filename (Kernel);
      Iter     : Properties_Hash.String_Hash_Table.Cursor;
      Root     : Node_Ptr;
      Descr    : Property_Description_Access;
      Val      : XML_Utils.String_Ptr;
      Success  : Boolean;

      package Resource_Hash is new Ada.Containers.Indefinite_Ordered_Maps
        (Key_Type     => String,
         Element_Type => Node_Ptr);
      --  For each resource, stores the XML node that represents it. The goal
      --  is that each resource has a single node

      use Resource_Hash;
      Nodes : Resource_Hash.Map;
      C     : Resource_Hash.Cursor;
      File  : Node_Ptr;

   begin
      Trace (Me, "Saving " & Filename.Display_Full_Name);
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
         Descr := Get_Element (Iter);
         exit when Descr = null;

         if Descr.Persistent then
            declare
               Prop : Node_Ptr;
               Key  : constant String := Get_Key (Iter);
               Pos  : constant Integer := Ada.Strings.Fixed.Index (Key, Sep);
               Src  : Node_Ptr;
            begin
               if Descr.Value = null then

                  --  Descr.Unparsed.Value might be null if the properties is
                  --  represented by XML children instead
                  if Descr.Unparsed.Value /= null then
                     Val := new String'(Descr.Unparsed.Value.all);
                  else
                     Val := null;
                  end if;

                  Prop := new Node'
                    (Tag        => new String'("property"),
                     Attributes => new String'
                       (Descr.Unparsed.Attributes.all),
                     Value      => Val,
                     Parent     => null,
                     Child      => null,
                     Next       => null,
                     Specific_Data => 1);

                  --  If there are any children to Descr.Unparsed, we must
                  --  preserve them

                  Src := Descr.Unparsed.Child;
                  while Src /= null loop
                     Add_Child (Prop, Deep_Copy (Src), Append => True);
                     Src := Src.Next;
                  end loop;

               else
                  Prop := new Node'
                    (Tag        => new String'("property"),
                     Attributes => new String'
                       ("name='" & Key (Pos + 2 .. Key'Last) & "'"),
                     Value      => null,
                     Parent     => null,
                     Child      => null,
                     Next       => null,
                     Specific_Data => 1);
                  Save (Descr.Value, Prop);
               end if;

               if Prop /= null then
                  C := Find (Nodes, Key (Key'First .. Pos - 1));
                  if Has_Element (C) then
                     File := Element (C);
                  else
                     File := new Node'
                       (Tag        => new String'("properties"),
                        Attributes => new String'
                          ("file='" & Key (Key'First .. Pos - 1) & "'"),
                        Value      => null,
                        Parent     => null,
                        Child      => null,
                        Next       => null,
                        Specific_Data => 1);

                     Include (Nodes, Key (Key'First .. Pos - 1), File);
                  end if;

                  Add_Child (File, Prop);
               end if;
            end;
         end if;

         Get_Next (All_Properties, Iter);
      end loop;

      --  Save the file in sorted order. This is really for the sake of the
      --  testsuite.

      C := First (Nodes);
      while Has_Element (C) loop
         File := Element (C);
         Add_Child (Root, File);
         Next (C);
      end loop;

      Print (Root, Filename, Success);
      Free (Root);

      Clear (Nodes);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      end if;
   end Save_Persistent_Properties;

   ----------------------
   -- Reset_Properties --
   ----------------------

   procedure Reset_Properties
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Reset (All_Properties);
   end Reset_Properties;

   -----------------------------------
   -- Restore_Persistent_Properties --
   -----------------------------------

   procedure Restore_Persistent_Properties
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Filename : constant Virtual_File :=
                   Get_Properties_Filename (Kernel);
      Root, File, Prop, Prop2 : Node_Ptr;
   begin
      Trace (Me, "Loading " & Filename.Display_Full_Name);

      if Is_Regular_File (Filename) then
         Root := Parse (Filename);

         if Root /= null then
            File := Root.Child;
         end if;

         while File /= null loop
            Prop := File.Child;

            while Prop /= null loop
               Prop2 := Deep_Copy (Prop);
               Free (Prop2.Tag);
               --  Prop.Tag is not needed, always "property", and this
               --  saves space in memory

               Set_Resource_Property
                 (Kernel,
                  Resource_Key  => Get_Attribute (File, "file"),
                  Resource_Kind => "file",
                  Name          => Get_Attribute (Prop, "name"),
                  Save_File     => False,
                  Property      => (Value      => null,
                                    Unparsed   => Prop2,
                                    Persistent => True));
               Prop := Prop.Next;
            end loop;

            File := File.Next;
         end loop;
         Free (Root);
      end if;

   exception
      when E : others => Trace (Me, E);
   end Restore_Persistent_Properties;

   ----------------------------
   -- Set_Language_From_File --
   ----------------------------

   procedure Set_Language_From_File
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : GNATCOLL.VFS.Virtual_File;
      Language : String := "")
   is
      Prop : String_Property_Access;
   begin
      if Language = "" then
         Remove_Property (Kernel, Filename, "language");
      else
         Prop := new String_Property'(Value => new String'(Language));
         Set_Property (Kernel, Filename, "language", Prop, Persistent => True);
      end if;
   end Set_Language_From_File;

   --------------------------
   -- File_Command_Handler --
   --------------------------

   procedure File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      File_Class : constant Class_Type := New_Class (Kernel, "File");
      Inst       : constant Class_Instance := Nth_Arg (Data, 1, File_Class);
      File       : constant Virtual_File := Get_Data (Inst);

      Name       : aliased constant String := "name";
      Value      : aliased constant String := "value";
      Persistent : aliased constant String := "persistent";

      Found      : Boolean;
      Prop       : String_Property_Access;
      Prop2      : aliased String_Property;
   begin
      if Command = "set_property" then
         Name_Parameters (Data, (2 => Name'Unchecked_Access,
                                 3 => Value'Unchecked_Access,
                                 4 => Persistent'Unchecked_Access));
         Prop := new String_Property'(Value => new String'(Nth_Arg (Data, 3)));
         Set_Property
           (Kernel,
            File,
            Name       => Nth_Arg (Data, 2),
            Property   => Prop,
            Persistent => Nth_Arg (Data, 4, False));

      elsif Command = "get_property" then
         Name_Parameters (Data, (2 => Name'Unchecked_Access));
         Get_Property
           (Property => Prop2,
            File     => File,
            Name     => Nth_Arg (Data, 2),
            Found    => Found);

         if not Found then
            Set_Error_Msg (Data, -"Property not found");

         elsif Prop2.Value = null then
            Set_Return_Value (Data, String'(""));

         else
            Set_Return_Value (Data, Prop2.Value.all);
         end if;

      else
         Name_Parameters (Data, (2 => Name'Unchecked_Access));
         Remove_Property
           (Kernel => Kernel,
            File   => File,
            Name   => Nth_Arg (Data, 2));
      end if;
   end File_Command_Handler;

   -----------------------------
   -- Project_Command_Handler --
   -----------------------------

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Project    : constant Project_Type  := Get_Data (Data, 1);
      Name       : aliased constant String := "name";
      Value      : aliased constant String := "value";
      Persistent : aliased constant String := "persistent";

      Found      : Boolean;
      Prop       : String_Property_Access;
      Prop2      : aliased String_Property;
   begin
      if Command = "set_property" then
         Name_Parameters (Data, (2 => Name'Unchecked_Access,
                                 3 => Value'Unchecked_Access,
                                 4 => Persistent'Unchecked_Access));
         Prop := new String_Property'(Value => new String'(Nth_Arg (Data, 3)));
         Set_Property
           (Get_Kernel (Data),
            Project,
            Name       => Nth_Arg (Data, 2),
            Property   => Prop,
            Persistent => Nth_Arg (Data, 4, False));

      elsif Command = "get_property" then
         Name_Parameters (Data, (2 => Name'Unchecked_Access));
         Get_Property
           (Property => Prop2,
            Project     => Project,
            Name     => Nth_Arg (Data, 2),
            Found    => Found);

         if not Found then
            Set_Error_Msg (Data, -"Property not found");

         elsif Prop2.Value = null then
            Set_Return_Value (Data, String'(""));

         else
            Set_Return_Value (Data, Prop2.Value.all);
         end if;

      else
         Name_Parameters (Data, (2 => Name'Unchecked_Access));
         Remove_Property
           (Kernel   => Get_Kernel (Data),
            Project  => Project,
            Name     => Nth_Arg (Data, 2));
      end if;
   end Project_Command_Handler;

   --------------------------------
   -- Properties_Command_Handler --
   --------------------------------

   procedure Properties_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "save_persistent_properties" then
         Save_Persistent_Properties (Get_Kernel (Data));
      end if;
   end Properties_Command_Handler;

   ------------------------------
   -- Register_Script_Commands --
   ------------------------------

   procedure Register_Script_Commands
     (Kernel : access Kernel_Handle_Record'Class)
   is
      File_Class    : constant Class_Type := New_Class (Kernel, "File");
      Project_Class : constant Class_Type := New_Class (Kernel, "Project");
   begin
      Register_Command
        (Kernel, "set_property",
         Minimum_Args => 2,
         Maximum_Args => 3,
         Class        => File_Class,
         Handler      => File_Command_Handler'Access);
      Register_Command
        (Kernel, "get_property",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => File_Class,
         Handler      => File_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_property",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => File_Class,
         Handler      => File_Command_Handler'Access);

      Register_Command
        (Kernel, "set_property",
         Minimum_Args => 2,
         Maximum_Args => 3,
         Class        => Project_Class,
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "get_property",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Project_Class,
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_property",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Project_Class,
         Handler      => Project_Command_Handler'Access);

      Register_Command
        (Kernel, "save_persistent_properties",
         Handler => Properties_Command_Handler'Access);
   end Register_Script_Commands;

end GPS.Kernel.Properties;
