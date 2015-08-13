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

with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with XML_Utils;                  use XML_Utils;
with GPR.Osint;                  use GPR.Osint;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

package body GPS.Properties is

   use Properties_Hash.String_Hash_Table;

   procedure Get_Resource_Property
     (Property      : out Property_Record'Class;
      Resource_Key  : String;
      Resource_Kind : String;
      Name          : String;
      Found         : out Boolean);
   --  Get property for any kind of resource

   ----------
   -- Free --
   ----------

   procedure Free (Description : in out Property_Description_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Property_Record'Class, Property_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Property_Description, Property_Description_Access);
   begin
      if Description.Value /= null then
         Destroy (Description.Value.all);
         Unchecked_Free (Description.Value);
      end if;

      if Description.Unparsed /= null then
         Free (Description.Unparsed);
      end if;

      Unchecked_Free (Description);
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

   overriding procedure Save
     (Property : access String_Property;
      Node     : in out XML_Utils.Node_Ptr) is
   begin
      if Property.Value /= null then
         Node.Value := new String'(Property.Value.all);
      end if;
   end Save;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Integer_Property;
      Node     : in out XML_Utils.Node_Ptr) is
   begin
      Node.Value := new String'(Integer'Image (Property.Value));
   end Save;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Boolean_Property;
      Node     : in out XML_Utils.Node_Ptr) is
   begin
      Node.Value := new String'(Boolean'Image (Property.Value));
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out String_Property; From : XML_Utils.Node_Ptr) is
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

   overriding procedure Load
     (Property : in out Integer_Property; From : XML_Utils.Node_Ptr) is
   begin
      Property.Value := Integer'Value (From.Value.all);
   exception
      when Constraint_Error =>
         Property.Value := 0;
   end Load;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Boolean_Property; From : XML_Utils.Node_Ptr) is
   begin
      Property.Value := Boolean'Value (From.Value.all);
   exception
      when Constraint_Error =>
         Property.Value := True;
   end Load;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Property : in out String_Property) is
   begin
      Free (Property.Value);
   end Destroy;

   ---------------------------
   -- Get_Resource_Property --
   ---------------------------

   procedure Get_Resource_Property
     (Property      : out Property_Record'Class;
      Resource_Key  : String;
      Resource_Kind : String;
      Name          : String;
      Found         : out Boolean)
   is
      pragma Unreferenced (Resource_Kind);
      Descr : Property_Description_Access;

   begin
      Descr := Get (All_Properties, Resource_Key & Sep & Name);
      Found := Descr /= null;

      if Found then
         if Descr.Value = null then
            Load (Property, Descr.Unparsed);
            Descr.Value := new Property_Record'Class'(Property);
            Free (Descr.Unparsed);  --  No longer needed
         end if;

         Property := Descr.Value.all;
         Found := True;
      end if;

   exception
      when others =>
         Found := False;
   end Get_Resource_Property;

   ------------------
   -- Get_Property --
   ------------------

   procedure Get_Property
     (Property    : out Property_Record'Class;
      Index_Name  : String;
      Index_Value : String;
      Name        : String;
      Found       : out Boolean) is
   begin
      Get_Resource_Property (Property, Index_Value, Index_Name, Name, Found);
   end Get_Property;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (File : GNATCOLL.VFS.Virtual_File) return String is
      Filename : String := +Full_Name (File, True);
   begin
      Canonical_Case_File_Name (Filename);

      return Filename;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Prj : Project_Type) return String is
   begin
      return To_String (Project_Path (Prj));
   end To_String;

   ------------------
   -- Get_Property --
   ------------------

   procedure Get_Property
     (Property : out Property_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Name     : String;
      Found    : out Boolean) is
   begin
      Get_Property (Property, "file", To_String (File), Name, Found);
   end Get_Property;

   ------------------
   -- Get_Property --
   ------------------

   procedure Get_Property
     (Property : out Property_Record'Class;
      Project  : Project_Type;
      Name     : String;
      Found    : out Boolean) is
   begin
      Get_Property (Property, "project", To_String (Project), Name, Found);
   end Get_Property;

end GPS.Properties;
