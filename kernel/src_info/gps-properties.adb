------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

with Ada.Tags;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GPR.Osint;                  use GPR.Osint;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

package body GPS.Properties is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.PROPERTIES");

   use Properties_Indefinite_Hashed_Maps;

   Current_Writer   : Writer;

   Languages_Loaded : Boolean := False;
   --  Have languages been loaded

   procedure Get_Resource_Property
     (Property : out Property_Record'Class;
      Key      : String;
      Name     : String;
      Found    : out Boolean);
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

   -----------
   -- Store --
   -----------

   function Store
     (Property : access Property_Record'Class)
      return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.JSON;
      Result : JSON_Value := Create_Object;
   begin
      Result.Set_Field ("type", Ada.Tags.External_Tag (Property.all'Tag));
      Property.Save (Result);
      return Result;

   exception
      when others =>
         return JSON_Null;
   end Store;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access String_Property;
      Value    : in out GNATCOLL.JSON.JSON_Value) is
   begin
      if Property.Value /= null then
         Value.Set_Field ("value", Property.Value.all);
      else
         Value.Set_Field ("value", "");
      end if;
   end Save;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Integer_Property;
      Value    : in out GNATCOLL.JSON.JSON_Value) is
   begin
      Value.Set_Field ("value", Property.Value);
   end Save;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Boolean_Property;
      Value    : in out GNATCOLL.JSON.JSON_Value) is
   begin
      Value.Set_Field ("value", Property.Value);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out String_Property;
      Value    : GNATCOLL.JSON.JSON_Value)
   is
      Data : constant String := Value.Get ("value");
   begin
      Free (Property.Value);

      if Data /= "" then
         Property.Value := new String'(Data);
      end if;
   end Load;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Integer_Property;
      Value    : GNATCOLL.JSON.JSON_Value) is
   begin
      Property.Value := Value.Get ("value");

   exception
      when Constraint_Error =>
         Property.Value := 0;
   end Load;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Boolean_Property;
      Value    : GNATCOLL.JSON.JSON_Value) is
   begin
      Property.Value := Value.Get ("value");

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
     (Property : out Property_Record'Class;
      Key      : String;
      Name     : String;
      Found    : out Boolean)
   is
      Descr : Property_Description_Access;

      procedure Append
        (Key      : String;
         Property : Property_Record'Class;
         Found    : Boolean);
      --  Append the property to the registry

      procedure Process
        (Key : String; Property : in out Property_Record'Class);
      --  Process language for file

      ------------
      -- Append --
      ------------

      procedure Append
        (Key      : String;
         Property : Property_Record'Class;
         Found    : Boolean) is
      begin
         Descr := new Property_Description;
         Descr.Persistent := True;

         if Found then
            Descr.Value := new Property_Record'Class'(Property);
         end if;

         declare
            C : constant Cursor := All_Properties.Find (Key & Sep & Name);

            ---------
            -- Get --
            ---------
            function Get (Property : Property_Access) return String;
            function Get (Property : Property_Access) return String is
            begin
               if Property = null then
                  return "'empty'";
               else
                  if Property.all in String_Property'Class then
                     return String_Property (Property.all).Value.all;
                  elsif Property.all in Integer_Property'Class then
                     return Integer_Property (Property.all).Value'Img;
                  elsif Property.all in Boolean_Property'Class then
                     return Boolean_Property (Property.all).Value'Img;
                  else
                     return "Unsupported property class:" &
                       Ada.Tags.External_Tag (Property'Tag);
                  end if;
               end if;
            end Get;

         begin
            if C /= No_Element then
               Trace
                 (Me, "Key '" & Key & Sep & Name &
                    "' already exsists with " & Get (Element (C).Value) &
                    " value. New value:" & Get (Descr.Value));
            else
               Insert (All_Properties, Key & Sep & Name, Descr);
            end if;
         end;
      end Append;

      -------------
      -- Process --
      -------------

      procedure Process
        (Key : String; Property : in out Property_Record'Class) is
      begin
         Append (Key, Property, True);
         String_Property (Property).Value := null;
      end Process;

      C : Cursor;
   begin
      if not Languages_Loaded
        and then Name = "language"
      then
         declare
            P : String_Property;
         begin
            --  The Current_Writer might be null (for instance in the case of
            --  gnatdoc or gps_cli)
            if Current_Writer /= null then
               Current_Writer.Get_Values (Name, P, Process'Access);
            end if;
            Languages_Loaded := True;
         end;
      end if;

      C := Find (All_Properties, Key & Sep & Name);

      if not Has_Element (C) then
         if Name = "language" then
            Found := False;
            return;
         end if;

         --  Getting property the first time
         begin
            Current_Writer.Get_Value (Key, Name, Property, Found);
            Append (Key, Property, Found);

         exception
            when E : others =>
               Trace (Me, E);
               Found := False;
               --  Parsing of property raises exception which may be related to
               --  wrong (old) format of record in DB, so remove this property
               --  from DB
               Current_Writer.Remove (Key, Name);
         end;

      else
         Descr := Element (C);
         if Descr.Value = null then
            --  Already looked up and not found last time
            Found := False;
         else
            Property := Descr.Value.all;
            Found    := True;
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Found := False;
   end Get_Resource_Property;

   ------------------
   -- Get_Property --
   ------------------

   procedure Get_Property
     (Property : out Property_Record'Class;
      Key      : String;
      Name     : String;
      Found    : out Boolean) is
   begin
      Get_Resource_Property (Property, Key, Name, Found);
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
      Get_Property (Property, To_String (File), Name, Found);
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
      Get_Property (Property, To_String (Project), Name, Found);
   end Get_Property;

   ----------------
   -- Set_Writer --
   ----------------

   procedure Set_Writer (Object : Writer) is
   begin
      Current_Writer := Object;
   end Set_Writer;

   -------------
   -- Restore --
   -------------

   procedure Restore
     (Property : in out Property_Record'Class;
      Value    : GNATCOLL.JSON.JSON_Value;
      Valid    : out Boolean)
   is
      use GNATCOLL.JSON;
   begin
      if String'(Value.Get ("type")) =
        Ada.Tags.External_Tag (Property'Tag)
      then
         Valid := True;
         Property.Load (Value);
      else
         Valid := False;
      end if;
   end Restore;

end GPS.Properties;
