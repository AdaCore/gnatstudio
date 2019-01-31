------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

--  Stores the properties in json format.
--  Schema of the json:
--      {"properties":
--        [
--          {'k': <the_key>,
--           'n': <the_name>,
--           'd': <the data>},
--          ...
--        ]
--       }

with Ada.Strings.Unbounded.Hash;

with GNAT.Strings;    use GNAT.Strings;
with GNATCOLL.JSON;   use GNATCOLL.JSON;
with GNATCOLL.Traces; use GNATCOLL.Traces;

package body GPS.Kernel.Properties.File_Writer is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.PROPERTIES.FILE_WRITER");

   use Key_Value;

   function Get_Properties_Filename
     (Kernel : access Kernel_Handle_Record'Class;
      Dump   : Boolean := False)
      return Virtual_File;
   --  Return the filename to use when saving the persistent properties for the
   --  current project

   procedure Write_To_File (Self : File_Writer_Record; File : Virtual_File);
   --  Write Self to File

   -----------------------------
   -- Get_Properties_Filename --
   -----------------------------

   function Get_Properties_Filename
     (Kernel : access Kernel_Handle_Record'Class;
      Dump   : Boolean := False)
      return Virtual_File is
   begin
      --  We are using the .gps directory. This means we have to keep
      --  in database information for files that do not belong to the current
      --  project.

      return Create_From_Dir
        (Get_Home_Dir (Kernel), "properties" &
           (if Dump then ".dump" else ".json"));
   end Get_Properties_Filename;

   -----------------
   -- Constructor --
   -----------------

   function Constructor
     (Kernel : access Kernel_Handle_Record'Class) return
     GPS.Properties.Writer
   is
      File : constant Virtual_File := Get_Properties_Filename (Kernel);
      Root : JSON_Value := JSON_Null;
      Arr  : JSON_Array := Empty_Array;

   begin
      if File.Is_Regular_File then
         declare
            S : GNAT.Strings.String_Access := File.Read_File;
         begin
            if S = null then
               Trace (Me, "Properties file empty");
            else
               Root := Read (S.all, +File.Full_Name.all);
               Arr := Root.Get ("properties");
               Free (S);
            end if;
         exception
            when E : others =>
               Trace (Me, E);
         end;
      else
         Trace (Me, "Properties file not present");
      end if;

      return W : constant GPS.Properties.Writer := new File_Writer_Record
        (Kernel)
      do
         if Arr /= Empty_Array then
            for J in 1 .. Length (Arr) loop
               Root := Get (Arr, J);
               File_Writer (W).Map.Insert
                 ((Root.Get ("k"), Root.Get ("n")), Root.Get ("d"));
            end loop;
         end if;
      end return;
   end Constructor;

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File (Self : File_Writer_Record; File : Virtual_File) is
      Root : JSON_Value;
      Arr  : JSON_Array;
      C     : Cursor;
   begin
      Root := Create_Object;

      C := Self.Map.First;

      while C /= No_Element loop
         declare
            N : JSON_Value;
            K : constant Key_Name := Key (C);
         begin
            N := Create_Object;
            Set_Field (N, "n", K.Name);
            Set_Field (N, "k", K.Key);
            Set_Field (N, "d", Element (C));
            Append (Arr, N);
         end;
         Next (C);
      end loop;

      Set_Field (Root, "properties", Arr);

      declare
         W : Writable_File;
      begin
         W := Write_File (File);
         Write (W, Write (Root));
         Close (W);
      exception
         when E : others =>
            Trace (Me, E);
            Trace (Me, "Could not write to properties file "
                     & File.Display_Full_Name);
      end;
   end Write_To_File;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self     : not null access File_Writer_Record;
      Key      : String;
      Name     : String;
      Property : out Property_Record'Class;
      Found : out Boolean)
   is
      C : Cursor;
   begin
      Found := False;
      C := Self.Map.Find
        ((To_Unbounded_String (Key), To_Unbounded_String (Name)));
      if C = No_Element then
         return;
      else
         Property.Restore (Read (Element (C)), Found);
      end if;
   end Get_Value;

   ----------------
   -- Get_Values --
   ----------------

   overriding procedure Get_Values
     (Self     :        not null access File_Writer_Record;
      Name     : String;
      Property : in out Property_Record'Class;
      Callback :        access procedure
        (Key : String; Property : in out Property_Record'Class))
   is
      Valid : Boolean;
      C     : Cursor;
   begin
      C := Self.Map.First;

      while C /= No_Element loop
         declare
            K : constant Key_Name := Key (C);
         begin
            if To_String (K.Name) = Name then
               Property.Restore (Read (Element (C)), Valid);
               if Valid then
                  Callback (To_String (K.Key), Property);
               end if;
            end if;
         end;

         Next (C);
      end loop;
   end Get_Values;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self     : not null access File_Writer_Record;
      Key      : String;
      Name     : String;
      Property : Property_Description)
   is
      Value : constant GNATCOLL.JSON.JSON_Value :=
        Property.Value.Store;
   begin
      Self.Map.Insert (Key      => (To_Unbounded_String (Key),
                                    To_Unbounded_String (Name)),
                       New_Item => Write (Value));
   end Insert;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Self     : not null access File_Writer_Record;
      Key      : String;
      Name     : String;
      Property : Property_Description)
   is
      Value : constant GNATCOLL.JSON.JSON_Value :=
         Property.Value.Store;
      K : constant Key_Name := (To_Unbounded_String (Key),
                                To_Unbounded_String (Name));
   begin
      if Self.Map.Contains (K) then
         Self.Map.Replace (Key => K, New_Item => Write (Value));
      else
         Self.Map.Insert (Key => K, New_Item => Write (Value));
      end if;
   end Update;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Self : not null access File_Writer_Record;
      Key  : String;
      Name : String)
   is
      K : constant Key_Name :=
        (To_Unbounded_String (Key), To_Unbounded_String (Name));
   begin
      if Self.Map.Contains (K) then
         Self.Map.Delete (K);
      end if;
   end Remove;

   -------------------
   -- Dump_Database --
   -------------------

   overriding procedure Dump_Database
     (Self : not null access File_Writer_Record)
   is
      C    : Cursor;
      W    : Writable_File;
   begin
      W := Write_File (Get_Properties_Filename (Self.Kernel, Dump => True));

      C := Self.Map.First;

      while C /= No_Element loop
         Write (W, To_String (Key (C).Key)
                & "@" & To_String (Key (C).Name) & ":"
                & To_String (Element (C)) & ASCII.LF);
         Next (C);
      end loop;

      Close (W);
   exception
      when E : others =>
         Close (W);
         Trace (Me, E);
   end Dump_Database;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out File_Writer_Record) is
   begin
      Write_To_File
        (Self,
         Get_Properties_Filename (Self.Kernel, Dump => False));

      if Active (Dump) then
         Dump_Database (Self'Access);
      end if;

      Self.Map.Clear;
   end Finalize;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Key_Name) return Ada.Containers.Hash_Type is
   begin
      return Hash (Key.Key & Key.Name);
   end Hash;

end GPS.Kernel.Properties.File_Writer;
