------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with VSS.Strings.Conversions;
with GNATCOLL.Utils;

with GPS.Editors;

package body DAP.Types.Breakpoints is

   function Is_Same_Location
     (Data : Breakpoint_Data;
      File : Virtual_File;
      Line : Editable_Line_Type)
      return Boolean;
   function Is_Same_Location
     (Data   : Breakpoint_Data;
      Marker : Location_Marker)
      return Boolean;
   --  Return true if the breakpoint's location matches the given one.

   function Is_Duplicate (L, R : Breakpoint_Data) return Boolean;
   pragma Unreferenced (Is_Duplicate);
   --  Used to detect breakpoint duplicates.

   function Get_Location_File (Data : Breakpoint_Data) return Virtual_File;
   --  Return the breakpoint's location file, if any.
   --  This is valid only for SLOC breakpoints: No_File will be returned
   --  otherwise.

   function Copy
     (Data      : Breakpoint_Data;
      Full_Copy : Boolean := False) return Breakpoint_Data;
   --  Copy the given breakpoint. If Full_Copy is True, the fields set by the
   --  running debugger once the debuggee is known will also be copied (e.g:
   --  breakpoint's number, address of the breakpoint's SLOC...). Otherwise,
   --  only the information that needs to be persistent will be copied (e.g:
   --  breakpoint's type, SLOC...).

   -------
   -- = --
   -------

   function "="
     (Data : Breakpoint_Data;
      Num  : Breakpoint_Identifier)
      return Boolean is
   begin
      return Data.Num = Num;
   end "=";

   ----------------------
   -- Is_Same_Location --
   ----------------------

   function Is_Same_Location
     (Data : Breakpoint_Data;
      File : Virtual_File;
      Line : Editable_Line_Type)
      return Boolean is
   begin
      if Data.Kind = On_Line then
         return GPS.Editors.Get_Line (Data.Location.Marker) = Line
           and then GPS.Editors.Get_File (Data.Location.Marker) = File;
      else
         return False;
      end if;
   end Is_Same_Location;

   ----------------------
   -- Is_Same_Location --
   ----------------------

   function Is_Same_Location
     (Data   : Breakpoint_Data;
      Marker : Location_Marker)
      return Boolean is
   begin
      if Similar (Data.Location.Marker, Marker) then
         return True;
      end if;

      return False;
   end Is_Same_Location;

   ------------------
   -- Is_Duplicate --
   ------------------

   function Is_Duplicate (L, R : Breakpoint_Data) return Boolean is
   begin
      if L.Kind /= R.Kind then
         return False;
      end if;

      case L.Kind is
         when On_Line =>
            return Is_Same_Location (L, Get_Location (R));

         when On_Subprogram =>
            return L.Subprogram = R.Subprogram;

         when On_Instruction =>
            return L.Location.Address = R.Location.Address;

         when On_Exception =>
            return L.Exception_Name = R.Exception_Name;
      end case;
   end Is_Duplicate;

   ----------
   -- Copy --
   ----------

   function Copy
     (Data      : Breakpoint_Data;
      Full_Copy : Boolean := False) return Breakpoint_Data
   is
      Result : Breakpoint_Data := Data;
   begin
      --  If we are not doing a full copy, reset the fields that are set by
      --  the debugger itself once the debuggee has started (e.g: address of
      --  a source breakpoint).

      if not Full_Copy then
         Result.Num := No_Breakpoint;
         Result.Verified := True;

         if Result.Kind = On_Line then
            Result.Location.Address := Invalid_Address;
         end if;
      end if;

      return Result;
   end Copy;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Data : Breakpoint_Data) return Location_Marker is
   begin
      if Data.Kind = On_Line then
         return Data.Location.Marker;
      else
         return No_Marker;
      end if;
   end Get_Location;

   -----------------------
   -- Get_Location_File --
   -----------------------

   function Get_Location_File (Data : Breakpoint_Data) return Virtual_File
   is
      Loc : Location_Marker;
   begin
      case Data.Kind is
         when On_Line =>
            Loc := Get_Location (Data);
            if Loc = No_Marker then
               return No_File;
            else
               return GPS.Editors.Get_File (Loc);
            end if;

         when others =>
            return No_File;
      end case;
   end Get_Location_File;

   ----------------
   -- Get_Ignore --
   ----------------

   function Get_Ignore (Data : Breakpoint_Data) return Virtual_String is
   begin
      if Data.Ignore = 0 then
         return Empty_Virtual_String;
      else
         declare
            S : constant String := Data.Ignore'Img;
         begin
            return VSS.Strings.Conversions.To_Virtual_String
              (S (S'First + 1 .. S'Last));
         end;
      end if;
   end Get_Ignore;

   ---------------
   -- To_String --
   ---------------

   function To_String (Data : Breakpoint_Data) return String is
   begin
      case Data.Kind is
         when On_Line =>
            if Data.Location.Marker /= No_Marker then
               return Get_Location_File (Data).Display_Base_Name
                 & ":"
                 & GNATCOLL.Utils.Image
                 (Integer (GPS.Editors.Get_Line
                  (Data.Location.Marker)),
                  Min_Width => 0);
            else
               return "(no location)";
            end if;

         when On_Subprogram =>
            return To_String (Data.Subprogram);

         when On_Instruction =>
            return Address_To_String (Data.Location.Address);

         when On_Exception =>
            return "exception " & To_String (Data.Exception_Name);
      end case;
   end To_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : out Breakpoint_Holder;
      Vector    : Breakpoint_Vectors.Vector;
      Full_Copy : Boolean := False)
   is
      Data : Breakpoint_Data;
   begin
      --  Clear any breakpoints stored in this holder first.
      Self.Vector.Clear;

      --  Copy the breakpoints into the holder
      for Idx in Vector.First_Index .. Vector.Last_Index loop
         Data := Copy (Vector (Idx), Full_Copy => Full_Copy);
         Self.Vector.Append (Data);
      end loop;
   end Initialize;

   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self    : Breakpoint_Holder;
      Indexes : Breakpoint_Index_Lists.List :=
        Breakpoint_Index_Lists.Empty_List)
      return Breakpoint_Vectors.Vector is
   begin
      if Indexes.Is_Empty then
         return Self.Vector;
      else
         declare
            Result : Breakpoint_Vectors.Vector;
         begin
            for Idx of Indexes loop
               Result.Append (Self.Vector (Idx));
            end loop;

            return Result;
         end;
      end if;
   end Get_Breakpoints;

   -------------------------------
   -- Get_Breakpoint_From_Index --
   -------------------------------

   function Get_Breakpoint_From_Index
     (Self : Breakpoint_Holder;
      Idx  : Positive) return Breakpoint_Data is
   begin
      if Idx in Self.Vector.First_Index .. Self.Vector.Last_Index then
         return Self.Vector (Idx);
      else
         return Empty_Breakpoint_Data;
      end if;
   end Get_Breakpoint_From_Index;

   ----------------------------
   -- Get_Breakpoint_From_Id --
   ----------------------------

   function Get_Breakpoint_From_Id
     (Self : Breakpoint_Holder;
      Id   : Breakpoint_Identifier) return Breakpoint_Data
   is
      Cursor : constant Breakpoint_Vectors.Cursor :=
        Self.Vector.Find (Breakpoint_Data'(Num => Id, others => <>));
   begin
      if Breakpoint_Vectors.Has_Element (Cursor) then
         return Breakpoint_Vectors.Element (Cursor);
      else
         return Empty_Breakpoint_Data;
      end if;
   end Get_Breakpoint_From_Id;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data)
   is
      use Breakpoint_Vectors;
      C : constant Breakpoint_Vectors.Cursor := Self.Vector.Find (Data);
   begin
      if C /= Breakpoint_Vectors.No_Element then
         Self.Vector.Replace_Element (C, Data);
      end if;
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data;
      Idx  : Positive) is
   begin
      if Idx in Self.Vector.First_Index .. Self.Vector.Last_Index then
         Self.Vector.Replace_Element (Idx, Data);
      end if;
   end Replace;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Breakpoint_Holder) is
   begin
      Self.Vector.Clear;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self   : in out Breakpoint_Holder;
      Marker : Location_Marker)
      return Boolean is
   begin
      for Data of Self.Vector loop
         if Is_Same_Location (Data, Marker) then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   ---------------------------
   -- Set_Breakpoints_State --
   ---------------------------

   procedure Set_Breakpoints_State
     (Self    : in out Breakpoint_Holder;
      Indexes : Breakpoint_Index_Lists.List;
      State   : Boolean) is
   begin
      for Idx of Indexes loop
         Self.Vector (Idx).Enabled := State;
      end loop;
   end Set_Breakpoints_State;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self        : in out Breakpoint_Holder;
      Breakpoints : Breakpoint_Vectors.Vector;
      Full_Copy   : Boolean := False)
   is
      C    : Breakpoint_Vectors.Cursor;
      Data : Breakpoint_Data;
   begin
      for Idx in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         if not Breakpoints.Contains (Self.Vector (Idx)) then
            --  no more exist
            Self.Vector.Delete (Idx);
         end if;
      end loop;

      for Idx in Breakpoints.First_Index .. Breakpoints.Last_Index loop
         Data := Copy (Breakpoints (Idx), Full_Copy => Full_Copy);
         C := Self.Vector.Find (Data);

         if Breakpoint_Vectors.Has_Element (C) then
            Self.Vector.Replace_Element (C, Data);
         else
            Self.Vector.Append (Data);
         end if;
      end loop;
   end Replace;

   -------------------
   -- Get_For_Files --
   -------------------

   function Get_For_Files
     (Self         : Breakpoint_Holder;
      Enabled_Only : Boolean := True)
      return Breakpoint_Hash_Maps.Map
   is
      Result  : Breakpoint_Hash_Maps.Map;
      File    : Virtual_File;
      Indexes : Breakpoint_Index_Lists.List;
      Data    : Breakpoint_Data;
   begin
      for Idx in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         Data := Self.Vector (Idx);

         if (Data.Enabled or else not Enabled_Only)
           and then Data.Kind = On_Line
         then
            File := Get_Location_File (Data);

            if Result.Contains (File) then
               Indexes := Result.Element (File);
               Indexes.Append (Idx);
               Result.Replace (File, Indexes);
            else
               Indexes.Clear;
               Indexes.Append (Idx);
               Result.Insert (File, Indexes);
            end if;
         end if;
      end loop;

      return Result;
   end Get_For_Files;

   ------------------
   -- Get_For_File --
   ------------------

   function Get_For_File
     (Self         : Breakpoint_Holder;
      File         : Virtual_File;
      Enabled_Only : Boolean := True)
      return Breakpoint_Vectors.Vector
   is
      Result : Breakpoint_Vectors.Vector;
   begin
      for Data of Self.Vector loop
         if (Data.Enabled or else not Enabled_Only)
           and then Get_Location_File (Data) = File
         then
            Result.Append (Data);
         end if;
      end loop;

      return Result;
   end Get_For_File;

   ------------------
   -- Get_For_File --
   ------------------

   function Get_For_File
     (Self         : Breakpoint_Holder;
      File         : Virtual_File;
      Enabled_Only : Boolean := True)
      return Breakpoint_Index_Lists.List
   is
      Indexes : Breakpoint_Index_Lists.List;
      Data    : Breakpoint_Data;
   begin
      for Idx in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         Data := Self.Vector (Idx);

         if (Data.Enabled or else not Enabled_Only)
           and then Get_Location_File (Data) = File
         then
            Indexes.Append (Idx);
         end if;
      end loop;

      return Indexes;
   end Get_For_File;

   ------------------
   -- Get_For_Kind --
   ------------------

   function Get_For_Kind
     (Self         : Breakpoint_Holder;
      Kind         : Breakpoint_Kind;
      Enabled_Only : Boolean := True)
      return Breakpoint_Index_Lists.List
   is
      Indexes : Breakpoint_Index_Lists.List;
      Data    : Breakpoint_Data;
   begin
      for Idx in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         Data := Self.Vector (Idx);

         if (Data.Enabled or else not Enabled_Only)
           and then Data.Kind = Kind
         then
            Indexes.Append (Idx);
         end if;
      end loop;

      return Indexes;
   end Get_For_Kind;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data) is
   begin
      Self.Vector.Append (Data);
   end Append;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self : in out Breakpoint_Holder;
      Id   : Breakpoint_Identifier)
   is
      Cursor : Breakpoint_Vectors.Cursor :=
        Self.Vector.Find (Breakpoint_Data'(Num => Id, others => <>));
   begin
      if Breakpoint_Vectors.Has_Element (Cursor) then
         Self.Vector.Delete (Cursor);
      end if;
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      Indexes : Breakpoint_Index_Lists.List) is
   begin
      for Idx of Indexes loop
         Self.Vector.Delete (Idx);
      end loop;
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Line    : Editable_Line_Type)
   is
      Indexes : Breakpoint_Index_Lists.List;
   begin
      for Idx in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         if Is_Same_Location (Self.Vector (Idx), File, Line) then
            Indexes.Append (Idx);
         end if;
      end loop;

      Self.Delete (Indexes => Indexes);
   end Delete;

   ----------------------
   -- Set_Ignore_Count --
   ----------------------

   procedure Set_Ignore_Count
     (Self    : in out Breakpoint_Holder;
      Id      : Breakpoint_Identifier;
      Count   : Natural) is
   begin
      for Data of Self.Vector loop
         if Data.Num = Id then
            Data.Ignore := Count;
            return;
         end if;
      end loop;
   end Set_Ignore_Count;

end DAP.Types.Breakpoints;
