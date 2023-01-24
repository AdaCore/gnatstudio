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

with GPS.Editors;
with DAP.Persistent_Breakpoints;
with DAP.Preferences;

package body DAP.Breakpoint_Maps is

   -------
   -- = --
   -------

   function "="
     (Data : Breakpoint_Data;
      Num  : Breakpoint_Identifier) return Boolean is
   begin
      if Data.Num = Num then
         return True;
      else
         for L of Data.Locations loop
            if L.Num = Num then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end "=";

   -----------------------------
   -- Breakpoint_Vector_Equal --
   -----------------------------

   function Breakpoint_Vector_Equal
     (L, R : Breakpoint_Vectors.Vector) return Boolean
   is
   begin
      if Positive (L.Length) /= Positive (R.Length) then
         return False;
      end if;

      for I in L.First_Index .. L.Last_Index loop
         if Breakpoint_Data_Equal (L.Element (I), R.Element (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Breakpoint_Vector_Equal;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Data : Breakpoint_Data) return Location_Marker is
   begin
      if Data.Locations.Is_Empty then
         return No_Marker;
      else
         return Data.Locations.First_Element.Location;
      end if;
   end Get_Location;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (Data : Breakpoint_Data) return Address_Type is
   begin
      if Data.Locations.Is_Empty then
         return Invalid_Address;
      else
         return Data.Locations.First_Element.Address;
      end if;
   end Get_Address;

   ----------------------
   -- Is_Same_Location --
   ----------------------

   function Is_Same_Location (L, R : Breakpoint_Data) return Boolean is
   begin
      if L.Subprogram /= Null_Unbounded_String then
         return L.Subprogram = R.Subprogram;
      else
         return GPS.Editors.Get_Line (Get_Location (L)) =
           GPS.Editors.Get_Line (Get_Location (R))
           and then GPS.Editors.Get_File (Get_Location (L)) =
             GPS.Editors.Get_File (Get_Location (R));
      end if;
   end Is_Same_Location;

   ----------
   -- Copy --
   ----------

   procedure Copy (To : in out Breakpoint_Data; From : Breakpoint_Data) is
   begin
      To.Num       := From.Num;
      To.Locations := From.Locations;
   end Copy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Breakpoint_Holder;
      List : Breakpoint_Vectors.Vector) is
   begin
      Self.Clear;
      Self.All_Breakpoints := List;
      Self.Synchronize_Lists;
   end Initialize;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self    : in out Breakpoint_Holder;
      Data    : in out Breakpoint_Data;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      File : Virtual_File;
      List : Breakpoint_Vectors.Vector;
   begin
      --  Test whether we do not duplicate Data
      for Index in
        Self.All_Breakpoints.First_Index .. Self.All_Breakpoints.Last_Index
      loop
         if Self.All_Breakpoints.Element (Index) = Data then
            return;

         elsif Is_Same_Location
           (Self.All_Breakpoints.Element (Index), Data)
         then
            Data.Id  := Self.All_Breakpoints.Element (Index).Id;
            Data.Num := Self.All_Breakpoints.Element (Index).Num;

            if Data.Subprogram /= Null_Unbounded_String then
               List := Self.Subprograms;
               for Idx in List.First_Index .. List.Last_Index loop
                  if List.Element (Idx).Id = Data.Id then
                     List.Replace_Element (Idx, Data);
                     Changed.Insert (No_File, List);
                     exit;
                  end if;
               end loop;

            elsif Get_Location (Data) /= No_Marker then
               File := GPS.Editors.Get_File (Get_Location (Data));
               List := Self.Get_For_File (File);

               for Idx in List.First_Index .. List.Last_Index loop
                  if List.Element (Idx).Id = Data.Id then
                     List.Replace_Element (Idx, Data);
                     Changed.Insert (File, List);
                     exit;
                  end if;
               end loop;
            end if;

            return;
         end if;
      end loop;

      Self.Add_To_Changed (Data, Changed);
   end Add;

   -----------
   -- Added --
   -----------

   procedure Added
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map;
      Updated : out Boolean)
   is
      List       : Breakpoint_Vectors.Vector := Self.Get_For_File (File);
      Cursor     : Breakpoint_Vectors.Cursor;
      Data       : Breakpoint_Data;
      Duplicates : Boolean := False;
   begin
      Updated := False;

      --  Copy debugger specific data, Actual list is wider because also
      --  contains added breakpoints.
      for Idx in List.First_Index .. List.Last_Index loop
         Data := List.Element (Idx);
         if Data.Num /= Actual.Element (Idx).Num then
            --  Num may be changed
            Updated := True;
         end if;

         Copy (Data, Actual.Element (Idx));
         List.Replace_Element (Idx, Data);
         Cursor := Self.All_Breakpoints.Find (Data);
         Self.All_Breakpoints.Replace_Element (Cursor, Data);
      end loop;

      for Index in List.Last_Index + 1 .. Actual.Last_Index loop
         Data := Actual.Element (Index);

         --  Test whether we do not duplicate Data
         for Idx in
           Self.All_Breakpoints.First_Index .. Self.All_Breakpoints.Last_Index
         loop
            if Is_Same_Location
              (Self.All_Breakpoints.Element (Idx), Data)
            then
               --  We already have a breakpoint for the location,
               --  This one should be deleted
               Data.Num   := 0;
               Duplicates := True;
               exit;
            end if;
         end loop;

         if Data.Num /= 0 then
            --  New breakpoint, add to the lists
            Updated := True;

            if Data.Id = 0 then
               Data.Id := DAP.Persistent_Breakpoints.Get_Next_Id;
            end if;

            Self.All_Breakpoints.Append (Data);
            List.Append (Data);
         end if;
      end loop;

      Self.Set_For_File (File, List);

      if Duplicates then
         Changed.Insert (File, List);
      end if;
   end Added;

   ----------------------
   -- Added_Subprogram --
   ----------------------

   procedure Added_Subprogram
     (Self    : in out Breakpoint_Holder;
      Added   : Breakpoint_Data;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map;
      Updated : out Boolean)
   is
      List       : Breakpoint_Vectors.Vector := Self.Subprograms;
      Cursor     : Breakpoint_Vectors.Cursor;
      Index      : Integer := 1;
      Data, D    : Breakpoint_Data;
      Count      : Integer;
      Duplicates : Boolean := False;
   begin
      Updated := False;

      --  Copy debugger specific data, Actual list is wider because also
      --  contains added breakpoints.
      for Idx in List.First_Index .. List.Last_Index loop
         Data  := List.Element (Idx);
         Count := Data.Locations.Last_Index;
         if Data.Num /= Actual.Element (Index).Num then
            --  Num may be changed
            Updated := True;
         end if;

         Copy (Data, Actual.Element (Index));
         Index := Index + 1;
         for I in 2 .. Count loop
            Copy (D, Actual.Element (Index));
            Data.Locations.Append (D.Locations.First_Element);
            Index := Index + 1;
         end loop;

         List.Replace_Element (Idx, Data);
         Cursor := Self.All_Breakpoints.Find (Data);
         Self.All_Breakpoints.Replace_Element (Cursor, Data);
      end loop;

      Data := Added;
      if Index <= Actual.Last_Index then
         Copy (Data, Actual.Element (Index));
         Index := Index + 1;
         while Index <= Actual.Last_Index loop
            Copy (D, Actual.Element (Index));
            Data.Locations.Append (D.Locations.First_Element);
            Index := Index + 1;
         end loop;

         --  Test whether we do not duplicate Data
         for Idx in
           Self.All_Breakpoints.First_Index .. Self.All_Breakpoints.Last_Index
         loop
            if Is_Same_Location
              (Self.All_Breakpoints.Element (Idx), Data)
            then
               --  We already have a breakpoint for the location,
               --  This one should be deleted
               Data.Num   := 0;
               Duplicates := True;
               exit;
            end if;
         end loop;

         if Data.Num /= 0 then
            --  New breakpoint, add to the lists
            Updated := True;

            if Data.Id = 0 then
               Data.Id := DAP.Persistent_Breakpoints.Get_Next_Id;
            end if;

            Self.All_Breakpoints.Append (Data);
            List.Append (Data);
         end if;
      end if;

      Self.Subprograms := List;

      if Duplicates then
         Changed.Insert (No_File, List);
      end if;
   end Added_Subprogram;

   --------------------
   -- Add_To_Changed --
   --------------------

   procedure Add_To_Changed
     (Self    : in out Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : in out Breakpoint_Hash_Maps.Map)
   is
      File   : Virtual_File;
      Cursor : Breakpoint_Hash_Maps.Cursor;
      List   : Breakpoint_Vectors.Vector;
   begin
      if Data.Subprogram /= Null_Unbounded_String then
         File := No_File;
      else
         File := GPS.Editors.Get_File (Get_Location (Data));
      end if;

      Cursor := Changed.Find (File);
      if Has_Element (Cursor) then
         List := Element (Cursor);
      else
         List := Self.Get_For_File (File);
      end if;

      List.Append (Data);

      if Has_Element (Cursor) then
         Changed.Replace_Element (Cursor, List);
      else
         Changed.Insert (File, List);
      end if;
   end Add_To_Changed;

   -------------------------
   -- Delete_From_Changed --
   -------------------------

   procedure Delete_From_Changed
     (Self    : in out Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : in out Breakpoint_Hash_Maps.Map)
   is
      File   : Virtual_File;
      Cursor : Breakpoint_Hash_Maps.Cursor;
      List   : Breakpoint_Vectors.Vector;
   begin
      if Data.Subprogram /= Null_Unbounded_String then
         File := No_File;
      else
         File := GPS.Editors.Get_File (Get_Location (Data));
      end if;

      Cursor := Changed.Find (File);
      if Has_Element (Cursor) then
         List := Element (Cursor);
      else
         List := Self.Get_For_File (File);
      end if;

      for I in List.First_Index .. List.Last_Index loop
         if List.Element (I).Id = Data.Id then
            List.Delete (I);
            exit;
         end if;
      end loop;

      if Has_Element (Cursor) then
         Changed.Replace_Element (Cursor, List);
      else
         Changed.Insert (File, List);
      end if;
   end Delete_From_Changed;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Line    : Editable_Line_Type;
      Updated : out Boolean;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      Idx  : Integer := Self.All_Breakpoints.First_Index;
      Data : Breakpoint_Data;
   begin
      Updated := False;
      while Idx <= Self.All_Breakpoints.Last_Index loop
         Data := Self.All_Breakpoints.Element (Idx);
         for L of Data.Locations loop
            if GPS.Editors.Get_Line (L.Location) = Line
              and then GPS.Editors.Get_File (L.Location) = File
            then
               if Data.Disposition /= Disable then
                  Self.Delete_From_Changed (Data, Changed);
               else
                  Updated := True;
                  Self.All_Breakpoints.Delete (Idx);
               end if;
               exit;
            end if;
         end loop;
         Idx := Idx + 1;
      end loop;
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      Nums    : Breakpoint_Identifier_Lists.List;
      Updated : out Boolean;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      Idx  : Integer;
      Data : Breakpoint_Data;
   begin
      Updated := False;
      for Num of Nums loop
         Idx := Self.All_Breakpoints.First_Index;
         while Idx <= Self.All_Breakpoints.Last_Index loop
            Data := Self.All_Breakpoints.Element (Idx);
            if Data.Num = Num then
               if Data.Disposition /= Disable then
                  Self.Delete_From_Changed (Data, Changed);
               else
                  Updated := True;
                  Self.All_Breakpoints.Delete (Idx);
                  Idx := Idx - 1;
               end if;
            end if;
            Idx := Idx + 1;
         end loop;
      end loop;
   end Delete;

   ---------------------
   -- Delete_Disabled --
   ---------------------

   procedure Delete_Disabled (Self : in out Breakpoint_Holder) is
      Idx  : Integer := Self.All_Breakpoints.First_Index;
      Data : Breakpoint_Data;
   begin
      while Idx <= Self.All_Breakpoints.Last_Index loop
         Data := Self.All_Breakpoints.Element (Idx);
         if Data.Disposition = Disable then
            Self.All_Breakpoints.Delete (Idx);
            Idx := Idx - 1;
         end if;
         Idx := Idx + 1;
      end loop;
   end Delete_Disabled;

   -----------------------
   -- Delete_From_Lists --
   -----------------------

   procedure Delete_From_Lists
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data)
   is
      List : Breakpoint_Vectors.Vector;
   begin
      if Data.Subprogram /= Null_Unbounded_String then
         for Idx in
           Self.Subprograms.First_Index .. Self.Subprograms.Last_Index
         loop
            if Self.Subprograms.Element (Idx).Id = Data.Id then
               Self.Subprograms.Delete (Idx);
               exit;
            end if;
         end loop;

      elsif Get_Location (Data) /= No_Marker then
         List := Self.Get_For_File
           (GPS.Editors.Get_File (Get_Location (Data)));
         for Idx in List.First_Index .. List.Last_Index loop
            if List.Element (Idx).Id = Data.Id then
               List.Delete (Idx);
               Self.Set_For_File
                 (GPS.Editors.Get_File (Get_Location (Data)), List);
               exit;
            end if;
         end loop;
      end if;
   end Delete_From_Lists;

   -------------
   -- Deleted --
   -------------

   procedure Deleted
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Updated : out Boolean)
   is
      use Breakpoint_Vectors;
      Data   : Breakpoint_Data;
      List   : Vector := Self.Get_For_File (File);
      Cursor : Breakpoint_Vectors.Cursor;
      Index  : Integer := 1;
   begin
      Updated := False;

      while Index <= List.Last_Index loop
         Data   := List.Element (Index);
         Cursor := Actual.Find (Data);

         if Has_Element (Cursor) then
            Copy (Data, Element (Cursor));
            List.Replace_Element (Index, Data);
            Cursor := Self.All_Breakpoints.Find (Data);
            Self.All_Breakpoints.Replace_Element (Cursor, Data);
            Index := Index + 1;
         else
            Updated := True;
            List.Delete (Index);
            Cursor := Self.All_Breakpoints.Find (Data);
            Self.All_Breakpoints.Delete (Cursor);
         end if;
      end loop;

      Self.Set_For_File (File, List);
   end Deleted;

   ------------------------
   -- Deleted_Subprogram --
   ------------------------

   procedure Deleted_Subprogram
     (Self    : in out Breakpoint_Holder;
      Actual  : Breakpoint_Vectors.Vector;
      Updated : out Boolean)
   is
      Data  : Breakpoint_Data;
      Index : Integer := 1;
   begin
      Updated          := False;
      Self.Subprograms := Actual;

      while Index <= Self.All_Breakpoints.Last_Index loop
         Data := Self.All_Breakpoints.Element (Index);
         if Data.Subprogram /= Null_Unbounded_String
           and then not Self.Subprograms.Contains (Data)
         then
            Updated := True;
            Self.All_Breakpoints.Delete (Index);
         else
            Index := Index + 1;
         end if;
      end loop;
   end Deleted_Subprogram;

   --------------------------
   -- Initialized_For_File --
   --------------------------

   procedure Initialized_For_File
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      use type Ada.Containers.Count_Type;

      List    : Breakpoint_Vectors.Vector := Self.Get_For_File (File);
      Idx_Old : Integer := 1;
      Idx_New : Integer := 1;
      Data    : Breakpoint_Data;

      ------------
      -- Update --
      ------------

      procedure Update;
      procedure Update is
         Cursor : Breakpoint_Vectors.Cursor;
      begin
         Data := List.Element (Idx_Old);
         Copy (Data, Actual.Element (Idx_New));
         List.Replace_Element (Idx_Old, Data);

         Cursor := Self.All_Breakpoints.Find (Data);
         Self.All_Breakpoints.Replace_Element (Cursor, Data);

         Idx_Old := Idx_Old + 1;
         Idx_New := Idx_New + 1;
      end Update;

      ---------------------------
      -- Is_Duplicate_Location --
      ---------------------------

      function Is_Duplicate_Location (Index : Natural) return Natural;
      function Is_Duplicate_Location (Index : Natural) return Natural is
      begin
         for Idx in List.First_Index .. List.Last_Index loop
            if Idx /= Index
              and then Is_Same_Location
                (List.Element (Idx), Actual.Element (Index))
            then
               return Idx;
            end if;
         end loop;

         return 0;
      end Is_Duplicate_Location;

      ------------
      -- Delete --
      ------------

      procedure Delete (Duplicate : Boolean);
      procedure Delete (Duplicate : Boolean) is
      begin
         Data := List.Element (Idx_Old);
         for Index in
           Self.All_Breakpoints.First_Index ..
             Self.All_Breakpoints.Last_Index
         loop
            if Self.All_Breakpoints.Element (Index).Id = Data.Id then
               if Duplicate then
                  Self.All_Breakpoints.Delete (Index);
               else
                  if DAP.Preferences.Pending_Breakpoints.Get_Pref then
                     Self.Pending.Append
                       (Self.All_Breakpoints.Element (Index));
                  end if;
                  Self.All_Breakpoints.Delete (Index);
               end if;
               exit;
            end if;
         end loop;

         List.Delete (Idx_Old);
      end Delete;

   begin
      Changed := Breakpoint_Hash_Maps.Empty_Map;

      while Idx_Old <= List.Last_Index
        and then Idx_New <= Actual.Last_Index
      loop
         if Is_Same_Location
           (List.Element (Idx_Old), Actual.Element (Idx_New))
         then
            --  Breakpoint on self location, store
            Update;

         elsif Is_Duplicate_Location (Idx_New) /= 0 then
            --  Dublicate, delete it
            Delete (True);
            Idx_New := Idx_New + 1;
         else
            --  Breakpoint moved but not duplicated, store new location
            Update;
         end if;
      end loop;

      while Idx_Old <= List.Last_Index loop
         --  Remove breakpoints that were not set
         Delete (False);
      end loop;

      Self.Set_For_File (File, List);
      if List.Length /= Actual.Length then
         --  We need to synchronize breakpoints because we removed duplicates
         Changed.Insert (File, List);
      end if;
   end Initialized_For_File;

   ---------------------------------
   -- Initialized_For_Subprograms --
   ---------------------------------

   procedure Initialized_For_Subprograms
     (Self    : in out Breakpoint_Holder;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      pragma Unreferenced (Changed);

      List    : Breakpoint_Vectors.Vector := Self.Get_For_Subprograms;
      Idx_Old : Integer := 1;
      Idx_New : Integer := 1;
      Data    : Breakpoint_Data;
      Cursor  : Breakpoint_Vectors.Cursor;
   begin
      --  for now we support one breakpoint for subprogram
      while Idx_Old <= List.Last_Index
        and then Idx_New <= Actual.Last_Index
      loop
         Data := List.Element (Idx_Old);
         Copy (Data, Actual.Element (Idx_New));
         Idx_Old := Idx_Old + 1;
         Idx_New := Idx_New + 1;
         List.Replace_Element (Idx_Old, Data);

         Cursor := Self.All_Breakpoints.Find (Data);
         Self.All_Breakpoints.Replace_Element (Cursor, Data);
      end loop;

      --  delete breakpoints that can't be set
      while Idx_Old <= List.Last_Index loop
         Cursor := Self.All_Breakpoints.Find (List.Element (Idx_Old));
         if Breakpoint_Vectors.Has_Element (Cursor) then
            if DAP.Preferences.Pending_Breakpoints.Get_Pref then
               Data     := Breakpoint_Vectors.Element (Cursor);
               Data.Num := 0;
               Self.All_Breakpoints.Replace_Element (Cursor, Data);
            else
               Self.All_Breakpoints.Delete (Cursor);
            end if;
         end if;
         List.Delete (Idx_Old);
      end loop;
   end Initialized_For_Subprograms;

   -------------------
   -- Get_For_Files --
   -------------------

   function Get_For_Files
     (Self : in out Breakpoint_Holder)
      return Breakpoint_Hash_Maps.Map is
   begin
      return Self.Per_Files;
   end Get_For_Files;

   ------------------
   -- Get_For_File --
   ------------------

   function Get_For_File
     (Self : in out Breakpoint_Holder;
      File : Virtual_File)
      return Breakpoint_Vectors.Vector
   is
      C : Breakpoint_Hash_Maps.Cursor;
      V : Breakpoint_Vectors.Vector;
   begin
      if File = No_File then
         return Self.Subprograms;
      end if;

      C := Self.Per_Files.Find (File);
      if Has_Element (C) then
         return Element (C);
      else
         return V;
      end if;
   end Get_For_File;

   ------------------
   -- Set_For_File --
   ------------------

   procedure Set_For_File
     (Self : in out Breakpoint_Holder;
      File : Virtual_File;
      List : Breakpoint_Vectors.Vector)
   is
      C : Breakpoint_Hash_Maps.Cursor;
   begin
      if File = No_File then
         Self.Subprograms := List;
      else
         C := Self.Per_Files.Find (File);
         if Has_Element (C) then
            if List.Is_Empty then
               Self.Per_Files.Delete (C);
            else
               Self.Per_Files.Replace_Element (C, List);
            end if;

         elsif not List.Is_Empty then
            Self.Per_Files.Insert (File, List);
         end if;
      end if;
   end Set_For_File;

   -------------------------
   -- Get_For_Subprograms --
   -------------------------

   function Get_For_Subprograms
     (Self : in out Breakpoint_Holder)
      return Breakpoint_Vectors.Vector is
   begin
      return Self.Subprograms;
   end Get_For_Subprograms;

   -----------------
   -- Get_Pending --
   -----------------

   function Get_Pending
     (Self : Breakpoint_Holder)
      return Breakpoint_Vectors.Vector is
   begin
      return Self.Pending;
   end Get_Pending;

   --------------------------
   -- Get_Breakpoints_List --
   --------------------------

   function Get_Breakpoints_List
     (Self : Breakpoint_Holder)
      return Breakpoint_Vectors.Vector is
   begin
      return Self.All_Breakpoints;
   end Get_Breakpoints_List;

   -----------------------
   -- Synchronize_Lists --
   -----------------------

   procedure Synchronize_Lists (Self : in out Breakpoint_Holder) is
      File : Virtual_File;
      C    : Breakpoint_Hash_Maps.Cursor;
      V    : Breakpoint_Vectors.Vector;
   begin
      for Data of Self.All_Breakpoints loop
         if Data.Disposition /= Disable then
            if Data.Subprogram /= Null_Unbounded_String then
               Self.Subprograms.Append (Data);

            elsif Get_Location (Data) /= No_Marker then
               File := GPS.Editors.Get_File (Get_Location (Data));
               C := Self.Per_Files.Find (File);
               if Has_Element (C) then
                  V := Element (C);
                  V.Append (Data);
                  Self.Per_Files.Replace_Element (C, V);
               else
                  V.Append (Data);
                  Self.Per_Files.Insert (File, V);
               end if;
            end if;
         end if;
      end loop;
   end Synchronize_Lists;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Breakpoint_Holder) is
   begin
      Self.Pending.Clear;
      Self.Subprograms.Clear;
      Self.Per_Files.Clear;
      Self.All_Breakpoints.Clear;
   end Clear;

   -----------------
   -- Set_Enabled --
   -----------------

   procedure Set_Enabled
     (Self    : in out Breakpoint_Holder;
      Nums    : Breakpoint_Identifier_Lists.List;
      State   : Boolean;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      Data : Breakpoint_Data;
   begin
      if State then
         --  Disabled Bp is only in the common list, so mark them there as
         --  Changed and add them into resulting lists to send to
         --  the debugger.

         for Num of Nums loop
            for Index in
              Self.All_Breakpoints.First_Index ..
                Self.All_Breakpoints.Last_Index
            loop
               Data := Self.All_Breakpoints.Element (Index);
               if Data.Num = Num
                 and then Data.Disposition = Disable
               then
                  Data.Change_State := True;
                  Self.All_Breakpoints.Replace_Element (Index, Data);

                  if Data.Subprogram /= Null_Unbounded_String
                    or else Get_Location (Data) /= No_Marker
                  then
                     Self.Add_To_Changed (Data, Changed);
                  end if;
               end if;
            end loop;
         end loop;

      else
         --  Disable breakpoints, mark as Changed in the common list and
         --  delete from resulting lists for sending to the debugger
         for Num of Nums loop
            for Index in
              Self.All_Breakpoints.First_Index ..
                Self.All_Breakpoints.Last_Index
            loop
               Data := Self.All_Breakpoints.Element (Index);
               if Data.Num = Num
                 and then Data.Disposition /= Disable
               then
                  Data.Change_State := True;
                  Self.All_Breakpoints.Replace_Element (Index, Data);

                  if Data.Subprogram /= Null_Unbounded_String
                    or else Get_Location (Data) /= No_Marker
                  then
                     Self.Delete_From_Changed (Data, Changed);
                  end if;
               end if;
            end loop;
         end loop;
      end if;
   end Set_Enabled;

   ------------
   -- Actual --
   ------------

   procedure Actual
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Boolean)
   is
      use Breakpoint_Vectors;

      Old     : constant Vector := Self.Get_For_File (File);
      The_New : Vector;
      Data    : Breakpoint_Data;
      Cursor  : Breakpoint_Vectors.Cursor;
   begin
      --  Always changed because breakpoints Nums are changed
      Changed := True;

      --  Set disabled for deleted breakpoints in the flat list
      for Idx in Old.First_Index .. Old.Last_Index loop
         if not Actual.Contains (Old.Element (Idx)) then
            Cursor  := Self.All_Breakpoints.Find (Old.Element (Idx));
            Data    := Element (Cursor);

            Data.Disposition  := Disable;
            Data.Change_State := False;

            Self.All_Breakpoints.Replace_Element (Cursor, Data);
         end if;
      end loop;

      --  Update breakpoints information: nums, locations etc.
      for Idx in Actual.First_Index .. Actual.Last_Index loop
         Cursor  := Self.All_Breakpoints.Find (Actual.Element (Idx));
         Data    := Element (Cursor);
         Copy (Data, Actual.Element (Idx));

         Data.Disposition  := Keep;
         Data.Change_State := False;

         Self.All_Breakpoints.Replace_Element (Cursor, Data);
         The_New.Append (Data);
      end loop;

      Self.Set_For_File (File, The_New);
   end Actual;

   procedure Synch
     (Self   : in out Breakpoint_Holder;
      File   : Virtual_File;
      Actual : Breakpoint_Vectors.Vector)
   is
      use Breakpoint_Vectors;
      The_New : Vector;
      Data    : Breakpoint_Data;
      Cursor  : Breakpoint_Vectors.Cursor;
   begin
      --  Update breakpoints information: nums, locations etc.
      for Idx in Actual.First_Index .. Actual.Last_Index loop
         Cursor  := Self.All_Breakpoints.Find (Actual.Element (Idx));
         Data    := Element (Cursor);
         Copy (Data, Actual.Element (Idx));

         Self.All_Breakpoints.Replace_Element (Cursor, Data);
         The_New.Append (Data);
      end loop;

      Self.Set_For_File (File, The_New);
   end Synch;

   -----------------------
   -- Actual_Subprogram --
   -----------------------

   procedure Actual_Subprogram
     (Self    : in out Breakpoint_Holder;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Boolean)
   is
      use Breakpoint_Vectors;

      Old     : constant Vector := Self.Subprograms;
      The_New : Vector;
      Data    : Breakpoint_Data;
      Cursor  : Breakpoint_Vectors.Cursor;
      Index   : Integer;
   begin
      Changed := False;

      --  Set disabled for deleted breakpoints in the flat list
      for Idx in Old.First_Index .. Old.Last_Index loop
         Index := 0;
         Data  := Old.Element (Idx);
         for New_Data of Actual loop
            --  Actual does not have IDs set, so looking for data manually
            if New_Data = Data.Num then
               Index := 1;
            end if;
         end loop;

         if Index = 0 then
            Changed := True;
            Cursor  := Self.All_Breakpoints.Find (Old.Element (Idx));
            Data    := Element (Cursor);

            Data.Disposition  := Disable;
            Data.Change_State := False;

            Self.All_Breakpoints.Replace_Element (Cursor, Data);
         end if;
      end loop;

      --  Update enabled breakpoints information: nums, locations etc.
      for Idx in Actual.First_Index .. Actual.Last_Index loop
         Index := Old.First_Index;
         Data  := Actual.Element (Idx);
         while Index <= Old.Last_Index loop
            if Old.Element (Index) = Data.Num then
               exit;
            end if;
            Index := Index + 1;
         end loop;

         if Index <= Old.Last_Index then
            The_New.Append (Old.Element (Index));
         else
            Changed := True;
            Index   := Self.All_Breakpoints.First_Index;
            Data    := Actual.Element (Idx);
            while Index <= Self.All_Breakpoints.Last_Index loop
               if Self.All_Breakpoints.Element (Index) = Data.Num then
                  exit;
               end if;
               Index := Index + 1;
            end loop;

            if Index <= Self.All_Breakpoints.Last_Index then
               Data := Self.All_Breakpoints.Element (Index);
               Copy (Data, Actual.Element (Idx));

               Data.Disposition  := Keep;
               Data.Change_State := False;

               Self.All_Breakpoints.Replace_Element (Index, Data);
               The_New.Append (Data);
            end if;
         end if;
      end loop;

      Self.Subprograms := The_New;
   end Actual_Subprogram;

   -----------------
   -- Get_Next_Id --
   -----------------

   function Get_Next_Id
     (Self : in out Breakpoint_Persistent_Holder)
      return Breakpoint_Identifier is
   begin
      Self.Id := Self.Id + 1;
      return Self.Id;
   end Get_Next_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Breakpoint_Persistent_Holder;
      List : Breakpoint_Vectors.Vector) is
   begin
      Self.All_Breakpoints := List;
      Self.Reorder;
   end Initialize;

   -------------
   -- Reorder --
   -------------

   procedure Reorder (Self : in out Breakpoint_Persistent_Holder)
   is
      D   : Breakpoint_Data;
      Idx : Natural := Self.All_Breakpoints.First_Index;
   begin
      Self.Id := 0;

      while Idx <= Self.All_Breakpoints.Last_Index loop
         D := Self.All_Breakpoints.Element (Idx);

         Self.Id := Self.Id + 1;
         D.Id    := Self.Id;
         D.Num   := Self.Id;

         Self.All_Breakpoints.Replace_Element (Idx, D);
         Idx := Idx + 1;
      end loop;
   end Reorder;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self       : in out Breakpoint_Persistent_Holder;
      Executable : Virtual_File;
      List       : Breakpoint_Vectors.Vector)
   is
      Idx  : Natural := Self.All_Breakpoints.First_Index;
      C    : Breakpoint_Vectors.Cursor;
      Name : constant String := +Base_Name (Executable);
   begin
      while Idx <= Self.All_Breakpoints.Last_Index loop
         if Self.All_Breakpoints.Element (Idx).Executable = Name
           and then not List.Contains (Self.All_Breakpoints.Element (Idx))
         then
            --  no more exist
            Self.All_Breakpoints.Delete (Idx);
         else
            Idx := Idx + 1;
         end if;
      end loop;

      for Data of List loop
         C := Self.All_Breakpoints.Find (Data);
         if Breakpoint_Vectors.Has_Element (C) then
            Self.All_Breakpoints.Replace_Element (C, Data);
         else
            Self.All_Breakpoints.Append (Data);
         end if;
      end loop;
   end Replace;

   -----------
   -- Added --
   -----------

   procedure Added
     (Self : in out Breakpoint_Persistent_Holder;
      Data : Breakpoint_Data)
   is
      D : Breakpoint_Data := Data;
   begin
      if D.Id = 0 then
         D.Id := Self.Get_Next_Id;
         D.Num := D.Id;
      end if;

      Self.All_Breakpoints.Append (D);
   end Added;

   -------------
   -- Deleted --
   -------------

   procedure Deleted
     (Self : in out Breakpoint_Persistent_Holder;
      File : Virtual_File;
      Line : Editable_Line_Type)
   is
      Index : Integer := Self.All_Breakpoints.First_Index;
      Data  : Breakpoint_Data;
   begin
      while Index <= Self.All_Breakpoints.Last_Index loop
         Data := Self.All_Breakpoints.Element (Index);

         for L of Data.Locations loop
            if GPS.Editors.Get_Line (L.Location) = Line
              and then GPS.Editors.Get_File (L.Location) = File
            then
               Self.All_Breakpoints.Delete (Index);
               Index := Index - 1;
               exit;
            end if;
         end loop;
         Index := Index + 1;
      end loop;
   end Deleted;

   -------------
   -- Deleted --
   -------------

   procedure Deleted
     (Self : in out Breakpoint_Persistent_Holder;
      Nums : Breakpoint_Identifier_Lists.List)
   is
      Index : Integer;
      Data  : Breakpoint_Data;
   begin
      for Num of Nums loop
         Index := Self.All_Breakpoints.First_Index;
         while Index <= Self.All_Breakpoints.Last_Index loop
            Data := Self.All_Breakpoints.Element (Index);

            if Data.Num = Num then
               Self.All_Breakpoints.Delete (Index);
            else
               Index := Index + 1;
            end if;
         end loop;
      end loop;
   end Deleted;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Breakpoint_Persistent_Holder) is
   begin
      Self.All_Breakpoints.Clear;
      Self.Id := 0;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self     : in out Breakpoint_Persistent_Holder;
      Location : Location_Marker)
      return Boolean is
   begin
      for Index in
        Self.All_Breakpoints.First_Index .. Self.All_Breakpoints.Last_Index
      loop
         for L of Self.All_Breakpoints.Element (Index).Locations loop
            if L.Location = Location then
               return True;
            end if;
         end loop;
      end loop;

      return False;
   end Contains;

   -----------------
   -- Set_Enabled --
   -----------------

   procedure Set_Enabled
     (Self  : in out Breakpoint_Persistent_Holder;
      Ids   : Breakpoint_Identifier_Lists.List;
      State : Boolean)
   is
      Data : Breakpoint_Data;
   begin
      for Id of Ids loop
         for Index in
           Self.All_Breakpoints.First_Index ..
             Self.All_Breakpoints.Last_Index
         loop
            Data := Self.All_Breakpoints.Element (Index);
            if Data.Id = Id then
               if State then
                  Data.Disposition := Keep;
               else
                  Data.Disposition := Disable;
               end if;
               Self.All_Breakpoints.Replace_Element (Index, Data);

               exit;
            end if;
         end loop;
      end loop;
   end Set_Enabled;

   --------------------------
   -- Get_Breakpoints_List --
   --------------------------

   function Get_Breakpoints_List
     (Self : Breakpoint_Persistent_Holder)
      return Breakpoint_Vectors.Vector is
   begin
      return Self.All_Breakpoints;
   end Get_Breakpoints_List;

   --------------------------
   -- Get_Breakpoints_List --
   --------------------------

   function Get_Breakpoints_List
     (Self       : Breakpoint_Persistent_Holder;
      Executable : Virtual_File)
      return Breakpoint_Vectors.Vector
   is
      Result : Breakpoint_Vectors.Vector;
      Name   : constant String := +Base_Name (Executable);
   begin
      for Data of Self.All_Breakpoints loop
         if Data.Executable = Null_Unbounded_String
           or else Data.Executable = Name
         then
            Result.Append (Data);
         end if;
      end loop;
      return Result;
   end Get_Breakpoints_List;

end DAP.Breakpoint_Maps;
