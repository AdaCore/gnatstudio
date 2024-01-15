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

with DAP.Modules.Preferences;

package body DAP.Modules.Breakpoints is

   function Is_Same_Location
     (Data : Breakpoint_Data;
      File : Virtual_File;
      Line : Editable_Line_Type)
      return Boolean;

   function Is_Same_Location
     (Data   : Breakpoint_Data;
      Marker : Location_Marker)
      return Boolean;

   function Is_Duplicate (L, R : Breakpoint_Data) return Boolean;

   function Get_Location_File (Data : Breakpoint_Data) return Virtual_File;

   -------
   -- = --
   -------

   function "="
     (Data : Breakpoint_Data;
      Num  : Breakpoint_Identifier)
      return Boolean is
   begin
      if Data.Num = Num then
         return True;

      else
         for Loc of Data.Locations loop
            if Loc.Num = Num then
               return True;
            end if;
         end loop;
      end if;

      return False;
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
      for Loc of Data.Locations loop
         if GPS.Editors.Get_Line (Loc.Marker) = Line
           and then GPS.Editors.Get_File (Loc.Marker) = File
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Same_Location;

   ----------------------
   -- Is_Same_Location --
   ----------------------

   function Is_Same_Location
     (Data   : Breakpoint_Data;
      Marker : Location_Marker)
      return Boolean is
   begin
      for Loc of Data.Locations loop
         if Similar (Loc.Marker, Marker) then
            return True;
         end if;
      end loop;

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

         when On_Address =>
            return L.Address = R.Address;

         when On_Exception =>
            return L.Except = R.Except;
      end case;
   end Is_Duplicate;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Data : Breakpoint_Data) return Location_Marker is
   begin
      if Data.Locations.Is_Empty then
         return No_Marker;
      else
         return Data.Locations.First_Element.Marker;
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

         when On_Subprogram =>
            return Subprograms_File;

         when On_Address =>
            return Addreses_File;

         when On_Exception =>
            return Exceptions_File;
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
            if not Data.Locations.Is_Empty then
               return Get_Location_File (Data).Display_Base_Name
                 & ":"
                 & GNATCOLL.Utils.Image
                 (Integer (GPS.Editors.Get_Line
                  (Data.Locations.First_Element.Marker)),
                  Min_Width => 0);
            else
               return "(no location)";
            end if;

         when On_Subprogram =>
            return To_String (Data.Subprogram);

         when On_Address =>
            return Address_To_String (Data.Address);

         when On_Exception =>
            return "exception " & To_String (Data.Except);
      end case;
   end To_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : in out Breakpoint_Holder;
      Vector      : Breakpoint_Vectors.Vector;
      Clear_Ids   : Boolean := False;
      Initialized : Boolean := False)
   is
      Id : Breakpoint_Identifier := 0;
   begin
      Self.Vector.Clear;

      for Item of Vector loop
         declare
            Data : Breakpoint_Data := Item;
         begin
            if Clear_Ids then
               if Data.State = Enabled then
                  Data.Num := 0;
               else
                  Id := Id - 1;
                  Data.Num := Id;
               end if;

               if Data.Kind = On_Line then
                  for Loc of Data.Locations loop
                     Loc.Num := 0;
                  end loop;
               else
                  Data.Locations.Clear;
               end if;
            end if;
            Self.Vector.Append (Data);
         end;
      end loop;

      if not Initialized then
         Self.In_Initialization := True;
      end if;
   end Initialize;

   -----------------
   -- Initialized --
   -----------------

   procedure Initialized (Self : in out Breakpoint_Holder) is
   begin
      Self.In_Initialization := False;
   end Initialized;

   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self : Breakpoint_Holder)
      return Breakpoint_Vectors.Vector is
   begin
      return Self.Vector;
   end Get_Breakpoints;

   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self       : Breakpoint_Holder;
      Executable : Virtual_File)
      return Breakpoint_Vectors.Vector
   is
      Result : Breakpoint_Vectors.Vector;
   begin
      for Data of Self.Vector loop
         if Data.Executable = No_File
           or else Data.Executable = Executable
         then
            Result.Append (Data);
         end if;
      end loop;

      return Result;
   end Get_Breakpoints;

   -----------------
   -- Get_Next_Id --
   -----------------

   function Get_Next_Id
     (Self : in out Breakpoint_Holder)
      return Breakpoint_Identifier is
   begin
      Self.Id := Self.Id + 1;
      return Self.Id;
   end Get_Next_Id;

   -----------
   -- Added --
   -----------

   procedure Added
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data)
   is
      D : Breakpoint_Data := Data;
   begin
      if Self.In_Initialization then
         return;
      end if;

      if D.Num = 0 then
         D.Num := Self.Get_Next_Id;
      end if;

      Self.Vector.Append (D);
   end Added;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self    : in out Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Updated : out Triboolean)
   is
      use Breakpoint_Vectors;
      C   : constant Breakpoint_Vectors.Cursor := Self.Vector.Find (Data);
      Old : Breakpoint_Data;
   begin
      if C /= Breakpoint_Vectors.No_Element then

         Old := Element (C);
         if Old.Ignore /= Data.Ignore
           or else Old.Condition /= Data.Condition
         then
            Updated := True;

         elsif Old.Commands /= Data.Commands then
            Updated := Indeterminate;

         else
            Updated := False;
         end if;

         Self.Vector.Replace_Element (C, Data);
      else
         Updated := True;
         Self.Vector.Append (Data);
      end if;
   end Replace;

   -------------
   -- Deleted --
   -------------

   procedure Deleted
     (Self : in out Breakpoint_Holder;
      File : Virtual_File;
      Line : Editable_Line_Type)
   is
      Index : Integer := Self.Vector.First_Index;
   begin
      while Index <= Self.Vector.Last_Index loop
         if Is_Same_Location (Self.Vector (Index), File, Line) then
            Self.Vector.Delete (Index);
         else
            Index := Index + 1;
         end if;
      end loop;
   end Deleted;

   -------------
   -- Deleted --
   -------------

   procedure Deleted
     (Self : in out Breakpoint_Holder;
      Nums : Breakpoint_Identifier_Lists.List)
   is
      Dummy : Boolean;
   begin
      for Num of Nums loop
         Self.Deleted (Num, Dummy);
      end loop;
   end Deleted;

   -------------
   -- Deleted --
   -------------

   procedure Deleted
     (Self    : in out Breakpoint_Holder;
      Num     : Breakpoint_Identifier;
      Changed : out Boolean)
   is
      Index : Integer := Self.Vector.First_Index;
   begin
      Changed := False;
      while Index <= Self.Vector.Last_Index loop
         declare
            Data : Breakpoint_Data := Self.Vector.Element (Index);
         begin
            if Data = Num then
               if Data.State = Changing then
                  Data.State := Disabled;
                  Self.Vector.Replace_Element (Index, Data);
                  Changed := True;
               else
                  Self.Vector.Delete (Index);
               end if;
               return;

            else
               Index := Index + 1;
            end if;
         end;
      end loop;
   end Deleted;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Breakpoint_Holder) is
   begin
      Self.Vector.Clear;
      Self.Id := 0;
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

   -----------------
   -- Set_Enabled --
   -----------------

   procedure Set_Enabled
     (Self  : in out Breakpoint_Holder;
      Ids   : Breakpoint_Identifier_Lists.List;
      State : Boolean) is
   begin
      for Id of Ids loop
         for Data of Self.Vector loop
            if Data = Id then
               if State then
                  Data.State := Enabled;
               else
                  Data.State := Disabled;
               end if;
               exit;
            end if;
         end loop;
      end loop;
   end Set_Enabled;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self       : in out Breakpoint_Holder;
      Executable : Virtual_File;
      List       : Breakpoint_Vectors.Vector)
   is
      Idx : Natural := Self.Vector.First_Index;
      C   : Breakpoint_Vectors.Cursor;
   begin
      while Idx <= Self.Vector.Last_Index loop
         if Self.Vector (Idx).Executable = Executable
           and then not List.Contains (Self.Vector (Idx))
         then
            --  no more exist
            Self.Vector.Delete (Idx);
         else
            Idx := Idx + 1;
         end if;
      end loop;

      for Data of List loop
         C := Self.Vector.Find (Data);
         if Breakpoint_Vectors.Has_Element (C) then
            Self.Vector.Replace_Element (C, Data);
         else
            Self.Vector.Append (Data);
         end if;
      end loop;
   end Replace;

   -----------------
   -- Set_Numbers --
   -----------------

   procedure Set_Numbers (Self : in out Breakpoint_Holder) is
   begin
      Self.Id := 0;
      for Data of Self.Vector loop
         Self.Id  := Self.Id + 1;
         Data.Num := Self.Id;
      end loop;
   end Set_Numbers;

   -------------------
   -- Get_For_Files --
   -------------------

   function Get_For_Files
     (Self : Breakpoint_Holder)
      return Breakpoint_Hash_Maps.Map
   is
      Result : Breakpoint_Hash_Maps.Map;
      File   : Virtual_File;
      Vector : Breakpoint_Vectors.Vector;
   begin
      for Data of Self.Vector loop
         if Data.State = Enabled
           and then Data.Kind = On_Line
         then
            File := Get_Location_File (Data);

            if Result.Contains (File) then
               Vector := Result.Element (File);
               Vector.Append (Data);
               Result.Replace (File, Vector);

            else
               Result.Insert (File, Breakpoint_Vectors.To_Vector (Data, 1));
            end if;
         end if;
      end loop;

      return Result;
   end Get_For_Files;

   ------------------
   -- Get_For_File --
   ------------------

   function Get_For_File
     (Self          : Breakpoint_Holder;
      File          : Virtual_File;
      With_Changing : Boolean := False)
      return Breakpoint_Vectors.Vector
   is
      Result : Breakpoint_Vectors.Vector;
   begin
      for Data of Self.Vector loop
         if (Data.State = Enabled
             or else (With_Changing and then Data.State = Changing))
           and then Get_Location_File (Data) = File
         then
            Result.Append (Data);
         end if;
      end loop;

      return Result;
   end Get_For_File;

   -------------
   -- Get_For --
   -------------

   function Get_For
     (Self          : Breakpoint_Holder;
      Kind          : Breakpoint_Kind;
      With_Changing : Boolean := False)
      return Breakpoint_Vectors.Vector
   is
      Result : Breakpoint_Vectors.Vector;
   begin
      for Data of Self.Vector loop
         if (Data.State = Enabled
             or else (With_Changing and then Data.State = Changing))
           and then Data.Kind = Kind
         then
            Result.Append (Data);
         end if;
      end loop;

      return Result;
   end Get_For;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self    : Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : out Breakpoint_Vectors.Vector) is
   begin
      Changed := Self.Get_For_File (Get_Location_File (Data));
      for D of Changed loop
         if Is_Duplicate (D, Data) then
            Changed.Clear;
            return;
         end if;
      end loop;

      Changed.Append (Data);
   end Add;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data)
   is
      Local : Breakpoint_Data := Data;
   begin
      if Local.Num = 0 then
         Local.Num := Self.Get_Next_Id;
      end if;

      for D of Self.Get_For_File (Get_Location_File (Data)) loop
         if Is_Duplicate (D, Local) then
            return;
         end if;
      end loop;

      Self.Vector.Append (Local);
   end Append;

   -----------------
   -- Set_Enabled --
   -----------------

   procedure Set_Enabled
     (Self    : in out Breakpoint_Holder;
      Nums    : Breakpoint_Identifier_Lists.List;
      State   : Boolean;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      Cursor : Breakpoint_Hash_Maps.Cursor;
      File   : Virtual_File;

   begin
      for Num of Nums loop
         for Data of Self.Vector loop
            if Data = Num then
               if State then
                  if Data.State /= Enabled then
                     Changed.Include
                       (Get_Location_File (Data),
                        Breakpoint_Vectors.Empty_Vector);
                     Data.State := Changing;
                  end if;

               else
                  if Data.State = Enabled then
                     Changed.Include
                       (Get_Location_File (Data),
                        Breakpoint_Vectors.Empty_Vector);
                     Data.State := Changing;
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         File := Key (Cursor);
         Changed.Replace_Element
           (Cursor, Self.Get_For_File (File, State));
         Next (Cursor);
      end loop;
   end Set_Enabled;

   ----------------------
   -- Set_Ignore_Count --
   ----------------------

   procedure Set_Ignore_Count
     (Self    : in out Breakpoint_Holder;
      Id      : Breakpoint_Identifier;
      Count   : Natural;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      File : Virtual_File := No_File;

   begin
      for Data of Self.Vector loop
         if Data = Id then
            Data.Ignore := Count;
            File := Get_Location_File (Data);
            exit;
         end if;
      end loop;

      if File /= No_File then
         Changed.Insert (File, Self.Get_For_File (File, False));
      end if;
   end Set_Ignore_Count;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Line    : Editable_Line_Type;
      Changed : out Breakpoint_Hash_Maps.Map;
      Updated : out Boolean)
   is
      Idx    : Integer := Self.Vector.First_Index;
      Cursor : Breakpoint_Hash_Maps.Cursor;
      F      : Virtual_File;
      List   : Breakpoint_Vectors.Vector;
   begin
      Updated := False;
      while Idx <= Self.Vector.Last_Index loop
         declare
            Data : constant Breakpoint_Data := Self.Vector.Element (Idx);
         begin
            if Is_Same_Location (Data, File, Line) then
               if Data.State = Enabled then
                  Changed.Include
                    (Get_Location_File (Data),
                     Breakpoint_Vectors.Empty_Vector);
               else
                  --  not in the debugger, just delete
                  Updated := True;
                  Self.Vector.Delete (Idx);
                  Idx := Idx - 1;
               end if;
            end if;
         end;
         Idx := Idx + 1;
      end loop;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         F    := Key (Cursor);
         List := Self.Get_For_File (F);

         Idx := List.First_Index;
         while Idx <= List.Last_Index loop
            declare
               Data : constant Breakpoint_Data := List.Element (Idx);
            begin
               if Is_Same_Location (Data, File, Line) then
                  List.Delete (Idx);
               else
                  Idx := Idx + 1;
               end if;
            end;
         end loop;

         Changed.Replace_Element (Cursor, List);
         Next (Cursor);
      end loop;
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      Nums    : DAP.Types.Breakpoint_Identifier_Lists.List;
      Changed : out Breakpoint_Hash_Maps.Map;
      Updated : out Boolean)
   is
      Idx    : Integer;
      Cursor : Breakpoint_Hash_Maps.Cursor;
      F      : Virtual_File;
      List   : Breakpoint_Vectors.Vector;
   begin
      Updated := False;
      for Num of Nums loop
         Inner : for Idx in Self.Vector.First_Index ..
           Self.Vector.Last_Index
         loop
            declare
               Data : constant Breakpoint_Data := Self.Vector.Element (Idx);
            begin
               if Data = Num then
                  if Data.State = Enabled then
                     Changed.Include
                       (Get_Location_File (Data),
                        Breakpoint_Vectors.Empty_Vector);
                  else
                     --  not in the debugger, just delete
                     Updated := True;
                     Self.Vector.Delete (Idx);
                  end if;
                  exit Inner;
               end if;
            end;
         end loop Inner;
      end loop;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         F    := Key (Cursor);
         List := Self.Get_For_File (F);

         Idx := List.First_Index;
         while Idx <= List.Last_Index loop
            if Nums.Contains (List (Idx).Num) then
               List.Delete (Idx);
            else
               Idx := Idx + 1;
            end if;
         end loop;

         Changed.Replace_Element (Cursor, List);
         Next (Cursor);
      end loop;
   end Delete;

   --------------------------
   -- Initialized_For_File --
   --------------------------

   procedure Initialized_For_File
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      Idx_New : Integer := 1;
      Data, D : Breakpoint_Data;
      Dummy   : Breakpoint_Vectors.Vector;

   begin
      for Idx in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         exit when Idx_New > Actual.Last_Index;

         Data := Self.Vector.Element (Idx);
         if Data.State = Enabled
           and then Data.Kind = On_Line
           and then Get_Location_File (Data) = File
         then
            D := Actual.Element (Idx_New);
            if not Is_Same_Location
              (Data, D.Locations.First_Element.Marker)
            then
               D.State := Moved;
            end if;
            Self.Vector.Replace_Element (Idx, D);
            Idx_New := Idx_New + 1;
         end if;
      end loop;

      --  Delete duplicates
      Self.Delete_On_Line_Duplicates (File, Changed, Dummy);
   end Initialized_For_File;

   ---------------------------------
   -- Initialized_For_Subprograms --
   ---------------------------------

   procedure Initialized_For_Subprograms
     (Self   : in out Breakpoint_Holder;
      Actual : Breakpoint_Vectors.Vector)
   is
      Idx : Integer := Actual.First_Index;
   begin
      for Data of Self.Vector loop
         if Data.State = Enabled
           and then Data.Kind = On_Subprogram
         then
            if Data.Num = 0 then
               Data.Verified := Actual (Idx).Verified
                 and then Actual (Idx).Locations.Is_Empty;

               if Data.Verified then
                  Data.Num := Actual (Idx).Locations.First_Element.Num;
                  while Idx <= Actual.Last_Index loop
                     Data.Locations.Append
                       (Actual (Idx).Locations.First_Element);
                     Idx := Idx + 1;
                  end loop;

               else
                  Data.Num := Actual (Idx).Num;
               end if;

               exit;

            else
               Idx := Idx + Data.Locations.Last_Index;
            end if;
         end if;
      end loop;
   end Initialized_For_Subprograms;

   -------------------------------------------
   -- Delete_Unused_Subprograms_Breakpoints --
   -------------------------------------------

   procedure Delete_Unused_Subprograms_Breakpoints
     (Self    : in out Breakpoint_Holder;
      Changed : out Boolean)
   is
      Index : Integer;
      Data  : Breakpoint_Data;
   begin
      Self.Delete_Fake_Subprogram;

      Changed := False;
      Index   := Self.Vector.First_Index;

      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);
         if Data.Kind = On_Subprogram
           and then not Data.Verified
           and then not DAP.Modules.Preferences.Pending_Breakpoints.Get_Pref
         then
            Self.Vector.Delete (Index);
            Changed := True;
         else
            Index := Index + 1;
         end if;
      end loop;
   end Delete_Unused_Subprograms_Breakpoints;

   -------------------------
   -- Done_For_Exceptions --
   -------------------------

   procedure Done_For_Exceptions
     (Self    : in out Breakpoint_Holder;
      Actual  : Breakpoint_Vectors.Vector;
      Deleted : out Boolean)
   is
      Idx  : Integer := Actual.First_Index;
      Nums : Breakpoint_Identifier_Lists.List;
      Data : Breakpoint_Data;
   begin
      Deleted := False;

      for Data of Self.Vector loop
         if Data.State = Enabled
           and then Data.Kind = On_Exception
         then
            Data := Actual (Idx);
            Nums.Append (Data.Num);
            Idx  := Idx + 1;
         end if;
      end loop;

      --  Delete fake breakpoints from the notifications
      Idx := Self.Vector.First_Index;
      while Idx <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Idx);
         if Data.State = Enabled
           and then Data.Kind = On_Line
           and then Nums.Contains (Data.Num)
         then
            Self.Vector.Delete (Idx);
         else
            Idx := Idx + 1;
         end if;
      end loop;

      if not DAP.Modules.Preferences.Pending_Breakpoints.Get_Pref then
         --  Delete pending exception breakpoints
         Idx := Self.Vector.First_Index;
         while Idx <= Self.Vector.Last_Index loop
            Data := Self.Vector.Element (Idx);
            if Data.State = Enabled
              and then Data.Kind = On_Exception
              and then not Data.Verified
            then
               Deleted := True;
               Self.Vector.Delete (Idx);
            else
               Idx := Idx + 1;
            end if;
         end loop;
      end if;
   end Done_For_Exceptions;

   -------------------------------
   -- Delete_On_Line_Duplicates --
   -------------------------------

   procedure Delete_On_Line_Duplicates
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Changed : out Breakpoint_Hash_Maps.Map;
      Enabled : in out Breakpoint_Vectors.Vector)
   is
      Index   : Integer := Self.Vector.First_Index;
      Data    : Breakpoint_Data;
      Deleted : Boolean;
      Update  : Boolean := False;
      Lines   : Line_Sets.Set;
      Line    : Editable_Line_Type;
   begin
      Changed := Breakpoint_Hash_Maps.Empty_Map;

      while Index <= Self.Vector.Last_Index loop
         Data    := Self.Vector.Element (Index);
         Deleted := False;
         if Data.Kind = On_Line
           and then Get_Location_File (Data) = File
         then
            if Data.State = Moved then
               for D of Self.Vector loop
                  if D.Num /= Data.Num
                    and then Data.Kind = On_Line
                    and then Data.State /= Disabled
                    and then Is_Same_Location
                      (Data, D.Locations.First_Element.Marker)
                  then
                     Self.Vector.Delete (Index);
                     Index   := Index - 1;
                     Deleted := True;
                     Update  := True;
                     exit;
                  end if;
               end loop;

               if not Deleted then
                  Data.State := DAP.Modules.Breakpoints.Enabled;
                  Enabled.Append (Data);
                  Self.Vector.Replace_Element (Index, Data);
                  Index := Index + 1;
               end if;

            elsif not Data.Verified
              and then not DAP.Modules.Preferences.Pending_Breakpoints.Get_Pref
            then
               --  Delete pending breakpoint
               Self.Vector.Delete (Index);
               Index   := Index - 1;
               Deleted := True;
               Update  := True;
            end if;
         end if;
         Index := Index + 1;
      end loop;

      Index := Self.Vector.First_Index;
      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);
         if Data.State = DAP.Modules.Breakpoints.Enabled
           and then Data.Kind = On_Line
           and then Get_Location_File (Data) = File
         then
            Line := GPS.Editors.Get_Line (Get_Location (Data));
            if Lines.Contains (Line) then
               Update := True;
               Self.Vector.Delete (Index);
               Index := Index - 1;
            else
               Lines.Include (Line);
            end if;
         end if;
         Index := Index + 1;
      end loop;

      if Update then
         --  Prepare a new list when duplicates are deleted
         Changed.Insert (File, Self.Get_For_File (File));
      end if;
   end Delete_On_Line_Duplicates;

   ----------------------------
   -- Delete_Fake_Subprogram --
   ----------------------------

   procedure Delete_Fake_Subprogram (Self : in out Breakpoint_Holder) is
      Nums  : Breakpoint_Identifier_Lists.List;
      Index : Integer;
   begin
      for Data of Self.Vector loop
         if Data.State = Enabled
           and then Data.Kind = On_Subprogram
         then
            if Data.Locations.Is_Empty then
               Nums.Append (Data.Num);
            else
               for Loc of Data.Locations loop
                  Nums.Append (Loc.Num);
               end loop;
            end if;
         end if;
      end loop;

      Index := Self.Vector.First_Index;
      while Index <= Self.Vector.Last_Index loop
         declare
            Data : constant Breakpoint_Data := Self.Vector.Element (Index);
         begin
            if Data.State = Enabled
              and then Data.Kind /= On_Subprogram
              and then Nums.Contains (Data.Num)
            then
               Self.Vector.Delete (Index);
            else
               Index := Index + 1;
            end if;
         end;
      end loop;
   end Delete_Fake_Subprogram;

   -----------
   -- Added --
   -----------

   procedure Added
     (Self    : in out Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : out Breakpoint_Vectors.Vector;
      Check   : Boolean;
      Update  : out Boolean)
   is
      D      : Breakpoint_Data;
      Delete : Boolean := False;
      Index  : Integer := 0;
   begin
      Changed := Breakpoint_Vectors.Empty_Vector;
      Update  := False;

      --  Update already added by notification
      for Idx in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         D := Self.Vector.Element (Idx);
         if D = Data.Num then
            Self.Vector.Replace_Element (Idx, Data);
            Index := Idx;
            exit;
         end if;
      end loop;

      --  Add new BP if not found (not added via notifications)
      if Index = 0 then
         Self.Vector.Append (Data);
         Index := Self.Vector.Last_Index;
      end if;

      if not Data.Verified
        and then not DAP.Modules.Preferences.Pending_Breakpoints.Get_Pref
      then
         --  Pending bp that should be removed
         Delete := True;

      elsif Check then
         --  Check for duplicates
         for D of Self.Vector loop
            if D.State = Enabled
              and then D.Num /= Data.Num
              and then Is_Duplicate (D, Data)
            then
               Delete := True;
               exit;
            end if;
         end loop;
      end if;

      if Delete then
         --  Just added breakpoint pending or duplicates another, so delete it
         Self.Vector.Delete (Index);
         Changed := Self.Get_For_File (Get_Location_File (Data));
         Update := True;
      end if;
   end Added;

   ----------------------
   -- Added_Subprogram --
   ----------------------

   procedure Added_Subprogram
     (Self   : in out Breakpoint_Holder;
      Data   : Breakpoint_Data;
      Actual : Breakpoint_Vectors.Vector;
      Num    : out Breakpoint_Identifier)
   is
      Local : Breakpoint_Data := Data;
      Idx   : Integer := Actual.First_Index;
      Nums  : Breakpoint_Identifier_Lists.List;
   begin
      Num := 0;

      --  calculate the position where "sub_breakpoints" for the added one
      --  start
      for D of Self.Vector loop
         if D.State = Enabled
           and then D.Kind = On_Subprogram
         then
            Idx := Idx + D.Locations.Last_Index;
         end if;
      end loop;

      --  collect all "sub_breakpoints" created for subprogram name
      while Idx <= Actual.Last_Index loop
         if Actual (Idx).Verified then
            Local.Locations.Append (Actual (Idx).Locations.First_Element);
         end if;
         Nums.Append (Actual (Idx).Locations.First_Element.Num);
         Idx := Idx + 1;
      end loop;
      Local.Verified := not Local.Locations.Is_Empty;

      if Local.Verified
        or else DAP.Modules.Preferences.Pending_Breakpoints.Get_Pref
      then
         --  add confirmed or when pending should be preserved
         Local.Num := Local.Locations.First_Element.Num;
         Num := Local.Num;
         Self.Vector.Append (Local);
      end if;

      --  delete fake Bp from notification added as a line bp
      Idx := Self.Vector.First_Index;
      while Idx <= Self.Vector.Last_Index loop
         if Self.Vector (Idx).Kind = On_Line
           and then Nums.Contains (Self.Vector (Idx).Num)
         then
            Self.Vector.Delete (Idx);
         else
            Idx := Idx + 1;
         end if;
      end loop;
   end Added_Subprogram;

   --------------------------
   -- Lines_Status_Changed --
   --------------------------

   procedure Lines_Status_Changed
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map;
      Enabled : out Breakpoint_Vectors.Vector;
      Id      : out Integer)
   is
      Index   : Integer := Self.Vector.First_Index;
      Idx     : Integer := Actual.First_Index;
      Data, D : Breakpoint_Data;
      Nums    : Breakpoint_Identifier_Lists.List;
   begin
      Id := 0;
      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);

         if Data.Kind = On_Line
           and then Get_Location_File (Data) = File
         then
            case Data.State is
               when DAP.Modules.Breakpoints.Enabled =>
                  if Nums.Contains (Data.Num) then
                     --  delete fake Bp from notification
                     Self.Vector.Delete (Index);
                     Index := Index - 1;
                  else
                     Idx := Idx + 1;
                  end if;

               when Changing =>
                  D := Actual.Element (Idx);
                  if Is_Same_Location
                    (Data, D.Locations.First_Element.Marker)
                  then
                     D.State := DAP.Modules.Breakpoints.Enabled;
                     Enabled.Append (D);
                  else
                     D.State := Moved;
                  end if;

                  Nums.Append (D.Num);
                  Self.Vector.Replace_Element (Index, D);
                  if Id = 0 then
                     Id := Integer (D.Num);
                  else
                     Id := -1;
                  end if;
                  Idx := Idx + 1;

               when Disabled | Moved =>
                  null;
            end case;
         end if;
         Index := Index + 1;
      end loop;

      Self.Delete_On_Line_Duplicates (File, Changed, Enabled);
   end Lines_Status_Changed;

   -------------------------------
   -- Subprogram_Status_Changed --
   -------------------------------

   procedure Subprogram_Status_Changed
     (Self    : in out Breakpoint_Holder;
      Actual  : Breakpoint_Vectors.Vector;
      Enabled : out Breakpoint_Vectors.Vector)
   is
      Index : Integer := Self.Vector.First_Index;
      Idx   : Integer := Actual.First_Index;
      Data  : Breakpoint_Data;

   begin
      while Index <= Self.Vector.Last_Index
        and then Idx <= Actual.Last_Index
      loop
         Data := Self.Vector.Element (Index);
         if Data.Kind = On_Subprogram then
            case Data.State is
               when DAP.Modules.Breakpoints.Enabled =>
                  Idx := Idx + Data.Locations.Last_Index;

               when Changing =>
                  Data.Locations.Clear;
                  while Idx <= Actual.Last_Index loop
                     if Actual (Idx).Verified then
                        Data.Locations.Append
                          (Actual (Idx).Locations.First_Element);
                     end if;
                     Idx := Idx + 1;
                  end loop;

                  Data.Num      := Data.Locations.First_Element.Num;
                  Data.State    := DAP.Modules.Breakpoints.Enabled;
                  Data.Verified := not Data.Locations.Is_Empty;

                  if not Data.Locations.Is_Empty
                    or else DAP.Modules.Preferences.
                      Pending_Breakpoints.Get_Pref
                  then
                     Enabled.Append (Data);
                  end if;
                  Self.Vector.Replace_Element (Index, Data);

                  exit;

               when Disabled | Moved =>
                  null;
            end case;
         end if;
         Index := Index + 1;
      end loop;
   end Subprogram_Status_Changed;

   --------------------------------------
   -- Address_Exception_Status_Changed --
   --------------------------------------

   procedure Address_Exception_Status_Changed
     (Self    : in out Breakpoint_Holder;
      Kind    : Breakpoint_Kind;
      Actual  : Breakpoint_Vectors.Vector;
      Enabled : out Breakpoint_Vectors.Vector;
      Deleted : out Boolean)
   is
      Index   : Integer := Self.Vector.First_Index;
      Idx     : Integer := Actual.First_Index;
      Data, D : Breakpoint_Data;
      Nums    : Breakpoint_Identifier_Lists.List;
   begin
      Deleted := False;

      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);

         if Data.Kind = Kind then
            case Data.State is
               when DAP.Modules.Breakpoints.Enabled =>
                  if Nums.Contains (Data.Num) then
                     --  delete fake Bp from notification. Nums is filled with
                     --  Enabled Ids we already processed from the Actual, so
                     --  this one is from the notifications
                     Self.Vector.Delete (Index);
                     Index := Index - 1;
                  else
                     Idx := Idx + 1;
                  end if;

               when Changing =>
                  D := Actual.Element (Idx);
                  Nums.Append (D.Num);
                  Data.Num := D.Num;
                  Data.Locations := D.Locations;
                  Data.State := DAP.Modules.Breakpoints.Enabled;
                  if Data.Verified
                    or else DAP.Modules.Preferences.
                      Pending_Breakpoints.Get_Pref
                  then
                     Self.Vector.Replace_Element (Index, Data);
                     Enabled.Append (Data);
                  else
                     Deleted := True;
                     Index   := Index - 1;
                     Self.Vector.Delete (Index);
                  end if;
                  Idx := Idx + 1;

               when Disabled | Moved =>
                  null;
            end case;
         end if;
         Index := Index + 1;
      end loop;
   end Address_Exception_Status_Changed;

   -------------
   -- Changed --
   -------------

   procedure Changed
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data) is
   begin
      for Old_Data of Self.Vector loop
         for Loc of Old_Data.Locations loop
            if Loc.Num = Data.Num then
               Loc := Data.Locations.First_Element;
               return;
            end if;
         end loop;
      end loop;
   end Changed;

   ---------------------------
   -- Break_Unbreak_Address --
   ---------------------------

   procedure Break_Unbreak_Address
     (Self       : in out Breakpoint_Holder;
      Address    : Address_Type;
      Executable : Virtual_File;
      Changed    : out Breakpoint_Vectors.Vector)
   is
      Deleted : Boolean := False;
   begin
      for D of Self.Vector loop
         if D.Kind = On_Address then
            if D.Address = Address then
               Deleted := True;
            else
               Changed.Append (D);
            end if;
         end if;
      end loop;

      if not Deleted then
         Changed.Append
           (Breakpoint_Data'
              (Kind       => On_Address,
               Num        => 0,
               Address    => Address,
               Executable => Executable,
               others     => <>));
      end if;
   end Break_Unbreak_Address;

end DAP.Modules.Breakpoints;
