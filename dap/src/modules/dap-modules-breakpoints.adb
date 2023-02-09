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

   function Get_Location_File (Data : Breakpoint_Data) return Virtual_File;

   function Is_Line_Bp (Data : Breakpoint_Data) return Boolean;
   function Is_Subprogram_Bp (Data : Breakpoint_Data) return Boolean;

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
      Loc : constant Location_Marker := Get_Location (Data);
   begin
      if Loc = No_Marker then
         return No_File;
      else
         return GPS.Editors.Get_File (Loc);
      end if;
   end Get_Location_File;

   ----------------
   -- Is_Line_Bp --
   ----------------

   function Is_Line_Bp (Data : Breakpoint_Data) return Boolean is
   begin
      return Data.Subprogram = Null_Unbounded_String;
   end Is_Line_Bp;

   ----------------------
   -- Is_Subprogram_Bp --
   ----------------------

   function Is_Subprogram_Bp (Data : Breakpoint_Data) return Boolean is
   begin
      return Data.Subprogram /= Null_Unbounded_String;
   end Is_Subprogram_Bp;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Breakpoint_Holder;
      Vector : Breakpoint_Vectors.Vector;
      Clear  : Boolean := False)
   is
      Data : Breakpoint_Data;
      Id   : Breakpoint_Identifier := 0;
   begin
      if Clear then
         for Item of Vector loop
            Data := Item;
            if Data.State = Enabled then
               Data.Num := 0;
            else
               Id := Id - 1;
               Data.Num := Id;
            end if;

            if Is_Line_Bp (Data) then
               for Loc of Data.Locations loop
                  Loc.Num := 0;
               end loop;
            else
               Data.Locations.Clear;
            end if;
            Self.Vector.Append (Data);
         end loop;

      else
         Self.Vector := Vector;
      end if;
      Self.In_Initialization := True;
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
      Name   : constant String := +Base_Name (Executable);
   begin
      for Data of Self.Vector loop
         if Is_Line_Bp (Data)
           or else Data.Executable = Name
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
   -- Deleted --
   -------------

   procedure Deleted
     (Self : in out Breakpoint_Holder;
      File : Virtual_File;
      Line : Editable_Line_Type)
   is
      Index : Integer := Self.Vector.First_Index;
      Data  : Breakpoint_Data;
   begin
      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);
         if Is_Same_Location (Data, File, Line) then
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
      Nums : Breakpoint_Identifier_Lists.List) is
   begin
      for Num of Nums loop
         Self.Deleted (Num);
      end loop;
   end Deleted;

   -------------
   -- Deleted --
   -------------

   procedure Deleted
     (Self : in out Breakpoint_Holder;
      Num  : Breakpoint_Identifier)
   is
      Index : Integer := Self.Vector.First_Index;
      Data  : Breakpoint_Data;
   begin
      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);

         if Data = Num then
            if Data.State = Changing then
               Data.State := Disabled;
               Self.Vector.Replace_Element (Index, Data);
            else
               Self.Vector.Delete (Index);
            end if;
            return;

         else
            Index := Index + 1;
         end if;
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
      for Index in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         if Is_Same_Location (Self.Vector.Element (Index), Marker) then
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
      State : Boolean)
   is
      Data : Breakpoint_Data;
   begin
      for Id of Ids loop
         for Index in Self.Vector.First_Index .. Self.Vector.Last_Index loop
            Data := Self.Vector.Element (Index);
            if Data = Id then
               if State then
                  Data.State := Enabled;
               else
                  Data.State := Disabled;
               end if;
               Self.Vector.Replace_Element (Index, Data);

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
      Idx  : Natural := Self.Vector.First_Index;
      C    : Breakpoint_Vectors.Cursor;
      Name : constant String := +Base_Name (Executable);
   begin
      while Idx <= Self.Vector.Last_Index loop
         if Self.Vector.Element (Idx).Executable = Name
           and then not List.Contains (Self.Vector.Element (Idx))
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

   procedure Set_Numbers (Self : in out Breakpoint_Holder)
   is
      D   : Breakpoint_Data;
      Idx : Natural := Self.Vector.First_Index;
   begin
      Self.Id := 0;

      while Idx <= Self.Vector.Last_Index loop
         D := Self.Vector.Element (Idx);

         Self.Id := Self.Id + 1;
         D.Num   := Self.Id;

         Self.Vector.Replace_Element (Idx, D);
         Idx := Idx + 1;
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
           and then Is_Line_Bp (Data)
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

   -------------------------
   -- Get_For_Subprograms --
   -------------------------

   function Get_For_Subprograms
     (Self          : Breakpoint_Holder;
      With_Changing : Boolean := False)
      return Breakpoint_Vectors.Vector
   is
      Result : Breakpoint_Vectors.Vector;
   begin
      for Data of Self.Vector loop
         if (Data.State = Enabled
             or else (With_Changing and then Data.State = Changing))
           and then Is_Subprogram_Bp (Data)
         then
            Result.Append (Data);
         end if;
      end loop;

      return Result;
   end Get_For_Subprograms;

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
           and then Is_Line_Bp (Data)
           and then Get_Location_File (Data) = File
         then
            Result.Append (Data);
         end if;
      end loop;

      return Result;
   end Get_For_File;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self    : Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : out Breakpoint_Vectors.Vector) is
   begin
      Changed := Self.Get_For_File (Get_Location_File (Data));
      Changed.Append (Data);
   end Add;

   --------------------
   -- Add_Subprogram --
   --------------------

   procedure Add_Subprogram
     (Self    : Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : out Breakpoint_Vectors.Vector) is
   begin
      Changed := Self.Get_For_Subprograms;
      for D of Changed loop
         if D.Subprogram = Data.Subprogram then
            Changed.Clear;
            return;
         end if;
      end loop;

      Changed.Append (Data);
   end Add_Subprogram;

   -----------------
   -- Set_Enabled --
   -----------------

   procedure Set_Enabled
     (Self    : in out Breakpoint_Holder;
      Nums    : Breakpoint_Identifier_Lists.List;
      State   : Boolean;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      Data   : Breakpoint_Data;
      Cursor : Breakpoint_Hash_Maps.Cursor;
      File   : Virtual_File;
   begin
      for Num of Nums loop
         for Index in Self.Vector.First_Index .. Self.Vector.Last_Index loop
            Data := Self.Vector.Element (Index);
            if Data = Num then
               if State then
                  if Data.State /= Enabled then
                     Changed.Include
                       ((if Is_Line_Bp (Data)
                        then Get_Location_File (Data)
                        else No_File),
                        Breakpoint_Vectors.Empty_Vector);
                     Data.State := Changing;
                     Self.Vector.Replace_Element (Index, Data);
                  end if;

               else
                  if Data.State = Enabled then
                     Changed.Include
                       ((if Is_Line_Bp (Data)
                        then Get_Location_File (Data)
                        else No_File),
                        Breakpoint_Vectors.Empty_Vector);
                     Data.State := Changing;
                     Self.Vector.Replace_Element (Index, Data);
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         File := Key (Cursor);
         Changed.Replace_Element
           (Cursor,
            (if File = No_File
             then Self.Get_For_Subprograms (State)
             else Self.Get_For_File (File, State)));
         Next (Cursor);
      end loop;
   end Set_Enabled;

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
      Data   : Breakpoint_Data;
      Cursor : Breakpoint_Hash_Maps.Cursor;
      F      : Virtual_File;
      List   : Breakpoint_Vectors.Vector;
   begin
      Updated := False;
      while Idx <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Idx);
         if Is_Same_Location (Data, File, Line) then
            if Data.State = Enabled then
               Changed.Include
                 ((if Is_Line_Bp (Data)
                  then Get_Location_File (Data)
                  else No_File),
                  Breakpoint_Vectors.Empty_Vector);
            else
               --  not in the debugger, just delete
               Updated := True;
               Self.Vector.Delete (Idx);
               Idx := Idx - 1;
            end if;
         end if;
         Idx := Idx + 1;
      end loop;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         F := Key (Cursor);

         if F = No_File then
            List := Self.Get_For_Subprograms;
         else
            List := Self.Get_For_File (F);
         end if;

         Idx := List.First_Index;
         while Idx <= List.Last_Index loop
            Data := List.Element (Idx);
            if Is_Same_Location (Data, File, Line) then
               List.Delete (Idx);
            else
               Idx := Idx + 1;
            end if;
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
      Idx    : Integer := Self.Vector.First_Index;
      Data   : Breakpoint_Data;
      Cursor : Breakpoint_Hash_Maps.Cursor;
      F      : Virtual_File;
      List   : Breakpoint_Vectors.Vector;
   begin
      Updated := False;
      for Num of Nums loop
         while Idx <= Self.Vector.Last_Index loop
            Data := Self.Vector.Element (Idx);
            if Data = Num then
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
            Idx := Idx + 1;
         end loop;
      end loop;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         F := Key (Cursor);
         if F = No_File then
            List := Self.Get_For_Subprograms;
         else
            List := Self.Get_For_File (F);
         end if;

         Idx := List.First_Index;
         while Idx <= List.Last_Index loop
            if Nums.Contains (List.Element (Idx).Num) then
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

   begin
      for Idx in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         exit when Idx_New > Actual.Last_Index;

         Data := Self.Vector.Element (Idx);
         if Data.State = Enabled
           and then Is_Line_Bp (Data)
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
      Self.Delete_Duplicates (File, Changed);
   end Initialized_For_File;

   -----------------------
   -- Delete_Duplicates --
   -----------------------

   procedure Delete_Duplicates
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Changed : out Breakpoint_Hash_Maps.Map)
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
         Data := Self.Vector.Element (Index);
         if Data.State = Moved then
            Deleted := False;
            for D of Self.Vector loop
               if D.Num /= Data.Num
                 and then Is_Line_Bp (Data)
                 and then Data.State /= Disabled
                 and then Is_Same_Location
                   (Data, D.Locations.First_Element.Marker)
               then
                  Self.Vector.Delete (Index);
                  Index := Index - 1;
                  Deleted := True;
                  Update  := True;
                  exit;
               end if;
            end loop;

            if not Deleted then
               Data.State := Enabled;
               Self.Vector.Replace_Element (Index, Data);
               Index := Index + 1;
            end if;
         end if;
         Index := Index + 1;
      end loop;

      Index := Self.Vector.First_Index;
      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);
         if Data.State = Enabled
           and then Is_Line_Bp (Data)
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
   end Delete_Duplicates;

   -----------
   -- Added --
   -----------

   procedure Added
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Data    : Breakpoint_Data;
      Changed : out Breakpoint_Vectors.Vector;
      Update  : out Boolean)
   is
      D          : Breakpoint_Data;
      Duplicates : Boolean := False;
      Index      : Integer;
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

      --  Check for duplicates
      for Idx in Self.Vector.First_Index .. Self.Vector.Last_Index loop
         D := Self.Vector.Element (Idx);
         if D.State = Enabled
           and then D.Num /= Data.Num
           and then Is_Line_Bp (D)
           and then Is_Same_Location (Data, D.Locations.First_Element.Marker)
         then
            Duplicates := True;
            exit;
         end if;
      end loop;

      if Duplicates then
         --  Just added breakpoint duplicates another, so delete it
         Self.Vector.Delete (Index);
         Changed := Self.Get_For_File (File);
      else
         Update := True;
      end if;
   end Added;

   --------------------
   -- Status_Changed --
   --------------------

   procedure Status_Changed
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map)
   is
      Index   : Integer := Self.Vector.First_Index;
      Idx     : Integer := Actual.First_Index;
      Data, D : Breakpoint_Data;
      Nums    : Breakpoint_Identifier_Lists.List;
   begin
      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);

         if Is_Line_Bp (Data)
           and then Get_Location_File (Data) = File
         then
            case Data.State is
               when Enabled =>
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
                     D.State := Enabled;
                  else
                     D.State := Moved;
                  end if;

                  Nums.Append (D.Num);
                  Self.Vector.Replace_Element (Index, D);
                  Idx := Idx + 1;

               when Disabled | Moved =>
                  null;
            end case;
         end if;
         Index := Index + 1;
      end loop;

      Self.Delete_Duplicates (File, Changed);
   end Status_Changed;

   ---------------------------------
   -- Initialized_For_Subprograms --
   ---------------------------------

   procedure Initialized_For_Subprograms
     (Self   : in out Breakpoint_Holder;
      Actual : Breakpoint_Vectors.Vector;
      Last   : Boolean)
   is
      Index : Integer := Self.Vector.First_Index;
      Idx   : Integer := Actual.First_Index;
      Data  : Breakpoint_Data;
   begin
      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);
         if Data.State = Enabled
           and then Is_Subprogram_Bp (Data)
         then
            if Data.Num = 0 then
               Data.Num := Actual.Element (Idx).Locations.First_Element.Num;
               while Idx <= Actual.Last_Index loop
                  Data.Locations.Append
                    (Actual.Element (Idx).Locations.First_Element);
                  Idx := Idx + 1;
               end loop;
               Self.Vector.Replace_Element (Index, Data);
               exit;

            else
               Idx := Idx + Data.Locations.Last_Index;
            end if;
         end if;
         Index := Index + 1;
      end loop;

      if Last then
         Self.Delete_Fake_Subprogram;
      end if;
   end Initialized_For_Subprograms;

   ----------------------------
   -- Delete_Fake_Subprogram --
   ----------------------------

   procedure Delete_Fake_Subprogram (Self : in out Breakpoint_Holder) is
      Index : Integer := Self.Vector.First_Index;
      Data  : Breakpoint_Data;
      Nums  : Breakpoint_Identifier_Lists.List;
   begin
      while Index <= Self.Vector.Last_Index loop
         Data := Self.Vector.Element (Index);
         if Data.State = Enabled then
            if Is_Subprogram_Bp (Data) then
               for Loc of Data.Locations loop
                  Nums.Append (Loc.Num);
               end loop;

            elsif Nums.Contains (Data.Num) then
               Self.Vector.Delete (Index);
               Index := Index - 1;
            end if;
         end if;

         Index := Index + 1;
      end loop;
   end Delete_Fake_Subprogram;

   ----------------------
   -- Added_Subprogram --
   ----------------------

   procedure Added_Subprogram
     (Self   : in out Breakpoint_Holder;
      Data   : Breakpoint_Data;
      Actual : Breakpoint_Vectors.Vector)
   is
      Local : Breakpoint_Data := Data;
      Idx   : Integer := Actual.First_Index;
      Nums  : Breakpoint_Identifier_Lists.List;
   begin
      for D of Self.Vector loop
         if D.State = Enabled
           and then Is_Subprogram_Bp (D)
         then
            Idx := Idx + D.Locations.Last_Index;
         end if;
      end loop;

      while Idx <= Actual.Last_Index loop
         Local.Locations.Append (Actual.Element (Idx).Locations.First_Element);
         Nums.Append (Actual.Element (Idx).Locations.First_Element.Num);
         Idx := Idx + 1;
      end loop;
      Local.Num := Local.Locations.First_Element.Num;
      Self.Vector.Append (Local);

      Idx := Self.Vector.First_Index;
      while Idx <= Self.Vector.Last_Index loop
         if Is_Line_Bp (Self.Vector.Element (Idx))
           and then Nums.Contains (Self.Vector.Element (Idx).Num)
         then
            Self.Vector.Delete (Idx);
         else
            Idx := Idx + 1;
         end if;
      end loop;
   end Added_Subprogram;

   -------------------------------
   -- Subprogram_Status_Changed --
   -------------------------------

   procedure Subprogram_Status_Changed
     (Self   : in out Breakpoint_Holder;
      Actual : Breakpoint_Vectors.Vector;
      Last   : Boolean)
   is
      Index : Integer := Self.Vector.First_Index;
      Idx   : Integer := Actual.First_Index;
      Data  : Breakpoint_Data;

   begin
      while Index <= Self.Vector.Last_Index
        and then Idx <= Actual.Last_Index
      loop
         Data := Self.Vector.Element (Index);
         if Is_Subprogram_Bp (Data) then
            case Data.State is
               when Enabled =>
                  Idx := Idx + Data.Locations.Last_Index;

               when Changing =>
                  Data.Locations.Clear;
                  while Idx <= Actual.Last_Index loop
                     Data.Locations.Append
                       (Actual.Element (Idx).Locations.First_Element);
                     Idx := Idx + 1;
                  end loop;
                  Data.Num   := Data.Locations.First_Element.Num;
                  Data.State := Enabled;
                  Self.Vector.Replace_Element (Index, Data);
                  exit;

               when Disabled | Moved =>
                  null;
            end case;
         end if;
         Index := Index + 1;
      end loop;

      if Last then
         Self.Delete_Fake_Subprogram;
      end if;
   end Subprogram_Status_Changed;

   -------------
   -- Changed --
   -------------

   procedure Changed
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data)
   is
      Index : Integer := Self.Vector.First_Index;
      Idx   : Integer;
      D     : Breakpoint_Data;
   begin
      while Index <= Self.Vector.Last_Index loop
         D   := Self.Vector.Element (Index);
         Idx := Data.Locations.First_Index;
         while Idx <= D.Locations.Last_Index loop
            if D.Locations.Element (Idx).Num = Data.Num then
               D.Locations.Replace_Element
                 (Idx, Data.Locations.First_Element);
               Self.Vector.Replace_Element (Index, D);
               return;
            end if;
            Idx := Idx + 1;
         end loop;
         Index := Index + 1;
      end loop;
   end Changed;

end DAP.Modules.Breakpoints;
