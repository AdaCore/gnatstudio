-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Codefix.Merge_Utils is

   -------------------
   -- Generic_Merge --
   -------------------

   procedure Generic_Merge
     (Result              : out Merge_Type;
      Object_1, Object_2  : Merge_Type;
      Success             : out Boolean;
      Chronologic_Changes : Boolean)
   is

      It_1, It_2      : Merge_Iterator;
      New_Unit        : Merged_Unit;
      Internal_Result : Merge_Type;

      procedure Merge_Original;
      procedure Merge_Modified;
      procedure Merge_Deleted;
      procedure Merge_Created;

      procedure Merge_Original is
      begin
         case Get_Merge_Info (Data (It_2)) is
            when Original_Unit =>
               Append (Internal_Result, Clone (Data (It_1)));
               It_1 := Next (It_1);
               It_2 := Next (It_2);
            when Unit_Modified =>
               Append (Internal_Result, Clone (Data (It_2)));
               It_1 := Next (It_1);
               It_2 := Next (It_2);
            when Unit_Deleted =>
               Append (Internal_Result, Clone (Data (It_2)));
               It_1 := Next (It_1);
               It_2 := Next (It_2);
            when Unit_Created =>
               Append (Internal_Result, Clone (Data (It_1)));
               It_1 := Next (It_1);
         end case;
      end Merge_Original;

      procedure Merge_Modified is
      begin
         case Get_Merge_Info (Data (It_2)) is
            when Original_Unit =>
               Append (Internal_Result, Clone (Data (It_1)));
               It_1 := Next (It_1);
               It_2 := Next (It_2);
            when Unit_Modified =>
               Merge_Units
                 (New_Unit,
                  Data (It_1),
                  Data (It_2),
                  Success,
                  Chronologic_Changes);

               Set_Merge_Info (New_Unit, Unit_Modified);

               if Success then
                  Append (Internal_Result, New_Unit);
               elsif Chronologic_Changes then
                  Append (Internal_Result, Clone (Data (It_2)));
                  Success := True;
               end if;

               It_1 := Next (It_1);
               It_2 := Next (It_2);
            when Unit_Deleted =>
               Success := False;
            when Unit_Created =>
               Append (Internal_Result, Clone (Data (It_1)));
               It_1 := Next (It_1);
         end case;
      end Merge_Modified;

      procedure Merge_Deleted is
      begin
         case Get_Merge_Info (Data (It_2)) is
            when Original_Unit =>
               Append (Internal_Result, Clone (Data (It_1)));
               It_1 := Next (It_1);
               It_2 := Next (It_2);
            when Unit_Modified =>
               Success := False;
            when Unit_Deleted =>
               Append (Internal_Result, Clone (Data (It_1)));
               It_1 := Next (It_1);
               It_2 := Next (It_2);
            when Unit_Created =>
               Append (Internal_Result, Clone (Data (It_1)));
               It_1 := Next (It_1);
         end case;
      end Merge_Deleted;

      procedure Merge_Created is
      begin
         case Get_Merge_Info (Data (It_2)) is
            when Original_Unit =>
               Append (Internal_Result, Clone (Data (It_2)));
               It_2 := Next (It_2);
            when Unit_Modified =>
               Append (Internal_Result, Clone (Data (It_2)));
               It_2 := Next (It_2);
            when Unit_Deleted =>
               Append (Internal_Result, Clone (Data (It_2)));
               It_2 := Next (It_2);
            when Unit_Created =>
               Append (Internal_Result, Clone (Data (It_1)));

               if Data (It_1) /= Data (It_2) then
                  It_1 := Next (It_1);
               else
                  It_1 := Next (It_1);
                  It_2 := Next (It_2);
               end if;
               --  This block means that if the same thing is added at the same
               --  position in both objects, then the result must have only one
               --  occurence of it.
         end case;
      end Merge_Created;

      --  begin of Merge

   begin

      It_1 := First (Object_1);
      It_2 := First (Object_2);
      Success := True;

      while not Is_Null (It_1) and then not Is_Null (It_2) loop
         if It_1 < It_2 then
            Append (Internal_Result, Clone (Data (It_1)));
            It_1 := Next (It_1);
         elsif It_2 < It_1 then
            Append (Internal_Result, Clone (Data (It_2)));
            It_2 := Next (It_2);
         else
            case Get_Merge_Info (Data (It_1)) is
               when Original_Unit =>
                  Merge_Original;
               when Unit_Modified =>
                  Merge_Modified;
               when Unit_Created =>
                  Merge_Created;
               when Unit_Deleted =>
                  Merge_Deleted;
            end case;
         end if;

         if not Success then
            return;
         end if;
      end loop;


      while not Is_Null (It_1) loop
         Append (Internal_Result, Clone (Data (It_1)));
         It_1 := Next (It_1);
      end loop;

      while not Is_Null (It_2) loop
         Append (Internal_Result, Clone (Data (It_2)));
         It_2 := Next (It_2);
      end loop;

      Result := Internal_Result;

   end Generic_Merge;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (This : in out Mergable_String; Start : Natural; Len : Natural := 0) is

      Position   : constant Natural := Get_Array_Position (This, Start);
      Offset     : Natural := 0;
      Copy_Infos : constant Merge_Array := This.Infos.all;
      J, K       : Natural;
   begin
      J := Position;
      K := 1;

      if J not in This.Infos'Range then
         raise Codefix_Panic;
      end if;

      loop
         while Copy_Infos (J) = Unit_Deleted loop
            J := J + 1;

            if J > This.Infos'Last then
               raise Codefix_Panic;
            end if;
         end loop;

         if Copy_Infos (J) = Original_Unit
           or else Copy_Infos (J) = Unit_Modified
         then
            This.Infos (J) := Unit_Deleted;
         elsif Copy_Infos (J) = Unit_Created then
            Delete_Char (This.Str, J - Offset);
            Delete_Info (This.Infos, J - Offset);
            Offset := Offset + 1;
         end if;

         J := J + 1;

         exit when K = Len or else J > This.Infos'Last;

         K := K + 1;

      end loop;
   end Delete;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This : in out Mergable_String; Start : Natural; Value : String)
   is
      Insertion_Array : constant Merge_Array (Value'Range) :=
        (others => Unit_Created);
      New_Str         : String_Access;
      New_Infos       : Ptr_Merge_Array;
      Position        : constant Natural := Get_Array_Position (This, Start);
   begin
      if Position > This.Str'First and then Position <= This.Str'Last then
         New_Str := new String'
           (This.Str (This.Str'First .. Position - 1)
              & Value
              & This.Str (Position .. This.Str'Last));
         New_Infos := new Merge_Array'
           (This.Infos (This.Infos'First .. Position - 1)
              & Insertion_Array
              & This.Infos (Position .. This.Str'Last));
      elsif Position = This.Str'First then
         New_Str := new String'(Value & This.Str.all);
         New_Infos := new Merge_Array'(Insertion_Array & This.Infos.all);
      else
         New_Str := new String'(This.Str.all & Value);
         New_Infos := new Merge_Array'(This.Infos.all & Insertion_Array);
      end if;

      Free (This.Str);
      Free (This.Infos);
      This.Str := New_Str;
      This.Infos := New_Infos;

   end Insert;

   ------------
   -- Modify --
   ------------

   procedure Modify
     (This : in out Mergable_String; Start : Natural; Value : String)
   is
      Position : Natural := Get_Array_Position (This, Start);
   begin
      for J in Value'Range loop
         while This.Infos (Position) = Unit_Deleted loop
            Position := Position + 1;
         end loop;

         This.Str (Position) := Value (J);
         if This.Infos (Position) = Original_Unit then
            This.Infos (Position) := Unit_Modified;
         end if;

         Position := Position + 1;
      end loop;
   end Modify;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This : in out Mergable_String; Start, Len : Natural; Value : String) is
   begin
      if Len > Value'Length then
         Modify (This, Start, Value);
         Delete (This, Start + Value'Length, Len - Value'Length);
      elsif Len < Value'Length then
         Modify (This, Start, Value (Value'First .. Value'First + Len - 1));
         Insert (This, Start + Len, Value (Value'First + Len .. Value'Last));
      else
         Modify (This, Start, Value);
      end if;
   end Replace;

   ------------------
   -- Merge_String --
   ------------------

   procedure Merge_String
     (Result              : out Mergable_String;
      Object_1, Object_2  : Mergable_String;
      Success             : out Boolean;
      Chronologic_Changes : Boolean)
   is

      procedure Merge_Intern is new Generic_Merge
        (Mergable_String,
         String_Char,
         String_Iterator,
         Merge_Units => Merge_Null);

   begin
      Merge_Intern
        (Result,
         Object_1,
         Object_2,
         Success,
         Chronologic_Changes);
   end Merge_String;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Mergable_String) is
   begin
      Free (This.Str);
      Free (This.Infos);
   end Free;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Mergable_String) return String is

      function Get_Block (Current_Char : Natural) return String;

      function Get_Block (Current_Char : Natural) return String is
      begin
         if This.Infos (Current_Char) = Unit_Deleted then
            for J in Current_Char .. This.Str'Last loop
               if This.Infos (J) /= Unit_Deleted then
                  return Get_Block (J);
               end if;
            end loop;

            return "";
         else
            for J in Current_Char .. This.Str'Last loop
               if This.Infos (J) = Unit_Deleted then
                  return This.Str (Current_Char .. J - 1) & Get_Block (J);
               end if;
            end loop;
         end if;

         return This.Str (Current_Char .. This.Str'Last);
      end Get_Block;

   begin
      if This.Str'Length > 0 then
         return Get_Block (This.Str'First);
      else
         return "";
      end if;
   end To_String;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (This : in out Mergable_String; Value : Mergable_String) is
   begin
      Free (This.Str);
      Free (This.Infos);
      This.Str := Clone (Value.Str);
      This.Infos := new Merge_Array'(Value.Infos.all);
   end Assign;

   -----------
   -- Clone --
   -----------

   function Clone (This : Mergable_String) return Mergable_String is
   begin
      return (Clone (This.Str), new Merge_Array'(This.Infos.all));
   end Clone;

   ------------------------
   -- To_Mergable_String --
   ------------------------

   function To_Mergable_String (This : String) return Mergable_String is
      Result : Mergable_String;
   begin
      Result.Str := new String'(This);
      Result.Infos := new Merge_Array (This'Range);
      Result.Infos.all := (others => Original_Unit);
      return Result;
   end To_Mergable_String;

   -----------
   -- First --
   -----------

   function First (This : Mergable_String) return String_Iterator is
   begin
      return (This, This.Str'First);
   end First;

   ----------
   -- Next --
   ----------

   function Next (This : String_Iterator) return String_Iterator is
      Result : String_Iterator := This;
   begin
      Result.Position := Result.Position + 1;
      return Result;
   end Next;

   ----------
   -- Data --
   ----------

   function Data (This : String_Iterator) return String_Char is
   begin
      return
        (This.Object.Str (This.Position),
         This.Object.Infos (This.Position));
   end Data;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (This : String_Iterator) return Boolean is
   begin
      return This.Position > This.Object.Str'Last;
   end Is_Null;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : String_Iterator) return Boolean is

      function Real_Position
        (This : Mergable_String; Pos : Natural) return Natural;
      --  Returns the real index after having ignored the created
      --  characters.

      function Real_Position
        (This : Mergable_String; Pos : Natural) return Natural
      is
         Total : Natural := 0;
      begin
         for J in This.Infos'First .. Pos loop
            if This.Infos (J) /= Unit_Created then
               Total := Total + 1;
            end if;
         end loop;

         return Total;
      end Real_Position;

   begin
      return Real_Position (Left.Object, Left.Position) <
        Real_Position (Right.Object, Right.Position);
   end "<";

   --------------------
   -- Get_Merge_Info --
   --------------------

   function Get_Merge_Info (This : String_Char) return Merge_Info is
   begin
      return This.Info;
   end Get_Merge_Info;

   --------------------
   -- Set_Merge_Info --
   --------------------

   procedure Set_Merge_Info (This : in out String_Char; Value : Merge_Info) is
   begin
      This.Info := Value;
   end Set_Merge_Info;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Mergable_String; Object : String_Char) is
      Garbage_Str   : String_Access := This.Str;
      Garbage_Infos : Ptr_Merge_Array := This.Infos;
   begin
      if This.Str = null then
         This.Str := new String'(1 => Object.Char);
         This.Infos := new Merge_Array'(1 => Object.Info);
      else
         This.Str := new String'(This.Str.all & Object.Char);
         This.Infos := new Merge_Array'(This.Infos.all & Object.Info);

         Free (Garbage_Str);
         Free (Garbage_Infos);
      end if;
   end Append;

   -----------
   -- Clone --
   -----------

   function Clone (This : String_Char) return String_Char is
   begin
      return This;
   end Clone;

   ----------------
   -- Merge_Null --
   ----------------

   procedure Merge_Null
     (Result              : out String_Char;
      Object_1, Object_2  : String_Char;
      Success             : out Boolean;
      Chronologic_Changes : Boolean)
   is
      pragma Unreferenced (Object_1, Object_2, Chronologic_Changes);
   begin
      Result := (Character'Val (0), Original_Unit);
      Success := False;
   end Merge_Null;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : String_Char) return Boolean is
   begin
      return Left.Char = Right.Char;
   end "=";

   ------------------------
   -- Get_Array_Position --
   ------------------------

   function Get_Array_Position (Str : Mergable_String; Position : Natural)
     return Natural is
      Existent_Chars : Natural := 0;
   begin
      for J in 1 .. Str.Infos'Last loop
         if Str.Infos (J) /= Unit_Deleted then
            Existent_Chars := Existent_Chars + 1;
         end if;

         if Existent_Chars = Position then
            return J;
         end if;
      end loop;

      return Str.Infos'Last + 1;
   end Get_Array_Position;

   -----------------
   -- Delete_Char --
   -----------------

   procedure Delete_Char (This : in out String_Access; Position : Natural) is
      Garbage : String_Access := This;
   begin
      if Position < This'Last then
         This := new String'(This (This'First .. Position - 1) &
                               This (Position + 1 .. This'Last));
      else
         This := new String'(This (This'First .. Position - 1));
      end if;

      Free (Garbage);
   end Delete_Char;

   -----------------
   -- Delete_Info --
   -----------------

   procedure Delete_Info (This : in out Ptr_Merge_Array; Position : Natural) is
      Garbage : Ptr_Merge_Array := This;
   begin
      if Position < This'Last then
         This := new Merge_Array'(This (This'First .. Position - 1) &
                                    This (Position + 1 .. This'Last));
      else
         This := new Merge_Array'(This (This'First .. Position - 1));
      end if;

      Free (Garbage);
   end Delete_Info;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Mask_Iterator) is
   begin
      This.Info_Index := 1;
      This.Real_Index := 1;
   end Reset;

   -------------------
   -- Get_Next_Area --
   -------------------

   procedure Get_Next_Area
     (This       : Mergable_String;
      It         : in out Mask_Iterator;
      Start, Len : out Natural;
      Info       : out Merge_Info) is
   begin

      Len := 0;

      if It.Info_Index > This.Infos'Last then
         return;
      end if;

      Start := It.Real_Index;
      Info := This.Infos (It.Info_Index);

      case Info is
         when Unit_Modified | Unit_Created | Original_Unit =>
            while It.Info_Index <= This.Infos'Last
              and then This.Infos (It.Info_Index) = Info
            loop
               Len := Len + 1;
               It.Real_Index := It.Real_Index + 1;
               It.Info_Index := It.Info_Index + 1;
            end loop;
         when Unit_Deleted =>
            while It.Info_Index <= This.Infos'Last
              and then This.Infos (It.Info_Index) = Info
            loop
               Len := Len + 1;
               It.Info_Index := It.Info_Index + 1;
            end loop;
      end case;

   end Get_Next_Area;

end Codefix.Merge_Utils;
