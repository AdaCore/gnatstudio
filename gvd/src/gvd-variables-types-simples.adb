------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with Language;                use Language;
with String_Utils;            use String_Utils;
with MI.Lexer;                use MI.Lexer;

package body GVD.Variables.Types.Simples is

   Unknown_Value_Str : constant String := "<unknown>";
   --  The string representation of a null value for the user.

   function Quote_Non_Printable_Characters (Str : String) return String;
   --  Protect non-printable characters in the string

   ------------
   -- At_End --
   ------------

   overriding function At_End (Self : Field_Iterator) return Boolean is
   begin
      return Self.Fields.Is_Empty
        or else Self.Idx > Integer (Self.Fields.Length);
   end At_End;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (Self : not null access GVD_Debugger_Output_Type) is
   begin
      Self.Value.Clear;
      if not Self.As_Record.Is_Empty then
         Self.As_Record.Clear;
      end if;
   end Clear;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : not null access GVD_Simple_Type) is
   begin
      Self.Value       := Null_Unbounded_String;
      Self.Has_Changed := False;
   end Clear;

   -----------
   -- Clone --
   -----------

   overriding procedure Clone
     (Self : not null access GVD_Simple_Type;
      Item : not null GVD_Generic_Type_Access) is
   begin
      GVD_Generic_Type (Self.all).Clone (Item);

      if GVD_Simple_Type_Access (Item).Value /= Null_Unbounded_String then
         Self.Value := GVD_Simple_Type_Access (Item).Value;
      end if;
   end Clone;

   -----------
   -- Clone --
   -----------

   overriding procedure Clone
     (Self : not null access GVD_Debugger_Output_Type;
      Item : not null GVD_Generic_Type_Access) is
   begin
      GVD_Generic_Type (Self.all).Clone (Item);
      Self.Refresh_Cmd := GVD_Debugger_Output_Type_Access (Item).Refresh_Cmd;
   end Clone;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Self : Field_Iterator) return GVD_Type_Holder'Class is
   begin
      return Self.Fields (Self.Idx).Typ;
   end Data;

   ----------------
   -- Field_Name --
   ----------------

   overriding function Field_Name
     (Self : Field_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String
   is
      pragma Unreferenced (Lang, Base);
   begin
      return To_String (Self.Fields (Self.Idx).Name);
   end Field_Name;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Self : not null access GVD_Debugger_Output_Type) is
   begin
      if not Self.As_Record.Is_Empty then
         Self.As_Record.Clear;
      end if;

      GVD_Generic_Type (Self.all).Free;
   end Free;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access GVD_Debugger_Output_Type) return String
   is
      Value : Unbounded_String;
   begin
      if Self.Value.Is_Empty then
         return Unknown_Value_Str;

      elsif Self.Split_Lines then
         return "";   --  value is split into components

      else
         for L of Self.Value loop
            Append (Value, To_String (L.Value) & ASCII.LF);
         end loop;
         return To_String (Value);
      end if;
   end Get_Simple_Value;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access GVD_Simple_Type) return String is
   begin
      if Self.Value = Null_Unbounded_String then
         return Unknown_Value_Str;
      else
         return To_String (Self.Value);
      end if;
   end Get_Simple_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self : not null access GVD_Simple_Type)
      return Unbounded_String is
   begin
      return Self.Value;
   end Get_Value;

   ----------------
   -- Is_Changed --
   ----------------

   overriding function Is_Changed
     (Self : not null access GVD_Simple_Type) return Boolean is
   begin
      return Self.Has_Changed;
   end Is_Changed;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Field_Iterator) is
   begin
      Self.Idx := Self.Idx + 1;
   end Next;

   ---------------------
   -- New_Access_Type --
   ---------------------

   function New_Access_Type return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Access_Type);
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Access_Type;

   -----------------------
   -- New_Debugger_Type --
   -----------------------

   function New_Debugger_Type
     (Cmd         : String;
      Split_Lines : Boolean := False)
      return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Debugger_Output_Type);
   begin
      GVD_Debugger_Output_Type_Access (Data.Instance).Refresh_Cmd :=
        To_Unbounded_String (Cmd);
      GVD_Debugger_Output_Type_Access (Data.Instance).Split_Lines :=
        Split_Lines;
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Debugger_Type;

   -------------------
   -- New_Enum_Type --
   -------------------

   function New_Enum_Type return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Enum_Type);
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Enum_Type;

   ------------------
   -- New_Mod_Type --
   ------------------

   function New_Mod_Type
     (Modulo : Long_Integer)
      return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Mod_Type'
             (GVD_Simple_Type with Modulo => Modulo));
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Mod_Type;

   --------------------
   -- New_Range_Type --
   --------------------

   function New_Range_Type
     (Min, Max : Long_Integer) return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Range_Type'
             (GVD_Simple_Type with Min => Min, Max => Max));
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Range_Type;

   ---------------------
   -- New_Simple_Type --
   ---------------------

   function New_Simple_Type return GVD_Type_Holder is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Simple_Type);
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_Simple_Type;

   ------------------------------------
   -- Quote_Non_Printable_Characters --
   ------------------------------------

   function Quote_Non_Printable_Characters (Str : String) return String is
      Output : String (1 .. Str'Length * 4);
      Index  : Integer := Output'First;
   begin
      for S in Str'Range loop
         if not Is_Graphic (Str (S)) then
            declare
               Img : constant String :=
                 Integer'Image (Character'Pos (Str (S)));
            begin
               Output (Index) := '[';
               Output (Index + 1) := Img (Img'Last - 1);
               Output (Index + 2) := Img (Img'Last);
               Output (Index + 3) := ']';
               Index := Index + 4;
            end;
         else
            Output (Index) := Str (S);
            Index := Index + 1;
         end if;
      end loop;
      return Output (Output'First .. Index - 1);
   end Quote_Non_Printable_Characters;

   ---------------------
   -- Refresh_Command --
   ---------------------

   function Refresh_Command
     (Self : not null access GVD_Debugger_Output_Type)
      return String is
   begin
      return To_String (Self.Refresh_Cmd);
   end Refresh_Command;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   overriding procedure Reset_Recursive
     (Self : not null access GVD_Debugger_Output_Type)
   is

      procedure Mark_Unmodified (X : in out Line_Value);
      --  Mark X as unmodified

      procedure Mark_Unmodified (X : in out Line_Value) is
      begin
         X.Modified := False;
      end Mark_Unmodified;

   begin
      if not Self.Value.Is_Empty then
         for L in 1 .. Positive (Self.Value.Length) loop
            Self.Value.Update_Element (L, Mark_Unmodified'Access);
         end loop;
      end if;
   end Reset_Recursive;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   overriding procedure Reset_Recursive
     (Self : not null access GVD_Simple_Type) is
   begin
      Self.Has_Changed := False;
   end Reset_Recursive;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Self  : not null access GVD_Debugger_Output_Type;
      Value : String)
   is
      Old : constant Line_Vector.Vector := Self.Value;
   begin
      if Starts_With (Value, "^done,") then
         Self.Value.Clear;

         if Value (Value'Last) = ']' then
            declare
               use Token_Lists;

               Tokens   : Token_List_Controller;
               T        : Token_Lists.Cursor;
               V        : Ada.Strings.Unbounded.Unbounded_String;
               Count    : Natural := 0;
               Add_Last : Boolean := False;
            begin
               Tokens.List := Build_Tokens
                 (Value
                    (Integer'Min (Index (Value, "[") + 1, Value'Last) ..
                         Value'Last - 1));

               T := Tokens.List.First;

               while T /= Token_Lists.No_Element
                 and then Element (T).Code /= End_Of_File
               loop

                  if Element (T).Code = L_Brace then
                     if Count = 0 then
                        Add_Last := False;
                     end if;

                     Count := Count + 1;

                     if Count = 1
                       and then V /= ""
                         and then Element (V, Length (V)) /= '='
                     then
                        Self.Value.Append ((V, False));
                        V := Null_Unbounded_String;

                     elsif V /= "" then
                        Add_Last := True;
                        Append (V, "{");
                     end if;

                  elsif Element (T).Code = R_Brace then
                     Count := Count - 1;

                     if Count = 0
                       and then V /= ""
                     then
                        Self.Value.Append ((V, False));
                        V := Null_Unbounded_String;

                     elsif Count > 0
                       or else Add_Last
                     then
                        Append (V, "}");
                     end if;

                  elsif Element (T).Code = Comma then
                     if Count > 0 then
                        V := V & To_Unbounded_String (Image (Element (T)));
                     else
                        if V /= "" then
                           Self.Value.Append ((V, False));
                           V := Null_Unbounded_String;
                        end if;
                     end if;

                  else
                     V := V & To_Unbounded_String (Image (Element (T)));
                  end if;

                  Next (T);
               end loop;

               if V /= "" then
                  Self.Value.Append ((V, False));
               end if;
            end;

         else
            Self.Value.Append
              ((Value    => To_Unbounded_String
                (Value (Integer'Min (7, Value'Last) .. Value'Last)),
                Modified => False));
         end if;

         for L in 1 .. Positive (Self.Value.Length) loop
            if Old.Is_Empty then
               Self.Value (L).Modified := True;
            else
               Self.Value (L).Modified :=
                 L > Natural (Old.Length)
                 or else Self.Value.Element (L).Value /=
                 To_String (Old (L).Value);
            end if;
         end loop;

      elsif Starts_With (Value, "^error,") then
         Self.Value.Clear;
         Self.Value.Append
           ((Value    => To_Unbounded_String
             ("Error:" &
                    Value (Integer'Min (Index (Value, """") + 1, Value'Last) ..
                      Value'Last - 1)),
             Modified => False));

      else
         declare
            S     : constant String    := Do_Tab_Expansion (Value, 8);
            Lines : String_List_Access := Split (S, ASCII.LF);
         begin
            --  Compute which lines have changed since the last update.

            Self.Value := Line_Vector.To_Vector (Lines'Length);
            for L in 1 .. Positive (Self.Value.Length) loop
               Self.Value (L).Value := To_Unbounded_String (Lines (L).all);
               if Old.Is_Empty then
                  Self.Value (L).Modified := True;
               else
                  Self.Value (L).Modified := L > Natural (Old.Length)
                    or else Lines (L).all /= To_String (Old (L).Value);
               end if;
            end loop;

            Free (Lines);
         end;
      end if;

      if Self.Split_Lines then
         Self.As_Record.Clear;
         Self.As_Record := Type_Vector.To_Vector (Self.Value.Length);

         for L in 1 .. Integer (Self.Value.Length) loop
            Self.As_Record (L).Name := Null_Unbounded_String;
            declare
               Data : constant GVD_Type_Holder_Data_Access :=
                 new GVD_Type_Holder_Data'
                   (Count => 1,
                    Instance => new GVD_Simple_Type'
                      (GVD_Base_Simple_Type with
                       Value       => Self.Value (L).Value,
                       Has_Changed => Self.Value (L).Modified));
            begin
               Self.As_Record (L).Typ := GVD_Type_Holder'
                 (Ada.Finalization.Controlled with Data);
            end;
         end loop;
      end if;

      Self.Valid := True;
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Self  : not null access GVD_Simple_Type;
      Value : String)
   is
      Q : constant String := Quote_Non_Printable_Characters (Value);
   begin
      Self.Has_Changed := Self.Value = Null_Unbounded_String
        or else To_String (Self.Value) /= Q;
      Self.Value := To_Unbounded_String (Q);
      Self.Valid := True;
   end Set_Value;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self : not null access GVD_Debugger_Output_Type)
      return Generic_Iterator'Class is
   begin
      if Self.As_Record.Is_Empty then
         return Create_Empty_Iterator;
      else
         return Start (Self.As_Record);
      end if;
   end Start;

   -----------
   -- Start --
   -----------

   function Start (Self : Type_Vector.Vector) return Generic_Iterator'Class is
   begin
      return Field_Iterator'
        (Generic_Iterator with
         Fields => Self,
         Idx    => (if Self.Is_Empty then 0 else 1));
   end Start;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Access_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Item.Data /= null
        and then Item.Data.Instance /= null
        and then Item.Data.Instance.all in GVD_Access_Type'Class;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Debugger_Output_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean
   is
      pragma Unreferenced (Self, Item);
   begin
      --  Never any aliasing
      return False;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Enum_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Item.Data /= null
        and then Item.Data.Instance /= null
        and then Item.Data.Instance.all in GVD_Enum_Type'Class;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Range_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean is
   begin
      return Item.Data /= null
        and then Item.Data.Instance /= null
        and then Item.Data.Instance.all in GVD_Range_Type'Class
        and then Self.Min = GVD_Range_Type_Access (Item.Data.Instance).Min
        and then Self.Max = GVD_Range_Type_Access (Item.Data.Instance).Max;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Simple_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean is
   begin
      return Item.Data /= null
        and then Item.Data.Instance /= null
        and then Item.Data.Instance.all in GVD_Simple_Type'Class
        and then Self.Type_Name = Item.Data.Instance.Type_Name;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Mod_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean is
   begin
      return Item.Data /= null
        and then Item.Data.Instance /= null
        and then Item.Data.Instance.all in GVD_Mod_Type'Class
        and then Self.Modulo = GVD_Mod_Type_Access (Item.Data.Instance).Modulo;
   end Structurally_Equivalent;

end GVD.Variables.Types.Simples;
