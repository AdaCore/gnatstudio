with GNAT.IO;  use GNAT.IO;

package body Generic_Values is

   package body Array_Indexes is

      -------------------
      -- Compute_Index --
      -------------------

      function Compute_Index (I : Indexes) return Long_Integer is
         Index : Long_Integer := I (I'First) - For_Array.Dimensions (1).First;
      begin
         for J in 2 .. For_Array.Num_Dimensions loop
            Index := Index * (For_Array.Dimensions (J).Last
                              - For_Array.Dimensions (J).First + 1)
              + I (I'First + J - 1) - For_Array.Dimensions (J).First;
         end loop;
         return Index;
      end Compute_Index;

   end Array_Indexes;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Simple_Type) is
   begin
      if Value.Value = null then
         Put ("{Simple: <null>}");
      else
         Put ("{Simple: " & Value.Value.all & "}");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Range_Type) is
   begin
      Put ("{Range" & Value.Min'Img & " .." & Value.Max'Img & " = ");
      if Value.Value /= null then
         Put (Value.Value.all);
      end if;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Mod_Type) is
   begin
      Put ("{Modulo " & Value.Max'Img & " = ");
      if Value.Value /= null then
         Put (Value.Value.all);
      end if;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Access_Type) is
   begin
      Put ("{Access ");
      if Value.Value = null then
         Put ("<null>)");
      else
         Put (Value.Value.all & "}");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Array_Type) is
   begin
      Put ("{Array (");
      for J in 1 .. Value.Num_Dimensions loop
         Put (Value.Dimensions (J).First'Img & " .. "
              & Value.Dimensions (J).Last'Img);
         if J /= Value.Num_Dimensions then
            Put (", ");
         end if;
      end loop;

      Put (") of ");
      Print (Value.Item_Type.all);

      Put ("= (");
      if Value.Values /= null then
         for J in Value.Values'Range loop
            Put (Value.Values (J).Index'Img & " => ");
            Print (Value.Values (J).Value.all);
            if J /= Value.Values'Last then
               Put (", ");
            end if;
         end loop;
      end if;
      Put (")}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Repeat_Type) is
   begin
      Put ("{<" & Value.Repeat_Num'Img & " times> : ");
      if Value.Value /= null then
         Print (Value.Value.all);
         Put ("}");
      else
         Put ("<null>}");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Record_Type) is
   begin
      Put ("{Record: ");
      if Value.Fields'Length = 0 then
         Put ("null record");
      end if;
      for J in Value.Fields'Range loop
         if Value.Fields (J).Variant_Part /= null then
            Put ("<variant_part on "
                 & Value.Fields (J).Name.all
                 & "> => ");
            for P in Value.Fields (J).Variant_Part'Range loop
               Put ("{");
               Print (Value.Fields (J).Variant_Part (P).all);
               Put ("}");
            end loop;

         else
            Put (Value.Fields (J).Name.all & " => ");
            if Value.Fields (J).Value /= null then
               Print (Value.Fields (J).Value.all);
            end if;
         end if;
         if J /= Value.Fields'Last then
            Put (", ");
         end if;
      end loop;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Union_Type) is
   begin
      Put ("{Union: ");
      for J in Value.Fields'Range loop
         Put (Value.Fields (J).Name.all & " => ");
         if Value.Fields (J).Value /= null then
            Print (Value.Fields (J).Value.all);
         end if;
         if J /= Value.Fields'Last then
            Put (", ");
         end if;
      end loop;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Enum_Type) is
   begin
      Put ("{Enumeration = ");
      if Value.Value = null then
         Put ("<Unknown>}");
      else
         Put (Value.Value.all & "}");
      end if;
   end Print;

   -----------------
   -- Clear_Value --
   -----------------

   procedure Clear_Value (Value : in out Simple_Type) is
   begin
      Free (Value.Value);
   end Clear_Value;

   -----------------
   -- Clear_Value --
   -----------------

   procedure Clear_Value (Value : in out Array_Type) is
   begin
      if Value.Values /= null then
         for J in Value.Values'Range loop
            Clear_Value (Value.Values (J).Value.all);
         end loop;
         Free (Value.Values);
      end if;
   end Clear_Value;

   -----------------
   -- Clear_Value --
   -----------------

   procedure Clear_Value (Value : in out Repeat_Type) is
   begin
      if Value.Value /= null then
         Clear_Value (Value.Value.all);
      end if;
   end Clear_Value;

   -----------------
   -- Clear_Value --
   -----------------

   procedure Clear_Value (Value : in out Record_Type) is
   begin
      for J in Value.Fields'Range loop
         if Value.Fields (J).Value /= null then
            Clear_Value (Value.Fields (J).Value.all);
            if Value.Fields (J).Variant_Part /= null then
               for V in Value.Fields (J).Variant_Part'Range loop
                  Clear_Value (Value.Fields (J).Variant_Part (V).all);
               end loop;
            end if;
         end if;
      end loop;
   end Clear_Value;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Simple_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Simple_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Range_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Range_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Mod_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Mod_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Access_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Access_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Enum_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Enum_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Array_Type)
                  return Generic_Type_Access
   is
      R : Array_Type_Access := new Array_Type'(Value);
   begin
      R.Values := null;
      R.Item_Type := Clone (Value.Item_Type.all);
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Repeat_Type)
                  return Generic_Type_Access
   is
      R : Repeat_Type_Access := new Repeat_Type'(Value);
   begin
      R.Value := Clone (Value.Value.all);
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Record_Type)
                  return Generic_Type_Access
   is
      R : Record_Type_Access := new Record_Type'(Value);
   begin
      for J in R.Fields'Range loop
         R.Fields (J).Name := new String'(R.Fields (J).Name.all);
         R.Fields (J).Value := Clone (R.Fields (J).Value.all);
         if R.Fields (J).Variant_Part /= null then
            R.Fields (J).Variant_Part
              := new Record_Type_Array'(R.Fields (J).Variant_Part.all);
            for V in R.Fields (J).Variant_Part'Range loop
               R.Fields (J).Variant_Part (V) := Record_Type_Access
                 (Clone (R.Fields (J).Variant_Part (V).all));
            end loop;
         end if;
      end loop;
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Union_Type)
                  return Generic_Type_Access
   is
      R : Union_Type_Access := new Union_Type'(Value);
   begin
      for J in R.Fields'Range loop
         R.Fields (J).Name := new String'(R.Fields (J).Name.all);
         R.Fields (J).Value := Clone (R.Fields (J).Value.all);
      end loop;
      return Generic_Type_Access (R);
   end Clone;

end Generic_Values;
