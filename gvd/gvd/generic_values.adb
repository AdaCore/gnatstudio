with GNAT.IO;  use GNAT.IO;

package body Generic_Values is

   package body Array_Indexes is

      ---------------------
      -- Compute_Indexes --
      ---------------------

      function Compute_Indexes (I : Indexes) return Integer is
         Index : Integer := I (I'First);
      begin
         for J in 2 .. For_Array.Num_Dimensions loop
            Index := Index * (For_Array.Dimensions (J).Last
                              - For_Array.Dimensions (J).First + 1)
              + I (I'First + J - 1);
         end loop;
         return Index;
      end Compute_Indexes;

   end Array_Indexes;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Simple_Type) is
   begin
      if Value.Value = null then
         Put_Line ("Simple: <null>");
      else
         Put_Line ("Simple: " & Value.Value.all);
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Access_Type) is
   begin
      Put ("Access");
      if Value.Value = null then
         Put_Line ("<null>");
      else
         Put_Line (Value.Value.all);
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Array_Type) is
   begin
      Put ("Array (");
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
      Put_Line (")");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Repeat_Type) is
   begin
      Put ("<repeat " & Value.Repeat_Num'Img & " times> : ");
      if Value.Value /= null then
         Print (Value.Value.all);
      else
         Put ("<null>");
      end if;
      New_Line;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Record_Type) is
   begin
      Put ("Record: (");
      for J in Value.Fields'Range loop
         Put (Value.Fields (J).Name.all & " => ");

         if Value.Fields (J).Variant_Part /= null then
            Put ("<" & Value.Fields (J).Variant_Part'Length'Img
                 & " variant_part>:");
            for P in Value.Fields (J).Variant_Part'Range loop
               Put ("(");
               Print (Value.Fields (J).Variant_Part (P).all);
               Put_Line (")");
            end loop;

         else
            if Value.Fields (J).Value /= null then
               Print (Value.Fields (J).Value.all);
            end if;
         end if;
         if J /= Value.Fields'Last then
            Put (", ");
         end if;
      end loop;
      Put_Line (")");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Enum_Type) is
   begin
      Put ("Enumeration = ");
      if Value.Value = null then
         Put_Line ("<Unknown>");
      else
         Put_Line (Value.Value.all);
      end if;
   end Print;

end Generic_Values;
