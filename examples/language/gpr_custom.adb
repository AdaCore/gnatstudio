with Language_Custom; use Language_Custom;

package body GPR_Custom is

   ------------------
   -- Comment_Line --
   ------------------

   function Comment_Line
     (Line : String; Comment : Boolean; Reserved : Integer) return chars_ptr
   is
   begin
      if Comment then
         declare
            S : String (1 .. Line'Length + 4);
         begin
            S (1 .. 4) := "--  ";
            S (5 .. Line'Length) := Line;

            return New_String (S);
         end;
      else
         if Line'Length > 4
           and then Line (Line'First .. Line'First + 3) = "--  "
         then
            return New_String (Line (Line'First + 4 .. Line'Last));
         else
            return New_String (Line);
         end if;
      end if;
   end Comment_Line;

end GPR_Custom;
