
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Wide_Wide_Unbounded;
with Types;       use Types;

with A;
with B;

procedure Main is
   My_Text, Your_Text : Ada.Strings.Wide_Wide_Unbounded.
     Unbounded_Wide_Wide_String;

   V_A : A_Type;
   V_B : B_Type;

   AA, BB : Rec;
begin
   if My_Text = Your_Text then
      Put_Line ("Equal");
   end if;

   Put_Line (Boolean'Image (V_A = V_B));
   Put_Line (Boolean'Image (AA.A = BB.B));
end;
