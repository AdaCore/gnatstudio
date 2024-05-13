with Ada.Text_IO;

procedure Main is
   procedure Print (I : Integer; B : Boolean) is
   begin
      Ada.Text_IO.Put_Line ("Hello" & B'Img & I'Img);
      Ada.Text_IO.Put_Line ("Hello" & B'Img & I'Img);
      Ada.Text_IO.Put_Line ("Hello" & B'Img & I'Img);
   end;

   I : constant Integer := 0;
   B : constant Boolean := False;
   Arguments : constant Integer := 5;

   type Rec is record
      A : Integer := 1;
      B : Boolean := False;
   end record;

   R : Rec;

   type New_Rec is record
     R1 : Rec;
     R2 : Rec;
   end record;

   NR : New_Rec;

begin
   Print (I, B);
end Main;
