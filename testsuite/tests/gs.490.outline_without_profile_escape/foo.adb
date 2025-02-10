with Ada.Text_IO;

procedure Foo is

   type Loc is record
      X : Integer;
      Y : Integer;
   end record;

   function "<" (Left, Right : Loc) return Boolean;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Loc) return Boolean is
   begin
      if Left.X = Right.X then
         return Left.Y < Right.Y;
      else
         return Left.X < Right.X;
      end if;
   end "<";

   A : constant Loc := (1, 2);
   B : constant Loc := (2, 1);

begin

   if A < B then
      Ada.Text_IO.Put_Line ("Hello World!");
   end if;

end Foo;
