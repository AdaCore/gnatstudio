with Ada.Assertions;
with Matrix_Math; use Matrix_Math;

procedure Main is

   Matrix_1 : constant Matrix :=
     (  ( 1.0,  1.0,  1.0 ),
        ( 2.0,  4.0,  8.0 ),
        ( 4.0, 16.0, 64.0 )
     );

   Matrix_2 : constant Matrix :=
     (  (       4.0,      -3.0,  4.0/3.0 ),
        ( -13.0/3.0,  19.0/4.0, -7.0/3.0 ),
        (  -1.0/6.0,   1.0/4.0, -1.0/6.0 )
     );

   Matrix_3 : Matrix(1..3,1..3);

begin

   -- A good multiplication
   Matrix_3 := Matrix_1 * Matrix_2;

   -- Put a breakpoint here to examine Matrix_3 in memory using the
   -- GDB debugger with GNATemulator.
   Ada.Assertions.Assert(Check => 100 mod 2 = 0);

end Main;
