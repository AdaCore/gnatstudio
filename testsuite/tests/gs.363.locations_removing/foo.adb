with A;

procedure Foo is
   type Bar is new Integer range 1 .. 100;
   type Baz (D : Integer) is record
      B : Boolean;
      case D is
         when 0 =>
            C0A : Integer;
         when 1 =>
            C1A, C1B : Integer;
         when 2 =>
            C2A, C2B, C3C : Integer;
         when others =>
            null;
      end case;
   end record;
begin
   A.E := 42;
   A.C;
end Foo;
