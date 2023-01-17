--  Testing GNAT Studio identifier highlighting.

procedure Main is

  type R is record
    item_a : Integer;
    item_b : Integer;
    item_c : Integer;
  end record;

  type S is record
    item_a : R;
    item_b : R;
    item_c : R;
  end record;

  v : S;

begin
  v.item_a.item_a := 11;
  v.item_a.item_b := 12;
  v.item_a.item_c := 13;
  --
  v.item_b.item_a := 21;
  v.item_b.item_b := 22;
  v.item_b.item_c := 23;
  --
  v.item_c.item_a := 31;
  v.item_c.item_b := 32;
  v.item_c.item_c := 33;
end;
