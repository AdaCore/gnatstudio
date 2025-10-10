procedure T is
   function F1 (A : Boolean; B : Boolean) return Boolean is
     (True);

   function F2 (A : Boolean; B : Boolean) return Boolean is
     (False);
begin
   null;
end T;
