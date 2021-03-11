package body Locpack is
   package body P is
      procedure Proc (X : in out Integer) is
      begin
         X := X + 1;
      end Proc;
   end P;
end Locpack;
