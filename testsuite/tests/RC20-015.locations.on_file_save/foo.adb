procedure Foo is

   procedure Bar (Ptr1, Ptr2 : access Integer) is
   begin
      if Ptr1 = null then
         Ptr1.all := Ptr1.all + 1; -- will generate a warning
      end if;

      if Ptr2 = null then
         Ptr2.all := Ptr2.all + 1; -- will generate a warning
      end if;
   end Bar;

   Ptr1 : access Integer := new Integer;
   Ptr2 : access Integer := new Integer;
begin
   Bar (Ptr1, Ptr2);
end Foo;
