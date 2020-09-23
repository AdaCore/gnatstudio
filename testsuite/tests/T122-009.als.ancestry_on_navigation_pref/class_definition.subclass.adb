
package body Class_Definition.Subclass is

   type B is new A with
      record
         F : Integer := 0;
      end record;

   procedure P1 (Pb : B);
   --  Comment for Class_Definition.Subclass.P1

   procedure Instansited_P1 is new Generic_P1 (Extended_A => B);

   procedure P1 (Pb : B) renames Instansited_P1;

   function Gen return A'Class is
      Vb : B;
   begin
      return A'Class (Vb);
   end Gen;

end Class_Definition.Subclass;
