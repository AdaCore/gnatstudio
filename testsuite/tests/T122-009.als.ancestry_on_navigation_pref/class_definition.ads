


package Class_Definition is

   type A is abstract tagged null record;

   procedure P1 (Pa : A) is abstract;

   generic
      type Extended_A is new A with private;
   procedure Generic_P1 (E : Extended_A);

end Class_Definition;
