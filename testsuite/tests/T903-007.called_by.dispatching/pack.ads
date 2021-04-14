package Pack is
   type Abstract_Record is abstract tagged
      record
         I : Integer;
      end record;
   procedure Print_I (X : Abstract_Record) is abstract;
   procedure Hello (X : Abstract_Record);

   type My_Record is new Abstract_Record with null record;
   procedure Print_I (X : My_Record);
   overriding procedure Hello (X : My_Record);
end Pack;
