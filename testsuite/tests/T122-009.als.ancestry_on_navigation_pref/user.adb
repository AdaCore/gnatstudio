
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Tags;
with Class_Definition;

package body User is

   type C is new Class_Definition.A with
      record
         F : Integer := 0;
      end record;

   procedure P1 (Pc : C) is
   begin
      Put_Line
        ("Hi, I'm : "
         & Ada.Tags.Expanded_Name (Class_Definition.A'Class (Pc)'Tag));
   end P1;

   function Gen return Class_Definition.A'Class is
      Vc : C;
   begin
      return Class_Definition.A'Class (Vc);
   end Gen;

end User;
