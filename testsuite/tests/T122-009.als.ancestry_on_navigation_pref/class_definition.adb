
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Tags;

package body Class_Definition is

   procedure Generic_P1 (E : Extended_A) is
   begin
      Put_Line
        ("Hi, I'm Generic_P1 for: "
            & Ada.Tags.Expanded_Name (Class_Definition.A'Class (E)'Tag));
   end Generic_P1;

end Class_Definition;
