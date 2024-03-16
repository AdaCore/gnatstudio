with Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;

procedure Main is
   Cmdline : Command_Line_Configuration;
   Value   : aliased Integer := -1;
begin
   Set_Usage (Cmdline, Help => "Basic command line example");
   Define_Switch
     (Cmdline, Output => Value'Access, Switch => "-i:",
      Long_Switch     => "--integer=", Help => "Prints the given integer");

   --  Parse the command line
   begin
      Getopt (Cmdline);
   exception
      when GNAT.Command_Line.Exit_From_Command_Line =>
         --  User provided -h or --help option
         return;
   end;

   Ada.Text_IO.Put_Line ("Value is:" & Value'Img);
end Main;
