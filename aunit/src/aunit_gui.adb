with Gtk; use Gtk;
with Gtk.Main;
with Make_Harness_Window_Pkg; use Make_Harness_Window_Pkg;
with Make_Suite_Window_Pkg; use Make_Suite_Window_Pkg;
with Make_Test_Window_Pkg; use Make_Test_Window_Pkg;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Aunit_Gui is
   Make_Harness_Window : Make_Harness_Window_Access;
   Make_Suite_Window : Make_Suite_Window_Access;
   Make_Test_Window : Make_Test_Window_Access;

   type Window_Type is (Harness, Suite, Test);

   Arg : Window_Type;

begin
   if Argument_Count /= 1 then
      return;
   end if;

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Arg := Window_Type'Value (Argument (1));

   case Arg is
      when Harness =>
         Gtk_New (Make_Harness_Window);
         Show_All (Make_Harness_Window);
         Gtk.Main.Main;

         if Make_Harness_Window.Procedure_Name /= null then
            Put (Make_Harness_Window.Procedure_Name.all);
         end if;

      when Suite =>
         Gtk_New (Make_Suite_Window);
         Show_All (Make_Suite_Window);
         Gtk.Main.Main;

         if Make_Suite_Window.Name /= null then
            Put (Make_Suite_Window.Name.all);
         end if;

      when Test =>
         Gtk_New (Make_Test_Window);
         Show_All (Make_Test_Window);
         Gtk.Main.Main;

         if Make_Test_Window.Name /= null then
            Put (Make_Test_Window.Name.all);
         end if;
   end case;

exception
   when Constraint_Error =>
      null;
end Aunit_Gui;
