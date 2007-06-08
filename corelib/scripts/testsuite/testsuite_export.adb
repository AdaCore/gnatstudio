with Scripts;        use Scripts;

package body Testsuite_Export is

   procedure On_Hello   (Data : in out Callback_Data'Class; Command : String);
   procedure C1_Handler (Data : in out Callback_Data'Class; Command : String);

   --------------
   -- On_Hello --
   --------------

   procedure On_Hello (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, "Hello " & Nth_Arg (Data, 1, "world") & " !");
   end On_Hello;

   ----------------
   -- C1_Handler --
   ----------------

   procedure C1_Handler
      (Data : in out Callback_Data'Class; Command : String)
   is
      --  This could also be kept as a variable somewhere, but fetching it
      --  is relatively cheap
      C1 : constant Class_Type := New_Class (Get_Repository (Data), "C1");

      Inst : Class_Instance := Nth_Arg (Data, 1, C1);
   begin
      if Command = Constructor_Method then
         Set_Data (Inst, C1, Integer'(Nth_Arg (Data, 2)));

      elsif Command = "method" then
         Set_Return_Value
            (Data, "Method applied to class"
             & Integer'Image (Get_Data (Inst, C1)) & " with param "
             & Nth_Arg (Data, 2));
      end if;
   end C1_Handler;

   ------------------------
   -- Register_Functions --
   ------------------------

   procedure Register_Functions (Repo : Scripts.Scripts_Repository) is
      C1 : Class_Type;
   begin
      Register_Command
        (Repo, "hello", 0, 1,
         Handler => On_Hello'Access);

      C1 := New_Class (Repo, "C1");
      Register_Command
        (Repo, Constructor_Method, 1, 1,
         Class   => C1,
         Handler => C1_Handler'Access);
      Register_Command
        (Repo, "method", 0, 1,
         Class   => C1,
         Handler => C1_Handler'Access);
   end Register_Functions;

end Testsuite_Export;
