with GNATCOLL.Scripts; use GNATCOLL.Scripts;
with GPS.Kernel.Scripts;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Aliases_Module.Scripts is

   Alias_Class_Name : constant String := "Alias";

   --  Static alias getter for the Alias class
   procedure Aliases_Static_Get
     (Data    : in out Callback_Data'Class;
      Command : String);

   ------------------------
   -- Aliases_Static_Get --
   ------------------------

   procedure Aliases_Static_Get
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Alias_Name : constant Unbounded_String := Nth_Arg (Data, 1);
      Alias : constant Alias_Info := Get_Alias (Alias_Name);
      Target_Class : constant Class_Type :=
        Data.Get_Repository.New_Class (Alias_Class_Name);
      Alias_Instance : constant Class_Instance :=
        Data.Get_Script.New_Instance (Target_Class);
   begin
      if Alias /= No_Alias_Info then
         Set_Property
           (Alias_Instance, "name", To_String (Alias.Name));
         Set_Property
           (Alias_Instance, "expansion", To_String (Alias.Expansion));
         Set_Return_Value (Data, Alias_Instance);
      end if;
   end Aliases_Static_Get;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : GPS.Kernel.Kernel_Handle) is
      Target_Class : constant Class_Type :=
        New_Class (Kernel.Scripts, Alias_Class_Name);
   begin
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "get",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Target_Class,
         Handler      => Aliases_Static_Get'Access,
         Static_Method => True);
   end Register_Commands;

end Aliases_Module.Scripts;
