package body Glib.Values is
   procedure Init (Value : in out GValue; G_Type : Glib.GType) is
      pragma Unreferenced (Value, G_Type);
   begin
      null;
   end Init;

   procedure Set_Boxed (Value : in out GValue; V_Address : System.Address) is
      pragma Unreferenced (Value, V_Address);
   begin
      null;
   end Set_Boxed;

   function  Get_Boxed (Value : GValue) return System.Address is
      pragma Unreferenced (Value);
   begin
      return System.Null_Address;
   end Get_Boxed;
end Glib.Values;
