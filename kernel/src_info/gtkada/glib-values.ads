package Glib.Values is
   type GValue is new Integer;

   procedure Init (Value : in out GValue; G_Type : Glib.GType);
   procedure Set_Boxed (Value : in out GValue; V_Address : System.Address);
   function  Get_Boxed (Value : GValue) return System.Address;

end Glib.Values;
