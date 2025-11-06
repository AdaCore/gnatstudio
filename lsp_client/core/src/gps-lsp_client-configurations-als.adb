with GNATCOLL.JSON;

package body GPS.LSP_Client.Configurations.ALS is

   overriding function Configuration_Settings
     (Self : ALS_Configuration) return GNATCOLL.JSON.JSON_Value
   is
      pragma Unreferenced (Self);
   begin
      --  Minimal headless configuration: defer detailed settings to the TUI
      --  front-end once preferences are implemented.
      return GNATCOLL.JSON.Create_Object;
   end Configuration_Settings;

   overriding function Is_Configuration_Supported
     (Self    : ALS_Configuration;
      Setting : Setting_Kind)
      return Boolean
   is
      pragma Unreferenced (Self, Setting);
   begin
      return False;
   end Is_Configuration_Supported;

end GPS.LSP_Client.Configurations.ALS;
