
package body Glib.Convert is
   function Locale_To_UTF8 (OS_String : String) return String is
   begin
      return OS_String;
   end Locale_To_UTF8;

   function Locale_From_UTF8 (UTF8_String : String) return String is
   begin
      return UTF8_String;
   end Locale_From_UTF8;
end Glib.Convert;
