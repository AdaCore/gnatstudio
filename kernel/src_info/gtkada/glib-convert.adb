
package body Glib.Convert is
   function Locale_To_UTF8 (OS_String : String) return String is
   begin
      return OS_String;
   end Locale_To_UTF8;

   function Locale_From_UTF8 (UTF8_String : String) return String is
   begin
      return UTF8_String;
   end Locale_From_UTF8;

   function Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural) return chars_ptr is
   begin
      Bytes_Read.all := UTF8_String'Length;
      Bytes_Written.all := UTF8_String'Length;
      return New_String (UTF8_String);
   end Locale_From_UTF8;
end Glib.Convert;
