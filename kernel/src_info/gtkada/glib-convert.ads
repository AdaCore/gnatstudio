with Interfaces.C.Strings; use Interfaces.C.Strings;
package Glib.Convert is
   function Locale_To_UTF8 (OS_String : String) return String;
   function Locale_From_UTF8 (UTF8_String : String) return String;
   function Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural) return chars_ptr;
end Glib.Convert;
