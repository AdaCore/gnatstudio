package Glib.Convert is
   function Locale_To_UTF8 (OS_String : String) return String;
   function Locale_From_UTF8 (UTF8_String : String) return String;
end Glib.Convert;
