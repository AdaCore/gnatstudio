package Glib.Unicode is
   function Is_Alpha (Char : Gunichar) return Boolean;
   function Is_Alnum (Char : Gunichar) return Boolean;
   function Utf8_Find_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   function Utf8_Get_Char (Str : UTF8_String) return Gunichar;
   function Utf8_Strdown (Str : UTF8_String) return UTF8_String;
end Glib.Unicode;
