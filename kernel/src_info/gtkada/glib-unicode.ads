package Glib.Unicode is
   function Is_Alpha (Char : Gunichar) return Boolean;
   function Is_Alnum (Char : Gunichar) return Boolean;
   function UTF8_Find_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   function UTF8_Get_Char (Str : UTF8_String) return Gunichar;
   function UTF8_Strdown (Str : UTF8_String) return UTF8_String;
end Glib.Unicode;
