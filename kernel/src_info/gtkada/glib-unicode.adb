with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Glib.Unicode is
   function Is_Alpha (Char : Gunichar) return Boolean is
   begin
      return Is_Letter (Character'Val (Char));
   end Is_Alpha;

   function Is_Alnum (Char : Gunichar) return Boolean is
   begin
      return Is_Alphanumeric (Character'Val (Char));
   end Is_Alnum;

   function UTF8_Find_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural
   is
      pragma Unreferenced (Str);
   begin
      return Index + 1;
   end UTF8_Find_Next_Char;

   function UTF8_Get_Char (Str : UTF8_String) return Gunichar is
   begin
      return Character'Pos (Str (Str'First));
   end UTF8_Get_Char;

   function UTF8_Strdown (Str : UTF8_String) return UTF8_String is
   begin
      return To_Lower (Str);
   end UTF8_Strdown;
end Glib.Unicode;
