package File_Buffer is

   procedure Init (File_Name : String);

   function Get_Line (Line : Integer) return String;

   procedure Done;

end File_Buffer;
