with Ada.Text_IO;       use Ada.Text_IO;

package Html_Output is


   procedure Doc_HTML_Subtitle
     (File        : in Ada.Text_IO.File_Type;
      Title       : in String);


   procedure Doc_HTML_Exception
     (File        : in Ada.Text_IO.File_Type;
      Name        : in String;
      Rename      : in String;
      Description : in String);


   procedure Doc_HTML_Type
     (File : in Ada.Text_IO.File_Type;
      Name        : in String;
      Type_Name   : in String;
      Description : in String);


   procedure Doc_HTML_Subprogram
     (File : in Ada.Text_IO.File_Type;
      Name        : in String;
      Type_Name   : in String;
      Description : in String);


   procedure Header
     (File  : in Ada.Text_IO.File_Type;
      Title : in String);


   procedure Footer
     (File : in Ada.Text_IO.File_Type);

end Html_Output;
