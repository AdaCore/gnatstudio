with Ada.Unchecked_Deallocation;

with Generic_List;

with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;

package Codefix.File_Io is

   type File_Interface is new Text_Interface with private;
   --  This is the implementation of the text interface for using in simple
   --  text files.

   type Ptr_Text_File is access all File_Interface'Class;

   procedure Free (This : in out File_Interface);

   function Get
     (This   : File_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural)
      return String;

   function Get_Line
     (This   : File_Interface;
      Cursor : Text_Cursor'Class)
      return String;

   procedure Replace
     (This      : in out File_Interface;
      Cursor    : Text_Cursor'Class;
      Len       : Natural;
      New_Value : String);

   procedure Add_Line
     (This        : in out File_Interface;
      Cursor      : Text_Cursor'Class;
      New_Line    : String);

   procedure Delete_Line
     (This : in out File_Interface;
      Cursor : Text_Cursor'Class);

   procedure Initialize
     (This : in out File_Interface;
      Path : String);

   function Read_File
     (This : File_Interface)
   return Dynamic_String;

   procedure Update (This : File_Interface);

   type Errors_File is new Errors_Interface with private;

   procedure Get_Direct_Message
     (This    : in out Errors_File;
      Current : out Error_Message);

   function No_More_Messages (This : Errors_File) return Boolean;

   procedure Open (This : in out Errors_File; File_Name : String);

private

   package List_Str is new Generic_List (Dynamic_String);
   use List_Str;

   type File_Interface is new Text_Interface with record
      Content   : List_Str.List;
   end record;

   function Get_Line_Node
     (This : File_Interface;
      Line : Positive) return List_Str.List_Node;

   type File_Type_Access is access all File_Type;

   type Errors_File is new Errors_Interface with record
      File    : File_Type_Access;
      Is_Open : Boolean := False;
   end record;

end Codefix.File_Io;
