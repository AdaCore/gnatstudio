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
   --  Free the memory associated with the File_Interface

   function Get
     (This   : File_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural)
     return String;
   --  Get Len characters from the file and the position specified by the
   --  cursor.

   function Get_Line
     (This   : File_Interface;
      Cursor : Text_Cursor'Class)
      return String;
   --  Get all character from the column and the line specified by the cursor
   --  to the end of the line.

   procedure Replace
     (This      : in out File_Interface;
      Cursor    : Text_Cursor'Class;
      Len       : Natural;
      New_Value : String);
   --  Replace the Len characters, from the position designed by the cursor, by
   --  New_Value.

   procedure Add_Line
     (This        : in out File_Interface;
      Cursor      : Text_Cursor'Class;
      New_Line    : String);
   --  Add a line at the cursor specified. To add a line at the
   --  begining of the text, set cursor line = 0.

   procedure Delete_Line
     (This : in out File_Interface;
      Cursor : Text_Cursor'Class);
   --  Delete the line where the cursor is.

   procedure Initialize
     (This : in out File_Interface;
      Path : String);
   --  Initialize the structure of the Text_Interface. Get all data from the
   --  disk.

   function Read_File
     (This : File_Interface)
     return Dynamic_String;
   --  Get the entire file in a Dynamic_String.

   procedure Update (This : File_Interface);
   --  Save the file.

   function Line_Max (This : File_Interface) return Natural;

   type Errors_File is new Errors_Interface with private;
   --  This type is an interface to the list of compilation errors that the
   --  compilator has found.

   procedure Free (This : in out Errors_File);
   --  Free the memory associated to an Error_File.

   procedure Get_Direct_Message
     (This    : in out Errors_File;
      Current : out Error_Message);
   --  Gets a message without any modification of cols or lines numbers.

   function No_More_Messages (This : Errors_File) return Boolean;
   --  Is true where all the messages are got fron Get_Message.

   procedure Open (This : in out Errors_File; File_Name : String);
   --  Opens the file where errors are recorded.

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
   procedure Free
      is new Ada.Unchecked_Deallocation (File_Type, File_Type_Access);

   type Errors_File is new Errors_Interface with record
      File    : File_Type_Access;
      Is_Open : Boolean := False;
   end record;

end Codefix.File_Io;
