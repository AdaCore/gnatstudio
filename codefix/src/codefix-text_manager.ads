with Ada.Unchecked_Deallocation;

with Ada_Analyzer; use Ada_Analyzer;
with Language; use Language;

with Generic_List;

package Codefix.Text_Manager is

   Text_Manager_Error : exception;

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   type Text_Cursor is tagged record
      Line, Col : Natural;
   end record;

   type File_Cursor is new Text_Cursor with record
      File_Name : Dynamic_String;
   end record;

   procedure Free (This : in out File_Cursor);
   --  Frees the memory used by fields of File_Cursor.

   function Clone (This : File_Cursor) return File_Cursor;

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   type Text_Interface is abstract tagged private;
   type Ptr_Text is access all Text_Interface'Class;
   procedure Free (This : in out Ptr_Text);

   procedure Initialize
     (This      : in out Text_Interface;
      File_Name : String) is abstract;

   procedure Free (This : in out Text_Interface) is abstract;

   function Get
     (This   : Text_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural)
     return String is abstract;
   --  Get Len characters from the file and the position specified by the
   --  cursor.

   function Get_Line
     (This   : Text_Interface;
      Cursor : Text_Cursor'Class)
     return String is abstract;
   --  Get all character from the file and the column specified by the cursor
   --  to the end of the line.

   procedure Replace
     (This      : in out Text_Interface;
      Cursor    : Text_Cursor'Class;
      Len       : Natural;
      New_Value : String) is abstract;
   --  Replace the Len characters, from the position designed by the cursor, by
   --  New_Value

   procedure Add_Line
     (This        : in out Text_Interface;
      Cursor      : Text_Cursor'Class;
      New_Line    : String) is abstract;
   --  Add a line at the cursor specified. To add a line at the
   --  begining of the text, set cursor line = 0.

   procedure Delete_Line
     (This : in out Text_Interface;
      Cursor : Text_Cursor'Class) is abstract;
   --  Delete the line where the cursor is.

   function Line_Length
     (This   : Text_Interface'Class;
      Cursor : Text_Cursor'Class)
     return Natural;
   --  Returns le length of a line from the position of the cursor.

   function Read_File
     (This : Text_Interface)
   return Dynamic_String is abstract;

   function Get_File_Name (This : Text_Interface) return String;

   procedure Update (This : Text_Interface) is abstract;

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   type Text_Navigator_Abstr is abstract tagged private;

   procedure Free (This : in out Text_Navigator_Abstr);

   function Get_Unit
     (Current_Text : Text_Navigator_Abstr;
      Cursor       : File_Cursor'Class)
   return Construct_Information;

   function Search_Body
     (Current_Text : Text_Navigator_Abstr;
      File_Name    : String;
      Spec         : Construct_Information)
     return Construct_Information;

   function Get
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class;
      Len    : Natural)
     return String;

   function Get_Line
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class)
     return String;

   function Read_File
     (This      : Text_Navigator_Abstr;
      File_Name : String) return Dynamic_String;

   procedure Replace
     (This      : in out Text_Navigator_Abstr;
      Cursor    : File_Cursor'Class;
      Len       : Natural;
      New_Value : String);

   procedure Add_Line
     (This        : in out Text_Navigator_Abstr;
      Cursor      : File_Cursor'Class;
      New_Line    : String);

   procedure Delete_Line
     (This : in out Text_Navigator_Abstr;
      Cursor : File_Cursor'Class);

   function Line_Length
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class)
     return Natural;

   procedure Update (This : Text_Navigator_Abstr);

   function New_Text_Interface (This : Text_Navigator_Abstr)
      return Ptr_Text is abstract;

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

   type Extract_Line is private;
   type Ptr_Extract_Line is access Extract_Line;

   function Get_String (This : Extract_Line) return String;
   function Get_Cursor (This : Extract_Line) return File_Cursor'Class;

   --  PAS FINIE !!!
   procedure Update (This         : Extract_Line;
                     Current_Text : in out Text_Navigator_Abstr'Class;
                     Offset_Line  : in out Integer);

   procedure Free (This : in out Extract_Line);

   function Clone
     (This      : Extract_Line;
      Recursive : Boolean := True)
   return Extract_Line;

   ----------------------------------------------------------------------------
   --  type Extract
   ----------------------------------------------------------------------------

   type Extract is private;

   type String_Mode is (Text_Ascii, Regular_Expression);

   function Clone (This : Extract) return Extract;

   procedure Get
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract);

   procedure Get_Line
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract);

   function Get_String (This : Extract; Position : Natural := 1) return String;
   --  Get the string of the line of an extract. Strings are ordonned in the
   --  order where they are recorded.

   procedure Set_String
     (This     : Extract;
      Value    : String;
      Position : Natural := 1);

   --  PAS FINIE !!!
   procedure Update
     (This         : Extract;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Offset_Line  : in out Natural);

   procedure Put_Line (This : Extract);
   procedure Put_Line_Original
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class);

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Format       : String := "(^[\w]*)");

   procedure Add_Word
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Word   : String);

   function Get_Word_Length
     (This   : Extract;
      Cursor : File_Cursor'Class;
      Format : String)
     return Natural;

   procedure Free (This : in out Extract);

   function Get_Line (This : Extract; Position : File_Cursor)
     return Ptr_Extract_Line;
   --  Returns the line with the number specified in the original text.

   function Get_Record (This : Extract; Number : Natural)
     return Ptr_Extract_Line;
   --  Returns the line recorded at the position Number in the extract.

   function Get_Number_Lines (This : Extract) return Natural;
   --  Returns the number of the lines in the extract.

   procedure Add_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Text   : String);
   --  Add a line AFTER the line specified by the cursor. Make a cursor with
   --  0 for the line number to add a line at the begenning of the file.

   procedure Delete_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class);
   --  Delete the line of the extract at the line number and in the file
   --  specified by the cursor.

   procedure Delete_All_Lines (This : in out Extract);
   --  Delete all the lines from the extract

   procedure Get_Entity
     (This : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor : File_Cursor);

private

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   package Text_List is new Generic_List (Ptr_Text);
   use Text_List;

   type Ptr_List_Text is access Text_List.List;

   type Text_Navigator_Abstr is abstract tagged record
      Files : Ptr_List_Text := new Text_List.List;
   end record;

   function Get_File
     (This : Text_Navigator_Abstr'Class;
      Name : String)
   return Ptr_Text;
   --  Returns the existent file interface, or create a new one if it
   --  doesn't exists.


   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   type Text_Interface is abstract tagged record
      Tokens_List : Construct_List_Access := new Construct_List;
      File_Name   : Dynamic_String;
   end record;

   function Get_Unit
     (Current_Text : Text_Interface;
      Cursor       : Text_Cursor'Class)
   return Construct_Information;

   function Search_Body
     (Current_Text : Text_Interface;
      Spec         : Construct_Information)
   return Construct_Information;

   type Line_Context is
     (Original_Line,
      Line_Modified,
      Line_Created,
      Line_Deleted);

   type Extract_Line is record
      Context         : Line_Context := Original_Line;
      Cursor          : File_Cursor;
      Original_Length : Natural := 0;
      Content         : Dynamic_String;
      Next            : Ptr_Extract_Line;
   end record;

   procedure Add_Element
    (This, Precedent, Element : Ptr_Extract_Line;
     Container : in out Extract);
   procedure Free is
      new Ada.Unchecked_Deallocation (Extract_Line, Ptr_Extract_Line);

   procedure Get
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract_Line);

   procedure Put_Line (This : Extract_Line; Detail : Boolean := True);
   procedure Put_Line_Original
     (This         : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class);

   type Extract is record
      First : Ptr_Extract_Line;
   end record;

   procedure Add_Element (This : in out Extract; Element : Ptr_Extract_Line);

   function Get_Word_Length
     (This   : Extract_Line;
      Col    : Natural;
      Format : String)
     return Natural;

   function Length (This : Extract_Line) return Natural;

end Codefix.Text_Manager;
