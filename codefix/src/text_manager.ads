with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package Text_Manager is

   ----------------------------------------------------------------------------
   --  type Dynamic_String
   ----------------------------------------------------------------------------

   --  All the functions of affectation (Affect and Get_Line) destroy the
   --  precedent element in the Dynamic_String. If you don't want to delete
   --  the precedent string, you have to initialize the Dynamic_String with
   --  the value null before calling these functions.

   Text_Manager_Error : exception;

   type Dynamic_String is access String;

   procedure Affect (This : in out Dynamic_String; Value : String);
   --  Delete the precedent string, and create a new initialized with Value.

   procedure Affect (This : in out Dynamic_String; Value : Dynamic_String);
   --  Delete the precedent string This, and create a new initialized with a
   --  copy of Value.

   procedure Get_Line (This : in out Dynamic_String);
   --  Delete the precedent string, and create a new initialized with the line
   --  red on the standart input.

   procedure Get_Line (File : File_Type; This : in out Dynamic_String);
   --  Delete the precedent string, and create a new initialized with the line
   --  red File.

   procedure Put_Line (This : Dynamic_String);
   --  Put the string referenced on the standart output.

   procedure Put_Line (File : File_Type; This : Dynamic_String);
   --  Put the string referenced on the file specified.

   procedure Free is new Ada.Unchecked_Deallocation (String, Dynamic_String);

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

   procedure Free (This : in out Text_Interface) is abstract;

   function Get
     (This   : Text_Interface;
      Cursor : File_Cursor'Class;
      Len    : Natural)
     return String is abstract;
   --  Get Len characters from the file and the position specified by the
   --  cursor.

   function Get_Line
     (This   : Text_Interface;
      Cursor : File_Cursor'Class)
     return String is abstract;
   --  Get all character from the file and the column specified by the cursor
   --  to the end of the line.

   procedure Replace
     (This      : in out Text_Interface;
      Cursor    : File_Cursor'Class;
      Len       : Natural;
      New_Value : String) is abstract;
   --  Replace the Len characters, from the position designed by the cursor, by
   --  New_Value

   procedure Add_Line
     (This        : in out Text_Interface;
      Cursor      : File_Cursor'Class;
      New_Line    : String) is abstract;
   --  Add a line at the cursor specified. To add a line at the
   --  begining of the text, set cursor line = 0.

   procedure Delete_Line
     (This : in out Text_Interface;
      Cursor : File_Cursor'Class) is abstract;
   --  Delete the line where the cursor is.

   function Line_Length
     (This   : Text_Interface'Class;
      Cursor : File_Cursor'Class)
     return Natural;
   --  Returns le length of a line from the position of the cursor.

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

   type Extract_Line is private;
   type Ptr_Extract_Line is access Extract_Line;

   function Get_String (This : Extract_Line) return String;
   function Get_Cursor (This : Extract_Line) return File_Cursor'Class;

   --  PAS FINIE !!!
   procedure Update (This         : Extract_Line;
                     Current_Text : in out Text_Interface'Class;
                     Offset_Line  : in out Natural);

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
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract);

   procedure Get_Line
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract);

   function Get_String (This : Extract; Position : Natural := 1) return String;

   procedure Set_String
     (This     : Extract;
      Value    : String;
      Position : Natural := 1);

   --  PAS FINIE !!!
   procedure Update
     (This         : Extract;
      Current_Text : in out Text_Interface'Class;
      Offset_Line  : in out Natural);

   procedure Put_Line (This : Extract);
   procedure Put_Line_Original
     (This : Extract;
      Current_Text : Text_Interface'Class);

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : Text_Cursor'Class;
      New_String   : String;
      Format       : String := "(^[\w]*)");

   procedure Add_Word
     (This   : in out Extract;
      Cursor : Text_Cursor'Class;
      Word   : String);

   function Get_Word_Length
     (This   : Extract;
      Cursor : Text_Cursor'Class;
      Format : String)
     return Natural;

   procedure Free (This : in out Extract);

   function Get_Line (This : Extract; Number : Natural)
     return Ptr_Extract_Line;
   --  Returns the line with the number specified in the original text.

   function Get_Record (This : Extract; Number : Natural)
     return Ptr_Extract_Line;
   --  Returns the line recorded at the position Number in the extract.

   function Get_Number_Lines (This : Extract) return Natural;

   procedure New_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Text   : String);

private

   type Text_Interface is abstract tagged null record;

   type Line_Context is
     (Original_Line,
      Line_Modified,
      Line_Created,
      Line_Deleted);

   type Extract_Line (Context : Line_Context := Original_Line) is record
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

   procedure Put_Line (This : Extract_Line);
   procedure Put_Line_Original
     (This : Extract_Line;
      Current_Text : Text_Interface'Class);

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

end Text_Manager;
