with Generic_List; use Generic_List;

with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Errors_Parser; use Codefix.Errors_Parser;

package Codefix.Errors_Manager is

   ----------------------------------------------------------------------------
   --  type Error_Message
   ----------------------------------------------------------------------------

   type Error_Message is new File_Cursor with private;

   procedure Initialize (This : in out Error_Message; Message : String);
   --  Parse the message headed in order to get the col number and the
   --  line number.

   procedure Initialize (This : in out Error_Message; Line, Col : Positive);

   function Get_Message (This : Error_Message) return String;
   --  Returns the message with the header.

   procedure Free (This : in out Error_Message);
   --  Frees the memory used by the object.

   ----------------------------------------------------------------------------
   --  type Errors_Interface
   ----------------------------------------------------------------------------

   type Errors_Interface is abstract tagged private;

   function Get_Message (This : in out Errors_Interface) return Error_Message
      is abstract;

   function No_More_Messages (This : Errors_Interface) return Boolean
      is abstract;

   ----------------------------------------------------------------------------
   --  type Correction_Manager
   ----------------------------------------------------------------------------

   type Correction_Manager is private;
   type Error_Id is private;

   type Error_Callback is access procedure
     (Message   : Error_Message;
      Id        : Error_Id;
      Solutions : Solutions_List);

   procedure Analyze
     (This        : in out Correction_Manager;
      Source_Text : Text_Interface'Class;
      Errors_List : Errors_Interface'Class;
      Callback    : Error_Callback := null);

   procedure Validate
     (This         : in out Correction_Manager;
      Error        : Error_Id;
      Choice       : Natural;
      Later_Update : Boolean := True);

   subtype Alternative_Choice is Natural range 0 .. 2;

   type Ambiguous_Callback is access procedure
     (Alternative_1, Alternative_2 : Extract;
      Delete_Choice                : out Alternative_Choice);

   procedure Update
     (This     : in out Correction_Manager;
      Success  : out Boolean;
      Callback : Ambiguous_Callback := null);
   --  Check a certain quantity of things...
   --  (doubles modifs for example)

   procedure Check_Ambiguities
     (Solutions        : in out Solution_List;
      Callback         : Ambiguous_Callback;
      No_More_Problems : out Boolean);

   procedure Free (This : in out Correction_Manager);

private

   type Error_Message is new File_Cursor with record
      Message : Dynamic_String;
   end record;

   procedure Parse_Head (Message : String; This : out Error_Message);
   function Clone (This : Error_Message) return Error_Message;

   package Memorized_Corrections is new Generic_List (Solution_List);
   use Memorized_Corrections;

   type Error_Id is record
      Ptr_Solutions : Memorized_Corrections.List_Node
         := Memorized_Corrections.Null_Node;
   end record;

   type Correction_Manager (Current_Text : Ptr_Text) is record
      Potential_Corrections : Memorized_Corrections;
      Valid_Corrections  : Solution_List;
   end record;

   procedure Add_Error
     (This      : in out Correction_Manager;
      Solutions : Solution_List;
      New_Error : out Error_Id);

   package Line_List is new Generic_List (Extract_Line, Free);
   use Line_List;

   function Sort (List : Solution_List) return Line_List.List;

end Codefix.Errors_Manager;
