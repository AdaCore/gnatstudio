with Text_Manager; use Text_Manager;

package Messages is

   type Error_Message is new File_Cursor with private;

   procedure Initialize (This : in out Error_Message; Message : String);
   --  Parse the message headed in order to get the col number and the
   --  line number.

   procedure Initialize (This : in out Error_Message; Line, Col : Positive);

   function Get_Message (This : Error_Message) return String;
   --  Returns the message with the header.

   procedure Free (This : in out Error_Message);
   --  Frees the memory used by the object.

private

   type Error_Message is new File_Cursor with record
      Message : Dynamic_String;
   end record;

   procedure Parse_Head (Message : String; This : out Error_Message);
   function Clone (This : Error_Message) return Error_Message;

end Messages;
