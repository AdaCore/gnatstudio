with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed;

package body Search_Callback is

   Cancelled : Boolean;

   procedure Abort_Search (State : Boolean := True) is
   begin
      Cancelled := State;
   end Abort_Search;

   function Callback
     (Match_Found : Boolean;
      File        : String;
      Line_Nr     : Positive := 1;
      Line_Text   : String   := "") return Boolean
   is
      use Ada.Strings.Fixed;
   begin
      if Match_Found then
         Put_Line (File
                   & ':' & Trim (Positive'Image (Line_Nr), Ada.Strings.Left)
                   & ':' & Line_Text);
      end if;

      if not Cancelled then
         return True;
      else
         Put_Line ("--- ABORTING !!!");
         return False;
      end if;
   end Callback;

end Search_Callback;
