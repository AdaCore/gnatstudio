package Search_Callback is

   procedure Abort_Search (State : Boolean := True);
   --  Called with True, make the search aborting. Call it with False before
   --  searching.

   function Callback
     (Match_Found : Boolean;
      File        : String;
      Line_Nr     : Positive := 1;
      Line_Text   : String   := "") return Boolean;
   --  Print every match 'file:line:text'.
   --  Ignore file calls.

end Search_Callback;
