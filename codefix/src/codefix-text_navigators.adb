package body Codefix.Text_Navigators is

   function New_Text_Interface (This : Text_Navigator) return Ptr_Text is
   begin
      return new Unique_File;
   end New_Text_Interface;

end Codefix.Text_Navigators;
