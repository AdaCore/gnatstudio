with Codefix.Text_Manager; use Codefix.Text_Manager;

generic

   type Unique_File is new Text_Interface with private;

package Codefix.Text_Navigators is

   type Text_Navigator is new Text_Navigator_Abstr with null record;

   function New_Text_Interface (This : Text_Navigator) return Ptr_Text;

end Codefix.Text_Navigators;
