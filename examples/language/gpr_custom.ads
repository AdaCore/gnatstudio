with Interfaces.C.Strings; use Interfaces.C.Strings;

package GPR_Custom is

   function Comment_Line
     (Line : String; Comment : Boolean; Reserved : Integer) return chars_ptr;
   pragma Export (Ada, Comment_Line, "gpr_comment_line");

end GPR_Custom;
