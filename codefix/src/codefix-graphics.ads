with Glib;                   use Glib;

with Glide_Kernel;           use Glide_Kernel;
with Vdiff_Pkg;              use Vdiff_Pkg;

with Codefix;                use Codefix;
with Codefix.Text_Manager;   use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Errors_Parser;  use Codefix.Errors_Parser;
with Codefix.Formal_Errors;  use Codefix.Formal_Errors;
with Codefix.File_Io;        use Codefix.File_Io;
with Codefix.Text_Navigators;
use Codefix.Formal_Errors.Extract_List;

with Codefix_Window_Pkg;     use Codefix_Window_Pkg;

package Codefix.Graphics is

   package Navigator is new Text_Navigators (File_Interface);
   use Navigator;

   Display_Lines_Before : constant Integer := 3;
   Display_Lines_After  : constant Integer := 3;

   type Graphic_Codefix_Record is new Codefix_Window_Record with record
      Current_Text       : Text_Navigator;
      Corrector          : Correction_Manager;
      Errors_Found       : Errors_File;
      Successful_Update : Boolean;
      Nb_Tabs            : Integer := 0;
      Current_Error      : Error_Id := Null_Error_Id;
      Kernel             : Kernel_Handle;
   end record;

   type Graphic_Codefix_Access is access all Graphic_Codefix_Record;

   procedure Gtk_New (Graphic_Codefix : out Graphic_Codefix_Access);

   procedure Initialize
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);

   procedure Free (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Free the memory associated to a Graphic_Codefix.

   procedure Quit (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Terminate the programm.

   procedure Next_Choice
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Display the next choice of solution for the current error.

   procedure Prev_Choice
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Display the previous choice of solution for the current error.

   procedure Load_Next_Error
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Load on the window the next error, an solutions associated to.

   procedure Valid_Current_Solution
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Valid the current solution chosen by the user.

   function Get_Nth_Solution
     (Graphic_Codefix : access Graphic_Codefix_Record'Class) return Gint;
   --  Return the number of the current solution in the solution list.

end Codefix.Graphics;
