with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Exceptions;           use Ada.Exceptions;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Gdk.Color;                use Gdk.Color;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.List;                 use Gtk.List;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Label;                use Gtk.Label;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Text;                 use Gtk.Text;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Window;               use Gtk.Window;

with Diff_Utils;               use Diff_Utils;
with Vdiff_Pkg;                use Vdiff_Pkg;
with Vdiff_Utils;              use Vdiff_Utils;
with Basic_Types;              use Basic_Types;

with Codefix;                  use Codefix;
with Codefix.Text_Manager;     use Codefix.Text_Manager;
with Codefix.Errors_Manager;   use Codefix.Errors_Manager;
with Codefix.Errors_Parser;    use Codefix.Errors_Parser;
with Codefix.Formal_Errors;    use Codefix.Formal_Errors;
use Codefix.Formal_Errors.Extract_List;
with Codefix.File_Io;          use Codefix.File_Io;
with Codefix.Text_Navigators;

with Gen_Proposition_Pkg;      use Gen_Proposition_Pkg;
with Final_Window_Pkg;         use Final_Window_Pkg;

with System.Storage_Elements;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Codefix.Graphics is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Graphic_Codefix : out Graphic_Codefix_Access) is
   begin
      Graphic_Codefix := new Graphic_Codefix_Record;
      Codefix.Graphics.Initialize (Graphic_Codefix);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Graphic_Codefix : access Graphic_Codefix_Record'Class) is
      Current_Id : Error_Id;
      Signal_Parameters : Signal_Parameter_Types (1 .. 0, 1 .. 0);

      --  type size_t is mod 2 ** Standard'Address_Size;
      --  type chars_ptr is new System.Storage_Elements.Integer_Address;
      --  type chars_ptr_array is array (size_t range <>) of chars_ptr;

      Signals : chars_ptr_array (1 .. 0);
      --  The list of signals defined for this object

      Kernel_Class : GObject_Class := Uninitialized_Class;

   begin
      Codefix_Window_Pkg.Initialize (Graphic_Codefix);

      Graphic_Codefix.Kernel := new Kernel_Handle_Record;
      Glib.Object.Initialize (Graphic_Codefix.Kernel);
      Initialize_Class_Record
        (Graphic_Codefix.Kernel,
         Signals,
         Kernel_Class,
         "GlideKernel",
         Signal_Parameters);

      Open (Graphic_Codefix.Errors_Found, Argument (1));

      Analyze
        (Graphic_Codefix.Corrector,
         Graphic_Codefix.Current_Text,
         Graphic_Codefix.Errors_Found,
         null);

      Remove_Page (Graphic_Codefix.Choices_Proposed, 0);

      Load_Next_Error (Graphic_Codefix);

   end Initialize;

   ----------
   -- Free --
   ----------

   procedure Free (Graphic_Codefix : access Graphic_Codefix_Record'Class) is
   begin
      Free (Graphic_Codefix.Current_Text);
      Free (Graphic_Codefix.Corrector);
      Free (Graphic_Codefix.Errors_Found);
      Free_Parsers;
   end Free;

   ----------
   -- Quit --
   ----------

   procedure Quit (Graphic_Codefix : access Graphic_Codefix_Record'Class) is
   begin
      Free (Graphic_Codefix);
      Gtk.Main.Main_Quit;
   end Quit;

   -----------------
   -- Next_Choice --
   -----------------

   procedure Next_Choice
     (Graphic_Codefix : access Graphic_Codefix_Record'Class) is
   begin
      Next_Page (Graphic_Codefix.Choices_Proposed);
   end Next_Choice;

   -----------------
   -- Prev_Choice --
   -----------------

   procedure Prev_Choice
     (Graphic_Codefix : access Graphic_Codefix_Record'Class) is
   begin
      Prev_Page (Graphic_Codefix.Choices_Proposed);
   end Prev_Choice;

   ---------------------
   -- Load_Next_Error --
   ---------------------

   procedure Load_Next_Error
     (Graphic_Codefix : access Graphic_Codefix_Record'Class) is

      procedure Display_Sol;
      procedure Modify_Tab;
      procedure Add_Tab;
      procedure Update_Fix_List;

      Solutions       : Solution_List;
      Current_Sol     : Extract_List.List_Node;
      Proposition     : Vdiff_Access;
      Label           : Gtk_Label;
      Current_Nb_Tabs : Integer;

      -----------------
      -- Display_Sol --
      -----------------

      procedure Display_Sol is

         Current_Line     : Ptr_Extract_Line;
         Extended_Extract : Extract;
         First_Iterator   : Text_Iterator_Access;
         Current_Iterator : Text_Iterator_Access;

      begin

         Extended_Extract := Clone (Data (Current_Sol));
         Extend_Before
           (Extended_Extract,
            Graphic_Codefix.Current_Text,
            Display_Lines_Before);
         Extend_After
           (Extended_Extract,
            Graphic_Codefix.Current_Text,
            Display_Lines_After);

         Current_Line := Get_First_Line (Extended_Extract);
         First_Iterator := new Text_Iterator;
         Current_Iterator := First_Iterator;

         if Current_Line = null then
            Free (Extended_Extract);
            return;
         end if;

         loop

            Current_Iterator.New_Line := new String'
              (Get_New_Text (Current_Line.all));

            if Get_Context (Current_Line.all) = Original_Line then
               Current_Iterator.Old_Line := new String'
                 (Current_Iterator.New_Line.all);
            elsif Get_Context (Current_Line.all) /= Line_Created then
               Current_Iterator.Old_Line := new String'(Get_Old_Text
                  (Current_Line.all, Graphic_Codefix.Current_Text));
            end if;

            Current_Iterator.Original_Position :=
              Get_Cursor (Current_Line.all).Line;

            case Get_Context (Current_Line.all) is
               when Original_Line =>
                  Current_Iterator.Action := Nothing;
               when Line_Modified =>
                  Current_Iterator.Action := Change;
               when Line_Created =>
                  Current_Iterator.Action := Append;
               when Line_Deleted =>
                  Current_Iterator.Action := Delete;
            end case;

            Current_Line := Next (Current_Line.all);

            exit when Current_Line = null;

            Current_Iterator.Next := new Text_Iterator;
            Current_Iterator := Current_Iterator.Next;

         end loop;

--         Current_Iterator.Next := null;
         Free (Current_Iterator.Next);

         Fill_Diff_Lists
           (Graphic_Codefix.Kernel,
            Proposition.Clist1,
            Proposition.Clist2,
            First_Iterator);

         Set_Text (Proposition.Label1, "Old text");
         Set_Text (Proposition.Label2, "Fixed text");
         Set_Text (Proposition.File_Label1,
                   Get_Cursor (Get_First_Line
                                 (Extended_Extract).all).File_Name.all);

         Set_Text (Proposition.File_Label2,
                   Get_Cursor (Get_First_Line
                                 (Extended_Extract).all).File_Name.all);

         Free (Extended_Extract);

      end Display_Sol;

      ----------------
      -- Modify_Tab --
      ----------------

      procedure Modify_Tab is
         --  Current_Container      : Gtk_Vbox;
         --  Old_Text, New_Text     : Gtk_Text;
         --  Old_Scroll, New_Scroll : Gtk_Scrolled_Window;

      begin

         null;

--         Current_Container := Gtk_Vbox
--           (Get_Nth_Page (Graphic_Codefix.Choices_Proposed,
--                          Gint (Current_Nb_Tabs)));

--         Old_Scroll := Gtk_Scrolled_Window (Get_Child (Current_Container, 0))
--         New_Scroll := Gtk_Scrolled_Window (Get_Child (Current_Container, 1))

--         Old_Text := Gtk_Text (Gtk.Widget.Widget_List.Get_Data
--                                 (Children (Old_Scroll)));

--         New_Text := Gtk_Text (Gtk.Widget.Widget_List.Get_Data
--                                 (Children (New_Scroll)));


--         Delete_Text (Old_Text);
--         Delete_Text (New_Text);

--         Display_Sol (New_Text, Old_Text);

      end Modify_Tab;

      -------------
      -- Add_Tab --
      -------------

      procedure Add_Tab is
      begin
         Gtk_New (Proposition);

         Gtk_New (Label, "Choice" & Integer'Image (Current_Nb_Tabs + 1));

         Display_Sol;

         Ref (Proposition.Main_Box);
         Remove (Proposition, Proposition.Main_Box);

         Append_Page
           (Graphic_Codefix.Choices_Proposed,
            Proposition.Main_Box,
            Label);

         Unref (Proposition.Main_Box);
      end Add_Tab;

      ---------------------
      -- Update_Fix_List --
      ---------------------

      procedure Update_Fix_List is
         New_Items       : String_List.Glist;
         Current_Extract : Extract_List.List_Node;

      begin

         Current_Extract := First
           (Get_Solutions (Graphic_Codefix.Current_Error));

         while Current_Extract /= Extract_List.Null_Node loop
            String_List.Append
              (New_Items, Get_Caption (Data (Current_Extract)));
            Current_Extract := Next (Current_Extract);
         end loop;

         Set_Popdown_Strings (Graphic_Codefix.Fix_Caption_List, New_Items);
      end Update_Fix_List;

      --  begin of Load_Next_Error

   begin

      if Graphic_Codefix.Current_Error /= Null_Error_Id then
         Graphic_Codefix.Current_Error := Next (Graphic_Codefix.Current_Error);
      else
         Graphic_Codefix.Current_Error :=
           Get_First_Error (Graphic_Codefix.Corrector);
      end if;

      if Graphic_Codefix.Current_Error = Null_Error_Id then
         declare
            Final_Window : Final_Window_Access;
         begin
            Gtk_New (Final_Window);
            Show_All (Final_Window);
            Final_Window.Graphic_Codefix := Graphic_Codefix_Access
              (Graphic_Codefix);
            return;
         end;
      end if;

      Current_Sol := First (Get_Solutions (Graphic_Codefix.Current_Error));

      Current_Nb_Tabs := 0;

      for J in 0 .. Graphic_Codefix.Nb_Tabs - 1 loop
         Remove_Page
           (Graphic_Codefix.Choices_Proposed,
            Gint (Current_Nb_Tabs));
      end loop;

      while Current_Sol /= Extract_List.Null_Node loop

         if Current_Nb_Tabs < Graphic_Codefix.Nb_Tabs then
            Add_Tab;
--            Modify_Tab;
         else
            Add_Tab;
         end if;

         Current_Sol := Next (Current_Sol);
         Current_Nb_Tabs := Current_Nb_Tabs + 1;

      end loop;

--      for J in Current_Nb_Tabs .. Graphic_Codefix.Nb_Tabs - 1 loop
--         Remove_Page
--           (Graphic_Codefix.Choices_Proposed,
--            Gint (Current_Nb_Tabs));
--      end loop;

      Graphic_Codefix.Nb_Tabs := Current_Nb_Tabs;
      Set_Text
        (Graphic_Codefix.Error_Caption,
         Get_Message (Get_Error_Message (Graphic_Codefix.Current_Error)));
      Update_Fix_List;

      Set_Current_Page (Graphic_Codefix.Choices_Proposed, 0);

      --  Maybe an other function to print changes in notebook ?
      Show_All (Graphic_Codefix);

   end Load_Next_Error;

   ----------------------------
   -- Valid_Current_Solution --
   ----------------------------

   procedure Valid_Current_Solution
     (Graphic_Codefix : access Graphic_Codefix_Record'Class) is

      Current_Extract : Extract_List.List_Node;

   begin

      Current_Extract := First (Get_Solutions (Graphic_Codefix.Current_Error));

      while Get_Caption (Data (Current_Extract)) /=
        Get_Text (Graphic_Codefix.Fix_Entry)
      loop

         Current_Extract := Next (Current_Extract);

         if Current_Extract = Extract_List.Null_Node then
            Raise_Exception
              (Codefix_Panic'Identity,
               "Text """ &
                 Get_Text (Graphic_Codefix.Fix_Entry) &
               """ red in graphic entry not found in intern solution list");
         end if;

      end loop;

      Validate
        (Graphic_Codefix.Corrector,
         Graphic_Codefix.Current_Error,
         Data (Current_Extract));

   end Valid_Current_Solution;

   ----------------------
   -- Get_Nth_Solution --
   ----------------------

   function Get_Nth_Solution
     (Graphic_Codefix : access Graphic_Codefix_Record'Class) return Gint is

      Current_Extract : Extract_List.List_Node;
      Current_Number  : Gint := 1;

   begin

      Current_Extract := First (Get_Solutions (Graphic_Codefix.Current_Error));

      while Get_Caption (Data (Current_Extract)) /=
        Get_Text (Graphic_Codefix.Fix_Entry)
      loop

         Current_Number := Current_Number + 1;
         Current_Extract := Next (Current_Extract);

         if Current_Extract = Extract_List.Null_Node then
            Raise_Exception
              (Codefix_Panic'Identity,
               "Text """ &
                 Get_Text (Graphic_Codefix.Fix_Entry) &
               """ red in graphic entry not found in intern solution list");
         end if;

      end loop;

      return Current_Number;

   end Get_Nth_Solution;

end Codefix.Graphics;
