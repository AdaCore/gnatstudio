--  Penser a detruire les pointeurs sur texte

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

with Text_Manager; use Text_Manager;
with File_Io; use File_Io;
with Formal_Errors.Parser;
use Formal_Errors; use Parser;
use Formal_Errors.Extract_List;
with Messages; use Messages;
with Dynamic_Arrays;

procedure Gnatfix is

   package Int_Io is new Integer_IO (Integer);
   use Int_Io;

   procedure Print_Help;
   function Get_Number (Min, Max : Integer) return Integer;
   procedure Free_Objects;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
   begin
      Put_Line ("Gnatfix, version 0.1");
      Put_Line ("Use : ");
      Put_Line ("gnatfix <error file>");
   end Print_Help;

   ----------------
   -- Get_Number --
   ----------------

   function Get_Number (Min, Max : Integer) return Integer is
      Number : Integer;
   begin
      loop
         begin
            Get (Number);
            Skip_Line;
            exit when Number >= Min and then Number <= Max;
            Put_Line ("Number out of bounds, try again.");
         exception
            when Data_Error =>
               Skip_Line;
               Put_Line ("Wrong number format, try again.");
         end;
      end loop;
      return Number;
   end Get_Number;

   Current_Text     : Ptr_Text := new File_Interface;
   Errors_List      : Correction_Manager (Current_Text);
   Errors_File      : File_Type;
   Error_Red        : Error_Message;
   Solutions        : Solution_List;
   Current_Solution : List_Node;
   Num_Sol          : Natural;
   Str_Err_Red      : String (1 .. 256);
   Len_Err_Red      : Natural;

   procedure Free_Objects is
   begin
      Free (Error_Red);
      Free (Errors_List);
      Free (Current_Text.all);
      Free (Current_Text);
      Free_Parsers;
   end Free_Objects;

   type Str2 is new string;

begin

   if Argument_Count /= 1 then
      Print_Help;
      Free_Objects;
      return;
   end if;

   Open (Errors_File, In_File, Argument (1));

   while not End_Of_File (Errors_File) loop
      Get_Line (Errors_File, Str_Err_Red, Len_Err_Red);
      Initialize (Error_Red, Str_Err_Red (1 .. Len_Err_Red));

      Put_Line ("Message : " & Get_Message (Error_Red));
      Solutions := Get_Solutions (Errors_List, Error_Red);

      if Length (Solutions) > 0 then
         Current_Solution := First (Solutions);
         Put_Line ("Old text : ");
         Put_Line_Original
           (Data (Current_Solution),
            Errors_List.Current_Text.all);
         Num_Sol := 0;
         while Current_Solution /= Null_Node loop
            Num_Sol := Num_Sol + 1;
            Put_Line ("Proposition" & Integer'Image (Num_Sol) & " : ");
            Put_Line (Data (Current_Solution));
            Current_Solution := Next (Current_Solution);
         end loop;
         Put_Line ("What correction do you want ? (0 means none)");
         Num_Sol := Get_Number (0, Num_Sol);
         if Num_Sol /= 0 then
            null;
            Add_Correction
              (Errors_List,
               Clone (Get_Extract (Solutions, Num_Sol)));
         end if;
      else
         Put_Line ("Impossible automatic correction.");
      end if;

      Remove_Nodes (Solutions, Null_Node);
      New_Line;
   end loop;

   Update (Errors_List);
   Save (File_Interface (Errors_List.Current_Text.all));

   Free_Objects;
end Gnatfix;
