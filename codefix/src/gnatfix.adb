--  Penser a detruire les pointeurs sur texte

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

with Codefix; use Codefix;
with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Errors_Parser; use Codefix.Errors_Parser;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;
with Codefix.File_Io; use Codefix.File_Io;
with Codefix.Text_Navigators;
use Codefix.Formal_Errors.Extract_List;
with Test_Lib; use Test_Lib;
use Test_Lib.Navigator;

procedure Gnatfix is

   procedure Print_Help;
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


   Current_Text       : Text_Navigator;
   Errors_List        : Correction_Manager;
   Errors_Found       : Errors_File;
   Successfull_Update : Boolean;

   procedure Free_Objects is
   begin
--      Free (Error_Red);
--      Free (Errors_List);
--      Free (Current_Text.all);
--      Free (Current_Text);
      Free_Parsers;
   end Free_Objects;

begin

   if Argument_Count /= 1 then
      Print_Help;
      Free_Objects;
      return;
   end if;

   Open (Errors_Found, Argument (1));
   Analyze
     (Errors_List,
      Current_Text,
      Errors_Found,
      Corrections_Proposed'Access);

   Update (Errors_List, Successfull_Update, Current_Text, Ambiguity'Access);

   if Successfull_Update then
      Put_Line ("Updated successfull");
   else
      Put_Line ("Update error");
   end if;

--   Save (File_Interface (Errors_List.Current_Text.all));

   Free_Objects;
end Gnatfix;
