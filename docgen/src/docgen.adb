with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with GNAT.OS_Lib;       use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Generic_List;
with Work_on_File;	use Work_on_File;

procedure docgen is

   Help_Requested : exception renames Ada.IO_Exceptions.Device_Error;
   --raised when the help option was used. no need to go on!
   Command_Line_Error : exception;
   --this is a  second comment
   Version      : constant String := "0.1";

   type Source_File is record
      Name          : String_Access;
      Prj_File_Name : String_Access;
   end record;

   procedure Free (Data : in out Source_File)
   --Free memory associated with X. For both file_Lists
   is
   begin
      Put_Line ("Free memory for the Sourcefile_List!");
   end;

   package File_List is new Generic_List (Source_File);
   Source_File_List : File_List.List;


   New_Source_File    : Source_File;
   Last_Prj_File_Name : String_Access;


   procedure Handle_Command_Line
     (Quit : out Boolean)
   is
      use Ada.Command_Line;

      File_Name_Set    : Boolean     := False;
      J                : Natural     := 1;
      N                : Natural     := Argument_Count;
      Print_Version    : Boolean     := False;
      New_Source_File  : Source_File;

   begin
      if N = 0 then raise Command_Line_Error; --Help_Requested;
      end if;
      Quit := False;
      while J <= N loop
         declare
            S : String := Argument (J);
         begin
            if S = "-h"    or else S = "-?" or else
               S = "-help" or else S = "--help"
            then
               raise Help_Requested;
            elsif S = "-v" or else S = "-version" or else S = "--version" then
               Print_Version := True;

            elsif S'Length > 5 then
               if S (S'Last - 3 .. S'Last) = ".gpr" then
                  Last_Prj_File_Name := new String'(S);
               elsif S (S'Last - 3 .. S'Last) = ".ads" or
                 S (S'Last - 3 .. S'Last) = ".adb" then
                     if Last_Prj_File_Name /= null then
                        New_Source_File.Name := new String'(S);
                        New_Source_File.Prj_File_Name := Last_Prj_File_Name;
                 	File_List.Append (Source_File_List, New_Source_File);
                     else
                        raise Command_Line_Error;
                  end if;
	       end if;
            end if;
         end;
         J := J + 1;
      end loop;

      --check if at least one source_file_name exist
      if File_List.Length (Source_File_List) = 0 then
         raise Command_Line_Error;
      end if;

      if Print_Version then
         Put_Line (Current_Error,
                   "docgen " & Version);
         if N = 1 then
            Quit := True;
            return;
         end if;
      end if;


   exception
      when Command_Line_Error =>
         --Check the remaining arguments if there's a help option
         --somewhere. If so, translate this into a Help_Requested.
         J := J + 1;
         while J <= N loop
            declare
               S : String := Argument (J);
            begin
               exit when S = "-?"    or else S = "-h" or else
                         S = "-help" or else S = "--help";
            end;
            J := J + 1;
         end loop;
         if J <= N then
            raise Help_Requested;
         else
            raise;
         end if;
   end Handle_Command_Line;




begin -- DocGen

   Parse_Command_Line :
   declare
     Quit : Boolean := false;
   begin
      Handle_Command_Line (Quit);
      if Quit then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
         return;
      end if;
   end Parse_Command_Line;

   --AD.User_Tags.Verify;

   --Process all files listed or all files from the project file
   Use_All_files :
   declare
      Node : File_List.List_Node;
      use File_List;
   begin
      Node := First(Source_File_List);
      Process_File (Data (Node).Prj_File_Name,
                    Data (Node).Name);
      for J in 1..Length(Source_File_List)-1 loop
         Node := Next (Node);
         Process_File (Data (Node).Prj_File_Name,
                       Data (Node).Name);
      end loop;
   end Use_All_Files;


   --AD.Indices.Close;
   --AD.Parameters.Close;
   --Asis.Implementation.Finalize ("");
   --Ada.Command_L





exception
         when Command_Line_Error =>
            Put_Line (Current_Error,
                      "Type ""docgen -?"" for more information.");

         when Help_Requested =>
            Put_Line (Current_Error, "NAME");
            Put_Line (Current_Error, "   docgen");
            New_Line (Current_Error);
            Put_Line (Current_Error, "SYNOPSIS");
            Put_Line
              (Current_Error,
               "  docgen (-h | -help | --help | -?)");
            Put_Line
              (Current_Error,
               "  docgen {-v | -version | --version |");
           Put_Line
              (Current_Error,
               "              -f source_file_name | -c config_file_name}");

end DocGen;
