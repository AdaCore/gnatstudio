------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNATCOLL.VFS_Utils;      use GNATCOLL.VFS_Utils;

with String_Utils;            use String_Utils;
with GNAT.Strings;            use GNAT.Strings;

package body Project_Templates is

   function CISW (S : String; Prefix : String) return Boolean;
   --  Case-Insensitive Starts_With: return true if S starts with Prefix,
   --  using case-insensitive comparison.

   function Find
     (S : String;
      C : Character;
      Start_Index : Integer := -1)
      return Natural;
   --  Return the index of the next occurrence of C in S, and return S'Last + 1
   --  if this was not found. If Start_Index is specified, start searching
   --  at Start_Index.
   --  ??? This should be in a general-purpose string handling package

   function To_Mixed (S : String) return String;
   --  Return Mixed_Casing version of S
   --  ??? This should be in a general-purpose string handling package

   --------------
   -- To_Mixed --
   --------------

   function To_Mixed (S : String) return String is
      O     : String := S;
      Upper : Boolean := True;
   begin
      for J in O'Range loop
         if Is_Letter (O (J)) then
            if Upper then
               O (J) := To_Upper (O (J));
               Upper := False;
            else
               O (J) := To_Lower (O (J));
            end if;
         else
            Upper := True;
         end if;
      end loop;

      return O;
   end To_Mixed;

   ----------
   -- CISW --
   ----------

   function CISW (S : String; Prefix : String) return Boolean is
      L : constant String := To_Lower (Prefix);
   begin
      if S'Length < Prefix'Length then
         return False;
      end if;

      for J in 0 .. L'Length - 1 loop
         if To_Lower (S (S'First + J)) /= L (L'First + J) then
            return False;
         end if;
      end loop;

      return True;
   end CISW;

   ----------
   -- Find --
   ----------

   function Find
     (S : String;
      C : Character;
      Start_Index : Integer := -1)
      return Natural
   is
      Ind : Natural;
   begin
      if Start_Index > 0 then
         Ind := Start_Index;
      else
         Ind := S'First;
      end if;

      while Ind <= S'Last loop
         if S (Ind) = C then
            return Ind;
         end if;

         Ind := Ind + 1;
      end loop;

      return Ind;
   end Find;

   -------------------------
   -- Read_Templates_File --
   -------------------------

   procedure Read_Templates_File
     (File      : Virtual_File;
      Errors    : out Unbounded_String;
      Templates : in out Project_Templates_List.List)
   is
      Contents              : GNAT.Strings.String_Access := File.Read_File;
      Lines                 : constant Unbounded_String_Array := Split
        (Contents.all, ASCII.LF);
      Current               : Project_Template := Null_Project_Template;
      Index, Index2, Index3 : Natural;
      In_Description        : Boolean := False;
   begin
      for J in Lines'Range loop
         declare
            Line : constant String := Strip_Quotes
              (Strip_CR (To_String (Lines (J))));
         begin
            if Line'Length = 0
              or else Starts_With (Line, "#")
            then
               --  A comment or an empty line do nothing
               null;

            elsif CISW (Line, "[description]") then
               In_Description := True;

            elsif CISW (Line, "name:") then
               Current.Label := To_Unbounded_String
                 (Strip_Quotes (Line (Line'First + 5 .. Line'Last)));

            elsif CISW (Line, "project:") then
               Current.Project := To_Unbounded_String
                 (Strip_Quotes (Line (Line'First + 8 .. Line'Last)));

            --  We get the python script used during the creation of the pages
            --  or during the post installation step.
            elsif CISW (Line, "script:") then
               declare
                  Python_Script : constant Virtual_File
                     := Create_From_Dir
                        (File.Dir,
                        +Strip_Quotes (Line (Line'First + 7 .. Line'Last)));
               begin
                  Current.Python_Script := Python_Script;
               end;

            elsif CISW (Line, "description:") then
               Current.Description := To_Unbounded_String
                 (Strip_Quotes (Line (Line'First + 12 .. Line'Last)));

            elsif CISW (Line, "category:") then
               Current.Category := To_Unbounded_String
                 (Strip_Quotes (Line (Line'First + 9 .. Line'Last)));

            elsif In_Description then
               Current.Description := Current.Description & ASCII.LF & Line;
            else
               Index := Find (Line, ':', Line'First + 1);
               if Index > Line'Last then
                  Append
                    (Errors, To_Unbounded_String
                       (+File.Base_Name & J'Img
                        & ": invalid syntax, expected "
                        & "<name>:<default value>:<description>"
                        & "[<choice_1>;<choice2>;...]"
                        & ASCII.LF));
               else
                  Index2 := Find (Line, ':', Index + 1);

                  if Index2 > Line'Last then
                     Append
                       (Errors, To_Unbounded_String
                          (+File.Base_Name & J'Img
                           & ": invalid syntax, expected "
                           & "<name>:<default value>:<description>"
                           & "[:<choice_1>;<choice2>;...]"
                           & ASCII.LF));
                  end if;

                  Index3 := Find (Line, ':', Index2 + 1);

                  declare
                     Choices : Unbounded_String_Array := Split
                       (Str  => Line (Index3 + 1 .. Line'Last),
                        On   => ';');
                  begin
                     for Choice of Choices loop
                        Choice := To_Unbounded_String
                          (Strip_Quotes (To_String (Choice)));
                     end loop;

                     Current.Variables.Append
                       (New_Item =>
                          Variable'(
                            Nb_Choices    => Choices'Length,
                            Label         => To_Unbounded_String
                              (Strip_Quotes (Line (Line'First .. Index - 1))),
                            Default_Value => To_Unbounded_String
                              (Strip_Quotes (Line (Index + 1 .. Index2 - 1))),
                            Description   => To_Unbounded_String
                              (Strip_Quotes (Line (Index2 + 1 .. Line'Last))),
                            Choices       => Choices));
                  end;
               end if;
            end if;
         end;
      end loop;

      if Current /= Null_Project_Template then
         Current.Source_Dir := File.Dir;

         --  Sanity check the contents of the template.

         if Current.Category = "" then
            Current.Category := To_Unbounded_String ("No Category");
         end if;

         if Current.Label = "" then
            Current.Category := To_Unbounded_String ("No Label");
         end if;

         Templates.Append (Current);
      end if;

      GNAT.Strings.Free (Contents);
   end Read_Templates_File;

   ------------------------
   -- Read_Templates_Dir --
   ------------------------

   procedure Read_Templates_Dir
     (Dir       : Virtual_File;
      Errors    : out Unbounded_String;
      Templates : in out Project_Templates_List.List)
   is
      Subdirs : File_Array_Access;
      Files   : File_Array_Access;
      Err     : Unbounded_String;
   begin
      if not Dir.Is_Directory then
         Errors := "Not a directory: " & To_Unbounded_String
           ((+Dir.Full_Name.all));
         return;
      end if;

      Subdirs := Read_Dir (Dir, Dirs_Only);

      if Subdirs /= null then
         for D in Subdirs'Range loop
            --  Read all files in subdir

            Files := Read_Dir (Subdirs (D), Files_Only);

            if Files /= null then
               for F in Files'Range loop
                  if File_Extension  (Files (F)) = Template_File_Extension then
                     Read_Templates_File (Files (F), Err, Templates);
                     Append (Errors, Err);
                  end if;
               end loop;

               Unchecked_Free (Files);
            end if;
         end loop;

         Unchecked_Free (Subdirs);
      end if;
   end Read_Templates_Dir;

   --------------------------
   -- Instantiate_Template --
   --------------------------

   procedure Instantiate_Template
     (Template    : Project_Template;
      Target_Dir  : Virtual_File;
      Assignments : Variable_Assignments.Map;
      Project     : out Virtual_File;
      Errors      : out Unbounded_String)
   is
      procedure Copy_Subdir
        (Source_Dir : Virtual_File;
         Target_Dir : Virtual_File);
      --  Process Subdir

      procedure Copy_File
        (Source_File : Virtual_File;
         Target_Dir  : Virtual_File);
      --  Process Source_File

      ---------------
      -- Copy_File --
      ---------------

      procedure Copy_File
        (Source_File : Virtual_File;
         Target_Dir  : Virtual_File)
      is
         Contents_A  : GNAT.Strings.String_Access;
         Target_Name : Unbounded_String;
         Target_Contents : Unbounded_String;
         Target    : Virtual_File;
         Writable  : Writable_File;

         use Variable_Assignments;
         C : Cursor;

         This_Is_The_Project : Boolean;

      begin
         --  Read the contents
         Contents_A := Read_File (Source_File);
         Target_Contents := To_Unbounded_String (Contents_A.all);
         GNAT.Strings.Free (Contents_A);

         Target_Name := To_Unbounded_String (+Source_File.Base_Name);

         This_Is_The_Project :=
           +Source_File.Base_Name = To_String (Template.Project);

         C := Assignments.First;

         --  Replace the filename and contents
         while Has_Element (C) loop
            --  Replace the lower by the lower, the upper by the upper, the
            --  mixed case by the mixed case
            declare
               K : constant String := To_String (Key (C));
               E : constant String := To_String (Element (C));
               Lower_Pattern : constant String := "@_" & To_Lower (K) & "_@";
               Upper_Pattern : constant String := "@_" & To_Upper (K) & "_@";
               Mixed_Pattern : constant String := "@_" & To_Mixed (K) & "_@";

               Lower_Replacement : constant String := To_Lower (E);
               Upper_Replacement : constant String := To_Upper (E);
               Mixed_Replacement : constant String := To_Mixed (E);
            begin
               Replace (Target_Name, Lower_Pattern, Lower_Replacement);
               Replace (Target_Name, Upper_Pattern, Upper_Replacement);
               Replace (Target_Name, Mixed_Pattern, Mixed_Replacement);
               Replace (Target_Contents, Lower_Pattern, Lower_Replacement);
               Replace (Target_Contents, Upper_Pattern, Upper_Replacement);
               Replace (Target_Contents, Mixed_Pattern, Mixed_Replacement);
            end;

            Next (C);
         end loop;

         Target := Create_From_Dir (Target_Dir, +To_String (Target_Name));

         if This_Is_The_Project then
            Project := Target;
         end if;

         if Target.Is_Regular_File
           and then not Target.Is_Writable
         then
            Append
              (Errors, "File not writable, did not overwrite: " &
               (+Target.Full_Name) & ASCII.LF);
         else
            Writable := Write_File (Target);
            Write (Writable, To_String (Target_Contents));
            Close (Writable);
         end if;
      end Copy_File;

      -----------------
      -- Copy_Subdir --
      -----------------

      procedure Copy_Subdir
        (Source_Dir : Virtual_File;
         Target_Dir : Virtual_File)
      is
         Files : File_Array_Access := Read_Dir (Source_Dir, Files_Only);
         Dirs  : File_Array_Access := Read_Dir (Source_Dir, Dirs_Only);
      begin
         --  Make sure the target dir exists, creating it if necessary

         if not Is_Directory (Target_Dir.Full_Name) then
            Make_Dir (Target_Dir);
         end if;

         --  First install all files
         if Files /= null then
            for J in Files'Range loop
               if File_Extension (Files (J)) /= Template_File_Extension
                 and then Files (J) /= Template.Python_Script
               then
                  Copy_File (Files (J), Target_Dir);
               end if;
            end loop;
            Unchecked_Free (Files);
         end if;

         --  Then install all subdirs
         if Dirs /= null then
            for J in Dirs'Range loop
               Copy_Subdir
                 (Dirs (J),
                  Create_From_Dir (Target_Dir, Dirs (J).Base_Dir_Name));
            end loop;

            Unchecked_Free (Dirs);
         end if;
      end Copy_Subdir;

   begin
      Copy_Subdir (Template.Source_Dir, Target_Dir);
   end Instantiate_Template;

   -------------------------
   -- Default_Assignments --
   -------------------------

   function Default_Assignments
     (Variables : Variables_List.List) return Variable_Assignments.Map
   is
      Result : Variable_Assignments.Map;

      use Variables_List;
      C : Cursor;
   begin
      C := Variables.First;

      while Has_Element (C) loop
         Result.Insert (Element (C).Label, Element (C).Default_Value);
         Next (C);
      end loop;

      return Result;
   end Default_Assignments;

end Project_Templates;
