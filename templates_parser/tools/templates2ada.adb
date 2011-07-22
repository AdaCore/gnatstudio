------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2006-2010, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Characters.Handling;                use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;                      use Ada.Strings.Fixed;
with Ada.Strings.Maps;                       use Ada.Strings.Maps;
with Ada.Text_IO;

with GNAT.Command_Line;                      use GNAT.Command_Line;

with Templates_Parser;                       use Templates_Parser;
with Templates_Parser.Utils;                 use Templates_Parser.Utils;

procedure Templates2Ada is

   use Ada;

   function "+" (Str : String) return Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   Opt_Output         : Unbounded_String := +"templates.ads";
   --  Output file name

   Opt_Templates_Dir  : Unbounded_String := +"resources/templates";
   --  Name of the directory in which templates are searched. Filenames stored
   --  in the generated Ada files will be relative to this directory.

   Opt_Recursive      : Boolean := False;
   --  Whether the search for template files should be recursive

   Opt_Templates_Ext  : String_Lists.List;
   --  List of extensions for the templates files

   Opt_Template : Unbounded_String := +"templates.tads";
   --  Name of the template file for the generated package.
   --  This gives you the opportunity of either generating a single package
   --  with nested packages (one for each template file), or a whole hierarchy
   --  of packages (by running gnatchop on the generated file).

   Opt_Verbose : Boolean := False;
   --  If true, this will output a warning when an HTTP parameter has a name
   --  made only of template parser tags, and then can't be generated in the
   --  packages as a result.

   HTTP_Get_Suffix : constant String := "@@";
   --  Suffix added in the internal tables for HTTP parameter names to indicate
   --  the ones that were defined through a HTTP_GET comment

   Help : Boolean := False;
   --  Set to true if -h option used

   Default_Template_Extension : constant String := ".thtml";
   --  The default extension used for template files

   N_Word_Set : constant Strings.Maps.Character_Set :=
                  To_Set (ASCII.HT & ASCII.LF & " ,()");

   type Template_Description is record
      Filename         : Unbounded_String;
      Variables        : Tag;
      Included         : Tag;
      HTTP             : Tag;
      From_Get         : Tag; --  Boolean to indicate if the corresponding
                              --  entry in HTTP is from a HTTP_GET parameter
      URL              : Tag;
      Ajax_Event       : Tag;
      Ajax_Action      : Tag;
      Ajax_File        : Tag;
      Set_Var, Set_Val : Tag;
   end record;

   package Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Template_Description);

   All_Templates : Maps.Map;
   All_Variables : Sets.Set;

   procedure Foreach_Template (Relative_Directory : String);
   --  Process each template in Directory (recursively if global options are
   --  set appropriately).

   procedure Process_Template (Relative_Name : String);
   --  Process a given templates file

   procedure Output_File;
   --  Create the output file

   procedure Next_Word
     (Str       : String;
      Index     : Integer;
      Starts_At :    out Integer;
      Ends_At   :    out Integer);
   --  Find the next word. This will skip all blank spaces at Index (and
   --  update Starts_At appropriately), then move Ends_At to the last
   --  non-blank character in the word.

   function Next_Line (Str : String; Index : Integer) return Integer;
   --  Return the first character on the next line

   function Search
     (Str     : String;
      Index   : Integer;
      Pattern : String;
      Forward : Boolean := True) return Integer;
   --  Search the first occurrence of Pattern after Index. Return Integer'First
   --  if not found.

   ----------------------
   -- Foreach_Template --
   ----------------------

   procedure Foreach_Template (Relative_Directory : String) is
      use type Directories.File_Kind;
      Search  : Directories.Search_Type;
      Dir_Ent : Directories.Directory_Entry_Type;
      C       : String_Lists.Cursor;
   begin
      Directories.Start_Search (Search, Relative_Directory, "");

      while Directories.More_Entries (Search) loop
         Directories.Get_Next_Entry (Search, Dir_Ent);

         if Directories.Kind (Dir_Ent) = Directories.Directory then
            if Opt_Recursive
              and then Directories.Simple_Name (Dir_Ent) /= "."
              and then Directories.Simple_Name (Dir_Ent) /= ".."
            then
               Foreach_Template
                 (Directories.Compose
                    (Relative_Directory,
                     Directories.Simple_Name (Dir_Ent)));
            end if;

         elsif Directories.Kind (Dir_Ent) = Directories.Ordinary_File then
            C := Opt_Templates_Ext.First;
            while String_Lists.Has_Element (C) loop
               declare
                  Simple : constant String :=
                             Directories.Simple_Name (Dir_Ent);
                  Ext    : constant String := String_Lists.Element (C);
               begin
                  if Simple'Length > Ext'Length
                    and then Simple
                      (Simple'Last - Ext'Length + 1
                       .. Simple'Last) = Ext
                  then
                     Process_Template
                       (Directories.Compose
                          (Relative_Directory,
                           Directories.Simple_Name (Dir_Ent)));
                  end if;
               end;
               String_Lists.Next (C);
            end loop;
         end if;
      end loop;

      Directories.End_Search (Search);

   exception
      when Text_IO.Name_Error =>
         Text_IO.Put_Line
           ("Can't find template directory " & Relative_Directory);
   end Foreach_Template;

   ---------------
   -- Next_Line --
   ---------------

   function Next_Line (Str : String; Index : Integer) return Integer is
      Result : Integer := Index;
   begin
      while Result <= Str'Last
        and then Str (Result) /= ASCII.LF
      loop
         Result := Result + 1;
      end loop;
      return Result + 1;
   end Next_Line;

   ---------------
   -- Next_Word --
   ---------------

   procedure Next_Word
     (Str       : String;
      Index     : Integer;
      Starts_At :    out Integer;
      Ends_At   :    out Integer) is
   begin
      Find_Token
        (Str (Index .. Str'Last),
         N_Word_Set, Strings.Outside, Starts_At, Ends_At);

      if Ends_At = 0 then
         Ends_At := Str'Last;
      end if;
   end Next_Word;

   -----------------
   -- Output_File --
   -----------------

   procedure Output_File is
      use Maps, Sets;
      Template         : constant String := To_String (Opt_Template);
      Output           : Text_IO.File_Type;
      T                : Translate_Set;
      C                : Maps.Cursor := First (All_Templates);
      C2               : Sets.Cursor := First (All_Variables);
      Variables        : Tag;
      Filenames        : Tag;
      Bases            : Tag;
      Variables_List   : Tag;
      Includes         : Tag;
      HTTPS            : Tag;
      URLs             : Tag;
      From_Get         : Tag;
      Ajax_Event       : Tag;
      Ajax_Action      : Tag;
      Ajax_File        : Tag;
      Set_Var, Set_Val : Tag;
   begin
      while Has_Element (C) loop
         Append (Variables,   Element (C).Variables);
         Append (Filenames,   Element (C).Filename);
         Append (Bases,       Key (C));
         Append (Includes,    Element (C).Included);
         Append (HTTPS,       Element (C).HTTP);
         Append (From_Get,    Element (C).From_Get);
         Append (URLs,        Element (C).URL);
         Append (Ajax_Event,  Element (C).Ajax_Event);
         Append (Ajax_Action, Element (C).Ajax_Action);
         Append (Ajax_File,   Element (C).Ajax_File);
         Append (Set_Var,     Element (C).Set_Var);
         Append (Set_Val,     Element (C).Set_Val);
         Next (C);
      end loop;

      while Has_Element (C2) loop
         Variables_List := Variables_List & Element (C2);
         Next (C2);
      end loop;

      Insert (T, Assoc ("VARIABLE",      Variables));
      Insert (T, Assoc ("FILENAME",      Filenames));
      Insert (T, Assoc ("BASENAME",      Bases));
      Insert (T, Assoc ("VARIABLE_LIST", Variables_List));
      Insert (T, Assoc ("INCLUDE",       Includes));
      Insert (T, Assoc ("HTTP",          HTTPS));
      Insert (T, Assoc ("FROM_GET",      From_Get));
      Insert (T, Assoc ("URL",           URLs));
      Insert (T, Assoc ("AJAX_EVENT",    Ajax_Event));
      Insert (T, Assoc ("AJAX_ACTION",   Ajax_Action));
      Insert (T, Assoc ("AJAX_FILE",     Ajax_File));
      Insert (T, Assoc ("SET_VAR",       Set_Var));
      Insert (T, Assoc ("SET_VAL",       Set_Val));

      Text_IO.Create (Output, Text_IO.Out_File, To_String (Opt_Output));

      if Directories.Exists (Template) then
         Text_IO.Put (Output, Parse (Template, T));
      else
         Text_IO.Put
           (Output,
            Parse (Utils.Get_Program_Directory
              & ".." & Directory_Separator
              & "share" & Directory_Separator
              & "examples" & Directory_Separator
              & "aws" & Directory_Separator
              & "templates" & Directory_Separator & Template,
              T));
      end if;

      Text_IO.Close (Output);

   exception
      when Text_IO.Name_Error =>
         Text_IO.Put_Line
           ("Can't find template file : " & To_String (Opt_Template));
   end Output_File;

   ----------------------
   -- Process_Template --
   ----------------------

   procedure Process_Template (Relative_Name : String) is
      use Ada.Streams, Ada.Streams.Stream_IO;
      use Sets;

      Result                     : Unbounded_String;
      File                       : Ada.Streams.Stream_IO.File_Type;
      Seen, Include, HTTP, URL   : Sets.Set;
      To_Ignore                  : Sets.Set;
      Variables, Includes, HTTPS : Tag;
      URLs, From_Get             : Tag;
      Ajax_Event, Ajax_Action    : Tag;
      Ajax_File                  : Tag;
      Set_Var, Set_Val           : Tag;
      C                          : Sets.Cursor;
      Inserted                   : Boolean;
      pragma Unreferenced (Result);

      procedure Process_Tag (Str : String; S : in out Integer);
      --  Process some text surrounded by @_..._@, and add it to the proper
      --  output tags. S points to the "@_" in Str, and is modified to
      --  point after the closing "_@".

      procedure Process_Tags (Str : String; First, Last : Integer);
      --  Process all tags referenced in Str (First .. Last)

      -----------------
      -- Process_Tag --
      -----------------

      procedure Process_Tag (Str : String; S : in out Integer) is
         First           : Integer := S + 2;
         Last            : Integer := First;
         Parents_Nesting : Natural := 0;
         Macro           : Boolean := False;
      begin
         while Last < Str'Last loop
            if Str (Last) = ':' then
               First := Last + 1;
            elsif Str (Last .. Last + 2) = ")_@" then
               Macro := True;
               exit;
            elsif Str (Last .. Last + 1) = "_@" then
               exit;
            end if;
            Last := Last + 1;
         end loop;

         if Macro then
            --  First word is the macro name, check if this is a AWS/Ajax macro
            S := S + 2;
            Next_Word (Str, S, First, Last);
            S := Last + 1;

            if Str (First .. Last) = "JS_ACTION" then
               --  First parameter is the event
               Next_Word (Str, S, First, Last);
               Append (Ajax_Event, Str (First .. Last));
               S := Last + 1;
               --  Second parameter is the action
               Next_Word (Str, S, First, Last);
               Append (Ajax_Action, Str (First .. Last));
               S := Last + 1;
               --  Record Ajax file location
               Append
                 (Ajax_File,
                  Directories.Base_Name (Relative_Name));
            end if;

         else
            --  Special case: a convention is that user-defined scripts
            --  might accept arguments that reference other tags, by using
            --  the syntax @_FILTER(@param):..._@, ie the parameter starts
            --  with a single @ sign. In this case, we want to make sure
            --  there is a entry made for the argument as well. Such
            --  filters might have multiple arguments. Multiple arguments
            --  must be comma-separated.

            for A in S + 2 .. Last - 1 loop
               if Str (A) = '(' then
                  Parents_Nesting := Parents_Nesting + 1;
               elsif Str (A) = ')' then
                  Parents_Nesting := Parents_Nesting - 1;
               elsif Str (A) = '@'
                 and then Parents_Nesting > 0
                 and then (Str (A - 1) = ',' or else Str (A - 1) = '(')
               then
                  for B in A + 1 .. Last - 1 loop
                     if Str (B) = ',' or else Str (B) = ')' then
                        Insert (Seen, Str (A + 1 .. B - 1), C, Inserted);
                        Insert
                          (All_Variables, Str (A + 1 .. B - 1),
                           C, Inserted);
                        exit;
                     end if;
                  end loop;
               end if;
            end loop;

            --  Remove attributes (can't be done in the loop above, because
            --  of complex structures like ADD_PARAM(AI='...'):PARAMETERS.

            for F in reverse First .. Last - 1 loop
               if Str (F) = ''' then
                  Last := F;
                  exit;
               end if;
            end loop;

            if Str (First) /= '$'
              and then Str (First .. Last - 1) /= "TABLE_LINE"
              and then Str (First .. Last - 1) /= "UP_TABLE_LINE"
              and then Str (First .. Last - 1) /= "NUMBER_LINE"
            then
               Insert (Seen, Str (First .. Last - 1), C, Inserted);
               Insert (All_Variables, Str (First .. Last - 1), C, Inserted);
            end if;

            S := Last + 2;
         end if;
      end Process_Tag;

      ------------------
      -- Process_Tags --
      ------------------

      procedure Process_Tags (Str : String; First, Last : Integer) is
         S : Integer := First;
      begin
         while S < Last loop
            if Str (S .. S + 1) = "@_" then
               Process_Tag (Str, S);
            else
               S := S + 1;
            end if;
         end loop;
      end Process_Tags;

   begin
      --  We cannot use the templates parser, since it wouldn't process
      --  correctly the @@IF@@ statements, and would only see one branch of
      --  them.

      Open (File, In_File, Relative_Name);

      declare
         Last, Last_Save : Integer;
         S, First        : Integer;
         Str             : String (1 .. Integer (Size (File)));
      begin
         String'Read (Stream (File), Str);
         S := Str'First;

         while S < Str'Last loop
            if S + 13 <= Str'Last
              and then Str (S .. S + 13) = "@@-- HTTP_GET("
            then
               Next_Word (Str, S + 14, First, Last);
               if not Contains (HTTP, Str (First .. Last)) then
                  Insert
                    (HTTP, Str (First .. Last) & HTTP_Get_Suffix, C, Inserted);
               end if;
               S := Next_Line (Str, Last + 1);

            elsif S + 13 <= Str'Last
              and then Str (S .. S + 13) = "@@-- HTTP_URL("
            then
               Next_Word (Str, S + 14, First, Last);
               Insert (URL, Str (First .. Last), C, Inserted);
               S := Next_Line (Str, Last + 1);

            elsif S + 4 <= Str'Last
              and then Str (S .. S + 3) = "@@--"
            then
               S := Next_Line (Str, S + 4);

            elsif S + 11 <= Str'Last
              and then Str (S .. S + 11) = "@@SET@@ SET_"
            then
               Next_Word (Str, S + 7, First, Last);
               Append (Set_Var, Str (First .. Last));
               S := Last + 1;
               Next_Word (Str, S, First, Last);
               S := Last + 1;
               Next_Word (Str, S, First, Last);
               Append (Set_Val, Str (First .. Last));

            elsif S + 6 <= Str'Last
              and then Str (S .. S + 6) = "@@SET@@"
            then
               Next_Word (Str, S + 7, First, Last);
               Insert (To_Ignore, Str (First .. Last), C, Inserted);

               --  The value of the variable could be either static or come
               --  from one or more other tags. We need to parse it, as a
               --  result.

               S := Next_Line (Str, Last + 1);
               Process_Tags (Str, Last + 1, S - 1);

            elsif Str (S .. S + 1) = "@_" then
               Process_Tag (Str, S);

            elsif S + 10 < Str'Last
              and then Str (S .. S + 10) = "@@INCLUDE@@"
            then
               Next_Word (Str, S + 12, First, Last);

               --  We could either have "@@INCLUDE@@ static_name.html"
               --  or "@@INCLUDE@@ @_TAG_@". In the latter case we need to
               --  handle the tag as usual.

               Process_Tags (Str, First, Last);
               Insert
                 (Include,
                  Directories.Base_Name (Str (First .. Last)), C, Inserted);
               S := Last + 2;

               --  Check for AWS/Ajax actions. AWS/Ajax is based on include
               --  files whose name is starting with "aws_action_". The first
               --  parameter being the event and the second the action. We use
               --  this naming scheme here.
               --
               --  @@INCLUDE@@ aws_action_xml.tjs (onclick, form_enter ...)

               if Index (Str (First .. Last), "aws_action_") /= 0 then
                  --  First parameter is the event
                  Next_Word (Str, S, First, Last);
                  Append (Ajax_Event, Str (First .. Last));
                  S := Last + 1;
                  --  Second parameter is the action
                  Next_Word (Str, S, First, Last);
                  Append (Ajax_Action, Str (First .. Last));
                  S := Last + 1;
                  --  Record Ajax file location
                  Append
                    (Ajax_File,
                     Directories.Base_Name (Relative_Name));
               end if;

            elsif S + 8 < Str'Last
              and then Str (S .. S + 8) = "<a name="""
            then
               --  Ignored, just so that we do not generate an HTTP entry for
               --  it.
               S := S + 9;

            elsif S + 11 < Str'Last
              and then Str (S .. S + 11) = "<form name="""
            then
               --  Ignored, just so that we do not generate an HTTP entry for
               --  it.
               S := S + 12;

            elsif S + 5 < Str'Last
              and then Str (S .. S + 5) = "name="""
            then
               First := S + 6;
               Last  := First;
               while Last <= Str'Last
                 and then Str (Last) /= '"'
               loop
                  Last := Last + 1;
               end loop;

               --  If the name contains some tags, this isn't a constant name.
               --  This is generally used for checkbox names, since the names
               --  need to be unique in such a case. In that case, try to find
               --  a prefix or a suffix as appropriate.

               Last_Save := Last - 1;
               Last      := Last_Save;

               --  The name might be something like "@_A_@:@_B_@:name", for
               --  which we will generate Name_Suffix.

               while First <= Str'Last and then Str (First) = '@' loop
                  First := Search (Str, First, "_@") + 2;
                  while First in Str'Range
                    and then not Is_Letter (Str (First))
                    and then Str (First) /= '@'
                  loop
                     First := First + 1;
                  end loop;
               end loop;

               --  It might also be something like "name:@_A_@:@_B_@" for which
               --  we will generate Name_Prefix.

               while Last >= Str'First and then Str (Last) = '@' loop
                  Last := Search (Str, Last, "@_", Forward => False) - 1;

                  while Last in Str'Range
                    and then not Is_Letter (Str (Last))
                    and then Str (Last) /= '@'
                  loop
                     Last := Last - 1;
                  end loop;
               end loop;

               if Opt_Verbose and then Last - First < 0 then
                  Text_IO.Put_Line
                    ("Can't process HTTP parameter: "
                     & Str (S + 6 .. Last_Save)
                     & " in template " & Relative_Name);
               end if;

               --  We do not insert the parameter in case a matching
               --  HTTP_GET parameter has already been inserted otherwise
               --  we end up with duplicated constants in the HTTP package.

               if First in Str'Range
                 and then Last in Str'Range
                 and then Last - First >= 0
                 and then
                   not Contains (HTTP, Str (First .. Last) & HTTP_Get_Suffix)
               then
                  Insert (HTTP, Str (First .. Last), C, Inserted);
               end if;

               S := Last + 2;

            else
               S := S + 1;
            end if;
         end loop;
      end;

      C := First (Seen);

      while Has_Element (C) loop
         if not Contains (To_Ignore, Element (C)) then
            Variables := Variables & Element (C);
         end if;
         Next (C);
      end loop;

      C := First (Include);

      while Has_Element (C) loop
         Includes := Includes & Element (C);
         Next (C);
      end loop;

      C := First (HTTP);

      while Has_Element (C) loop
         declare
            Name : constant String := Element (C);
         begin
            if Name'Length > 2
              and then Name (Name'Last - HTTP_Get_Suffix'Length + 1
                             .. Name'Last) = HTTP_Get_Suffix
            then
               HTTPS := HTTPS &
                  Name (Name'First .. Name'Last - HTTP_Get_Suffix'Length);
               From_Get := From_Get & True;

            else
               HTTPS := HTTPS & Name;
               From_Get := From_Get & False;
            end if;
         end;
         Next (C);
      end loop;

      C := First (URL);

      while Has_Element (C) loop
         URLs := URLs & Element (C);
         Next (C);
      end loop;

      Maps.Insert
        (All_Templates, Directories.Base_Name (Relative_Name),
         (+Relative_Name, Variables, Includes, HTTPS, From_Get, URLs,
          Ajax_Event, Ajax_Action, Ajax_File, Set_Var, Set_Val));

   exception
      when Text_IO.Name_Error =>
         Text_IO.Put_Line ("Can't open template file : " & Relative_Name);
   end Process_Template;

   ------------
   -- Search --
   ------------

   function Search
     (Str     : String;
      Index   : Integer;
      Pattern : String;
      Forward : Boolean := True) return Integer
   is
      Result : Integer;
   begin
      if Forward then
         Result := Index;
         while Result <= Str'Last - Pattern'Length + 1 loop
            if Str (Result .. Result + Pattern'Length - 1) = Pattern then
               return Result;
            end if;

            Result := Result + 1;
         end loop;

      else
         Result := Index - Pattern'Length + 1;
         while Result >= Str'First loop
            if Str (Result .. Result + Pattern'Length - 1) = Pattern then
               return Result;
            end if;

            Result := Result - 1;
         end loop;
      end if;
      return Integer'First;
   end Search;

begin
   loop
      case Getopt ("o: d: e: t: r v h") is
         when 'd'    => Opt_Templates_Dir  := +Parameter;
         when 'o'    => Opt_Output         := +Parameter;
         when 'e'    => Opt_Templates_Ext.Append (Parameter);
         when 't'    => Opt_Template       := +Parameter;
         when 'r'    => Opt_Recursive      := True;
         when 'v'    => Opt_Verbose        := True;
         when 'h'    => Help               := True;
         when others => exit;
      end case;
   end loop;

   if Opt_Templates_Ext.Is_Empty then
      Opt_Templates_Ext.Append (Default_Template_Extension);
   end if;

   if Help or else Ada.Command_Line.Argument_Count = 0 then
      Text_IO.New_Line;
      Text_IO.Put_Line ("Usage: templates2ada [options]");
      Text_IO.New_Line;
      Text_IO.Put_Line
        ("   -d dir   : template directory");
      Text_IO.Put_Line
        ("   -o file  : output filename (" & To_String (Opt_Output) & ')');
      Text_IO.Put_Line
        ("   -e ext   : template filename's extension (" &
         Default_Template_Extension & ')');
      Text_IO.Put_Line
        ("   -t tmplt : template filename (" & To_String (Opt_Template) & ')');
      Text_IO.Put_Line
        ("   -r       : search for templates file recursively");
      Text_IO.Put_Line
        ("   -v       : verbose mode");
      Text_IO.Put_Line
        ("   -h       : display this help message");
      Text_IO.New_Line;

   else
      Foreach_Template (To_String (Opt_Templates_Dir));

      Output_File;
   end if;
end Templates2Ada;
