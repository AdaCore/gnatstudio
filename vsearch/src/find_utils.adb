with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Language; use Language;

with Ada.Strings.Fixed;

with Ada.Unchecked_Deallocation;

package body Find_Utils is

   K_Max_File_Name_Length : constant := 256;
   --  ? Get from OS ?

   procedure Common_Init
     (Search          : out Code_Search;
      Look_For        : String;
      Match_Case      : Boolean;
      Whole_Word      : Boolean;
      Regexp          : Boolean;
      Scan_Comments   : Boolean;
      Scan_Strings    : Boolean;
      Scan_Statements : Boolean);
   --  Initialize files-independant fields.
   --
   --  Raise Search_Error if:
   --  * Look_For is empty, or can't compile
   --         ? XXX (iff it is a regexp) ?
   --  * Nor Comments nor Strings nor Statements are scanned



   -----------------
   -- Common_Init --
   -----------------

   procedure Common_Init
     (Search          : out Code_Search;
      Look_For        : String;
      Match_Case      : Boolean;
      Whole_Word      : Boolean;
      Regexp          : Boolean;
      Scan_Comments   : Boolean;
      Scan_Strings    : Boolean;
      Scan_Statements : Boolean)
   is
      Flags : Regexp_Flags := No_Flags;
      WD    : constant String := "\b";  -- Word_Delimiter
   begin
      if Look_For = ""
        or else not         (Scan_Comments
                     or else Scan_Strings
                     or else Scan_Statements)  -- ? really need 'or else' ?
      then
         raise Search_Error;
      end if;


      if not Match_Case then
         Flags := Case_Insensitive;
      end if;

      if Regexp then
         if Whole_Word then
            Search.Pattern := new Pattern_Matcher'
              (Compile (WD & Look_For & WD, Flags));
         else
            Search.Pattern := new Pattern_Matcher'
              (Compile (Look_For, Flags));
         end if;
      else
         if Whole_Word then
            Search.Pattern := new Pattern_Matcher'
              (Compile (WD & Quote (Look_For) & WD, Flags));
         elsif not Match_Case then  -- or optimized by Ada.Strings.Fixed.Index
            Search.Pattern := new Pattern_Matcher'
              (Compile (Quote (Look_For), Flags));
         end if;
      end if;



      Search.Look_For        := new String'(Look_For);
      Search.Match_Case      := Match_Case;
      Search.Whole_Word      := Whole_Word;
      Search.Regexp          := Regexp;
      Search.Scan_Comments   := Scan_Comments;
      Search.Scan_Strings    := Scan_Strings;
      Search.Scan_Statements := Scan_Statements;
   exception
      when Expression_Error =>
         --  ? Free (Pattern_Matcher); ?
         raise Search_Error;
   end Common_Init;

   ---------------
   -- Do_Search --
   ---------------

   procedure Do_Search
     (Search   : in out Code_Search;
      Callback : Poll_Search_Handler)
   is

      function Explore_Directory (Directory : String) return Boolean;
      --  Explore the directory to scan files.
      --  Direct and indirect sub-directories are recursively explored iff
      --  Search.Recurse.
      --  Scanned files are selected with Search.Files_Pattern.
      --  Return False when the search was aborted.
      --
      --  Directory: Where explore to select files

      function Scan_File (Name : String) return Boolean;
      --  Determine the language context of the file, and then scan it.
      --  Return False when the search was aborted.
      --
      --  Name: The file to scan

      function Scan_File_Without_Context (Name : String) return Boolean;
      --  Scan the file in order to find matches, with no context.
      --  Callback is called whenever a match is found, and at the end of the
      --  file.
      --  Return False when the search was aborted.
      --
      --  Name: The file to scan

      function Scan_File_With_Context
        (Name    : String;
         Context : Language_Context) return Boolean;
      --  Scan the file in order to find matches, using the given context.
      --  Callback is called whenever a match is found, and at the end of the
      --  file.
      --  Return False when the search was aborted.
      --
      --  Name     The file to scan
      --  Context  The language syntactic context used within the file

      function Scan_Line_With_Context
        (Name    : String;
         Line    : String;
         Line_Nr : Positive;
         Context : Language_Context) return Boolean;
      --  Scan a line given the current lexical state and the language context.
      --  Callback isn't called iff no match occurs in the line within allowed
      --  lexical states (ie called once when multiple matches).
      --  Return False when the search was aborted.
      --
      --  Name     File currently scanned
      --  Line     Line to check for matches
      --  Line_Nr  Line number
      --  Context  The language syntactic context used within the file

      function Contain_Match (Text : String) return Boolean;
      --  Return False iff no match occurs within the given text (ie True if a
      --  match or more).
      --
      --  Text: Text to check for matches



      -------------------
      -- Contain_Match --
      -------------------

      function Contain_Match (Text : String) return Boolean is
         use Ada.Strings.Fixed;
      begin
         --  ? testing order is optimized ?
         if (Search.Match_Case
           and then not Search.Whole_Word
           and then not Search.Regexp)
         then
            return Index (Text, Search.Look_For.all) /= 0;
         else
            return Match (Search.Pattern.all, Text) /= Text'First - 1;
         end if;
      end Contain_Match;

      -----------------------
      -- Explore_Directory --
      -----------------------

      function Explore_Directory (Directory : String) return Boolean is
         Continue  : Boolean := True;

         Dir_Name  : Dir_Name_Str (1 .. Directory'Length + 1) :=
                       Directory & Directory_Separator;
         Dir       : Dir_Type;
         File_Name : String (1 .. K_Max_File_Name_Length);
         Last      : Natural;

      begin
         Open (Dir, Dir_Name);

         loop
            Read (Dir, File_Name, Last);

            exit when Last = 0;

            declare
               Full_Name : String (1 .. Dir_Name'Length + Last) :=
                             Dir_Name & File_Name (1 .. Last);
            begin
               --  Is_Directory adds ASCII.NUL

               if Is_Directory (Full_Name) then
                  if Search.Recurse
                    and then File_Name (1 .. Last) /= "."
                    and then File_Name (1 .. Last) /= ".."
                  then
                     Continue := Explore_Directory (Full_Name);
                  end if;
               elsif Match (File_Name (1 .. Last), Search.Files_Pattern) then
                  Continue := Scan_File (Full_Name);
               end if;
            end;

            exit when not Continue;
         end loop;

         Close (Dir);
         return Continue;

      exception
         when Directory_Error =>
            return True;  -- ignore opening error
      end Explore_Directory;

      ---------------
      -- Scan_File --
      ---------------

      function Scan_File (Name : String) return Boolean is
         Language : Language_Access := Get_Language_From_File (Name);
      begin
         if Language = null then
            return Scan_File_Without_Context (Name);
         else
            return Scan_File_With_Context (Name,
                                           Get_Language_Context (Language));
         end if;
      end Scan_File;

      ----------------------------
      -- Scan_File_With_Context --
      ----------------------------

      function Scan_File_With_Context
        (Name    : String;
         Context : Language_Context) return Boolean
      is
         FD       : File_Descriptor := Open_Read (Name & ASCII.NUL, Text);
         Continue : Boolean := True;

      begin
         if FD = Invalid_FD then
            return Callback (False, Name);
         end if;

         declare
            Len     : Positive := Positive (File_Length (FD));
            Buffer  : aliased String (1 .. Len);
            Pos     : Positive := 1;
            BOL     : Positive;  -- Beginning Of Line
            Line_Nr : Positive := 1;

         begin
            Len := Read (FD, Buffer'Address, Len);  -- Let's hope all is read

            Search.Lexical_State := Statements;

            while Pos <= Len loop  -- Skip empty files
               BOL := Pos;

               while Pos <= Len and then Buffer (Pos) /= ASCII.LF loop
                  Pos := Pos + 1;
               end loop;

               Continue := Scan_Line_With_Context
                             (Name, Buffer (BOL .. Pos - 1), Line_Nr, Context);
               exit when not Continue;

               Pos := Pos + 1;  -- Skip ASCII.LF
               Line_Nr := Line_Nr + 1;
            end loop;
         end;

         Close (FD);
         return Continue and then Callback (False, Name);
      end Scan_File_With_Context;

      -------------------------------
      -- Scan_File_Without_Context --
      -------------------------------

      function Scan_File_Without_Context (Name : String) return Boolean is
         FD       : File_Descriptor := Open_Read (Name & ASCII.NUL, Text);
         Continue : Boolean := True;
      begin
         if FD = Invalid_FD then
            return Callback (False, Name);
         end if;

         declare
            Len     : Natural := Natural (File_Length (FD));
            Buffer  : aliased String (1 .. Len);
            Pos     : Positive := 1;
            BOL     : Positive;  -- Beginning Of Line
            Line_Nr : Positive := 1;
         begin
            Len := Read (FD, Buffer'Address, Len);

            while Pos <= Len loop
               BOL := Pos;

               while Pos <= Len and then Buffer (Pos) /= ASCII.LF loop
                  Pos := Pos + 1;
               end loop;

               if Contain_Match (Buffer (BOL .. Pos - 1)) then
                  Continue := Callback (True, Name, Line_Nr,
                                        Buffer (BOL .. Pos - 1));
                  exit when not Continue;
               end if;

               Pos := Pos + 1;  -- Skip ASCII.LF
               Line_Nr := Line_Nr + 1;
            end loop;
         end;

         Close (FD);
         return Continue and then Callback (False, Name);
      end Scan_File_Without_Context;

      ----------------------------
      -- Scan_Line_With_Context --
      ----------------------------

      function Scan_Line_With_Context
        (Name    : String;
         Line    : String;
         Line_Nr : Positive;
         Context : Language_Context) return Boolean
      is
         Scanning_Allowed :
            constant array (Recognized_Lexical_States) of Boolean :=
              (Statements     => Search.Scan_Statements,
               Strings        => Search.Scan_Strings,
               Mono_Comments  => Search.Scan_Comments,
               Multi_Comments => Search.Scan_Comments);

         EOL : constant Positive := Line'Last;  -- End Of Line

         Reached : Positive;
         Pos     : Positive;
         --  ~ Line (Reached .. Pos - 1) is whole the same lexical state

         Next               : Positive;
         Next_Lexical_State : Recognized_Lexical_States;
         --  ~ Line (Next) is from where Next_Lexical_State applies

         Str_Delim     : constant Character := Context.String_Delimiter;
         Quote_Char    : constant Character := Context.Quote_Character;
         NL_Comm_Start : constant String    := Context.New_Line_Comment_Start;
         M_Comm_Start  : constant String    := Context.Comment_Start;
         M_Comm_End    : constant String    := Context.Comment_End;

         Called : Boolean := False; -- Callback was called on this line ?

         Continue : Boolean := True;  --  ? really necessary ?
      begin
         --  !! handle multiline comment on 1 line !!

         Reached := Line'First;

         Whole_Line_Loop : loop
            Pos := Reached;

            Next := EOL + 1;  -- ~ Default when whole line is same lexical
            Next_Lexical_State := Search.Lexical_State;

            case Search.Lexical_State is
               when Statements =>
                  while Pos <= EOL loop
                     if M_Comm_Start'Length /= 0
                       and then Pos + M_Comm_Start'Length - 1 <= EOL
                       and then Line (Pos .. Pos + M_Comm_Start'Length - 1)
                                = M_Comm_Start
                     then
                        Next_Lexical_State := Multi_Comments;
                        Next := Pos + M_Comm_Start'Length;
                        exit;
                     elsif NL_Comm_Start'Length /= 0
                       and then Pos + NL_Comm_Start'Length - 1 <= EOL
                       and then Line (Pos .. Pos + NL_Comm_Start'Length - 1)
                                = NL_Comm_Start
                     then
                        Next_Lexical_State := Mono_Comments;
                        Next := Pos + NL_Comm_Start'Length;
                        exit;
                     elsif Line (Pos) = Str_Delim then
                        Next_Lexical_State := Strings;
                        Next := Pos + 1;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;

               when Strings =>
                  while Pos <= EOL loop
                     if Line (Pos) = Str_Delim
                       and then (Quote_Char = ASCII.NUL or else
                                 Line (Pos - 1) = Quote_Char)
                     then
                        Next_Lexical_State := Statements;
                        Next := Pos + 1;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;

               when Mono_Comments =>
                  Pos := EOL + 1;

               when Multi_Comments =>
                  while Pos <= EOL loop
                     if M_Comm_End'Length /= 0
                       and then Pos + M_Comm_End'Length - 1 <= EOL
                       and then Line (Pos .. Pos + M_Comm_End'Length - 1)
                                = M_Comm_End
                     then
                        Next_Lexical_State := Statements;
                        Next := Pos + M_Comm_End'Length;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;
            end case;

            if not Called
              and then Scanning_Allowed (Search.Lexical_State)
              and then Contain_Match (Line (Reached .. Pos - 1))
            then
               Continue := Callback (True, Name, Line_Nr, Line);
               Called := True;

               exit Whole_Line_Loop when not Continue;

               --  ??? improve by 'exit when not Callback'
               --  ??? OK since Search.Lexical_State needn't being updated and
               --  ???   called no more
            end if;

            Reached := Next;  -- Skip the processed text
            Search.Lexical_State := Next_Lexical_State;

            exit Whole_Line_Loop when Reached > EOL;
         end loop Whole_Line_Loop;

         return Continue;
      end Scan_Line_With_Context;

   --  Start of processing for Do_Search

      Continue : Boolean;
   begin
      pragma Assert (Poll_Search_Handler /= null);

      if Search.Files = null then
         Continue := Explore_Directory (Search.Directory.all);
      else
         for F in Search.Files'Range loop
            if Search.Files (F) /= null
              and then Search.Files (F).all /= ""
            then
               Continue := Scan_File (Search.Files (F).all);

               exit when not Continue;
            end if;
         end loop;
      end if;
   end Do_Search;

   ----------
   -- Free --
   ----------

   procedure Free (S : in out Code_Search) is
      procedure Free_String is new Standard.Ada.Unchecked_Deallocation
        (String, String_Access);
      procedure Free_Pattern_Matcher is new Standard.Ada.Unchecked_Deallocation
        (Pattern_Matcher, Pattern_Matcher_Access);
   begin
      Free_String          (S.Look_For);
      Free_String          (S.Directory);
      Free_Pattern_Matcher (S.Pattern);
   end Free;

   -----------------
   -- Init_Search --
   -----------------

   procedure Init_Search
     (Search          : out Code_Search;
      Look_For        : String;
      Files           : Project_Files_Access;
      Match_Case      : Boolean := False;
      Whole_Word      : Boolean := False;
      Regexp          : Boolean := False;
      Scan_Comments   : Boolean := True;
      Scan_Strings    : Boolean := True;
      Scan_Statements : Boolean := True)
   is
   begin
      if Files = null then
         raise Search_Error;
      end if;

      Common_Init (Search, Look_For, Match_Case, Whole_Word, Regexp,
                   Scan_Comments, Scan_Strings, Scan_Statements);

      Search.Files := Files;
   end Init_Search;

   -----------------
   -- Init_Search --
   -----------------

   procedure Init_Search
     (Search          : out Code_Search;
      Look_For        : String;
      Files_Pattern   : Regexp;
      Directory       : String  := "";
      Recurse         : Boolean := False;
      Match_Case      : Boolean := False;
      Whole_Word      : Boolean := False;
      Regexp          : Boolean := False;
      Scan_Comments   : Boolean := True;
      Scan_Strings    : Boolean := True;
      Scan_Statements : Boolean := True)
   is
   begin
      --  Ensure Files_Pattern is initialized

      declare
         Dummy : Boolean;
      begin
         Dummy := Match ("", Files_Pattern);
      exception
         when Constraint_Error =>
            raise Search_Error;
      end;

      Common_Init (Search, Look_For, Match_Case, Whole_Word, Regexp,
                   Scan_Comments, Scan_Strings, Scan_Statements);
      Search.Files_Pattern := Files_Pattern;
      Search.Recurse := Recurse;

      if Directory = "" then
         Search.Directory := new String'(Get_Current_Dir);
      else
         Search.Directory := new String'(Directory);
      end if;
   end Init_Search;

end Find_Utils;
