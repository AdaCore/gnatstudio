-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2001-2011, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Expect;                use GNAT.Expect;
with GNAT.Regexp;                use GNAT.Regexp;
with GNAT.Regpat;                use GNAT.Regpat;
with GNAT.Strings;

with Glib;                       use Glib;
with Glib.Unicode;               use Glib.Unicode;

with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Combo_Box;              use Gtk.Combo_Box;
with Gtk.Editable;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Table;                  use Gtk.Table;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Toggle_Button;          use Gtk.Toggle_Button;
with Gtk.Tooltips;               use Gtk.Tooltips;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.MDI;                 use Gtkada.MDI;

with Basic_Types;                use Basic_Types;
with Files_Extra_Info_Pkg;       use Files_Extra_Info_Pkg;
with GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup; use GPS.Kernel.Messages.Markup;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;          use GPS.Kernel.Styles;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Styles;                 use GPS.Styles;
with GPS.Styles.UI;              use GPS.Styles.UI;
with GUI_Utils;                  use GUI_Utils;
with Language;                   use Language;
with Language_Handlers;          use Language_Handlers;
with Osint;                      use Osint;
with Src_Editor_Box;             use Src_Editor_Box;
with Src_Editor_Module.Markers;  use Src_Editor_Module.Markers;
with Src_Editor_Module;          use Src_Editor_Module;
with Src_Editor_View;            use Src_Editor_View;
with Traces;                     use Traces;
with Vsearch;                    use Vsearch;
with UTF8_Utils;

package body Src_Contexts is

   Me : constant Debug_Handle := Create ("Src_Contexts");

   type Casing_Type is (Lower, Upper, Smart_Mixed, Unchanged);

   function Guess_Casing (S : String) return Casing_Type;
   --  Guess the casing which is used in S.
   --  S is encoded in UTF-8.

   function To_Casing (S : String; Casing : Casing_Type) return String;
   --  Return S transformed to match Casing.
   --  If S is not all lower-case, return S unchanged.
   --  S is encoded in UTF-8, and so is the result.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Match_Result, Match_Result_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Match_Result_Array, Match_Result_Array_Access);

   procedure Scan_Buffer
     (Buffer        : String;
      Buffer_First  : Natural;
      Context       : access Search_Context'Class;
      Callback      : Scan_Callback;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Lang          : Language_Access := null;
      Ref_Line      : Editable_Line_Type := 1;
      Ref_Column    : Character_Offset_Type := 1;
      Was_Partial   : out Boolean);
   --  Search Context in buffer (Buffer_First .. Buffer'Last), searching only
   --  in the appropriate scope.
   --  Buffer is assumed to contain complete contexts (e.g the contents of
   --  a whole file).
   --  (Ref_Line, Ref_Column) is the position in the actual file that Buffer
   --  starts at
   --  Lexical_State is the scope at the first character in Buffer.
   --  On exit, Was_Partial is set to True if the search was interrupted
   --  because the callback returned False at some point
   --  Buffer should be in UTF-8.

   procedure Scan_File
     (Context       : access Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Kernel        : Kernel_Handle := null;
      Name          : GNATCOLL.VFS.Virtual_File;
      Callback      : Scan_Callback;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type := 1;
      Start_Column  : Character_Offset_Type := 1;
      Force_Read    : Boolean := False;
      Was_Partial   : out Boolean);
   --  Search Context in the file Name, searching only in the appropriate
   --  scope.
   --  If there is already an opened editor for this file, its contents will be
   --  used, otherwise the file is read from the disk.
   --  The search will start at position (Start_Line, Start_Column)
   --  Lexical_State is the scope at current_line, current_column.
   --  If Force_Read is True, then this procedure does not check whether there
   --  already exists an open editor. This should be set to False when running
   --  in text-only mode. Kernel can be null only if Force_Read is True.
   --  On exit, Was_Partial is set to True if the callback returned False at
   --  some point.

   procedure Scan_Editor
     (Context       : access Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Editor        : MDI_Child;
      Callback      : Scan_Callback;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type := 1;
      Start_Column  : Character_Offset_Type := 1;
      Was_Partial   : out Boolean);
   --  Same as above, but works directly on the editor. This is usefull for
   --  example when the editor has no file yet.

   function Scan_And_Store
     (Context  : access Search_Context'Class;
      Handler  : access Language_Handler_Record'Class;
      Kernel   : Kernel_Handle;
      Str      : String := "";
      File     : Virtual_File := GNATCOLL.VFS.No_File;
      Scope    : Search_Scope;
      Lang     : Language_Access := null) return Match_Result_Array_Access;
   --  Same as above, but behaves as if there was a default callback that
   --  stores the results in an array
   --  If Str is not the empty string, it is considered as a buffer to parse.
   --  If it is empty and File is not No_File, then that file is parsed
   --  instead.
   --  It returns the list of matches that were found in the buffer, or null if
   --  no match was found. It is the responsability of the caller to free the
   --  returned array.
   --  If Kernel is null, not check is done whether an editor is currently open
   --  for the file.

   procedure Scan_Next
     (Context        : access Search_Context'Class;
      Kernel         : access Kernel_Handle_Record'Class;
      Editor         : access Source_Buffer_Record'Class;
      Scope          : Search_Scope;
      Lexical_State  : in out Recognized_Lexical_States;
      Lang           : Language_Access;
      Current_Line   : Editable_Line_Type;
      Current_Column : Character_Offset_Type;
      Backward       : Boolean;
      Dialog_On_Failure : Boolean := True;
      Result         : out Match_Result_Access);
   --  Return the next occurrence of Context in Editor, just before or just
   --  after Current_Line, Current_Column. If no match is found after the
   --  current position, for a forward search, return the first occurrence from
   --  the beginning of the editor. Likewise for a backward search.
   --  Note that the index in the result might be incorrect, although the line
   --  and column will always be correct.
   --  null is returned if there is no match.
   --  Current_Scope is the scope at current_line, current_column.

   procedure First_Match
     (Context       : access Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Kernel        : Kernel_Handle;
      Name          : GNATCOLL.VFS.Virtual_File;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type := 1;
      Start_Column  : Character_Offset_Type := 1;
      Result        : out Match_Result_Access;
      Force_Read    : Boolean := False);
   --  Lightweight interface that returns the first occurence of Context in the
   --  file Name.
   --  The returned value must be freed by the caller
   --  Current_Scope is the scope at (Start_Line, Start_Column)
   --  See description of Force_Read in Scan_File. Kernel can be null only if
   --  Force_Read is True.

   procedure First_Match
     (Context       : access Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Editor        : MDI_Child;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type := 1;
      Start_Column  : Character_Offset_Type := 1;
      Result        : out Match_Result_Access);
   --  Same as above, using an editor instead

   procedure Highlight_Result
     (Kernel      : access Kernel_Handle_Record'Class;
      File_Name   : GNATCOLL.VFS.Virtual_File;
      Look_For    : String;
      Match       : Match_Result;
      Give_Focus  : Boolean;
      Interactive : Boolean);
   --  Print the result of the search in the console
   --  If Give_Focus is true, the focus will be given to the editor

   procedure Free (Result : in out Match_Result_Array_Access);
   --  Free Result and its components

   procedure Initialize_Scope_Combo
     (Combo  : access Gtk_Combo_Box_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  Initialize the combo box with all the entries for the selection of the
   --  scope.

   function Auxiliary_Search
     (Context         : access Current_File_Context;
      Editor          : Source_Editor_Box;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Auxiliary function, factorizes code between Search and Replace.
   --  Return True in case of success

   function Locations_Category_Name (Look_For : String) return String;
   --  Return the name of the category to use in the Locations window

   procedure Search_From_File
     (Context       : access File_Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Kernel        : Kernel_Handle;
      Callback      : Scan_Callback;
      File          : GNATCOLL.VFS.Virtual_File;
      More_Matches  : out Boolean;
      Matches_Found : out Boolean);
   --  Call Callback on matches found in the file given in parmeter.

   procedure Search_From_Editor
     (Context       : access File_Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Callback      : Scan_Callback;
      Editor        : MDI_Child;
      More_Matches  : out Boolean;
      Matches_Found : out Boolean);
   --  Call Callback on matches found in the file given in parmeter.

   function Replace_From_Editor
     (Context         : access File_Search_Context'Class;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Child           : MDI_Child) return Boolean;
   --  Replace the matches by the string given in parameter, from the editor
   --  contained in Child. Return True if there is matches still to be
   --  replaced, false otherwise.

   function Replace_From_File
     (Context         : access Abstract_Files_Context'Class;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      File            : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Replace the matches by the string given in parameter, from the file
   --  given in parameter. If there's an editor opened on this file, this
   --  editor will be taken instead. Return True if there is matches still to
   --  be replaced, false otherwise.

   -----------------
   -- Scan_Buffer --
   -----------------

   procedure Scan_Buffer
     (Buffer        : String;
      Buffer_First  : Natural;
      Context       : access Search_Context'Class;
      Callback      : Scan_Callback;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Lang          : Language_Access := null;
      Ref_Line      : Editable_Line_Type := 1;
      Ref_Column    : Character_Offset_Type := 1;
      Was_Partial   : out Boolean)
   is
      Scanning_Allowed : constant array (Recognized_Lexical_States) of Boolean
        := (Statements     => Scope = Whole or else Scope = All_But_Comments,
            Strings        => Scope = Whole
              or else Scope in Comments_And_Strings .. All_But_Comments,
            Mono_Comments  => Scope in Whole .. Comments_And_Strings,
            Multi_Comments => Scope in Whole .. Comments_And_Strings);
      --  Indicates what lexical states are valid, depending on the current
      --  scope.

      procedure Next_Scope_Transition
        (Buffer      : String;
         Pos         : in out Positive;
         State       : in out Recognized_Lexical_States;
         Section_End : out Integer;
         Lang        : Language_Context);
      --  Move Pos to the first character in buffer that isn't in the same
      --  lexical state as State (ie if State is one we want to search in, then
      --  Pos will be left on the first character we do not want to search).
      --
      --  Pos is purely internal, and represents the first character into the
      --  next section (ie after passing the section start string, like -- for
      --  comments). Section_End on the last point in the current section.

      ---------------------------
      -- Next_Scope_Transition --
      ---------------------------

      procedure Next_Scope_Transition
        (Buffer      : String;
         Pos         : in out Positive;
         State       : in out Recognized_Lexical_States;
         Section_End : out Integer;
         Lang        : Language_Context)
      is
         Str_Delim     : Character renames Lang.String_Delimiter;
         Quote_Char    : Character renames Lang.Quote_Character;
         M_Comm_Start  : String    renames Lang.Comment_Start;
         M_Comm_End    : String    renames Lang.Comment_End;
         Char_Delim    : Character renames Lang.Constant_Character;

         Looking_For   : constant Boolean := not Scanning_Allowed (State);
         --  Whether the final range should or should not be scanned.

         Matches       : Match_Array (0 .. 0);

      begin
         while Pos <= Buffer'Last
           and then Scanning_Allowed (State) /= Looking_For
         loop
            case State is
               --  Statements end on any other state

               when Statements =>
                  while Pos <= Buffer'Last loop
                     if M_Comm_Start'Length /= 0
                       and then Pos + M_Comm_Start'Length - 1 <= Buffer'Last
                       and then Buffer (Pos .. Pos + M_Comm_Start'Length - 1) =
                       M_Comm_Start
                     then
                        State := Multi_Comments;
                        Section_End := Pos - 1;
                        Pos := Pos + M_Comm_Start'Length;
                        exit;

                     else
                        if Lang.New_Line_Comment_Start = null then
                           if Lang.New_Line_Comment_Start_Regexp /= null then
                              Match (Lang.New_Line_Comment_Start_Regexp.all,
                                     Buffer, Matches, Pos);

                              if Matches (0) /= No_Match then
                                 State := Mono_Comments;
                                 Section_End := Pos - 1;
                                 Pos := Matches (0).Last + 1;
                                 exit;
                              end if;
                           end if;
                        else
                           if Pos <= Buffer'Last -
                                      Lang.New_Line_Comment_Start'Length + 1
                             and then
                               Buffer (Pos .. Pos +
                                       Lang.New_Line_Comment_Start'Length - 1)
                                 = Lang.New_Line_Comment_Start.all
                           then
                              State := Mono_Comments;
                              Section_End := Pos - 1;
                              Pos := Pos + Lang.New_Line_Comment_Start'Length;
                              exit;
                           end if;
                        end if;

                        if Buffer (Pos) = Str_Delim
                          and then (Pos = Buffer_First
                                    or else Pos = Buffer'Last
                                    or else Buffer (Pos - 1) /= Char_Delim
                                    or else Buffer (Pos + 1) /= Char_Delim)
                        then
                           State := Strings;
                           Section_End := Pos - 1;
                           Pos := Pos + 1;
                           exit;
                        end if;
                     end if;

                     Pos := Pos + 1;
                  end loop;

               --  Strings end on string delimiters

               when Strings =>
                  while Pos <= Buffer'Last loop
                     if Buffer (Pos) = Str_Delim
                       and then (Quote_Char = ASCII.NUL or else
                                 (Pos > Buffer_First and then
                                  Buffer (Pos - 1) /= Quote_Char)
                                 or else Pos = Buffer_First)
                     then
                        State := Statements;
                        Section_End := Pos - 1;
                        Pos := Pos + 1;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;

               --  Single line comments end on ASCII.LF characters

               when Mono_Comments =>
                  while Pos <= Buffer'Last
                    and then Buffer (Pos) /= ASCII.LF
                  loop
                     Pos := Pos + 1;
                  end loop;

                  Section_End := Pos - 1;
                  if Pos <= Buffer'Last then
                     Pos := Pos + 1;
                  end if;
                  State := Statements;

               --  Multi-line comments end with specific sequences

               when Multi_Comments =>
                  while Pos <= Buffer'Last loop
                     if M_Comm_End'Length /= 0
                       and then Pos + M_Comm_End'Length - 1 <= Buffer'Last
                       and then Buffer (Pos .. Pos + M_Comm_End'Length - 1) =
                       M_Comm_End
                     then
                        State := Statements;
                        Section_End := Pos - 1;
                        Pos := Pos + M_Comm_End'Length;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;
            end case;
         end loop;

         if Pos > Buffer'Last then
            Section_End := Buffer'Last;
         end if;
      end Next_Scope_Transition;

      Pos           : Positive := Buffer_First;
      Line_Start    : Positive;
      Line          : Editable_Line_Type := Ref_Line;
      Column        : Character_Offset_Type := Ref_Column;
      Dummy         : Visible_Column_Type := 1;
      Last_Index    : Positive := Buffer_First;
      Section_End   : Integer;
      Old_State     : Recognized_Lexical_States;
      Language      : Language_Context_Access;
      Ignored       : Natural := 0;

   begin  --  Scan_Buffer
      Was_Partial := False;

      if Buffer'Length = 0 then
         return;
      end if;

      --  If the language is null, we simply use the more efficient algorithm

      if Scope = Whole or else Lang = null then
         Scan_Buffer_No_Scope
           (Context     => Context,
            Buffer      => Buffer,
            Start_Index => Buffer_First,
            End_Index   => Buffer'Last,
            Callback    => Callback,
            Ref_Index   => Pos,
            Ref_Line    => Integer (Line),
            Ref_Column  => Column,
            Was_Partial => Was_Partial);
         return;
      end if;

      Language := Get_Language_Context (Lang);

      --  Always find the longest possible range, so that we can benefit
      --  as much as possible from the efficient string searching
      --  algorithms.

      while Pos <= Buffer'Last loop
         Line_Start := Pos;
         Old_State  := Lexical_State;

         Next_Scope_Transition
           (Buffer, Pos, Lexical_State, Section_End, Language.all);

         if Scanning_Allowed (Old_State) then
            Scan_Buffer_No_Scope
              (Context, Buffer, Integer (Line_Start), Section_End,
               Callback, Last_Index, Integer (Line), Column, Was_Partial);

            if Was_Partial then
               Lexical_State := Old_State;
               return;
            end if;
         end if;

         To_Line_Column
           (Buffer (Last_Index .. Buffer'Last), Pos,
            Natural (Line), Column, Dummy, Ignored);
         Last_Index := Pos;
      end loop;

      --  Memorize the lexical state when we found the last match, so that next
      --  time we look for the context we find it correctly.

      Lexical_State := Old_State;
   end Scan_Buffer;

   ---------------
   -- Scan_File --
   ---------------

   procedure Scan_File
     (Context       : access Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Kernel        : GPS.Kernel.Kernel_Handle := null;
      Name          : GNATCOLL.VFS.Virtual_File;
      Callback      : Scan_Callback;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type := 1;
      Start_Column  : Character_Offset_Type := 1;
      Force_Read    : Boolean := False;
      Was_Partial   : out Boolean)
   is
      Lang          : Language_Access;
      Buffer        : GNAT.Strings.String_Access;
      Child         : MDI_Child;
      Start         : Natural;
      Line          : Editable_Line_Type;
      Box           : Source_Editor_Box;
   begin
      --  If there is already an open editor, that might contain local
      --  modification, use its contents, otherwise read the buffer from the
      --  file itself.

      if not Force_Read then
         Child := Get_File_Editor (Kernel, Name);

         if Child /= null then
            Scan_Editor
              (Context,
               Handler,
               Child,
               Callback,
               Scope,
               Lexical_State,
               Start_Line,
               Start_Column,
               Was_Partial);

            return;
         end if;
      end if;

      --  ??? Would be nice to handle backward search, which is extremely hard
      --  with regular expressions

      Was_Partial := False;
      Lang := Get_Language_From_File (Handler, Name);

      if Child = null then
         Buffer := Read_File (Name);

         if Buffer = null then
            return;
         end if;

         Line  := 1;
         Start := Buffer'First;

         while Line < Start_Line loop
            Start := Start + 1;

            exit when Start > Buffer'Last;

            if Buffer (Start) = ASCII.LF then
               Line := Line + 1;
            end if;
         end loop;

         Start := Start + Natural (Start_Column) - 1;

         declare
            UTF8  : Unchecked_String_Access;
            Len   : Natural;
            Valid : Boolean;
         begin
            UTF8_Utils.Unknown_To_UTF8 (Buffer.all, UTF8, Len, Valid);
            if Valid then
               if UTF8 = null then
                  --  This means that Buffer is already UTF8: use it
                  Scan_Buffer
                    (Buffer.all, Start, Context, Callback, Scope,
                     Lexical_State, Lang, Start_Line, Start_Column,
                     Was_Partial);
               else
                  --  Use UTF8
                  Scan_Buffer
                    (UTF8 (1 .. Len), Start, Context, Callback, Scope,
                     Lexical_State, Lang, Start_Line, Start_Column,
                     Was_Partial);
                  Free (UTF8);
               end if;
            end if;
         end;
      else
         Box := Get_Source_Box_From_MDI (Child);

         if not Is_Valid_Position
           (Get_Buffer (Box), Start_Line, Start_Column)
         then
            return;
         end if;

         Buffer := new String'
           (Get_Text (Get_Buffer (Box), Start_Line, 1));

         Start := Natural (Start_Column);

         if Start <= Buffer'Last then
            Scan_Buffer
              (Buffer.all, Start, Context, Callback, Scope,
               Lexical_State, Lang, Start_Line, Start_Column, Was_Partial);
         end if;
      end if;

      Free (Buffer);

   exception
      when Invalid_Context =>
         Free (Buffer);
   end Scan_File;

   -----------------
   -- Scan_Editor --
   -----------------

   procedure Scan_Editor
     (Context       : access Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Editor        : MDI_Child;
      Callback      : Scan_Callback;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type := 1;
      Start_Column  : Character_Offset_Type := 1;
      Was_Partial   : out Boolean)
   is
      Lang   : Language_Access;
      Buffer : GNAT.Strings.String_Access;
      Start  : Natural;
      Box    : Source_Editor_Box;
   begin
      --  ??? Would be nice to handle backward search, which is extremely hard
      --  with regular expressions

      Box := Get_Source_Box_From_MDI (Editor);

      Was_Partial := False;
      Lang := Get_Language_From_File (Handler, Get_Filename (Box));

      if not Is_Valid_Position
        (Get_Buffer (Box), Start_Line, Start_Column)
      then
         return;
      end if;

      Buffer := new String'
        (Get_Text (Get_Buffer (Box), Start_Line, 1));

      Start := Natural (Start_Column);

      if Start <= Buffer'Last then
         Scan_Buffer
           (Buffer.all, Start, Context, Callback, Scope,
            Lexical_State, Lang, Start_Line, Start_Column, Was_Partial);
      end if;

      Free (Buffer);
   exception
      when Invalid_Context =>
         Free (Buffer);
   end Scan_Editor;

   -----------------------------
   -- Locations_Category_Name --
   -----------------------------

   function Locations_Category_Name (Look_For : String) return String is
   begin
      return -"Search for: " & Look_For;
   end Locations_Category_Name;

   ----------------------
   -- Highlight_Result --
   ----------------------

   procedure Highlight_Result
     (Kernel      : access Kernel_Handle_Record'Class;
      File_Name   : Virtual_File;
      Look_For    : String;
      Match       : Match_Result;
      Give_Focus  : Boolean;
      Interactive : Boolean)
   is
      function To_Positive (N : Natural) return Positive;
      --  If N > 0 then return N else return 1.

      -----------------
      -- To_Positive --
      -----------------

      function To_Positive (N : Natural) return Positive is
      begin
         if N = 0 then
            return 1;
         else
            return Positive (N);
         end if;
      end To_Positive;

   begin
      if Match.Begin_Line = Match.End_Line then
         if Interactive then
            Open_File_Editor
              (Kernel,
               File_Name,
               Match.Begin_Line,
               Match.Visible_Begin_Column,
               Match.Visible_End_Column,
               Focus => Give_Focus);
            Push_Current_Editor_Location_In_History (Kernel);

         else
            declare
               Message : constant Markup_Message_Access :=
                 Create_Markup_Message
                   (Get_Messages_Container (Kernel),
                    Locations_Category_Name (Look_For),
                    File_Name,
                    To_Positive (Match.Begin_Line),
                    Match.Visible_Begin_Column,
                    Match.Text,
                    0,
                    (Editor_Side => True, Locations => True));

            begin
               Message.Set_Highlighting
                 (Get_Or_Create_Style_Copy
                    (Kernel_Handle (Kernel),
                     Get_Name (Search_Results_Style)
                     & '/' & Locations_Category_Name (Look_For),
                     Search_Results_Style),
                  Integer (Match.Visible_End_Column
                    - Match.Visible_Begin_Column));
            end;
         end if;
      else
         --  When the location spans on multiple lines, we base the length
         --  to highlight on the pattern length.
         --  ??? This is not compatible with UTF-8
         if Interactive then
            Open_File_Editor
              (Kernel,
               File_Name,
               Match.Begin_Line,
               Match.Visible_Begin_Column,
               Match.Visible_Begin_Column
               + Visible_Column_Type (Match.Pattern_Length),
               Focus => Give_Focus);
            Push_Current_Editor_Location_In_History (Kernel);

         else
            declare
               Message : constant Markup_Message_Access :=
                 Create_Markup_Message
                   (Get_Messages_Container (Kernel),
                    Locations_Category_Name (Look_For),
                    File_Name,
                    To_Positive (Match.Begin_Line),
                    Match.Visible_Begin_Column,
                    Match.Text,
                    0,
                    (Editor_Side => True, Locations => True));

            begin
               Message.Set_Highlighting
                 (Get_Or_Create_Style_Copy
                    (Kernel_Handle (Kernel),
                     Get_Name (Search_Results_Style)
                     & '/' & Locations_Category_Name (Look_For),
                     Search_Results_Style),
                  Match.Pattern_Length);
            end;
         end if;
      end if;
   end Highlight_Result;

   ---------------
   -- Scan_Next --
   ---------------

   procedure Scan_Next
     (Context           : access Search_Context'Class;
      Kernel            : access Kernel_Handle_Record'Class;
      Editor            : access Source_Buffer_Record'Class;
      Scope             : Search_Scope;
      Lexical_State     : in out Recognized_Lexical_States;
      Lang              : Language_Access;
      Current_Line      : Editable_Line_Type;
      Current_Column    : Character_Offset_Type;
      Backward          : Boolean;
      Dialog_On_Failure : Boolean := True;
      Result            : out Match_Result_Access)
   is
      Continue_Till_End : Boolean := False;

      function Continue_Dialog (Message : String) return Boolean;
      --  Popup a dialog asking whether the user wants to continue, and return
      --  the result.

      function Stop_At_First_Callback (Match : Match_Result) return Boolean;
      --  Stop at the first match encountered

      function Backward_Callback (Match : Match_Result) return Boolean;
      --  Return the last match just before Current_Line and Current_Column

      ----------------------------
      -- Stop_At_First_Callback --
      ----------------------------

      function Stop_At_First_Callback (Match : Match_Result) return Boolean is
      begin
         Result := new Match_Result'(Match);
         return False;
      end Stop_At_First_Callback;

      -----------------------
      -- Backward_Callback --
      -----------------------

      function Backward_Callback (Match : Match_Result) return Boolean is
      begin
         --  If we have already found a match, and the current one is after the
         --  current position, we can stop there. Else, if we have passed the
         --  current position but don't have any match yet, we have to return
         --  the last match.
         if Match.Begin_Line > Integer (Current_Line)
           or else (Match.Begin_Line = Integer (Current_Line)
                    and then Match.End_Column >= Current_Column)
         then
            if not Continue_Till_End
              and then Result /= null
            then
               return False;
            end if;

            Continue_Till_End := True;
         end if;

         Unchecked_Free (Result);
         Result := new Match_Result'(Match);
         return True;
      end Backward_Callback;

      ---------------------
      -- Continue_Dialog --
      ---------------------

      function Continue_Dialog (Message : String) return Boolean is
         Buttons : Message_Dialog_Buttons;
      begin
         if Dialog_On_Failure then
            Buttons := Message_Dialog
              (Message,
               Confirmation,
               Button_Yes or Button_No,
               Button_Yes,
               "",
               -"Continue search ?",
               Justify_Center,
               Get_Current_Window (Kernel));

            return Buttons = Button_Yes;
         else
            return False;
         end if;
      end Continue_Dialog;

      Was_Partial       : Boolean;
      Buffer_Text       : GNAT.Strings.String_Access;

   begin
      Result := null;

      if Backward then
         Buffer_Text := Get_String (Source_Buffer (Editor));
         Scan_Buffer
           (Buffer_Text.all, 1, Context,
            Backward_Callback'Unrestricted_Access, Scope,
            Lexical_State, Lang,
            Was_Partial => Was_Partial);
         Free (Buffer_Text);

         --  Start from the end if necessary.

         if Continue_Till_End
           and then not Continue_Dialog
             (-"No more matches, restart from the end ?")
         then
            Stop_Macro (Kernel);
            Set_End_Notif_Done (Context.all, True);
            Unchecked_Free (Result);
            return;
         end if;

      else
         Scan_Buffer
           (Buffer        => Get_Text (Editor, Current_Line, 1),
            Buffer_First  => Natural (Current_Column),
            Context       => Context,
            Callback      => Stop_At_First_Callback'Unrestricted_Access,
            Scope         => Scope,
            Lexical_State => Lexical_State,
            Lang          => Lang,
            Ref_Line      => Current_Line,
            Ref_Column    => Current_Column,
            Was_Partial   => Was_Partial);

         --  Start from the beginning if necessary.
         --  Do not display the continue dialog if starting search from the
         --  beginning.

         if Result = null then
            if not Continue_Till_End then
               if Current_Line = 1 and then Current_Column = 1 then
                  return;
               elsif not Continue_Dialog
                 (-"No more matches, restart from the beginning ?")
               then
                  Stop_Macro (Kernel);
                  Set_End_Notif_Done (Context.all, True);
                  return;
               end if;
            end if;

            Lexical_State := Statements;
            Buffer_Text := Get_String (Source_Buffer (Editor));
            Scan_Buffer
              (Buffer_Text.all, 1, Context,
               Stop_At_First_Callback'Unrestricted_Access, Scope,
               Lexical_State, Lang,
               Was_Partial => Was_Partial);
            Free (Buffer_Text);
         end if;
      end if;
   end Scan_Next;

   --------------------
   -- Scan_And_Store --
   --------------------

   function Scan_And_Store
     (Context  : access Search_Context'Class;
      Handler  : access Language_Handler_Record'Class;
      Kernel   : Kernel_Handle;
      Str      : String := "";
      File     : Virtual_File := GNATCOLL.VFS.No_File;
      Scope    : Search_Scope;
      Lang     : Language_Access := null) return Match_Result_Array_Access
   is
      Result : Match_Result_Array_Access := null;

      function Callback (Match : Match_Result) return Boolean;
      --  Save Match in the result array.

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
         Tmp  : Match_Result_Array_Access;
      begin
         Tmp := Result;
         if Tmp = null then
            Result := new Match_Result_Array (1 .. 1);
         else
            Result := new Match_Result_Array (1 .. Tmp'Last + 1);
         end if;

         if Tmp /= null then
            Result (1 .. Tmp'Last) := Tmp.all;
            Unchecked_Free (Tmp);
         end if;

         Result (Result'Last) := new Match_Result'(Match);
         return True;
      end Callback;

      State       : Recognized_Lexical_States := Statements;
      Was_Partial : Boolean;
   begin
      if Str /= "" then
         Scan_Buffer (Str, Str'First, Context,
                      Callback'Unrestricted_Access, Scope,
                      Lexical_State => State,
                      Lang          => Lang,
                      Was_Partial   => Was_Partial);
      elsif File /= GNATCOLL.VFS.No_File then
         Scan_File (Context,
                    Handler, Kernel,
                    File, Callback'Unrestricted_Access, Scope,
                    Lexical_State => State,
                    Force_Read    => Kernel = null,
                    Was_Partial   => Was_Partial);
      end if;

      return Result;
   end Scan_And_Store;

   -----------------
   -- First_Match --
   -----------------

   procedure First_Match
     (Context       : access Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Kernel        : Kernel_Handle;
      Name          : GNATCOLL.VFS.Virtual_File;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type := 1;
      Start_Column  : Character_Offset_Type := 1;
      Result        : out Match_Result_Access;
      Force_Read    : Boolean := False)
   is
      function Callback (Match : Match_Result) return Boolean;
      --  Save Match in the result array.

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
      begin
         Result := new Match_Result'(Match);
         return False;
      end Callback;

      Was_Partial : Boolean;
   begin
      Scan_File (Context, Handler, Kernel,
                 Name, Callback'Unrestricted_Access, Scope,
                 Lexical_State, Start_Line, Start_Column, Force_Read,
                 Was_Partial);
   end First_Match;

   -----------------
   -- First_Match --
   -----------------

   procedure First_Match
     (Context       : access Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Editor        : MDI_Child;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type := 1;
      Start_Column  : Character_Offset_Type := 1;
      Result        : out Match_Result_Access)
   is
      function Callback (Match : Match_Result) return Boolean;
      --  Save Match in the result array.

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
      begin
         Result := new Match_Result'(Match);
         return False;
      end Callback;

      Was_Partial : Boolean;
   begin
      Scan_Editor (Context, Handler,
                 Editor, Callback'Unrestricted_Access, Scope,
                 Lexical_State, Start_Line, Start_Column,
                 Was_Partial);
   end First_Match;

   ----------
   -- Free --
   ----------

   procedure Free (Result : in out Match_Result_Array_Access) is
   begin
      if Result /= null then
         for R in Result'Range loop
            Unchecked_Free (Result (R));
         end loop;

         Unchecked_Free (Result);
      end if;
   end Free;

   overriding procedure Free (Context : in out Files_Context) is
   begin
      Directory_List.Free (Context.Dirs);
      Context.At_End := True;
      Free (Search_Context (Context));
   end Free;

   overriding procedure Free (Context : in out Files_Project_Context) is
   begin
      Unchecked_Free (Context.Files);
      Free (Search_Context (Context));
   end Free;

   overriding procedure Free (Context : in out Open_Files_Context) is
   begin
      Unchecked_Free (Context.Files);
      Free (Search_Context (Context));
   end Free;

   -------------------
   -- Set_File_List --
   -------------------

   procedure Set_File_List
     (Context : access Files_Project_Context;
      Files   : File_Array_Access) is
   begin
      Unchecked_Free (Context.Files);
      Context.Files := Files;
      Context.Current_File := Context.Files'First - 1;
   end Set_File_List;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   overriding function Get_Current_Progress
     (Context : access Files_Project_Context) return Integer is
   begin
      return Context.Current_File;
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   overriding function Get_Total_Progress
     (Context : access Files_Project_Context) return Integer is
   begin
      if Context.Files = null then
         return 0;
      else
         return Context.Files'Length;
      end if;
   end Get_Total_Progress;

   -------------------
   -- Set_File_List --
   -------------------

   procedure Set_File_List
     (Context : access Open_Files_Context;
      Files   : File_Array_Access) is
   begin
      Unchecked_Free (Context.Files);
      Context.Files := Files;
      Context.Current_File := Context.Files'First - 1;
   end Set_File_List;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   overriding function Get_Current_Progress
     (Context : access Open_Files_Context) return Integer is
   begin
      return Context.Current_File;
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   overriding function Get_Total_Progress
     (Context : access Open_Files_Context) return Integer is
   begin
      if Context.Files = null then
         return 0;
      else
         return Context.Files'Length;
      end if;
   end Get_Total_Progress;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   overriding function Get_Current_Progress
     (Context : access Files_Context) return Integer is
   begin
      return Context.Current_Dir;
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   overriding function Get_Total_Progress
     (Context : access Files_Context) return Integer is
   begin
      return Context.Total_Dirs;
   end Get_Total_Progress;

   -------------------
   -- Set_File_List --
   -------------------

   procedure Set_File_List
     (Context       : access Files_Context;
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : Virtual_File  := No_File;
      Recurse       : Boolean := False) is
   begin
      Context.Files_Pattern := Files_Pattern;
      Context.Recurse := Recurse;

      if Directory = No_File then
         Context.Directory := Get_Current_Dir;
      else
         Ensure_Directory (Directory);
         Context.Directory := Directory;
      end if;
   end Set_File_List;

   --------------------------
   -- Current_File_Factory --
   --------------------------

   function Current_File_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences   : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget) return Search_Context_Access
   is
      Scope    : constant Scope_Selector := Scope_Selector (Extra_Information);
   begin
      return Current_File_Factory
        (Kernel, All_Occurrences,
         Scope => Search_Scope'Val (Get_Active (Scope.Combo)));
   end Current_File_Factory;

   --------------------------
   -- Current_File_Factory --
   --------------------------

   function Current_File_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences   : Boolean;
      Scope             : Search_Scope := Whole) return Search_Context_Access
   is
      pragma Unreferenced (Kernel);

      Context  : Current_File_Context_Access;
   begin
      --  If we are looking for all the occurrences, we simply reuse another
      --  context, instead of the interactive Current_File_Context

      Context := new Current_File_Context;
      Context.All_Occurrences := All_Occurrences;
      Context.Scope := Scope;
      return Search_Context_Access (Context);
   end Current_File_Factory;

   --------------------------------
   -- Files_From_Project_Factory --
   --------------------------------

   function Files_From_Project_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences   : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget) return Search_Context_Access
   is
      Scope   : constant Scope_Selector := Scope_Selector (Extra_Information);
      Context : constant Files_Project_Context_Access :=
                  new Files_Project_Context;
   begin
      Context.Scope      := Search_Scope'Val (Get_Active (Scope.Combo));
      Context.All_Occurrences := All_Occurrences;
      Context.Begin_Line      := 0;
      Set_File_List (Context, Get_Project (Kernel).Source_Files (True));
      return Search_Context_Access (Context);
   end Files_From_Project_Factory;

   --------------------------------
   -- Files_From_Project_Factory --
   --------------------------------

   function Files_From_Project_Factory
     (Scope           : Search_Scope;
      All_Occurrences : Boolean) return Files_Project_Context_Access
   is
      Context : constant Files_Project_Context_Access :=
                  new Files_Project_Context;
   begin
      Context.Scope           := Scope;
      Context.All_Occurrences := All_Occurrences;
      Context.Begin_Line      := 0;
      return Context;
   end Files_From_Project_Factory;

   ---------------------------
   -- Get_Terminate_Message --
   ---------------------------

   overriding function Get_Terminate_Message
     (Context : access Files_Project_Context;
      Kind    : Operation_Kind) return String
   is
      pragma Unreferenced (Context);
   begin
      case Kind is
         when Replace =>
            return -"Finished replacing the string in files from project";
         when Search =>
            return "";
      end case;
   end Get_Terminate_Message;

   ------------------------
   -- Open_Files_Factory --
   ------------------------

   function Open_Files_Factory
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences    : Boolean;
      Extra_Information  : Gtk.Widget.Gtk_Widget) return Search_Context_Access
   is
      Scope : constant Scope_Selector := Scope_Selector (Extra_Information);
      Context : constant Open_Files_Context_Access := new Open_Files_Context;
      Open_File_List : GNATCOLL.VFS.File_Array_Access;

   begin
      --  GPS.Kernel.Open_Files returns a File_Array, but Set_File_List
      --  takes a File_Array_Access. Memory will be properly freed in
      --  Set_File_List

      Open_File_List          := new File_Array'(Open_Files (Kernel));
      Context.Scope           :=
        Search_Scope'Val (Get_Active (Scope.Combo));
      Context.All_Occurrences := All_Occurrences;
      Context.Begin_Line      := 0;
      Set_File_List (Context, Open_File_List);
      return Search_Context_Access (Context);
   end Open_Files_Factory;

   ---------------------------
   -- Get_Terminate_Message --
   ---------------------------

   overriding function Get_Terminate_Message
     (Context : access Open_Files_Context;
      Kind    : Operation_Kind) return String
   is
      pragma Unreferenced (Context);
   begin
      case Kind is
         when Replace =>
            return -"Finished replacing the string in open files";
         when Search =>
            return "";
      end case;
   end Get_Terminate_Message;

   -------------------
   -- Files_Factory --
   -------------------

   function Files_Factory
     (All_Occurrences : Boolean;
      Scope           : Search_Scope) return Files_Context_Access
   is
      Context : constant Files_Context_Access := new Files_Context;
   begin
      Context.Scope := Scope;
      Context.All_Occurrences := All_Occurrences;
      Context.Begin_Line := 0;
      return Context;
   end Files_Factory;

   -------------------
   -- Files_Factory --
   -------------------

   function Files_Factory
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences    : Boolean;
      Extra_Information  : Gtk.Widget.Gtk_Widget) return Search_Context_Access
   is
      pragma Unreferenced (Kernel);

      Context : Files_Context_Access;
      Extra   : constant Files_Extra_Scope := Files_Extra_Scope
        (Extra_Information);
      Re      : GNAT.Regexp.Regexp;

   begin
      if Get_Text (Extra.Files_Entry) /= "" then
         Context := Files_Factory
           (All_Occurrences,
            Search_Scope'Val (Get_Active (Extra.Combo)));
         Re := Compile
           (Get_Text (Extra.Files_Entry),
            Glob => True,
            Case_Sensitive => Integer (Get_File_Names_Case_Sensitive) /= 0);
         Set_File_List
           (Context,
            Files_Pattern => Re,
            Directory     =>
              Create_From_UTF8 (Get_Text (Extra.Directory_Entry)),
            Recurse       => Get_Active (Extra.Subdirs_Check));

         return Search_Context_Access (Context);
      end if;

      Trace (Me, "Files_Factory: no files pattern specified");
      return null;

   exception
      when Error_In_Regexp =>
         return null;
   end Files_Factory;

   ---------------------------
   -- Get_Terminate_Message --
   ---------------------------

   overriding function Get_Terminate_Message
     (Context : access Files_Context;
      Kind    : Operation_Kind) return String
   is
      pragma Unreferenced (Context);
   begin
      case Kind is
         when Replace =>
            return -"Finished replacing the string in selected files";
         when Search =>
            return "";
      end case;
   end Get_Terminate_Message;

   ----------------------
   -- Search_In_Editor --
   ----------------------

   procedure Search_In_Editor
     (Context         : access Current_File_Context;
      Start_At        : Gtk.Text_Iter.Gtk_Text_Iter;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean;
      Dialog_On_Failure : Boolean := True;
      Match_From      : out Gtk.Text_Iter.Gtk_Text_Iter;
      Match_Up_To     : out Gtk.Text_Iter.Gtk_Text_Iter;
      Found           : out Boolean)
   is
      Editor : constant Source_Buffer := Source_Buffer (Get_Buffer (Start_At));
      Lang   : Language_Access;
      Match  : Match_Result_Access;
      Column : Character_Offset_Type;
      Line   : Editable_Line_Type;
   begin
      Assert (Me, not Context.All_Occurrences,
              "All occurrences not supported for current_file_context");

      Lang := Get_Language_From_File
        (Get_Language_Handler (Kernel), Get_Filename (Editor));
      Get_Iter_Position (Editor, Start_At, Line, Column);

      --  If we had a previous selection, and it had a null length, move the
      --  cursor forward, otherwise we would keep hitting the same match. Of
      --  course, if the cursor was moved by the user since then, we do not
      --  have anything to do.

      if Context.Begin_Column /= 0
        and then Context.End_Column = Column
        and then Context.Begin_Column = Context.End_Column
      then
         --  The test below will return True if the character after the current
         --  one is eol.
         if Is_Valid_Position
           (Editor, Gint (Context.End_Line - 1), Gint (Column))
           and then not Ends_Line (Start_At)
         then
            Column := Column + 1;
         else
            Line   := Line + 1;
            Column := 1;
         end if;
      end if;

      Scan_Next
        (Context, Kernel,
         Editor         => Editor,
         Scope          => Context.Scope,
         Lexical_State  => Context.Current_Lexical,
         Lang           => Lang,
         Current_Line   => Line,
         Current_Column => Column,
         Dialog_On_Failure => Dialog_On_Failure,
         Backward       => Search_Backward,
         Result         => Match);

      if Match /= null then
         Found := True;
         Context.Begin_Line   := Editable_Line_Type (Match.Begin_Line);
         Context.Begin_Column := Match.Begin_Column;

         Context.End_Line     := Editable_Line_Type (Match.End_Line);
         Context.End_Column   := Match.End_Column;

         Get_Iter_At_Line_Offset
           (Editor, Match_From,
            Gint (Get_Buffer_Line (Editor, Context.Begin_Line) - 1),
            Gint (Context.Begin_Column - 1));
         Get_Iter_At_Line_Offset
           (Editor, Match_Up_To,
            Gint (Get_Buffer_Line (Editor, Context.End_Line) - 1),
            Gint (Context.End_Column - 1));

         Unchecked_Free (Match);

      else
         Found := False;

         --  The search could not be made, invalidate the context
         --  in case it was the last search in the file.
         Context.End_Line   := Context.Begin_Line;
         Context.End_Column := Context.Begin_Column;
      end if;
   end Search_In_Editor;

   ---------------------------
   -- Get_Terminate_Message --
   ---------------------------

   overriding function Get_Terminate_Message
     (Context : access Current_File_Context;
      Kind    : Operation_Kind) return String
   is
      pragma Unreferenced (Context);
   begin
      case Kind is
         when Replace =>
            return -"Finished replacing the string in current file";
         when Search =>
            return "";
      end case;
   end Get_Terminate_Message;

   ----------------------
   -- Auxiliary_Search --
   ----------------------

   function Auxiliary_Search
     (Context         : access Current_File_Context;
      Editor          : Source_Editor_Box;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean
   is
      Selection_Start : Gtk_Text_Iter;
      Selection_End   : Gtk_Text_Iter;
      Dummy           : Boolean;
      Match_From      : Gtk_Text_Iter;
      Match_Up_To     : Gtk_Text_Iter;
      Found           : Boolean;

   begin
      Get_Selection_Bounds
        (Get_Buffer (Editor), Selection_Start, Selection_End, Dummy);

      if Search_Backward then
         Search_In_Editor
           (Context         => Context,
            Start_At        => Selection_End,
            Kernel          => Kernel,
            Search_Backward => Search_Backward,
            Match_From      => Match_From,
            Match_Up_To     => Match_Up_To,
            Found           => Found);
      else
         if not Equal (Selection_Start, Selection_End) then
            --  Selection is not empty ?
            Forward_Char (Selection_Start, Dummy);
         end if;

         Search_In_Editor
           (Context         => Context,
            Start_At        => Selection_End,
            Kernel          => Kernel,
            Search_Backward => Search_Backward,
            Match_From      => Match_From,
            Match_Up_To     => Match_Up_To,
            Found           => Found);
      end if;

      if Found then
         Push_Current_Editor_Location_In_History (Kernel);
         Select_Region (Get_Buffer (Editor), Match_Up_To, Match_From);

         Center_Cursor (Get_View (Editor));
         return True;
      else
         return False;
      end if;
   end Auxiliary_Search;

   ------------
   -- Search --
   ------------

   overriding procedure Search
     (Context         : access Current_File_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Found           : out Boolean;
      Continue        : out Boolean)
   is
      Child  : constant MDI_Child := Find_Current_Editor (Kernel);
      Editor : Source_Editor_Box;

      function Interactive_Callback (Match : Match_Result) return Boolean;
      --  Callbacks for the general search function
      --  ??? This should be factorized somehow with the Search fonction
      --  from the Abstract_File_Context.

      --------------------------
      -- Interactive_Callback --
      --------------------------

      function Interactive_Callback (Match : Match_Result) return Boolean is
      begin
         Found := True;
         if Get_Filename (Editor) /= GNATCOLL.VFS.No_File then
            Highlight_Result
              (Kernel      => Kernel,
               File_Name   => Get_Filename (Editor),
               Look_For    => Context_Look_For (Context),
               Match       => Match,
               Give_Focus  => Give_Focus,
               Interactive => not Context.All_Occurrences);
         else
            Highlight_Result
              (Kernel      => Kernel,
               File_Name   => Editor.Get_Buffer.Get_File_Identifier,
               Look_For    => Context_Look_For (Context),
               Match       => Match,
               Give_Focus  => Give_Focus,
               Interactive => not Context.All_Occurrences);
         end if;

         return True;
      end Interactive_Callback;

      Dummy_Boolean : Boolean;
   begin
      Found := False;

      if Child = null then
         Continue := False;
         return;
      end if;

      Editor := Get_Source_Box_From_MDI (Child);

      Context.Current_File := To_Unbounded_String
        (Get_Filename (Get_Buffer (Editor)).Display_Full_Name);

      Raise_Child (Child, Give_Focus);

      if not Context.All_Occurrences then
         Found := Auxiliary_Search
           (Context, Editor, Kernel, Search_Backward);
         Continue := False; --  ??? Dummy boolean.
      else
         Search_From_Editor
           (Context,
            Get_Language_Handler (Kernel),
            Interactive_Callback'Unrestricted_Access,
            Child,
            Continue,
            Dummy_Boolean);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Found := False;
         Continue := False;
         return;
   end Search;

   -------------------------
   -- Replace_From_Editor --
   -------------------------

   function Replace_From_Editor
     (Context         : access File_Search_Context'Class;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Child           : MDI_Child) return Boolean
   is
      Editor          : Source_Editor_Box;
      Current_Matches : Boolean;
      Matches         : Match_Result_Array_Access;

      type Casings_A is array (Casing_Type) of String (Replace_String'Range);

      Casings : Casings_A;
      Current_Casing : Casing_Type := Unchanged;
   begin
      Casings (Unchanged) := Replace_String;

      if Case_Preserving then
         --  Pre-compute all casings of the string to replace
         for Casing in Lower .. Smart_Mixed loop
            Casings (Casing) := To_Casing (Replace_String, Casing);
         end loop;
      end if;

      Raise_Child (Child, Give_Focus);
      Editor := Get_Source_Box_From_MDI (Child);

      if Context.All_Occurrences then
         Matches := Scan_And_Store
           (Context => Context,
            Handler => Get_Language_Handler (Kernel),
            Kernel  => Kernel_Handle (Kernel),
            Str     => Get_Buffer (Editor),
            File    => GNATCOLL.VFS.No_File,
            Scope   => Context.Scope,
            Lang    => Get_Language (Get_Buffer (Editor)));

         if Matches /= null then
            --  Replace starting from the end, so as to preserve lines and
            --  columns

            Set_Avoid_Cursor_Move_On_Changes (Get_Buffer (Editor), True);

            for M in reverse Matches'Range loop
               if Case_Preserving then
                  Current_Casing := Guess_Casing
                    (Get_Text
                       (Get_Buffer (Editor),
                        Editable_Line_Type (Matches (M).Begin_Line),
                        Matches (M).Begin_Column,
                        Editable_Line_Type (Matches (M).End_Line),
                        Matches (M).End_Column));
               end if;

               Replace_Slice
                 (Get_Buffer (Editor),
                  Editable_Line_Type (Matches (M).Begin_Line),
                  Matches (M).Begin_Column,
                  Editable_Line_Type (Matches (M).End_Line),
                  Matches (M).End_Column,
                  Casings (Current_Casing));
            end loop;

            Set_Avoid_Cursor_Move_On_Changes (Get_Buffer (Editor), False);
            Free (Matches);

            return True;
         else
            return False;
         end if;
      else
         --  Test whether the current context text contains the search string.
         --  Warning: we cannot use selection here, since apparently there can
         --  be only one selection in the whole GPS window, and the selection
         --  in the source buffer will be erased when the focus is given to the
         --  search dialog.

         if Context.Begin_Line > 0
           and then Context.Begin_Column > 0
         then
            if Get_Options (Context).Regexp then
               Current_Matches := Match
                 (Context,
                  Get_Text
                    (Get_Buffer (Editor),
                     Context.Begin_Line,
                     Context.Begin_Column)) /= -1;
            elsif Get_Options (Context).Case_Sensitive then
               Current_Matches := Get_Text
                 (Get_Buffer (Editor),
                  Context.Begin_Line, Context.Begin_Column,
                  Context.End_Line, Context.End_Column) =
                 Context_As_String (Context);
            else
               Current_Matches := UTF8_Strdown
                 (Get_Text
                    (Get_Buffer (Editor),
                     Context.Begin_Line, Context.Begin_Column,
                     Context.End_Line, Context.End_Column)) =
                 UTF8_Strdown (Context_As_String (Context));
            end if;

            if Current_Matches then
               if Case_Preserving then
                  Current_Casing := Guess_Casing
                    (Get_Text
                       (Get_Buffer (Editor),
                        Context.Begin_Line,
                        Context.Begin_Column,
                        Context.End_Line,
                        Context.End_Column));
               end if;

               Replace_Slice
                 (Get_Buffer (Editor),
                  Context.Begin_Line,
                  Context.Begin_Column,
                  Context.End_Line,
                  Context.End_Column,
                  Casings (Current_Casing));

               Forward_Position
                 (Get_Buffer (Editor),
                  Context.Begin_Line,
                  Context.Begin_Column,
                  Casings (Current_Casing)'Length,
                  Context.End_Line,
                  Context.End_Column);

               Push_Current_Editor_Location_In_History (Kernel);

               if Search_Backward then
                  Context.End_Line := Context.Begin_Line;
                  Context.End_Column := Context.Begin_Column;
               else
                  Context.Begin_Line := Context.End_Line;
                  Context.Begin_Column := Context.End_Column - 1;
               end if;

               Set_Cursor_Position
                 (Get_Buffer (Editor),
                  Context.End_Line,
                  Context.End_Column,
                  GPS.Editors.With_Margin,
                  Internal => True);

               Save_Cursor_Position (Get_View (Editor));
            end if;
         end if;
      end if;

      return True;
   end Replace_From_Editor;

   ------------------------
   --  Replace_From_File --
   ------------------------

   function Replace_From_File
     (Context         : access Abstract_Files_Context'Class;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      File            : GNATCOLL.VFS.Virtual_File) return Boolean
   is
      Matches : Match_Result_Array_Access;
      Child   : MDI_Child;

      type Casings_A is array (Casing_Type) of String (Replace_String'Range);

      Casings : Casings_A;
      Current_Casing : Casing_Type := Unchanged;
   begin
      --  If the file is loaded in an editor, do the replacement directly
      --  there.

      Child := Find_Editor (Kernel, File);

      if Child /= null then
         return Replace_From_Editor
           (Context,
            Kernel,
            Replace_String,
            Case_Preserving,
            Search_Backward,
            Give_Focus,
            Child);

         --  Else, file isn't loaded, so we replace directly in the physical
         --  file.
      else
         --  Pre-compute all casings of the string to replace
         Casings (Unchanged) := Replace_String;

         if Case_Preserving then
            for Casing in Lower .. Smart_Mixed loop
               Casings (Casing) := To_Casing (Replace_String, Casing);
            end loop;
         end if;

         --  ??? Could be more efficient, since we have already read the
         --  file to do the search

         Matches := Scan_And_Store
           (Context => Context,
            Handler => Get_Language_Handler (Kernel),
            Kernel  => Kernel_Handle (Kernel),
            File    => File,
            Scope   => Context.Scope,
            Lang    => Get_Language_From_File
              (Get_Language_Handler (Kernel), File));

         if Matches /= null then
            declare
               Buffer   : GNAT.Strings.String_Access;
               FD       : File_Descriptor;
               Len      : Natural;

            begin
               Buffer := Read_File (File);

               if Buffer /= null then
                  --  ???  Should use VFS.Write_File
                  FD := Create_File (+Full_Name (File), Binary);
                  Len := Write (FD, Buffer (1)'Address,
                                Matches (Matches'First).Index - 1);
                  Len := Write (FD, Replace_String'Address,
                                Replace_String'Length);

                  for M in Matches'First + 1 .. Matches'Last loop
                     if Case_Preserving then
                        Current_Casing := Guess_Casing
                          (Buffer
                             (Matches (M).Index
                              .. Matches (M).Index
                              + Replace_String'Length - 1));
                     end if;

                     Len := Matches (M - 1).Index
                       + Matches (M - 1).Pattern_Length;
                     Len := Write
                       (FD, Buffer (Len)'Address, Matches (M).Index - Len);
                     Len := Write (FD, Casings (Current_Casing)'Address,
                                   Casings (Current_Casing)'Length);
                  end loop;

                  Len := Matches (Matches'Last).Index
                    + Matches (Matches'Last).Pattern_Length;
                  Len := Write
                    (FD, Buffer (Len)'Address, Buffer'Last - Len + 1);
                  Close (FD);
                  Free (Buffer);
               end if;
            end;
         end if;

         Free (Matches);
      end if;

      return True;
   end Replace_From_File;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Context         : access Current_File_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean) return Boolean
   is
      Child : constant MDI_Child := Find_Current_Editor (Kernel);

      Found : Boolean;
   begin
      if Child = null then
         return False;
      end if;

      Found := Replace_From_Editor
        (Context, Kernel, Replace_String,
         Case_Preserving, Search_Backward, Give_Focus, Child);

      return not Context.All_Occurrences and then Found;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Replace;

   ----------------------
   -- Search_From_File --
   ----------------------

   procedure Search_From_File
     (Context       : access File_Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Kernel        : Kernel_Handle;
      Callback      : Scan_Callback;
      File          : GNATCOLL.VFS.Virtual_File;
      More_Matches  : out Boolean;
      Matches_Found : out Boolean)
   is
      Match  : Match_Result_Access;
   begin
      More_Matches := False;
      Matches_Found := False;

      if File = GNATCOLL.VFS.No_File then
         return;
      end if;

      if not Context.All_Occurrences then
         --  Are there any more match in the current file ?
         --  Stop looking in this file if the editor was closed (we know it was
         --  opened when the first match was seen)

         if Context.Begin_Line /= 0 then
            First_Match
              (Context       => Context,
               Handler       => Handler,
               Kernel        => Kernel,
               Name          => File,
               Scope         => Context.Scope,
               Lexical_State => Context.Current_Lexical,
               Start_Line    => Context.Begin_Line,
               Start_Column  => Context.Begin_Column + 1,
               Result        => Match,
               Force_Read    => Kernel = null);

            if Match /= null then
               Context.Begin_Line   := Editable_Line_Type (Match.Begin_Line);
               Context.Begin_Column := Match.Begin_Column;
               Context.End_Line     := Editable_Line_Type (Match.End_Line);
               Context.End_Column   := Match.End_Column;
               More_Matches := Callback (Match.all);
               Matches_Found := True;
               Unchecked_Free (Match);

               return;
            end if;
         end if;

         More_Matches := False;

         --  Non interactive mode
      else
         declare
            State : Recognized_Lexical_States := Statements;
            Was_Partial : Boolean;
         begin
            Scan_File
              (Context,
               Handler,
               Kernel,
               File, Callback, Context.Scope,
               Lexical_State => State,
               Force_Read    => Kernel = null,
               Was_Partial   => Was_Partial);

            Matches_Found := True;
            More_Matches := Was_Partial;
         end;
      end if;
   end Search_From_File;

   ------------------------
   -- Search_From_Editor --
   ------------------------

   procedure Search_From_Editor
     (Context       : access File_Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Callback      : Scan_Callback;
      Editor        : MDI_Child;
      More_Matches  : out Boolean;
      Matches_Found : out Boolean)
   is
      Match  : Match_Result_Access;
   begin
      More_Matches := False;
      Matches_Found := False;

      if Editor = null then
         return;
      end if;

      if not Context.All_Occurrences then
         --  Are there any more match in the current file ?
         --  Stop looking in this file if the editor was closed (we know it was
         --  opened when the first match was seen)

         if Context.Begin_Line /= 0 then
            First_Match
              (Context       => Context,
               Handler       => Handler,
               Editor        => Editor,
               Scope         => Context.Scope,
               Lexical_State => Context.Current_Lexical,
               Start_Line    => Context.Begin_Line,
               Start_Column  => Context.Begin_Column + 1,
               Result        => Match);

            if Match /= null then
               Context.Begin_Line   := Editable_Line_Type (Match.Begin_Line);
               Context.Begin_Column := Match.Begin_Column;
               Context.End_Line     := Editable_Line_Type (Match.End_Line);
               Context.End_Column   := Match.End_Column;
               More_Matches := Callback (Match.all);
               Matches_Found := True;
               Unchecked_Free (Match);

               return;
            end if;
         end if;

         More_Matches := False;

         --  Non interactive mode
      else
         declare
            State : Recognized_Lexical_States := Statements;
            Was_Partial : Boolean;
         begin
            Scan_Editor
              (Context,
               Handler,
               Editor, Callback, Context.Scope,
               Lexical_State => State,
               Was_Partial   => Was_Partial);

            Matches_Found := True;
            More_Matches := Was_Partial;
         end;
      end if;
   end Search_From_Editor;

   ------------
   -- Search --
   ------------

   function Search
     (Context  : access Abstract_Files_Context;
      Handler  : access Language_Handler_Record'Class;
      Kernel   : Kernel_Handle;
      Callback : Scan_Callback)
      return Boolean
   is
      C      : constant Abstract_Files_Context_Access :=
                 Abstract_Files_Context_Access (Context);
      --  For dispatching purposes

      Button : Message_Dialog_Buttons;

      More_Matches : Boolean := False;
      Matches_Found : Boolean := False;
      Already_Looped : Boolean := False;

   begin
      if not Context.All_Occurrences then
         loop
            Search_From_File
              (Context, Handler, Kernel, Callback, Current_File (C),
               More_Matches, Matches_Found);

            if not Matches_Found then
               Move_To_Next_File (C);

               Context.Begin_Line := 1;
               Context.Begin_Column := 0;

               if Current_File (C) = GNATCOLL.VFS.No_File then
                  if not Already_Looped then
                     Button := Message_Dialog
                       (Msg           => (-"No more occurrences of '")
                        & Context_Look_For (C) &
                        (-("' found."
                           & ASCII.LF
                           & (-"in ")
                           & Context_Look_In (C.all) & ASCII.LF
                           & "Search from the beginning ?")),
                        Title         => -"Search",
                        Buttons       => Button_Yes or Button_No,
                        Justification => Justify_Left,
                        Parent        => Get_Current_Window (Kernel));

                     Already_Looped := True;

                     if Button = Button_Yes then
                        Move_To_First_File (C);
                     else
                        Stop_Macro (Kernel);
                        Set_End_Notif_Done (Context.all, True);

                        return False;
                     end if;
                  else
                     return False;
                  end if;
               end if;
            else
               return True;
            end if;
         end loop;

      --  Non interactive mode
      else
         Move_To_Next_File (C);

         if Current_File (C) = GNATCOLL.VFS.No_File then
            return False;

         else
            More_Matches := True;

            while More_Matches loop
               Search_From_File
                 (Context, Handler, Kernel, Callback, Current_File (C),
                  More_Matches, Matches_Found);
            end loop;

            return True;
         end if;
      end if;
   end Search;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset
     (Context : access Abstract_Files_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Get_Messages_Container (Kernel).Remove_Category
        (Locations_Category_Name (Context_Look_For (Context)),
         (Editor_Side => True,
          Locations   => True));
   end Reset;

   ------------
   -- Search --
   ------------

   overriding procedure Search
     (Context         : access Abstract_Files_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Found           : out Boolean;
      Continue        : out Boolean)
   is
      C : constant Abstract_Files_Context_Access :=
        Abstract_Files_Context_Access (Context);
      --  For dispatching purposes

      function Interactive_Callback (Match : Match_Result) return Boolean;
      --  Callbacks for the general search function

      --------------------------
      -- Interactive_Callback --
      --------------------------

      function Interactive_Callback (Match : Match_Result) return Boolean is
      begin
         Found := True;
         Highlight_Result
           (Kernel      => Kernel,
            File_Name   => Current_File (C),
            Look_For    => Context_Look_For (C),
            Match       => Match,
            Give_Focus  => Give_Focus,
            Interactive => not Context.All_Occurrences);
         return True;
      end Interactive_Callback;

   begin
      Found := False;
      Continue := False;
      if not Search_Backward then
         Continue := Search
           (Context,
            Get_Language_Handler (Kernel),
            Kernel_Handle (Kernel),
            Interactive_Callback'Unrestricted_Access);
      end if;
   end Search;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Context         : access Abstract_Files_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean) return Boolean
   is
      C            : constant Abstract_Files_Context_Access :=
                       Abstract_Files_Context_Access (Context);
      --  For dispatching purposes

      Interactive  : constant Boolean := not Context.All_Occurrences;

      Child        : MDI_Child;

      Dummy        : Boolean;
      pragma Unreferenced (Dummy);
   begin
      --  If we already have an occurrence, and the file is still open, the
      --  selection still there,... (which means the user hasn't touched
      --  anything), then do the actual replacement

      if Interactive then
         if Context.Begin_Line /= 0
           and then Is_Open (Kernel, Current_File (C))
         then
            Child := Find_Editor (Kernel, Current_File (C));

            if Child /= null then
               return Replace_From_Editor
                 (Context,
                  Kernel,
                  Replace_String,
                  Case_Preserving,
                  Search_Backward,
                  Give_Focus,
                  Child);
            end if;
         end if;

         return True;

      --  Non interactive case
      else
         Move_To_Next_File (C);

         if Current_File (C) = GNATCOLL.VFS.No_File then
            return False;
         end if;

         Dummy := Replace_From_File
           (Context,
            Kernel,
            Replace_String,
            Case_Preserving,
            Search_Backward,
            Give_Focus,
            Current_File (C));

         return True;
      end if;
   end Replace;

   ------------------
   -- Current_File --
   ------------------

   overriding function Current_File
     (Context : access Files_Project_Context) return GNATCOLL.VFS.Virtual_File
   is
   begin
      if Context.Files /= null
        and then Context.Current_File in Context.Files'Range
      then
         return Context.Files (Context.Current_File);
      else
         return GNATCOLL.VFS.No_File;
      end if;
   end Current_File;

   ------------------------
   -- Move_To_First_File --
   ------------------------

   overriding procedure Move_To_First_File
     (Context : access Files_Project_Context) is
   begin
      Context.Current_File    := 1;
      Context.Current_Lexical := Statements;
   end Move_To_First_File;

   -----------------------
   -- Move_To_Next_File --
   -----------------------

   overriding procedure Move_To_Next_File
     (Context : access Files_Project_Context) is
   begin
      Context.Current_File    := Context.Current_File + 1;
      Context.Current_Lexical := Statements;
   end Move_To_Next_File;

   ------------------
   -- Current_File --
   ------------------

   overriding function Current_File
     (Context : access Open_Files_Context) return GNATCOLL.VFS.Virtual_File
   is
   begin
      if Context.Files /= null
        and then Context.Current_File in Context.Files'Range
      then
         return Context.Files (Context.Current_File);
      else
         return GNATCOLL.VFS.No_File;
      end if;
   end Current_File;

   ------------------------
   -- Move_To_First_File --
   ------------------------

   overriding procedure Move_To_First_File
     (Context : access Open_Files_Context) is
   begin
      Context.Current_File    := 1;
      Context.Current_Lexical := Statements;
   end Move_To_First_File;

   -----------------------
   -- Move_To_Next_File --
   -----------------------

   overriding procedure Move_To_Next_File
     (Context : access Open_Files_Context) is
   begin
      Context.Current_File    := Context.Current_File + 1;
      Context.Current_Lexical := Statements;
   end Move_To_Next_File;

   ------------------
   -- Current_File --
   ------------------

   overriding function Current_File
     (Context : access Files_Context) return Virtual_File is
   begin
      return Context.Current_File;
   end Current_File;

   ------------------------
   -- Move_To_First_File --
   ------------------------

   overriding procedure Move_To_First_File (Context : access Files_Context) is
   begin
      --  ??? Can this function be called at any other place than when the end
      --  is reached ?
      Context.At_End := False;
      Directory_List.Free (Context.Dirs);
      Context.Current_Dir := 0;
      Move_To_Next_File (Context);
   end Move_To_First_File;

   -----------------------
   -- Move_To_Next_File --
   -----------------------

   overriding procedure Move_To_Next_File (Context : access Files_Context) is
      use Directory_List;
      File      : Virtual_File;

   begin
      Context.Current_File := GNATCOLL.VFS.No_File;
      Context.Current_Lexical := Statements;

      --  If not at the end
      if Context.At_End then
         return;
      end if;

      if Context.Dirs = Null_List then
         Prepend
           (Context.Dirs,
            new Dir_Data'
              (Name  => Context.Directory,
               Files => Context.Directory.Read_Dir,
               F_Idx => 1));
      end if;

      while Context.Current_File = GNATCOLL.VFS.No_File loop
         declare
            Data : Dir_Data_Access renames Head (Context.Dirs);
         begin
            if Data.F_Idx > Data.Files'Last then
               Next (Context.Dirs);
               Context.Current_Dir := Context.Current_Dir + 1;

               if Context.Dirs = Null_List then
                  --  No more searches
                  Context.At_End := True;
                  return;
               end if;

            else
               File := Data.Files (Data.F_Idx);
               --  Data is an access type. The actual value in the list will
               --  correctly be updated.
               Data.F_Idx := Data.F_Idx + 1;

               if Is_Directory (File) then
                  if Context.Recurse and then not Is_Symbolic_Link (File) then
                     --  ??? Do not try to follow symbolic links for now,
                     --  so that we avoid infinite recursions.
                     Prepend
                       (Context.Dirs,
                        new Dir_Data'
                          (Name  => File,
                           Files => File.Read_Dir,
                           F_Idx => 1));
                     Context.Total_Dirs := Context.Total_Dirs + 1;
                  end if;

               --  ??? Should check that we have a text file
               elsif Match (+Base_Name (File), Context.Files_Pattern) then
                  Context.Current_File := File;
                  return;
               end if;
            end if;
         end;
      end loop;

   exception
      when Directory_Error =>
         Trace (Me, "Move_To_Next_File: Directory error");
         Context.Current_File := GNATCOLL.VFS.No_File;
   end Move_To_Next_File;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Dir_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Dir_Data, Dir_Data_Access);
   begin
      Unchecked_Free (D.Files);
      Unchecked_Free (D);
   end Free;

   ----------------------------
   -- Initialize_Scope_Combo --
   ----------------------------

   procedure Initialize_Scope_Combo
     (Combo  : access Gtk_Combo_Box_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      Add_Unique_Combo_Entry (Combo, -"Whole Text", True);
      Add_Unique_Combo_Entry (Combo, -"Comments Only");
      Add_Unique_Combo_Entry (Combo, -"Comments + Strings");
      Add_Unique_Combo_Entry (Combo, -"Strings Only");
      Add_Unique_Combo_Entry (Combo, -"All but Comments");

      Set_Tip (Get_Tooltips (Kernel),
               Combo, -"Restrict the scope of the search");

      Kernel_Callback.Connect
        (Combo, Gtk.Combo_Box.Signal_Changed, Vsearch.Reset_Search'Access,
         Kernel_Handle (Kernel));
   end Initialize_Scope_Combo;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Selector : out Scope_Selector;
      Kernel   : access Kernel_Handle_Record'Class)
   is
      Box   : Gtk.Box.Gtk_Hbox;
      Label : Gtk_Label;

   begin
      Selector := new Scope_Selector_Record;
      Gtk.Box.Initialize_Vbox (Gtk.Box.Gtk_Box (Selector));

      Gtk.Box.Gtk_New_Hbox (Box);
      Pack_Start (Selector, Box, False, True, 2);

      Gtk_New (Label, -"In:");
      Set_Alignment (Label, 0.0, 0.5);
      Gtk.Box.Pack_Start (Box, Label, False);

      Gtk_New_Text (Selector.Combo);
      Gtk.Box.Pack_Start (Box, Selector.Combo, True, True, 2);
      Initialize_Scope_Combo (Selector.Combo, Kernel);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Extra  : out Files_Extra_Scope;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Label : Gtk_Label;
   begin
      Extra := new Files_Extra_Scope_Record;
      Files_Extra_Info_Pkg.Initialize (Extra, Kernel, 1);

      Gtk_New (Label, -"In:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Extra.Files_Table, Label, 0, 1, 0, 1, Fill, 0);

      Gtk_New_Text (Extra.Combo);
      Initialize_Scope_Combo (Extra.Combo, Kernel);
      Attach (Extra.Files_Table, Extra.Combo, 1, 2, 0, 1, Fill, 0);

      Kernel_Callback.Connect
        (Extra.Subdirs_Check, Signal_Toggled, Reset_Search'Access,
         Kernel_Handle (Kernel));
      Kernel_Callback.Connect
        (Extra.Files_Entry, Gtk.Editable.Signal_Changed, Reset_Search'Access,
         Kernel_Handle (Kernel));
      Kernel_Callback.Connect
        (Extra.Directory_Entry, Gtk.Editable.Signal_Changed,
         Reset_Search'Access, Kernel_Handle (Kernel));
   end Gtk_New;

   ---------------------
   -- Context_Look_In --
   ---------------------

   overriding function Context_Look_In (Self : Files_Context) return String is
      pragma Unreferenced (Self);
   begin
      return -"selected files";
   end Context_Look_In;

   ---------------------
   -- Context_Look_In --
   ---------------------

   overriding function Context_Look_In
     (Self : Files_Project_Context) return String
   is
      pragma Unreferenced (Self);
   begin
      return -"any file in the project";
   end Context_Look_In;

   ---------------------
   -- Context_Look_In --
   ---------------------

   overriding function Context_Look_In
     (Self : Open_Files_Context) return String
   is
      pragma Unreferenced (Self);
   begin
      return -"any opened file";
   end Context_Look_In;

   ---------------------
   -- Context_Look_In --
   ---------------------

   overriding function Context_Look_In
     (Self : Current_File_Context) return String is
   begin
      return To_String (Self.Current_File);
   end Context_Look_In;

   ------------------
   -- Guess_Casing --
   ------------------

   function Guess_Casing (S : String) return Casing_Type
   is
      Index_1    : Integer;
      Index_2    : Integer;
      First_Char : Gunichar;
   begin
      if S = "" then
         return Lower;
      end if;

      Index_1 := UTF8_Next_Char (S, S'First);

      First_Char := UTF8_Get_Char (S (S'First .. Index_1 - 1));

      if not Is_Alpha (First_Char) then
         return Unchanged;
      end if;

      --  First character is lower: this string is Lower
      if Is_Lower (First_Char) then
         return Lower;
      end if;

      --  There is only one character: this string is Upper
      if Index_1 > S'Last then
         return Upper;
      end if;

      Index_2 := UTF8_Next_Char (S, Index_1);

      --  The first character is not lower and the second character is:
      --  this string is Smart_Mixed
      if Is_Lower (UTF8_Get_Char (S (Index_1 .. Index_2 - 1))) then
         return Smart_Mixed;
      end if;

      --  The first two characters are upper: this string is Upper
      return Upper;
   end Guess_Casing;

   ---------------
   -- To_Casing --
   ---------------

   function To_Casing (S : String; Casing : Casing_Type) return String is
      Lower_S : constant String := UTF8_Strdown (S);

   begin
      --  If S is not all lower case: return S
      if Lower_S /= S then
         return S;
      end if;

      case Casing is
         when Unchanged =>
            return S;

         when Lower =>
            return Lower_S;

         when Upper =>
            return UTF8_Strup (S);

         when Smart_Mixed =>
            declare
               O       : String (S'Range);
               I1, I2  : Natural := S'First;
               Capitalize_Next : Boolean := True;
            begin
               loop
                  I1 := I2;
                  I2 := UTF8_Next_Char (S, I1);

                  if Capitalize_Next then
                     O (I1 .. I2 - 1) := UTF8_Strup (S (I1 .. I2 - 1));
                  else
                     O (I1 .. I2 - 1) := S (I1 .. I2 - 1);
                  end if;

                  Capitalize_Next := not Is_Alpha
                    (UTF8_Get_Char (S (I1 .. I2 - 1)));

                  if I2 > S'Last then
                     return O;
                  end if;
               end loop;
            end;
      end case;
   end To_Casing;

end Src_Contexts;
