------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
pragma Warnings (Off);
with Ada.Strings.Unbounded.Aux;  use Ada.Strings.Unbounded.Aux;
pragma Warnings (On);
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Regexp;                use GNAT.Regexp;
with GNAT.Regpat;                use GNAT.Regpat;
with GNAT.Strings;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.Xref;

with Glib;                       use Glib;
with Glib.Convert;
with Glib.Error;                 use Glib.Error;

with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Combo_Box;
with Gtk.Combo_Box_Text;         use Gtk.Combo_Box_Text;
with Gtk.Editable;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Toggle_Button;          use Gtk.Toggle_Button;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;

with Gtkada.Dialogs;             use Gtkada.Dialogs;

with Files_Extra_Info_Pkg;       use Files_Extra_Info_Pkg;
with GPS.Default_Styles;         use GPS.Default_Styles;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Charsets;        use GPS.Kernel.Charsets;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup; use GPS.Kernel.Messages.Markup;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Search;                 use GPS.Search;
with GUI_Utils;                  use GUI_Utils;
with Informational_Popups;       use Informational_Popups;
with Language;                   use Language;
with Language_Handlers;          use Language_Handlers;
with GPR.Osint;
with Projects;                   use Projects;
with Src_Editor_Box;             use Src_Editor_Box;
with Src_Editor_Buffer;          use Src_Editor_Buffer;
with Src_Editor_Module.Markers;  use Src_Editor_Module.Markers;
with Src_Editor_Module;          use Src_Editor_Module;
with Src_Editor_View;            use Src_Editor_View;
with UTF8_Utils;
with Commands;                   use Commands;
with Vsearch;                    use Vsearch;

package body Src_Contexts is
   use type GNATCOLL.Xref.Visible_Column;

   Me : constant Trace_Handle := Create ("GPS.SOURCE_EDITOR.CONTEXTS");

   package Match_Vectors is new Ada.Containers.Vectors
     (Positive, GPS.Search.Search_Context);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GError, GError_Access);

   procedure Scan_Buffer
     (Buffer               : String;
      From                 : Character_Offset_Type;
      Context              : access Root_Search_Context'Class;
      Callback             : Scan_Callback;
      Scope                : Search_Scope;
      Lexical_State        : in out Recognized_Lexical_States;
      Lang                 : Language_Access := null;
      Ref                  : in out Buffer_Position;
      Was_Partial          : out Boolean;
      Display_Matched_Only : Boolean := False);
   --  Search Context in buffer starting from From character, searching only
   --  in the appropriate scope.
   --  Buffer is assumed to contain complete contexts (e.g the contents of
   --  a whole file).
   --  Lexical_State is the scope at the first character in Buffer.
   --  On exit, Was_Partial is set to True if the search was interrupted
   --  because the callback returned False at some point
   --  Buffer should be in UTF-8.

   procedure Scan_File
     (Context              : access Root_Search_Context'Class;
      Handler              : access Language_Handler_Record'Class;
      Kernel               : Kernel_Handle := null;
      Name                 : GNATCOLL.VFS.Virtual_File;
      Callback             : Scan_Callback;
      Scope                : Search_Scope;
      Lexical_State        : in out Recognized_Lexical_States;
      Start_Line           : Editable_Line_Type := 1;
      Start_Column         : Character_Offset_Type := 1;
      Force_Read           : Boolean := False;
      Was_Partial          : out Boolean;
      Display_Matched_Only : Boolean := False);
   --  Search Context in the file Name, searching only in the appropriate
   --  scope.
   --  If there is already an opened editor for this file, its contents will be
   --  used, otherwise the file is read from the disk.
   --  The search will start at position (Start_Line, Start_Column)
   --  Lexical_State is the scope at Start.
   --  If Force_Read is True, then this procedure does not check whether there
   --  already exists an open editor. This should be set to False when running
   --  in text-only mode. Kernel can be null only if Force_Read is True.
   --  On exit, Was_Partial is set to True if the callback returned False at
   --  some point.

   procedure Scan_Editor
     (Context              : access Root_Search_Context'Class;
      Handler              : access Language_Handler_Record'Class;
      Editor               : MDI_Child;
      Callback             : Scan_Callback;
      Scope                : Search_Scope;
      Lexical_State        : in out Recognized_Lexical_States;
      Start_Line           : Editable_Line_Type := 1;
      Start_Column         : Character_Offset_Type := 1;
      Was_Partial          : out Boolean;
      Display_Matched_Only : Boolean := False);
   --  Same as above, but works directly on the editor. This is useful for
   --  example when the editor has no file yet.

   procedure Scan_And_Store
     (Context  : access Root_Search_Context'Class;
      Handler  : access Language_Handler_Record'Class;
      Kernel   : Kernel_Handle;
      Str      : String := "";
      File     : Virtual_File := GNATCOLL.VFS.No_File;
      Ref      : Buffer_Position;
      Scope    : Search_Scope;
      Lang     : Language_Access := null;
      Result   : out Match_Vectors.Vector);
   --  Same as above, but behaves as if there was a default callback that
   --  stores the results in an array
   --  If Str is not the empty string, it is considered as a buffer to parse.
   --  Ref is only used when Str is not the empty string.
   --  If it is empty and File is not No_File, then that file is parsed
   --  instead.
   --  It returns the list of matches that were found in the buffer, or null if
   --  no match was found. It is the responsability of the caller to free the
   --  returned array.
   --  If Kernel is null, not check is done whether an editor is currently open
   --  for the file.

   procedure Scan_Next
     (Context          : access Root_Search_Context'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Editor           : access Source_Buffer_Record'Class;
      Scope            : Search_Scope;
      Lexical_State    : in out Recognized_Lexical_States;
      Lang             : Language_Access;
      Current_Line     : Editable_Line_Type;
      Current_Column   : Character_Offset_Type;
      Backward         : Boolean;
      Result           : out GPS.Search.Search_Context;
      Start_Line       : Editable_Line_Type := 1;
      Start_Column     : Character_Offset_Type := 1;
      End_Line         : Editable_Line_Type := 0;
      End_Column       : Character_Offset_Type := 0;
      Failure_Response : Search_Failure_Response := Informational_Popup);
   --  Return the next occurrence of Context in Editor, just before or just
   --  after Current_Line, Current_Column. If no match is found after the
   --  current position, for a forward search, return the first occurrence from
   --  the beginning of the editor. Likewise for a backward search.
   --
   --  Note that the index in the result might be incorrect, although the line
   --  and column will always be correct.
   --  null is returned if there is no match.
   --
   --  Current_Scope is the scope at current_line, current_column.
   --  Restrict search to given range (if specified):
   --  Start_Line:Start_Column .. End_Line:End_Column
   --
   --  Failure_Response is used to select which type of response is displayed
   --  in case of failure.

   procedure First_Match
     (Context       : access Root_Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Kernel        : Kernel_Handle;
      Name          : GNATCOLL.VFS.Virtual_File;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type;
      Start_Column  : Character_Offset_Type;
      Result         : out GPS.Search.Search_Context;
      Text          : out GNAT.Strings.String_Access;
      Force_Read    : Boolean := False);
   --  Lightweight interface that returns the first occurence of Context in the
   --  file Name.
   --  The returned value must be freed by the caller
   --  Current_Scope is the scope at (Start_Line, Start_Column)
   --  See description of Force_Read in Scan_File. Kernel can be null only if
   --  Force_Read is True.

   procedure First_Match
     (Context       : access Root_Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Editor        : MDI_Child;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type;
      Start_Column  : Character_Offset_Type;
      Result         : out GPS.Search.Search_Context;
      Text          : out GNAT.Strings.String_Access);
   --  Same as above, using an editor instead

   procedure Highlight_Result
     (Kernel      : access Kernel_Handle_Record'Class;
      File_Name   : GNATCOLL.VFS.Virtual_File;
      Look_For    : String;
      Match       : GPS.Search.Search_Context;
      Text        : String;
      Give_Focus  : Boolean;
      Interactive : Boolean);
   --  Print the result of the search in the console
   --  If Give_Focus is true, the focus will be given to the editor

   procedure Initialize_Scope_Combo
     (Combo  : access Gtk_Combo_Box_Text_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  Initialize the combo box with all the entries for the selection of the
   --  scope.

   function Auxiliary_Search
     (Context              : access Current_File_Context'Class;
      Editor               : Source_Editor_Box;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Start_Line           : Editable_Line_Type := 1;
      Start_Column         : Character_Offset_Type := 1;
      End_Line             : Editable_Line_Type := 0;
      End_Column           : Character_Offset_Type := 0)
      return Source_Search_Occurrence;
   --  Auxiliary function, factorizes code between Search and Replace.
   --  Return True in case of success.
   --  When From_Selection_Start is True, the search begins at the beginning of
   --  the currently selected text, if any. Otherwise, the search will start
   --  from then end of the selected text.
   --  Restrict search to given range (if specified):
   --  Start_Line:Start_Column .. End_Line:End_Column

   function Locations_Category_Name (Look_For : String) return String;
   --  Return the name of the category to use in the Locations window

   procedure Search_From_Editor
     (Context              : access File_Search_Context'Class;
      Handler              : access Language_Handler_Record'Class;
      Callback             : Scan_Callback;
      Editor               : MDI_Child;
      More_Matches         : out Boolean;
      Matches_Found        : out Boolean;
      Display_Matched_Only : Boolean := False);
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

   procedure Replace_Matched
     (Replacement : Replacement_Pattern;
      Matches     : Match_Vectors.Vector;
      Buffer      : Src_Editor_Buffer.Source_Buffer);
   --  Replace all search mathes in given Buffer with Replacement

   procedure Focus_To_Editor (Editor : MDI_Child);
   --  Make the given Editor visible

   function Get_Window_For_Informational_Popup
     (Kernel : not null access Kernel_Handle_Record'Class) return Gtk_Window;
   --  Return the window on which the loop back popup should be displayed.
   --  This should be the current editor's window (if any) or the GPS current
   --  window.

   --------------
   -- Is_Equal --
   --------------

   overriding function Is_Equal
     (Left  : not null access Source_Search_Occurrence_Record;
      Right : not null access Source_Search_Occurrence_Record)
      return Boolean is
   begin
      return (Left.Match_From = Right.Match_From
              and then Left.Match_Up_To = Right.Match_Up_To);
   end Is_Equal;

   -----------------
   -- Scan_Buffer --
   -----------------

   procedure Scan_Buffer
     (Buffer               : String;
      From                 : Character_Offset_Type;
      Context              : access Root_Search_Context'Class;
      Callback             : Scan_Callback;
      Scope                : Search_Scope;
      Lexical_State        : in out Recognized_Lexical_States;
      Lang                 : Language_Access := null;
      Ref                  : in out Buffer_Position;
      Was_Partial          : out Boolean;
      Display_Matched_Only : Boolean := False)
   is
      Scanning_Allowed : constant array (Recognized_Lexical_States) of Boolean
        := (Statements     => Scope in Whole | All_But_Comments,
            Strings        => Scope in Whole
                              | Comments_And_Strings .. All_But_Comments,
            Mono_Comments  => Scope in Whole .. Comments_And_Strings,
            Multi_Comments => Scope in Whole .. Comments_And_Strings);
      --  Indicates what lexical states are valid, depending on the current
      --  scope.

      Buffer_First  : Natural;
      --  Index of From character in Buffer

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
         M_Comm_Start  : GNAT.Strings.String_Access
            renames Lang.Syntax.Comment_Start;
         M_Comm_End    : GNAT.Strings.String_Access
            renames Lang.Syntax.Comment_End;
         Char_Delim    : Character renames Lang.Constant_Character;

         Looking_For   : constant Boolean := not Scanning_Allowed (State);
         --  Whether the final range should or should not be scanned.

         Matches       : Match_Array (0 .. 0);

      begin
         Section_End := Buffer'Last;

         while Pos <= Buffer'Last
           and then Scanning_Allowed (State) /= Looking_For
         loop
            case State is
               --  Statements end on any other state

               when Statements =>
                  while Pos <= Buffer'Last loop
                     if M_Comm_Start /= null
                       and then Starts_With
                         (Buffer (Pos .. Buffer'Last), M_Comm_Start.all)
                     then
                        State := Multi_Comments;
                        Section_End := Pos - 1;
                        Pos := Pos + M_Comm_Start'Length;
                        exit;

                     else
                        if Lang.Syntax.New_Line_Comment_Start = null then
                           if Lang.Syntax.New_Line_Comment_Start_Regexp /=
                             null
                           then
                              Match
                                (Lang.Syntax.New_Line_Comment_Start_Regexp.all,
                                 Buffer, Matches, Pos);

                              if Matches (0) /= GNAT.Regpat.No_Match then
                                 State := Mono_Comments;
                                 Section_End := Pos - 1;
                                 Pos := Matches (0).Last + 1;
                                 exit;
                              end if;
                           end if;
                        else
                           if Starts_With
                             (Buffer (Pos .. Buffer'Last),
                              Lang.Syntax.New_Line_Comment_Start.all)
                           then
                              State := Mono_Comments;
                              Section_End := Pos - 1;
                              Pos := Pos
                                + Lang.Syntax.New_Line_Comment_Start'Length;
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
                     if M_Comm_End /= null
                       and then Starts_With
                         (Buffer (Pos .. Buffer'Last),
                          M_Comm_End.all)
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

      Pos           : Positive;
      Line_Start    : Positive;
      Dummy         : Visible_Column_Type := 1;
      Section_End   : Integer;
      Old_State     : Recognized_Lexical_States;
      Language      : Language_Context_Access;
      Ignored       : Natural := 0;

   begin  --  Scan_Buffer
      Was_Partial := False;

      if Buffer'Length = 0 then
         return;
      end if;

      Buffer_First := UTF8_Utils.Column_To_Index (Buffer, From);
      Pos := Buffer_First;

      if Buffer_First > Buffer'Last then
         return;
      end if;

      --  If the language is null, we simply use the more efficient algorithm

      if Scope = Whole or else Lang = null then
         Context.Scan_Buffer_No_Scope
           (Buffer               => Buffer,
            Start_Index          => Buffer_First,
            End_Index            => Buffer'Last,
            Callback             => Callback,
            Ref                  => Ref,
            Was_Partial          => Was_Partial,
            Display_Matched_Only => Display_Matched_Only);
         return;
      end if;

      Language := Get_Language_Context (Lang);
      Old_State := Lexical_State;

      --  Always find the longest possible range, so that we can benefit
      --  as much as possible from the efficient string searching
      --  algorithms.

      while Pos <= Buffer'Last loop
         Line_Start := Pos;
         Old_State  := Lexical_State;

         Next_Scope_Transition
           (Buffer, Pos, Lexical_State, Section_End, Language.all);

         if Scanning_Allowed (Old_State) then
            Context.Scan_Buffer_No_Scope
              (Buffer, Integer (Line_Start), Section_End,
               Callback,
               Ref => Ref, Was_Partial => Was_Partial,
               Display_Matched_Only => Display_Matched_Only);

            if Was_Partial then
               Lexical_State := Old_State;
               return;
            end if;
         end if;
      end loop;

      --  Memorize the lexical state when we found the last match, so that next
      --  time we look for the context we find it correctly.

      Lexical_State := Old_State;
   end Scan_Buffer;

   ---------------
   -- Scan_File --
   ---------------

   procedure Scan_File
     (Context              : access Root_Search_Context'Class;
      Handler              : access Language_Handler_Record'Class;
      Kernel               : Kernel_Handle := null;
      Name                 : GNATCOLL.VFS.Virtual_File;
      Callback             : Scan_Callback;
      Scope                : Search_Scope;
      Lexical_State        : in out Recognized_Lexical_States;
      Start_Line           : Editable_Line_Type := 1;
      Start_Column         : Character_Offset_Type := 1;
      Force_Read           : Boolean := False;
      Was_Partial          : out Boolean;
      Display_Matched_Only : Boolean := False)
   is
      Lang   : Language_Access;
      Buffer : GNAT.Strings.String_Access;
      Child  : MDI_Child;
      UTF8, Tmp : GNAT.Strings.String_Access;
      Valid  : Boolean;
      Ref    : Buffer_Position;
      Line   : Editable_Line_Type;
      Start  : Integer;
   begin
      Was_Partial := False;

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
               Start_Line           => Start_Line,
               Start_Column         => Start_Column,
               Was_Partial          => Was_Partial,
               Display_Matched_Only => Display_Matched_Only);
            return;
         end if;
      end if;

      --  ??? Would be nice to handle backward search, which is extremely hard
      --  with regular expressions

      Lang := Get_Language_From_File (Handler, Name);

      if Child = null then
         Buffer := Read_File (Name);

         if Buffer = null then
            return;
         end if;

         UTF8_Utils.Unknown_To_UTF8 (Buffer.all, UTF8, Valid);
         if Valid then
            if UTF8 = null then
               Tmp := Buffer;
            else
               Tmp := UTF8;
            end if;

            Line  := 1;
            Start := Tmp'First;
            while Start <= Tmp'Last
              and then Line < Start_Line
            loop
               if Tmp (Start) = ASCII.LF then
                  Line := Line + 1;
               end if;
               Start := Start + 1;
            end loop;

            Ref := (Start, Integer (Line), 1, 1);
            Scan_Buffer
              (Tmp (Start .. Tmp'Last), 1, Context, Callback, Scope,
               Lexical_State, Lang,
               Ref => Ref, Was_Partial => Was_Partial,
               Display_Matched_Only => Display_Matched_Only);

            Free (UTF8);
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
     (Context              : access Root_Search_Context'Class;
      Handler              : access Language_Handler_Record'Class;
      Editor               : MDI_Child;
      Callback             : Scan_Callback;
      Scope                : Search_Scope;
      Lexical_State        : in out Recognized_Lexical_States;
      Start_Line           : Editable_Line_Type := 1;
      Start_Column         : Character_Offset_Type := 1;
      Was_Partial          : out Boolean;
      Display_Matched_Only : Boolean := False)
   is
      Lang   : Language_Access;
      Box    : Source_Editor_Box;
      Ref    : Buffer_Position;
   begin
      --  ??? Would be nice to handle backward search, which is extremely hard
      --  with regular expressions

      Was_Partial := False;
      Box := Get_Source_Box_From_MDI (Editor);

      Lang := Get_Language_From_File (Handler, Get_Filename (Box));

      if not Is_Valid_Position (Get_Buffer (Box), Start_Line, Start_Column)
      then
         return;
      end if;

      declare
         Unbounded_Buffer : constant Unbounded_String :=
           Get_Text (Get_Buffer (Box), Start_Line, 1);
         Buffer           : Big_String_Access;
         Len              : Natural;

      begin
         Get_String (Unbounded_Buffer, Buffer, Len);
         Ref := (1, Integer (Start_Line), 1, 1);
         Scan_Buffer
           (Buffer (1 .. Len),
            Start_Column,
            Context,
            Callback,
            Scope,
            Lexical_State,
            Lang,
            Ref => Ref,
            Was_Partial => Was_Partial,
            Display_Matched_Only => Display_Matched_Only);
      end;
   end Scan_Editor;

   -----------------------------
   -- Locations_Category_Name --
   -----------------------------

   function Locations_Category_Name (Look_For : String) return String is
   begin
      return -"Search for: " & Glib.Convert.Escape_Text (Look_For);
   end Locations_Category_Name;

   ----------------------
   -- Highlight_Result --
   ----------------------

   procedure Highlight_Result
     (Kernel      : access Kernel_Handle_Record'Class;
      File_Name   : Virtual_File;
      Look_For    : String;
      Match       : GPS.Search.Search_Context;
      Text        : String;
      Give_Focus  : Boolean;
      Interactive : Boolean)
   is
      function To_Positive (N : Natural) return Positive;
      --  If N > 0 then return N else return 1.

      procedure Do_Highlight
        (Column_End : Basic_Types.Visible_Column_Type;
         Length     : Highlight_Length);
      --  Common code to highlight text

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

      ------------------
      -- Do_Highlight --
      ------------------

      procedure Do_Highlight
        (Column_End : Basic_Types.Visible_Column_Type;
         Length     : Highlight_Length) is
      begin
         if Interactive then
            Open_File_Action_Hook.Run
              (Kernel,
               File => File_Name,
               Project => GNATCOLL.Projects.No_Project,   --   ??? any project
               Line    => Match.Start.Line,
               Column  => Match.Start.Visible_Column,
               Column_End => Column_End,
               Focus => Give_Focus);
            Push_Current_Editor_Location_In_History (Kernel);

         else
            declare
               Message : constant Markup_Message_Access :=
                 Create_Markup_Message
                   (Container  => Get_Messages_Container (Kernel),
                    Category   => Locations_Category_Name (Look_For),
                    File       => File_Name,
                    Line       => To_Positive (Match.Start.Line),
                    Column     => Match.Start.Visible_Column,
                    Text       => Text,
                    Importance => Unspecified,
                    Flags      => Side_And_Locations);
            begin
               Message.Set_Highlighting (Search_Results_Style, Length);
            end;
         end if;
      end Do_Highlight;

   begin
      if Is_Empty_Match (Match) then
         Do_Highlight
           (Column_End => Match.Start.Visible_Column + 1,
            Length     => 1);
      elsif Match.Start.Line = Match.Finish.Line then
         Do_Highlight
           (Column_End => Match.Finish.Visible_Column + 1,
            Length     => Highlight_Length
              (Match.Finish.Visible_Column - Match.Start.Visible_Column) + 1);
      else
         --  When the location spans on multiple lines, we base the length
         --  to highlight on the pattern length.
         --  ??? This is not compatible with UTF-8
         Do_Highlight
           (Column_End =>
               Match.Start.Visible_Column
                 + Visible_Column_Type
                   (Match.Finish.Index - Match.Start.Index + 1),
            Length     => Highlight_Length
              (Match.Finish.Index - Match.Start.Index + 1));
      end if;
   end Highlight_Result;

   ---------------
   -- Scan_Next --
   ---------------

   procedure Scan_Next
     (Context          : access Root_Search_Context'Class;
      Kernel           : access Kernel_Handle_Record'Class;
      Editor           : access Source_Buffer_Record'Class;
      Scope            : Search_Scope;
      Lexical_State    : in out Recognized_Lexical_States;
      Lang             : Language_Access;
      Current_Line     : Editable_Line_Type;
      Current_Column   : Character_Offset_Type;
      Backward         : Boolean;
      Result           : out GPS.Search.Search_Context;
      Start_Line       : Editable_Line_Type := 1;
      Start_Column     : Character_Offset_Type := 1;
      End_Line         : Editable_Line_Type := 0;
      End_Column       : Character_Offset_Type := 0;
      Failure_Response : Search_Failure_Response := Informational_Popup)
   is
      Continue_Till_End : Boolean := False;

      function Continue_Search_Response
        (Message : String := "") return Boolean;
      --  Return True if we should continue the search in case of failure
      --  depending On Failure_Response.

      function Stop_At_First_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean;
      --  Stop at the first match encountered

      function From_Beginning_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean;
      --  Used to restart the search from the beginning of the file.
      --
      --  Display an informational popup telling that the search restarted from
      --  the beginning if a match was encountered.

      function Backward_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean;
      --  Return the last match just before Current_Line and Current_Column

      ----------------------------
      -- Stop_At_First_Callback --
      ----------------------------

      function Stop_At_First_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean
      is
         pragma Unreferenced (Text);
      begin
         Result := Match;
         return False;
      end Stop_At_First_Callback;

      -----------------------------
      -- From_Beginning_Callback --
      -----------------------------

      function From_Beginning_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean
      is
         pragma Unreferenced (Text);
      begin
         Result := Match;

         if Failure_Response = Informational_Popup
           and then Result /= GPS.Search.No_Match
         then
            Display_Informational_Popup
              (Get_Window_For_Informational_Popup (Kernel),
               Icon_Name             => "gps-undo-symbolic",
               No_Transparency_Color => Default_Style.Get_Pref_Bg);
         end if;

         return False;
      end From_Beginning_Callback;

      -----------------------
      -- Backward_Callback --
      -----------------------

      function Backward_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean
      is
         pragma Unreferenced (Text);
      begin
         --  If we have already found a match, and the current one is after the
         --  current position, we can stop there. Else, if we have passed the
         --  current position but don't have any match yet, we have to return
         --  the last match.
         if Match.Start.Line > Integer (Current_Line)
           or else (Is_Empty_Match (Match)
                    and then Match.Start.Line = Integer (Current_Line)
                    and then Match.Start.Column >= Current_Column)
           or else (Match.Finish.Line = Integer (Current_Line)
                    and then Match.Finish.Column + 1 >= Current_Column)
         then
            if not Continue_Till_End
              and then Result /= GPS.Search.No_Match
            then
               return False;
            end if;

            Continue_Till_End := True;
         end if;

         Result := Match;

         return True;
      end Backward_Callback;

      ------------------------------
      -- Continue_Search_Response --
      ------------------------------

      function Continue_Search_Response
        (Message : String := "") return Boolean
      is
         Buttons : Message_Dialog_Buttons;
      begin
         case Failure_Response is
            when Dialog =>
               Buttons := GPS_Message_Dialog
                 (Message,
                  Confirmation,
                  Button_Yes or Button_No,
                  Button_Yes,
                  "",
                  -"Continue search ?",
                  Justify_Center,
                  Get_Current_Window (Kernel));

               return Buttons = Button_Yes;
            when Informational_Popup =>
               return True;
            when None =>
               return False;
         end case;
      end Continue_Search_Response;

      Was_Partial  : Boolean;
      Begin_Line   : Editable_Line_Type := Start_Line;
      Begin_Column : Character_Offset_Type := Start_Column;
      Ref          : Buffer_Position;

   begin
      Result := GPS.Search.No_Match;

      if Backward then
         declare
            Buffer : constant Unbounded_String :=
              Get_Text (Editor, Start_Line, 1, End_Line, End_Column);
            Text   : Big_String_Access;
            Len    : Natural;

         begin
            Get_String (Buffer, Text, Len);
            Ref := (1, Integer (Start_Line), 1, 1);
            Scan_Buffer
              (Buffer        => Text (1 .. Len),
               From          => Start_Column,
               Context       => Context,
               Callback      => Backward_Callback'Unrestricted_Access,
               Scope         => Scope,
               Lexical_State => Lexical_State,
               Lang          => Lang,
               Was_Partial   => Was_Partial,
               Ref           => Ref);
         end;

         --  Start from the end if necessary.

         if Continue_Till_End
           and then not Continue_Search_Response
             (-"No more matches, restart from the end ?")
         then
            Stop_Macro_Action_Hook.Run (Kernel);
            Set_End_Notif_Done (Context.all, True);
            Result := GPS.Search.No_Match;
            return;
         end if;

         --  If a match was found after the current one, it means that the
         --  search restarted from the end of the file. In this case, display
         --  an informational popup to notify the user that the search
         --  restarted.

         if Failure_Response = Informational_Popup
           and then (Result.Start.Line > Integer (Current_Line)
                     or else (Result.Start.Line = Integer (Current_Line)
                              and then Result.Start.Column >= Current_Column))
         then
            Display_Informational_Popup
              (Get_Window_For_Informational_Popup (Kernel),
               Icon_Name             => "gps-redo-symbolic",
               No_Transparency_Color => Default_Style.Get_Pref_Bg);
         end if;
      else
         if Current_Line > Begin_Line or else
           (Current_Line = Begin_Line and Current_Column > Begin_Column)
         then
            Begin_Line := Current_Line;
            Begin_Column := Current_Column;
         end if;

         declare
            Buffer : constant Unbounded_String :=
              Get_Text (Editor, Begin_Line, 1, End_Line, End_Column);
            Text   : Big_String_Access;
            Len    : Natural;

         begin
            Get_String (Buffer, Text, Len);
            Ref := (1, Integer (Begin_Line), 1, 1);
            Scan_Buffer
              (Buffer        => Text (1 .. Len),
               From          => Begin_Column,
               Context       => Context,
               Callback      => Stop_At_First_Callback'Unrestricted_Access,
               Scope         => Scope,
               Lexical_State => Lexical_State,
               Lang          => Lang,
               Was_Partial   => Was_Partial,
               Ref           => Ref);
         end;

         --  Start from the beginning if necessary.
         --  Do not display the continue dialog if starting search from the
         --  beginning.

         if Result = GPS.Search.No_Match then
            if not Continue_Till_End then
               if Current_Line = 1 and then Current_Column = 1 then
                  return;
               elsif not Continue_Search_Response
                 (-"No more matches, restart from the beginning ?")
               then
                  Stop_Macro_Action_Hook.Run (Kernel);
                  Set_End_Notif_Done (Context.all, True);
                  return;
               end if;
            end if;

            Lexical_State := Statements;

            declare
               Buffer : constant Unbounded_String :=
                 Get_Text (Editor, Start_Line, 1, End_Line, End_Column);
               Text   : Big_String_Access;
               Len    : Natural;

            begin
               Get_String (Buffer, Text, Len);
               Ref := (1, Integer (Start_Line), 1, 1);
               Scan_Buffer
                 (Text (1 .. Len),
                  Start_Column,
                  Context,
                  From_Beginning_Callback'Unrestricted_Access, Scope,
                  Lexical_State, Lang,
                  Was_Partial => Was_Partial,
                  Ref         => Ref);
            end;
         end if;
      end if;
   end Scan_Next;

   --------------------
   -- Scan_And_Store --
   --------------------

   procedure Scan_And_Store
     (Context  : access Root_Search_Context'Class;
      Handler  : access Language_Handler_Record'Class;
      Kernel   : Kernel_Handle;
      Str      : String := "";
      File     : Virtual_File := GNATCOLL.VFS.No_File;
      Ref      : Buffer_Position;
      Scope    : Search_Scope;
      Lang     : Language_Access := null;
      Result   : out Match_Vectors.Vector)
   is
      function Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean;
      --  Save Match in the result array.

      --------------
      -- Callback --
      --------------

      function Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean
      is
         pragma Unreferenced (Text);
      begin
         Result.Append (Match);
         return True;
      end Callback;

      State       : Recognized_Lexical_States := Statements;
      Was_Partial : Boolean;
      R           : Buffer_Position;
   begin
      Result.Clear;

      if Str /= "" then
         R := Ref;
         Scan_Buffer (Str, 1, Context,
                      Callback'Unrestricted_Access, Scope,
                      Lexical_State => State,
                      Lang          => Lang,
                      Ref           => R,
                      Was_Partial   => Was_Partial);
      elsif File /= GNATCOLL.VFS.No_File then
         Scan_File (Context,
                    Handler, Kernel,
                    File, Callback'Unrestricted_Access, Scope,
                    Lexical_State => State,
                    Force_Read    => Kernel = null,
                    Was_Partial   => Was_Partial);
      end if;
   end Scan_And_Store;

   -----------------
   -- First_Match --
   -----------------

   procedure First_Match
     (Context       : access Root_Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Kernel        : Kernel_Handle;
      Name          : GNATCOLL.VFS.Virtual_File;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type;
      Start_Column  : Character_Offset_Type;
      Result        : out GPS.Search.Search_Context;
      Text          : out GNAT.Strings.String_Access;
      Force_Read    : Boolean := False)
   is
      function Callback
        (Match : GPS.Search.Search_Context;
         Txt   : String) return Boolean;
      --  Save Match in the result array.

      --------------
      -- Callback --
      --------------

      function Callback
        (Match : GPS.Search.Search_Context;
         Txt   : String) return Boolean
      is
      begin
         Result := Match;
         Text   := new String'(Txt);
         return False;
      end Callback;

      Was_Partial : Boolean;
   begin
      Result := GPS.Search.No_Match;
      Scan_File (Context, Handler, Kernel,
                 Name, Callback'Unrestricted_Access, Scope,
                 Lexical_State, Start_Line, Start_Column,
                 Force_Read, Was_Partial);
   end First_Match;

   -----------------
   -- First_Match --
   -----------------

   procedure First_Match
     (Context       : access Root_Search_Context'Class;
      Handler       : access Language_Handler_Record'Class;
      Editor        : MDI_Child;
      Scope         : Search_Scope;
      Lexical_State : in out Recognized_Lexical_States;
      Start_Line    : Editable_Line_Type;
      Start_Column  : Character_Offset_Type;
      Result        : out GPS.Search.Search_Context;
      Text          : out GNAT.Strings.String_Access)
   is
      function Callback
        (Match : GPS.Search.Search_Context;
         Txt   : String) return Boolean;
      --  Save Match in the result array.

      --------------
      -- Callback --
      --------------

      function Callback
        (Match : GPS.Search.Search_Context;
         Txt   : String) return Boolean
      is
      begin
         Result := Match;
         Text   := new String'(Txt);
         return False;
      end Callback;

      Was_Partial : Boolean;
   begin
      Result := GPS.Search.No_Match;
      Scan_Editor
        (Context, Handler,
         Editor, Callback'Unrestricted_Access, Scope,
         Lexical_State, Start_Line, Start_Column,
         Was_Partial);
   end First_Match;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Context : in out Files_Context) is
   begin
      Context.Dirs.Clear;
      Context.At_End := True;
      Free (Context.Replacement);
      Free (Root_Search_Context (Context));
   end Free;

   overriding procedure Free (Context : in out Files_Project_Context) is
   begin
      Unchecked_Free (Context.Files);
      Free (Context.Replacement);
      Free (Root_Search_Context (Context));
   end Free;

   overriding procedure Free (Context : in out Open_Files_Context) is
   begin
      Unchecked_Free (Context.Files);
      Free (Context.Replacement);
      Free (Root_Search_Context (Context));
   end Free;

   -------------------
   -- Set_File_List --
   -------------------

   procedure Set_File_List
     (Context : access Files_Project_Context;
      Files   : File_Sets.Set)
   is
      Idx : Integer;
   begin
      Unchecked_Free (Context.Files);
      Context.Files := new File_Array (1 .. Integer (Files.Length));
      Idx := Context.Files'First;
      for F of Files loop
         Context.Files (Idx) := F;
         Idx := Idx + 1;
      end loop;

      Context.Current_File := Context.Files'First - 1;
   end Set_File_List;

   -------------------
   -- Set_File_List --
   -------------------

   procedure Set_File_List
     (Context : access Files_Project_Context;
      Files   : GNATCOLL.VFS.File_Array_Access) is
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

   -------------------
   -- Set_File_List --
   -------------------

   procedure Set_File_List
     (Context : access Open_Files_Context;
      Files   : File_Sets.Set)
   is
      Idx : Integer;
   begin
      Unchecked_Free (Context.Files);
      Context.Files := new File_Array (1 .. Integer (Files.Length));
      Idx := Context.Files'First;
      for F of Files loop
         Context.Files (Idx) := F;
         Idx := Idx + 1;
      end loop;
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

   --------------------
   -- Create_Context --
   --------------------

   overriding function Create_Context
     (Module          : not null access Current_File_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access is
      pragma Unreferenced (Module);
   begin
      return Current_File_Factory
        (Kernel, All_Occurrences,
         Scope => Search_Scope'Val (Selector.Get_Scope_Combo.Get_Active));
   end Create_Context;

   --------------------------
   -- Current_File_Factory --
   --------------------------

   function Current_File_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences   : Boolean;
      Scope             : Search_Scope := Whole)
      return Root_Search_Context_Access
   is
      pragma Unreferenced (Kernel);

      Context  : Current_File_Context_Access;
   begin
      --  If we are looking for all the occurrences, we simply reuse another
      --  context, instead of the interactive Current_File_Context

      Context := new Current_File_Context;
      Context.All_Occurrences := All_Occurrences;
      Context.Scope := Scope;
      return Root_Search_Context_Access (Context);
   end Current_File_Factory;

   --------------------
   -- Create_Context --
   --------------------

   overriding function Create_Context
     (Module          : not null access Current_Selection_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access
   is
      pragma Unreferenced (Module);
      Scope  : constant Search_Scope :=
                 Search_Scope'Val (Selector.Get_Scope_Combo.Get_Active);
      Result : Current_Selection_Context_Access;
   begin
      Result := new Current_Selection_Context;

      Get_Selection (Kernel, Result.Selection_From, Result.Selection_To);

      Result.All_Occurrences := All_Occurrences;
      Result.Scope := Scope;

      return Root_Search_Context_Access (Result);
   end Create_Context;

   ---------------------
   -- Focus_To_Editor --
   ---------------------

   procedure Focus_To_Editor (Editor : MDI_Child) is
   begin
      Raise_Child (Editor, True);
      Present (Gtk_Window (Get_Toplevel (Get_Widget (Editor))));
   end Focus_To_Editor;

   ----------------------------------------
   -- Get_Window_For_Informational_Popup --
   ----------------------------------------

   function Get_Window_For_Informational_Popup
     (Kernel : not null access Kernel_Handle_Record'Class) return Gtk_Window
   is
      Child    : constant MDI_Child := Find_Current_Editor (Kernel);
      Toplevel : constant Gtk_Widget :=
                   (if Child /= null then
                       Child.Get_Widget.Get_Toplevel
                    else
                       null);
   begin
      if Toplevel /= null and then Toplevel.Is_Toplevel then
         return Gtk_Window (Toplevel);
      else
         return Get_Current_Window (Kernel);
      end if;
   end Get_Window_For_Informational_Popup;

   ------------
   -- Search --
   ------------

   overriding function Search
     (Context              : access Current_Selection_Context;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Give_Focus           : Boolean;
      Found                : out Boolean;
      Continue             : out Boolean;
      Display_Matched_Only : Boolean := False) return Search_Occurrence
   is
      pragma Unreferenced (Display_Matched_Only);
      function Interactive_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean;
      --  Callbacks for the general search function
      --  ??? This should be factorized somehow with the Search fonction
      --  from the Abstract_File_Context.

      Child        : constant MDI_Child := Find_Current_Editor (Kernel);
      Editor       : Source_Editor_Box;
      Occurrence   : Source_Search_Occurrence;
      Begin_Line   : Editable_Line_Type;
      Begin_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Character_Offset_Type;

      --------------------------
      -- Interactive_Callback --
      --------------------------

      function Interactive_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean
      is
      begin
         if Match.Start.Line > Natural (End_Line) or else
           (Match.Start.Line = Natural (End_Line) and then
                Match.Start.Column > End_Column)
         then
            return False;
         end if;

         Found := True;

         if Get_Filename (Editor) /= GNATCOLL.VFS.No_File then
            Highlight_Result
              (Kernel      => Kernel,
               File_Name   => Get_Filename (Editor),
               Look_For    => Context_Look_For (Context),
               Match       => Match,
               Text        => Text,
               Give_Focus  => Give_Focus,
               Interactive => not Context.All_Occurrences);
         else
            Highlight_Result
              (Kernel      => Kernel,
               File_Name   => Editor.Get_Buffer.Get_File_Identifier,
               Look_For    => Context_Look_For (Context),
               Match       => Match,
               Text        => Text,
               Give_Focus  => Give_Focus,
               Interactive => not Context.All_Occurrences);
         end if;

         Occurrence := new Source_Search_Occurrence_Record'
           (Search_Occurrence_Record with
            Editor_Child => Child,
            Match_From   =>
              (Editable_Line_Type (Match.Start.Line), Match.Start.Column),
            Match_Up_To  =>
              (Editable_Line_Type (Match.Finish.Line), Match.Finish.Column));
         Initialize (Occurrence, Pattern => Text);

         return True;
      end Interactive_Callback;

   begin
      Found := False;

      if Child = null then
         Continue := False;
         return null;
      end if;

      Editor := Get_Source_Box_From_MDI (Child);

      if Give_Focus then
         Focus_To_Editor (Child);
      end if;

      declare
         Buffer      : constant Gtk_Text_Buffer :=
           Get_Buffer (Context.Selection_From);
         Range_Start : Gtk_Text_Iter;
         Range_End   : Gtk_Text_Iter;
      begin
         Buffer.Get_Iter_At_Mark (Range_Start, Context.Selection_From);
         Buffer.Get_Iter_At_Mark (Range_End, Context.Selection_To);

         Begin_Line := Editable_Line_Type (Get_Line (Range_Start) + 1);
         End_Line := Editable_Line_Type (Get_Line (Range_End) + 1);
         Begin_Column :=
           Character_Offset_Type (Get_Line_Offset (Range_Start) + 1);
         End_Column :=
           Character_Offset_Type (Get_Line_Offset (Range_End) + 1);

         if not Context.All_Occurrences then
            Occurrence := Auxiliary_Search
              (Context, Editor, Kernel, Search_Backward, From_Selection_Start,
               Begin_Line, Begin_Column, End_Line, End_Column);
            Found := Occurrence /= null;

            if not Found then
               Buffer.Select_Range (Range_Start, Range_End);
            end if;

            Continue := False; --  ??? Dummy boolean.
         else
            declare
               State : Recognized_Lexical_States := Statements;
            begin
               Scan_Editor
                 (Context,
                  Get_Language_Handler (Kernel),
                  Child,
                  Interactive_Callback'Unrestricted_Access,
                  Context.Scope,
                  Start_Line    => Begin_Line,
                  Start_Column  => Begin_Column,
                  Lexical_State => State,
                  Was_Partial   => Continue);

               Continue := False;  --  Ignore callback termination
            end;
         end if;
      end;

      return Search_Occurrence (Occurrence);
   exception
      when E : others =>
         Trace (Me, E);
         Found := False;
         Continue := False;
         return null;
   end Search;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Context         : access Current_Selection_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean) return Boolean
   is
      Child        : constant MDI_Child := Find_Current_Editor (Kernel);
      Buffer       : Src_Editor_Buffer.Source_Buffer;
      Editor       : Source_Editor_Box;
      Matches      : Match_Vectors.Vector;
      Begin_Line   : Editable_Line_Type;
      Begin_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Character_Offset_Type;
   begin
      if Context.All_Occurrences then
         if Child = null then
            return False;
         end if;

         Editor := Get_Source_Box_From_MDI (Child);
         Buffer := Get_Buffer (Editor);

         declare
            Range_Start : Gtk_Text_Iter;
            Range_End   : Gtk_Text_Iter;
         begin
            Buffer.Get_Iter_At_Mark (Range_Start, Context.Selection_From);
            Buffer.Get_Iter_At_Mark (Range_End, Context.Selection_To);

            Begin_Line := Editable_Line_Type (Get_Line (Range_Start) + 1);
            End_Line := Editable_Line_Type (Get_Line (Range_End) + 1);
            Begin_Column :=
              Character_Offset_Type (Get_Line_Offset (Range_Start) + 1);
            End_Column :=
              Character_Offset_Type (Get_Line_Offset (Range_End) + 1);
         end;

         declare
            Text : constant String := To_String (Buffer.Get_Text
              (Begin_Line, Begin_Column, End_Line, End_Column));
            Ref          : constant Buffer_Position :=
              (Text'First, Integer (Begin_Line), Begin_Column,
               Visible_Column_Type (Begin_Column));
         begin
            Scan_And_Store
              (Context => Context,
               Handler => Get_Language_Handler (Kernel),
               Kernel  => Kernel_Handle (Kernel),
               Str     => Text,
               File    => GNATCOLL.VFS.No_File,
               Scope   => Context.Scope,
               Ref     => Ref,
               Lang    => Get_Language (Buffer),
               Result  => Matches);
         end;

         if not Matches.Is_Empty then
            Context.Nb_Of_Replacements :=
              Context.Nb_Of_Replacements + Natural (Matches.Length);

            Context.Replacement.Initialize
              (Replace_String  => Replace_String,
               Case_Preserving => Case_Preserving,
               Is_Regexp       => Context.Is_Regexp);

            Replace_Matched
              (Replacement => Context.Replacement,
               Matches     => Matches,
               Buffer      => Buffer);
         end if;

         return False;
      else
         --  Parent's implementaion is fine in this case
         return Current_File_Context (Context.all).Replace
           (Kernel          => Kernel,
            Replace_String  => Replace_String,
            Case_Preserving => Case_Preserving,
            Search_Backward => Search_Backward,
            Give_Focus      => Give_Focus);
      end if;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Replace;

   --------------------
   -- Create_Context --
   --------------------

   overriding function Create_Context
     (Module          : not null access Files_From_Project_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access
   is
      pragma Unreferenced (Module);
      Context : constant Files_Project_Context_Access :=
                  new Files_Project_Context;
   begin
      Context.Scope := Search_Scope'Val (Selector.Get_Scope_Combo.Get_Active);
      Context.All_Occurrences := All_Occurrences;
      Context.Current         := GPS.Search.No_Match;
      Set_File_List (Context, Get_Project (Kernel).Source_Files (True));
      return Root_Search_Context_Access (Context);
   end Create_Context;

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
      Context.Current         := GPS.Search.No_Match;
      return Context;
   end Files_From_Project_Factory;

   --------------------
   -- Create_Context --
   --------------------

   overriding function Create_Context
     (Module          : not null access Files_From_Root_Project_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access
   is
      pragma Unreferenced (Module);
      Project  : constant Standard.Projects.Project_Type_Array :=
        Vsearch.Get_Selected_Project (Kernel);
      Context  : constant Files_Project_Context_Access :=
                  new Files_Project_Context;
   begin
      Context.Scope           :=
        Search_Scope'Val (Selector.Get_Scope_Combo.Get_Active);
      Context.All_Occurrences := All_Occurrences;
      Context.Current         := GPS.Search.No_Match;

      if Project'Length /= 0 then
         --  Search in selected project if any
         Set_File_List (Context, Source_Files_Non_Recursive (Project));
      else
         --  Search in root project if no project selected
         Set_File_List (Context, Get_Project (Kernel).Source_Files (False));
      end if;

      return Root_Search_Context_Access (Context);
   end Create_Context;

   --------------------
   -- Create_Context --
   --------------------

   overriding function Create_Context
     (Module          : not null access Runtime_Files_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access
   is
      pragma Unreferenced (Module);
      Files   : GNATCOLL.VFS.File_Array :=
        Get_Registry (Kernel).Environment.Predefined_Source_Files;
      Last    : Natural := Files'First - 1;
      Context : constant Files_Project_Context_Access :=
                  new Runtime_Files_Context;
   begin
      --  Collect specification files in the begining of Files array
      for J in Files'Range loop
         if Files (J).Has_Suffix (".ads") then
            Last := Last + 1;
            Files (Last) := Files (J);
         end if;
      end loop;

      Context.Scope           :=
        Search_Scope'Val (Selector.Get_Scope_Combo.Get_Active);
      Context.All_Occurrences := All_Occurrences;
      Context.Current         := GPS.Search.No_Match;
      Set_File_List (Context, new File_Array'(Files (Files'First .. Last)));
      return Root_Search_Context_Access (Context);
   end Create_Context;

   ---------------------------
   -- Get_Terminate_Message --
   ---------------------------

   overriding function Get_Terminate_Message
     (Context : access Files_Project_Context;
      Kind    : Operation_Kind) return String is
   begin
      case Kind is
         when Replace =>
            return -("Finished replacing the string in files from project "
                     & '('
                     & GNATCOLL.Utils.Image (Context.Nb_Of_Replacements, 1)
                     & " occurrences)");
         when Search =>
            return "";
      end case;
   end Get_Terminate_Message;

   --------------------
   -- Create_Context --
   --------------------

   overriding function Create_Context
     (Module          : not null access Open_Files_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access
   is
      pragma Unreferenced (Module);
      Context : constant Open_Files_Context_Access := new Open_Files_Context;
   begin
      --  GPS.Kernel.Open_Files returns a File_Array, but Set_File_List
      --  takes a File_Array_Access. Memory will be properly freed in
      --  Set_File_List

      Context.Scope           :=
        Search_Scope'Val (Selector.Get_Scope_Combo.Get_Active);
      Context.All_Occurrences := All_Occurrences;
      Context.Current         := GPS.Search.No_Match;
      Set_File_List (Context, Kernel.Open_Files.all);
      return Root_Search_Context_Access (Context);
   end Create_Context;

   ---------------------------
   -- Get_Terminate_Message --
   ---------------------------

   overriding function Get_Terminate_Message
     (Context : access Open_Files_Context;
      Kind    : Operation_Kind) return String is
   begin
      case Kind is
         when Replace =>
            return -("Finished replacing the string in open files "
                     & '('
                     & GNATCOLL.Utils.Image (Context.Nb_Of_Replacements, 1)
                     & " occurrences)");
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
      Context.Current         := GPS.Search.No_Match;
      return Context;
   end Files_Factory;

   --------------------
   -- Create_Context --
   --------------------

   overriding function Create_Context
     (Module          : not null access Files_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access
   is
      pragma Unreferenced (Kernel, Module);

      Context        : Files_Context_Access;
      Extra          : constant Files_Extra_Scope := Files_Extra_Scope
        (Selector);
      Files_Selector : constant Files_Extra_Info_Access :=
                         Files_Extra_Info_Access (Extra.Get_Optional_Widget);
      Re             : GNAT.Regexp.Regexp;
   begin
      if Get_Text (Files_Selector.Files_Entry) /= "" then
         Context := Files_Factory
           (All_Occurrences,
            Search_Scope'Val (Extra.Get_Scope_Combo.Get_Active));
         Re := Compile
           (Get_Text (Files_Selector.Files_Entry),
            Glob => True,
            Case_Sensitive => Integer
              (GPR.Osint.Get_File_Names_Case_Sensitive) /= 0);
         Set_File_List
           (Context,
            Files_Pattern => Re,
            Directory     =>
              Create_From_UTF8 (Get_Text (Files_Selector.Directory_Entry)),
            Recurse       => Get_Active (Files_Selector.Subdirs_Check));

         return Root_Search_Context_Access (Context);
      end if;

      Trace (Me, "Files_Factory: no files pattern specified");
      return null;

   exception
      when Error_In_Regexp =>
         return null;
   end Create_Context;

   ---------------------------
   -- Get_Terminate_Message --
   ---------------------------

   overriding function Get_Terminate_Message
     (Context : access Files_Context;
      Kind    : Operation_Kind) return String is
   begin
      case Kind is
         when Replace =>
            return -("Finished replacing the string in selected files "
                     & '('
                     & GNATCOLL.Utils.Image (Context.Nb_Of_Replacements, 1)
                     & " occurrences)");
         when Search =>
            return "";
      end case;
   end Get_Terminate_Message;

   ----------------------
   -- Search_In_Editor --
   ----------------------

   procedure Search_In_Editor
     (Context          : access Current_File_Context;
      Start_At         : Gtk.Text_Iter.Gtk_Text_Iter;
      Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward  : Boolean;
      Match_From       : out Editor_Coordinates;
      Match_Up_To      : out Editor_Coordinates;
      Found            : out Boolean;
      Start_Line       : Editable_Line_Type := 1;
      Start_Column     : Character_Offset_Type := 1;
      End_Line         : Editable_Line_Type := 0;
      End_Column       : Character_Offset_Type := 0;
      Failure_Response : Search_Failure_Response := Informational_Popup)
   is
      Editor : constant Source_Buffer := Source_Buffer (Get_Buffer (Start_At));
      Lang   : Language_Access;
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

      if Context.Current /= GPS.Search.No_Match
        and then Is_Empty_Match (Context.Current)
        and then not Search_Backward
      then
         Forward_Position
           (Buffer       => Editor,
            Start_Line   => Line,
            Start_Column => Column,
            Length       => 1,
            End_Line     => Line,
            End_Column   => Column);
      end if;

      Scan_Next
        (Context, Kernel,
         Editor           => Editor,
         Scope            => Context.Scope,
         Lexical_State    => Context.Current_Lexical,
         Lang             => Lang,
         Current_Line     => Line,
         Current_Column   => Column,
         Failure_Response => Failure_Response,
         Backward         => Search_Backward,
         Result           => Context.Current,
         Start_Line       => Start_Line,
         Start_Column     => Start_Column,
         End_Line         => End_Line,
         End_Column       => End_Column);

      Found := Context.Current /= GPS.Search.No_Match;
      if Found then
         Match_From :=
           (Line => Editable_Line_Type (Context.Current.Start.Line),
            Col  => Context.Current.Start.Column);

         if Is_Empty_Match (Context.Current) then
            Match_Up_To := Match_From;
         else
            Match_Up_To :=
              (Line => Editable_Line_Type (Context.Current.Finish.Line),
               Col => Context.Current.Finish.Column + 1);
         end if;
      end if;
   end Search_In_Editor;

   ---------------------------
   -- Get_Terminate_Message --
   ---------------------------

   overriding function Get_Terminate_Message
     (Context : access Current_File_Context;
      Kind    : Operation_Kind) return String is
   begin
      case Kind is
         when Replace =>
            return -("Finished replacing the string in current file "
                     & '('
                     & GNATCOLL.Utils.Image (Context.Nb_Of_Replacements, 1)
                     & " occurrences)");
         when Search =>
            return "";
      end case;
   end Get_Terminate_Message;

   ----------------------
   -- Auxiliary_Search --
   ----------------------

   function Auxiliary_Search
     (Context              : access Current_File_Context'Class;
      Editor               : Source_Editor_Box;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Start_Line           : Editable_Line_Type := 1;
      Start_Column         : Character_Offset_Type := 1;
      End_Line             : Editable_Line_Type := 0;
      End_Column           : Character_Offset_Type := 0)
      return Source_Search_Occurrence
   is
      Selection_Start : Gtk_Text_Iter;
      Selection_End   : Gtk_Text_Iter;
      Dummy           : Boolean;
      Occurrence      : Source_Search_Occurrence;
      Match_From      : Editor_Coordinates;
      Match_Up_To     : Editor_Coordinates;
      Found           : Boolean;
   begin
      Get_Selection_Bounds
        (Get_Buffer (Editor), Selection_Start, Selection_End, Dummy);

      Search_In_Editor
        (Context         => Context,
         Start_At        => (if From_Selection_Start then
                                Selection_Start
                             else
                                Selection_End),
         Kernel          => Kernel,
         Search_Backward => Search_Backward,
         Match_From      => Match_From,
         Match_Up_To     => Match_Up_To,
         Found           => Found,
         Start_Line      => Start_Line,
         Start_Column    => Start_Column,
         End_Line        => End_Line,
         End_Column      => End_Column);

      if Found then
         Occurrence := new Source_Search_Occurrence_Record'
           (Search_Occurrence_Record with
            Editor_Child => Find_Child (Kernel, Editor),
            Match_From   => Match_From,
            Match_Up_To  => Match_Up_To);
         Initialize (Occurrence, Pattern => Context_Look_For (Context));

         Push_Current_Editor_Location_In_History (Kernel);

         Editor.Set_Cursor_Location
           (Line        => Match_From.Line,
            Column      => Match_From.Col,
            Force_Focus => False,
            Centering   => GPS.Editors.Minimal,
            Extend_Selection => False);

         Select_Region
           (Get_Buffer (Editor),
            Match_From.Line, Match_From.Col,
            Match_Up_To.Line, Match_Up_To.Col);

         Center_Cursor (Get_View (Editor));
      end if;

      return Occurrence;
   end Auxiliary_Search;

   ------------
   -- Search --
   ------------

   overriding function Search
     (Context              : access Current_File_Context;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Give_Focus           : Boolean;
      Found                : out Boolean;
      Continue             : out Boolean;
      Display_Matched_Only : Boolean := False) return Search_Occurrence
   is
      Child      : constant MDI_Child := Find_Current_Editor (Kernel);
      Occurrence : Source_Search_Occurrence;
      Editor     : Source_Editor_Box;
      Buffer     : Source_Buffer;

      function Interactive_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean;
      --  Callbacks for the general search function
      --  ??? This should be factorized somehow with the Search fonction
      --  from the Abstract_File_Context.

      --------------------------
      -- Interactive_Callback --
      --------------------------

      function Interactive_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean
      is
      begin
         Found := True;

         if Get_Filename (Editor) /= GNATCOLL.VFS.No_File then
            Highlight_Result
              (Kernel      => Kernel,
               File_Name   => Get_Filename (Editor),
               Look_For    => Context_Look_For (Context),
               Match       => Match,
               Text        => Text,
               Give_Focus  => Give_Focus,
               Interactive => not Context.All_Occurrences);
         else
            Highlight_Result
              (Kernel      => Kernel,
               File_Name   => Editor.Get_Buffer.Get_File_Identifier,
               Look_For    => Context_Look_For (Context),
               Match       => Match,
               Text        => Text,
               Give_Focus  => Give_Focus,
               Interactive => not Context.All_Occurrences);
         end if;

         Occurrence := new Source_Search_Occurrence_Record'
           (Search_Occurrence_Record with
            Editor_Child => Child,
            Match_From   =>
              (Editable_Line_Type (Match.Start.Line), Match.Start.Column),
            Match_Up_To  =>
              (Editable_Line_Type (Match.Finish.Line), Match.Finish.Column));
         Initialize (Occurrence, Pattern => Text);

         return True;
      end Interactive_Callback;

      Dummy_Boolean : Boolean;
   begin
      Found := False;

      if Child = null then
         Continue := False;
         return null;
      end if;

      Editor := Get_Source_Box_From_MDI (Child);
      Buffer := Get_Buffer (Editor);

      Context.Current_File := To_Unbounded_String
        (Get_Filename (Buffer).Display_Full_Name);

      if Give_Focus then
         Focus_To_Editor (Child);
      end if;

      if not Context.All_Occurrences then
         Occurrence := Auxiliary_Search
           (Context,
            Editor,
            Kernel,
            Search_Backward,
            From_Selection_Start);
         Found := Occurrence /= null;

         Continue := False; --  ??? Dummy boolean.
      else
         Search_From_Editor
           (Context,
            Get_Language_Handler (Kernel),
            Interactive_Callback'Unrestricted_Access,
            Child,
            Continue,
            Dummy_Boolean,
            Display_Matched_Only => Display_Matched_Only);
      end if;

      return Search_Occurrence (Occurrence);
   exception
      when E : others =>
         Trace (Me, E);
         Found := False;
         Continue := False;
         return  null;
   end Search;

   ---------------------
   -- Replace_Matched --
   ---------------------

   procedure Replace_Matched
     (Replacement : Replacement_Pattern;
      Matches     : Match_Vectors.Vector;
      Buffer      : Src_Editor_Buffer.Source_Buffer)
   is
      use type Ada.Containers.Count_Type;
   begin
      --  Replace starting from the end, so as to preserve lines and
      --  columns

      Set_Avoid_Cursor_Move_On_Changes (Buffer, True);

      declare
         G : Group_Block := Buffer.New_Undo_Group;
      begin
         Buffer.Get_Highlighter.Disable_Highlighting;

         for M of reverse Matches loop
            if Is_Empty_Match (M) then
               Insert
                 (Buffer,
                  Editable_Line_Type (M.Start.Line),
                  M.Start.Column,
                  Replacement.Replacement_Text
                    (M, "", Buffer.Get_Language.Keywords));
            else
               declare
                  Text : constant String := To_String (Get_Text
                    (Buffer,
                     Editable_Line_Type (M.Start.Line),
                     M.Start.Column,
                     Editable_Line_Type (M.Finish.Line),
                     M.Finish.Column + 1));
               begin
                  Replace_Slice
                    (Buffer,
                     Editable_Line_Type (M.Start.Line),
                     M.Start.Column,
                     Editable_Line_Type (M.Finish.Line),
                     M.Finish.Column + 1,
                     Replacement.Replacement_Text
                       (M, Text, Buffer.Get_Language.Keywords));
               end;
            end if;
         end loop;

         Buffer.Get_Highlighter.Enable_Highlighting;
      end;

      Set_Avoid_Cursor_Move_On_Changes (Buffer, False);

      Buffer.Get_Kernel.Get_Construct_Database.Update_Contents
        (Buffer.Get_Filename, Purge => Matches.Length > 50);
      --  If there are a lot of changes then update contents by purging old
      --  one and construct new contents from scratch.
   end Replace_Matched;

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
   begin
      Context.Replacement.Initialize
        (Replace_String  => Replace_String,
         Case_Preserving => Case_Preserving,
         Is_Regexp       => Context.Is_Regexp);

      if Give_Focus then
         Focus_To_Editor (Child);
      end if;

      Editor := Get_Source_Box_From_MDI (Child);

      if Context.All_Occurrences then
         Editor.Get_Buffer.Freeze_Context;
         declare
            Text : constant String := Get_Buffer (Editor);
            Ref  : constant Buffer_Position := (Text'First, 1, 1, 1);
            Matches : Match_Vectors.Vector;
         begin
            Scan_And_Store
              (Context => Context,
               Handler => Get_Language_Handler (Kernel),
               Kernel  => Kernel_Handle (Kernel),
               Str     => Text,
               File    => GNATCOLL.VFS.No_File,
               Ref     => Ref,
               Scope   => Context.Scope,
               Lang    => Get_Language (Get_Buffer (Editor)),
               Result  => Matches);

            if not Matches.Is_Empty then
               Context.Nb_Of_Replacements :=
                 Context.Nb_Of_Replacements + Natural (Matches.Length);

               Replace_Matched
                 (Replacement => Context.Replacement,
                  Matches     => Matches,
                  Buffer      => Get_Buffer (Editor));
               Editor.Get_Buffer.Thaw_Context;
               return True;
            else
               Editor.Get_Buffer.Thaw_Context;
               return False;
            end if;

         exception
            when others =>
               Editor.Get_Buffer.Thaw_Context;
               raise;
         end;
      else
         --  Test whether the current context text contains the search string.
         --  Warning: we cannot use selection here, since apparently there can
         --  be only one selection in the whole GPS window, and the selection
         --  in the source buffer will be erased when the focus is given to the
         --  search dialog.

         if Context.Current /= GPS.Search.No_Match then
            declare
               Original : constant String :=
                 (if Is_Empty_Match (Context.Current) then
                     ""
                  else
                     To_String (Editor.Get_Buffer.Get_Text
                       (Editable_Line_Type (Context.Current.Start.Line),
                        Context.Current.Start.Column,
                        Editable_Line_Type (Context.Current.Finish.Line),
                        Context.Current.Finish.Column + 1)));

               Text : constant String :=
                 Context.Replacement.Replacement_Text
                   (Context.Current,
                    Original,
                    Editor.Get_Buffer.Get_Language.Keywords);
            begin
               if Is_Empty_Match (Context.Current) then
                  Insert
                    (Get_Buffer (Editor),
                     Editable_Line_Type (Context.Current.Start.Line),
                     Context.Current.Start.Column,
                     Text);
               else
                  Replace_Slice
                    (Get_Buffer (Editor),
                     Editable_Line_Type (Context.Current.Start.Line),
                     Context.Current.Start.Column,
                     Editable_Line_Type (Context.Current.Finish.Line),
                     Context.Current.Finish.Column + 1,
                     Text);
               end if;

               Forward_Position
                 (Get_Buffer (Editor),
                  Editable_Line_Type (Context.Current.Start.Line),
                  Context.Current.Start.Column,
                  Text'Length,
                  Editable_Line_Type (Context.Current.Finish.Line),
                  Context.Current.Finish.Column);

               Context.Current.Finish.Index :=
                 Context.Current.Start.Index + Text'Length;
            end;

            Push_Current_Editor_Location_In_History (Kernel);

            if Search_Backward then
               Context.Current.Finish := Context.Current.Start;
            else
               Context.Current.Start.Line := Context.Current.Finish.Line;
               Context.Current.Start.Column :=
                 Context.Current.Finish.Column - 1;
            end if;

            Set_Cursor_Position
              (Get_Buffer (Editor),
               Editable_Line_Type (Context.Current.Finish.Line),
               Context.Current.Finish.Column,
               Internal => True);

            Get_View (Editor).Set_Position_Set_Explicitely;

            Save_Cursor_Position (Get_View (Editor));
         end if;
      end if;

      return True;
   end Replace_From_Editor;

   -----------------------
   -- Replace_From_File --
   -----------------------

   function Replace_From_File
     (Context         : access Abstract_Files_Context'Class;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      File            : GNATCOLL.VFS.Virtual_File) return Boolean
   is
      Matches : Match_Vectors.Vector;
      Child   : MDI_Child;
   begin
      --  If the file is loaded in an editor, do the replacement directly
      --  there.

      Child := Find_Editor (Kernel, File, No_Project);  --  any project

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

         --  ??? Could be more efficient, since we have already read the
         --  file to do the search

         Scan_And_Store
           (Context => Context,
            Handler => Get_Language_Handler (Kernel),
            Kernel  => Kernel_Handle (Kernel),
            File    => File,
            Scope   => Context.Scope,
            Ref     => Unknown_Position,  --  not used since Str unspecified
            Lang    => Get_Language_From_File
              (Get_Language_Handler (Kernel), File),
            Result  => Matches);

         if not Matches.Is_Empty then
            Context.Nb_Of_Replacements :=
              Context.Nb_Of_Replacements + Natural (Matches.Length);

            declare
               Buffer   : GNAT.Strings.String_Access;
               Last     : Positive := 1;
               Previously_Was_UTF8 : Boolean := True;
               Output_Buffer : Unbounded_String;
               Writable : Writable_File;
            begin
               declare
                  UTF8  : GNAT.Strings.String_Access;
                  Valid : Boolean;
               begin
                  Buffer := Read_File (File);
                  UTF8_Utils.Unknown_To_UTF8 (Buffer.all, UTF8, Valid);
                  if Valid then
                     if UTF8 /= null then
                        --  This means that Buffer is not already UTF8, and
                        --  we need to replace its contents with UTF8 before
                        --  proceeding.
                        Previously_Was_UTF8 := False;

                        Free (Buffer);
                        Buffer := UTF8;
                     end if;
                  end if;
               end;

               if Buffer /= null then
                  Context.Replacement.Initialize
                    (Replace_String  => Replace_String,
                     Case_Preserving => Case_Preserving,
                     Is_Regexp       => Context.Is_Regexp);

                  for M of Matches loop
                     Append
                       (Output_Buffer, Buffer (Last .. M.Start.Index - 1));
                     Append
                       (Output_Buffer,
                        Context.Replacement.Replacement_Text
                          (M,
                           Buffer (M.Start.Index .. M.Start.Index
                             + Replace_String'Length - 1),
                           Kernel.Get_Language_Handler.Get_Language_From_File
                             (File).Keywords));

                     Last := Index_After_Match (M);
                  end loop;

                  Append (Output_Buffer, Buffer (Last .. Buffer'Last));

                  if Previously_Was_UTF8 then
                     Writable := Write_File (File);
                     Write (Writable, To_String (Output_Buffer));
                     Close (Writable);
                  else
                     declare
                        Error    : GError_Access := new GError'(null);
                        Contents : constant String := Glib.Convert.Convert
                          (To_String (Output_Buffer),
                           Get_File_Charset (File), "UTF-8", Error);
                     begin
                        if Error.all = null then
                           Writable := Write_File (File);
                           Write (Writable, Contents);
                           Close (Writable);
                        else
                           Kernel.Insert
                             ("Could not save " & (+File.Full_Name.all)
                              & " with encoding " & Get_File_Charset (File)
                              & ", replace operation aborted for this file.",
                              Mode => GPS.Kernel.Error);
                           Error_Free (Error.all);
                           Error.all := null;
                        end if;

                        Unchecked_Free (Error);
                     end;
                  end if;
                  Free (Buffer);
               end if;
            end;
         end if;
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
         Trace (Me, E);
         return False;
   end Replace;

   ------------------------
   -- Highlight_Occurrence --
   ------------------------

   overriding procedure Highlight_Occurrence
     (Module     : not null access Current_File_Search_Module;
      Occurrence : not null access Search_Occurrence_Record'Class)
   is
      pragma Unreferenced (Module);
      Source_Occurrence : constant Source_Search_Occurrence :=
                            Source_Search_Occurrence (Occurrence);
      Editor            : Source_Editor_Box;
   begin
      if Source_Occurrence.Editor_Child = null then
         return;
      end if;

      Editor := Get_Source_Box_From_MDI (Source_Occurrence.Editor_Child);

      Editor.Set_Cursor_Location
        (Line             => Source_Occurrence.Match_From.Line,
         Column           => Source_Occurrence.Match_From.Col,
         Force_Focus      => False,
         Centering        => GPS.Editors.Minimal,
         Extend_Selection => False);

      Select_Region
        (Get_Buffer (Editor),
         Source_Occurrence.Match_From.Line,
         Source_Occurrence.Match_From.Col,
         Source_Occurrence.Match_Up_To.Line,
         Source_Occurrence.Match_Up_To.Col);

      Center_Cursor (Get_View (Editor));
   end Highlight_Occurrence;

   ------------------------------
   -- Give_Focus_To_Occurrence --
   ------------------------------

   overriding procedure Give_Focus_To_Occurrence
     (Module     : not null access Current_File_Search_Module;
      Occurrence : not null access Search_Occurrence_Record'Class) is
      pragma Unreferenced (Module);
      Source_Occurrence : constant Source_Search_Occurrence :=
                            Source_Search_Occurrence (Occurrence);
      Editor            : Source_Editor_Box;
   begin
      if Source_Occurrence.Editor_Child = null then
         return;
      end if;

      Focus_To_Editor (Source_Occurrence.Editor_Child);

      Editor := Get_Source_Box_From_MDI (Source_Occurrence.Editor_Child);

      Editor.Set_Cursor_Location
        (Line             => Source_Occurrence.Match_Up_To.Line,
         Column           => Source_Occurrence.Match_Up_To.Col,
         Force_Focus      => False,
         Centering        => GPS.Editors.Minimal,
         Extend_Selection => False);

      Center_Cursor (Get_View (Editor));
   end Give_Focus_To_Occurrence;

   ------------------------
   -- Search_From_Editor --
   ------------------------

   procedure Search_From_Editor
     (Context              : access File_Search_Context'Class;
      Handler              : access Language_Handler_Record'Class;
      Callback             : Scan_Callback;
      Editor               : MDI_Child;
      More_Matches         : out Boolean;
      Matches_Found        : out Boolean;
      Display_Matched_Only : Boolean := False)
   is
      Match  : GPS.Search.Search_Context;
      Text   : GNAT.Strings.String_Access;
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

         if Context.Current /= GPS.Search.No_Match then
            First_Match
              (Context       => Context,
               Handler       => Handler,
               Editor        => Editor,
               Scope         => Context.Scope,
               Lexical_State => Context.Current_Lexical,
               Start_Line    =>
                 Editable_Line_Type (Context.Current.Start.Line),
               Start_Column  => Context.Current.Start.Column + 1,
               Result        => Match,
               Text          => Text);

            if not GPS.Search.Failed (Match) then
               Context.Current := Match;
               More_Matches := Callback (Context.Current, Text.all);
               Matches_Found := True;
               Free (Text);
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
               Lexical_State        => State,
               Was_Partial          => Was_Partial,
               Display_Matched_Only => Display_Matched_Only);

            Matches_Found := True;
            More_Matches := Was_Partial;
         end;
      end if;
   end Search_From_Editor;

   ------------
   -- Search --
   ------------

   function Search
     (Context              : access Abstract_Files_Context;
      Handler              : access
        Language_Handlers.Language_Handler_Record'Class;
      Kernel               : GPS.Kernel.Kernel_Handle;
      Callback             : Scan_Callback;
      Display_Matched_Only : Boolean := False)
      return Boolean
   is
      C      : constant Abstract_Files_Context_Access :=
                 Abstract_Files_Context_Access (Context);
      --  For dispatching purposes

      Button         : Message_Dialog_Buttons;
      Matches_Found  : Boolean := False;
      Already_Looped : Boolean := False;
      State          : Recognized_Lexical_States;
      Was_Partial    : Boolean;
      Match          : GPS.Search.Search_Context;
      Text           : GNAT.Strings.String_Access;

   begin
      if not Context.All_Occurrences then
         loop
            Matches_Found := False;

            if Current_File (C) /= No_File then

               --  Are there any more match in the current file ? Stop looking
               --  in this file if the editor was closed (we know it was opened
               --  when the first match was seen)

               if Context.Current /= GPS.Search.No_Match then
                  First_Match
                    (Context       => Context,
                     Handler       => Handler,
                     Kernel        => Kernel,
                     Name          => Current_File (C),
                     Scope         => Context.Scope,
                     Lexical_State => Context.Current_Lexical,
                     Start_Line    =>
                       Editable_Line_Type (Context.Current.Start.Line),
                     Start_Column  => Context.Current.Start.Column + 1,
                     Result        => Match,
                     Text          => Text,
                     Force_Read    => Kernel = null);

                  if Match /= GPS.Search.No_Match then
                     Context.Current := Match;
                     Matches_Found := Callback (Context.Current, Text.all);
                  end if;

                  Free (Text);
               end if;
            end if;

            if not Matches_Found then
               Move_To_Next_File (C);
               Context.Current := GPS.Search.No_Match;

               --  make sure that Search_From_File will perform some search
               Context.Current.Start := (1, 1, 1, 1);

               if Current_File (C) = GNATCOLL.VFS.No_File then
                  if not Already_Looped then
                     Button := GPS_Message_Dialog
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
                        Stop_Macro_Action_Hook.Run (Kernel);
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
         end if;

         loop
            State := Statements;
            Scan_File
              (Context,
               Handler,
               Kernel,
               Name          => Current_File (C),
               Callback      => Callback,
               Scope         => Context.Scope,
               Lexical_State => State,
               Force_Read    => Kernel = null,
               Was_Partial   => Was_Partial,
               Display_Matched_Only => Display_Matched_Only);
            Matches_Found := True;
            exit when not Was_Partial;
         end loop;
         return True;
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
         Side_And_Locations);
      --  Call inherited Reset
      File_Search_Context (Context.all).Reset (Kernel);
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset
     (Context : access File_Search_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Context.Replacement.Reset;
      --  Call inherited Reset
      Root_Search_Context (Context.all).Reset (Kernel);
   end Reset;

   ------------
   -- Search --
   ------------

   overriding function Search
     (Context              : access Abstract_Files_Context;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Give_Focus           : Boolean;
      Found                : out Boolean;
      Continue             : out Boolean;
      Display_Matched_Only : Boolean := False) return Search_Occurrence
   is
      pragma Unreferenced (From_Selection_Start);
      C : constant Abstract_Files_Context_Access :=
        Abstract_Files_Context_Access (Context);
      --  For dispatching purposes

      Occurrence : Source_Search_Occurrence;

      function Interactive_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean;
      --  Callbacks for the general search function

      --------------------------
      -- Interactive_Callback --
      --------------------------

      function Interactive_Callback
        (Match : GPS.Search.Search_Context;
         Text  : String) return Boolean
      is
         File : constant GNATCOLL.VFS.Virtual_File := Current_File (C);
      begin
         Found := True;
         Highlight_Result
           (Kernel      => Kernel,
            File_Name   => File,
            Look_For    => Context_Look_For (C),
            Match       => Match,
            Text        => Text,
            Give_Focus  => Give_Focus,
            Interactive => not Context.All_Occurrences);

         Occurrence := new Source_Search_Occurrence_Record'
           (Search_Occurrence_Record with
            Editor_Child => Find_Editor (Kernel, File, No_Project),
            Match_From   =>
              (Editable_Line_Type (Match.Start.Line), Match.Start.Column),
            Match_Up_To  =>
              (Editable_Line_Type (Match.Finish.Line), Match.Finish.Column));
         Initialize (Occurrence, Pattern => Text);

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
            Interactive_Callback'Unrestricted_Access,
            Display_Matched_Only => Display_Matched_Only);
      end if;

      return Search_Occurrence (Occurrence);
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
         if Context.Current /= GPS.Search.No_Match
           and then Is_Open (Kernel, Current_File (C))
         then
            Child := Find_Editor
              (Kernel, Current_File (C), No_Project);  --  any project

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
      Context.Dirs.Clear;
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

      if Context.Dirs.Is_Empty then
         Prepend
           (Context.Dirs,
            new Dir_Data'
              (Name  => Context.Directory,
               Files => Context.Directory.Read_Dir,
               F_Idx => 1));
      end if;

      while Context.Current_File = GNATCOLL.VFS.No_File loop
         declare
            Data : Dir_Data_Access renames Context.Dirs.First_Element;
         begin
            if Data.F_Idx > Data.Files'Last then
               Context.Dirs.Delete_First;
               Context.Current_Dir := Context.Current_Dir + 1;

               if Context.Dirs.Is_Empty then
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
     (Combo  : access Gtk_Combo_Box_Text_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      Add_Unique_Combo_Entry (Combo, -"Whole Text", True);
      Add_Unique_Combo_Entry (Combo, -"Comments Only");
      Add_Unique_Combo_Entry (Combo, -"Comments + Strings");
      Add_Unique_Combo_Entry (Combo, -"Strings Only");
      Add_Unique_Combo_Entry (Combo, -"All but Comments");

      Set_Tooltip_Text (Combo, -"Restrict the scope of the search");

      Kernel_Callback.Connect
        (Combo, Gtk.Combo_Box.Signal_Changed, Vsearch.Reset_Search'Access,
         Kernel_Handle (Kernel));
   end Initialize_Scope_Combo;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Selector : not null access Simple_Scope_Selector_Record;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Gtk_New (Selector.Combo);
      Initialize_Scope_Combo (Selector.Combo, Kernel);
   end Initialize;

   ---------------------
   -- Get_Scope_Combo --
   ---------------------

   overriding function Get_Scope_Combo
     (Selector : not null access Simple_Scope_Selector_Record)
      return Gtk.Combo_Box_Text.Gtk_Combo_Box_Text
   is
      (Selector.Combo);

   -------------------------
   -- Get_Optional_Widget --
   -------------------------

   overriding function Get_Optional_Widget
     (Selector : not null access Simple_Scope_Selector_Record)
      return Gtk.Widget.Gtk_Widget
   is
     (null);

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Selector : not null access Files_Extra_Scope_Record;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Initialize (Simple_Scope_Selector_Record (Selector.all)'Access, Kernel);

      Selector.File_Info_Widget := new Files_Extra_Info_Record;
      Files_Extra_Info_Pkg.Initialize
        (Selector.File_Info_Widget, Kernel, 1);

      Kernel_Callback.Connect
        (Selector.File_Info_Widget.Subdirs_Check,
         Signal_Toggled,
         Reset_Search'Access,
         Kernel_Handle (Kernel));
      Kernel_Callback.Connect
        (Selector.File_Info_Widget.Files_Entry,
         Gtk.Editable.Signal_Changed,
         Reset_Search'Access,
         Kernel_Handle (Kernel));
      Kernel_Callback.Connect
        (Selector.File_Info_Widget.Directory_Entry,
         Gtk.Editable.Signal_Changed,
         Reset_Search'Access,
         Kernel_Handle (Kernel));
   end Initialize;

   -------------------------
   -- Get_Optional_Widget --
   -------------------------

   overriding function Get_Optional_Widget
     (Selector : not null access Files_Extra_Scope_Record)
      return Gtk.Widget.Gtk_Widget
   is
      (Gtk_Widget (Selector.File_Info_Widget));

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

   ---------------------
   -- Context_Look_In --
   ---------------------

   overriding function Context_Look_In
     (Self : Current_Selection_Context) return String
   is
      pragma Unreferenced (Self);
   begin
      return -"selected region";
   end Context_Look_In;

   ---------------------
   -- Context_Look_In --
   ---------------------

   overriding function Context_Look_In
     (Self : Runtime_Files_Context) return String
   is
      pragma Unreferenced (Self);
   begin
      return -"any file in the runtime";
   end Context_Look_In;

end Src_Contexts;
