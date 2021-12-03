------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2021, AdaCore                   --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.Strings;              use GNAT.Strings;
with GNAT.Regpat;               use GNAT.Regpat;

with GNATCOLL.JSON;             use GNATCOLL.JSON;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

with GPS.Kernel.Hooks;
with Default_Preferences;       use Default_Preferences;
with Default_Preferences.Enums;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

with VSS.JSON.Push_Writers;
with VSS.Strings.Conversions;
with VSS.Strings;
with VSS.String_Vectors;

with Config;                    use Config;
with GS_Text_Streams;
with Toolchains;                use Toolchains;
with Remote;
with Cpp_Module;                use Cpp_Module;

package body GPS.LSP_Client.Configurations.Clangd is

   Me : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("GPS.LSP.CLANGD_SUPPORT.DIAGNOSTICS", Off);

   Clang_Format_File_Name : constant Filesystem_String := ".clang-format";

   package String_String_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, String);

   package Unbounded_String_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Ada.Strings.Unbounded.Unbounded_String);

   function Hash
     (Item : GNATCOLL.VFS.Virtual_File) return Ada.Containers.Hash_Type;

   package Virtual_File_Sets is
     new Ada.Containers.Hashed_Sets
           (GNATCOLL.VFS.Virtual_File, Hash, GNATCOLL.VFS."=");

   type Formatting_Options is
     (None,
      BasedOnStyle,
      AccessModifierOffset,
      AlignAfterOpenBracket,
      AlignConsecutiveAssignment,
      AlignConsecutiveBitFields,
      AlignConsecutiveDeclarations,
      AlignConsecutiveMacros,
      AlignEscapedNewlines,
      AlignOperands,
      AlignTrailingComments,
      AllowAllArgumentsOnNextLine,
      AllowAllConstructorInitializersOnNextLine,
      AllowAllParametersOfDeclarationOnNextLine,
      AllowShortBlocksOnASingleLine,
      AllowShortCaseLabelsOnASingleLine,
      AllowShortEnumsOnASingleLine,
      AllowShortFunctionsOnASingleLine,
      AllowShortIfStatementsOnASingleLine,
      AllowShortLambdasOnASingleLine,
      AllowShortLoopsOnASingleLine,
      AlwaysBreakAfterReturnType,
      AlwaysBreakBeforeMultilineStrings,
      AlwaysBreakTemplateDeclarations,
      AttributeMacros,
      BinPackArguments,
      BinPackParameters,
      BitFieldColonSpacing,
      BraceWrapping,
      BreakAfterJavaFieldAnnotations,
      BreakBeforeBinaryOperators,
      BreakBeforeBraces,
      BreakBeforeConceptDeclarations,
      BreakBeforeTernaryOperators,
      BreakConstructorInitializers,
      BreakInheritanceList,
      BreakStringLiterals,
      ColumnLimit,
      CommentPragmas,
      CompactNamespaces,
      ConstructorInitializerAllOnOneLineOrOnePerLine,
      ConstructorInitializerIndentWidth,
      ContinuationIndentWidth,
      Cpp11BracedListStyle,
      DeriveLineEnding,
      DerivePointerAlignment,
      DisableFormat,
      ExperimentalAutoDetectBinPacking,
      FixNamespaceComments,
      ForEachMacros,
      IncludeBlocks,
      IncludeCategories,
      IncludeIsMainRegex,
      IncludeIsMainSourceRegex,
      IndentCaseBlocks,
      IndentCaseLabels,
      IndentExternBlock,
      IndentGotoLabels,
      IndentPPDirectives,
      IndentPragmas,
      IndentRequires,
      IndentWidth,
      IndentWrappedFunctionNames,
      InsertTrailingCommas,
      JavaImportGroups,
      JavaScriptQuotes,
      JavaScriptWrapImports,
      KeepEmptyLinesAtTheStartOfBlocks,
      Language,
      MacroBlockBegin,
      MacroBlockEnd,
      MaxEmptyLinesToKeep,
      NamespaceIndentation,
      NamespaceMacros,
      ObjCBinPackProtocolList,
      ObjCBlockIndentWidth,
      ObjCBreakBeforeNestedBlockParam,
      ObjCSpaceAfterProperty,
      ObjCSpaceBeforeProtocolList,
      PenaltyBreakAssignment,
      PenaltyBreakBeforeFirstCallParameter,
      PenaltyBreakComment,
      PenaltyBreakFirstLessLess,
      PenaltyBreakString,
      PenaltyBreakTemplateDeclaration,
      PenaltyExcessCharacter,
      PenaltyIndentedWhitespace,
      PenaltyReturnTypeOnItsOwnLine,
      PointerAlignment,
      RawStringFormats,
      ReflowComments,
      SortIncludes,
      SortJavaStaticImport,
      SortUsingDeclarations,
      SpaceAfterCStyleCast,
      SpaceAfterLogicalNot,
      SpaceAfterTemplateKeyword,
      SpaceAroundPointerQualifiers,
      SpaceBeforeAssignmentOperators,
      SpaceBeforeCpp11BracedList,
      SpaceBeforeCtorInitializerColon,
      SpaceBeforeInheritanceColon,
      SpaceBeforeParens,
      SpaceBeforeRangeBasedForLoopColon,
      SpaceBeforeSquareBrackets,
      SpaceInEmptyBlock,
      SpaceInEmptyParentheses,
      SpacesBeforeTrailingComments,
      SpacesInAngles,
      SpacesInCStyleCastParentheses,
      SpacesInConditionalStatement,
      SpacesInContainerLiterals,
      SpacesInParentheses,
      SpacesInSquareBrackets,
      Standard,
      StatementMacros,
      TabWidth,
      TypenameMacros,
      UseCRLF,
      UseTab,
      WhitespaceSensitiveMacros);

   procedure Set_Formatting (Root_Project : Project_Type);
   --  Set formatting options in the .clang-format file.

   procedure Check_Formatting_Option
     (S          : in out Unbounded_String;
      Check_Only : Formatting_Options := None;
      Option     : out Formatting_Options;
      Changed    : out Boolean);
   --  Checks whether S is an option and it's value is up-to-date.
   --  Changed is true when an option value has been changed and S
   --  will contain new string for the option.
   --  If Check_Only is set, process only that option.

   function Check_Formatting_Option
     (Option    : Formatting_Options;
      Value     : String;
      New_Value : out Unbounded_String)
      return Boolean;
   --  Checks whether the Option's Value is changed.
   --  Returns True and new option string in the New_Value if changed.

   function Is_Header_File
     (File            : Virtual_File;
      C_Spec_Suffix   : String;
      CPP_Spec_Suffix : String) return Boolean;
   --  Return True if the given File is a C or C++ header file, depending on
   --  the given spec suffixes.

   type On_Pref_Changed is new GPS.Kernel.Hooks.Preferences_Hooks_Function
     with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);

   type On_Project_Changing is new GPS.Kernel.Hooks.File_Hooks_Function
     with null record;
   overriding procedure Execute
      (Self   : On_Project_Changing;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : GNATCOLL.VFS.Virtual_File);
   --  Called when project will be unloaded.

   Formatting_Config_Dir      : Virtual_File_Sets.Set;
   --  Directory where clangd formatting configurations are located.

   Current_Formatting_Options : Unbounded_String_Vectors.Vector;
   --  Holds current option file lines

   Option_Regexp              : constant Pattern_Matcher :=
     Compile ("^([a-zA-Z]+)\s*:\s*(.+)$", Single_Line);

   -- Preferences --

   type BasedOnStyle_Kind is
     (LLVM, Google, Chromium, Mozilla, WebKit, Microsoft, GNU);

   function Image (Item : BasedOnStyle_Kind) return String;
   function Image (Item : Integer) return String;

   package BasedOnStyle_Formatting_Preferences is new
     Default_Preferences.Enums.Generics (BasedOnStyle_Kind);

   BasedOnStyle_Preference : BasedOnStyle_Formatting_Preferences.Preference;
   ContinuationIndentWidth_Preference : Integer_Preference;

   procedure Write_Clang_Format_Files;
   --  Write formatting options file in each configuration directory.

   -----------------------------
   -- Check_Formatting_Option --
   -----------------------------

   procedure Check_Formatting_Option
     (S          : in out Unbounded_String;
      Check_Only : Formatting_Options := None;
      Option     : out Formatting_Options;
      Changed    : out Boolean)
   is
      Matched : Match_Array (0 .. 2);
      Value   : Unbounded_String;
   begin
      Changed := False;
      Option  := None;
      Match (Option_Regexp, To_String (S), Matched);

      if Matched (0) /= No_Match then
         begin
            Option := Formatting_Options'Value
              (Slice (S, Matched (1).First, Matched (1).Last));

            if Check_Only = None
              or else Option = Check_Only
            then
               if Check_Formatting_Option
                 (Option,
                  Slice (S, Matched (2).First, Matched (2).Last),
                  Value)
               then
                  S       := Value;
                  Changed := True;
               end if;
            end if;

         exception
            when others =>
               null;
         end;
      end if;
   end Check_Formatting_Option;

   -----------------------------
   -- Check_Formatting_Option --
   -----------------------------

   function Check_Formatting_Option
     (Option    : Formatting_Options;
      Value     : String;
      New_Value : out Unbounded_String)
      return Boolean
   is
      Result : Boolean := False;

      procedure Compare (Name, Current : String);
      procedure Compare (Name, Current : String) is
      begin
         if Value /= Current then
            New_Value := To_Unbounded_String (Name & ": " & Current);
            Result := True;
         end if;
      end Compare;

   begin
      case Option is
         when BasedOnStyle =>
            Compare
              ("BasedOnStyle",
               Image (BasedOnStyle_Kind'(BasedOnStyle_Preference.Get_Pref)));

         when ColumnLimit =>
            Compare ("ColumnLimit", Image (Highlight_Column.Get_Pref));

         when IndentWidth =>
            Compare ("IndentWidth", Image (C_Indentation_Level.Get_Pref));

         when ContinuationIndentWidth =>
            Compare
              ("ContinuationIndentWidth",
               Image (Integer'(ContinuationIndentWidth_Preference.Get_Pref)));

         when UseTab =>
            Compare
              ("UseTab", (if C_Use_Tabs.Get_Pref then "Always" else "Never"));

         when ReflowComments =>
            Compare
              ("ReflowComments",
               (if C_Indent_Comments.Get_Pref then "true" else "false"));

         when others =>
            null;
      end case;

      return Result;
   end Check_Formatting_Option;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Project_Changing;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : GNATCOLL.VFS.Virtual_File) is
   begin
      Formatting_Config_Dir.Clear;
      Current_Formatting_Options.Clear;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      Changed : Boolean := False;

      ----------------
      -- Check_Pref --
      ----------------

      procedure Check_Pref (Which : Formatting_Options);

      procedure Check_Pref (Which : Formatting_Options) is
         Option   : Formatting_Options;
         Modified : Boolean;
      begin
         for S of Current_Formatting_Options loop
            Check_Formatting_Option (S, Which, Option, Modified);
            exit when Option = Language;
            Changed := Changed or else Modified;
         end loop;
      end Check_Pref;

   begin
      if Formatting_Config_Dir.Is_Empty then
         return;
      end if;

      if Pref = null then
         --  Multiple changes, checking all preferences
         declare
            Option   : Formatting_Options;
            Modified : Boolean;
         begin
            for S of Current_Formatting_Options loop
               Check_Formatting_Option (S, None, Option, Modified);
               exit when Option = Language;
               Changed := Changed or else Modified;
            end loop;
         end;

      elsif Pref = Preference (BasedOnStyle_Preference) then
         Check_Pref (BasedOnStyle);

      elsif Pref = Preference (Highlight_Column) then
         Check_Pref (ColumnLimit);

      elsif Pref = Preference (C_Indentation_Level) then
         Check_Pref (IndentWidth);

      elsif Pref = Preference (ContinuationIndentWidth_Preference) then
         Check_Pref (ContinuationIndentWidth);

      elsif Pref = Preference (C_Use_Tabs) then
         Check_Pref (UseTab);

      elsif Pref = Preference (C_Indent_Comments) then
         Check_Pref (ReflowComments);
      end if;

      if Changed then
         Write_Clang_Format_Files;
      end if;
   end Execute;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : GNATCOLL.VFS.Virtual_File) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Item.Display_Full_Name);
   end Hash;

   --------------------
   -- Is_Header_File --
   --------------------

   function Is_Header_File
     (File            : Virtual_File;
      C_Spec_Suffix   : String;
      CPP_Spec_Suffix : String) return Boolean
   is
      File_Ext : constant String := +File.File_Extension;
   begin
      if File_Ext = C_Spec_Suffix or else File_Ext = CPP_Spec_Suffix then
         return True;
      else
         return False;
      end if;
   end Is_Header_File;

   ----------------------------
   -- On_Server_Capabilities --
   ----------------------------

   procedure On_Server_Capabilities
     (Capabilities : in out LSP.Messages.ServerCapabilities) is null;

   -----------
   -- Image --
   -----------

   function Image (Item : BasedOnStyle_Kind) return String is
   begin
      case Item is
         when LLVM =>
            return "LLVM";
         when Google =>
            return "Google";
         when Chromium =>
            return "Chromium";
         when Mozilla =>
            return "Mozilla";
         when WebKit =>
            return "WebKit";
         when Microsoft =>
            return "Microsoft";
         when GNU =>
            return "GNU";
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Item : Integer) return String is
      S : constant String := Integer'Image (Item);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   ------------------------------------
   -- Prepare_Configuration_Settings --
   ------------------------------------

   overriding procedure Prepare_Configuration_Settings
     (Self : in out Clangd_Configuration)
   is
      Tree         : constant Project_Tree_Access :=
        Self.Kernel.Get_Project_Tree;
      Iter             : Project_Iterator;
      P                : Project_Type;
      Project_Dir      : constant Virtual_File :=
        Tree.Root_Project.Project_Path.Dir;
      Project_Dir_Name : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String
          (Project_Dir.Display_Dir_Name);
      C_Spec_Suffix    : constant String := Attribute_Value
        (Project      => P,
         Attribute    => Spec_Suffix_Attribute,
         Index        => "C",
         Default      => ".h",
         Use_Extended => True);
      CPP_Spec_Suffix  : constant String := Attribute_Value
        (Project      => P,
         Attribute    => Spec_Suffix_Attribute,
         Index        => "C++",
         Default      => ".hh",
         Use_Extended => True);
      Compilers  : String_String_Maps.Map;
      Drivers    : Unbounded_String;
      Includes   : Unbounded_String;

      Writer     : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Stream     : aliased GS_Text_Streams.File_UTF8_Output_Stream;
      Dir        : Virtual_File;

      procedure Process_Files (P : Project_Type);
      --  Process project's files and prepare a database for clangd

      -------------------
      -- Process_Files --
      -------------------

      procedure Process_Files (P : Project_Type) is
         Target     : constant String := P.Get_Target;
         Dirs       : constant GNATCOLL.VFS.File_Array := P.Source_Dirs;
         Files      : GNATCOLL.VFS.File_Array_Access   := P.Source_Files;
         Switches   : GNAT.Strings.String_List_Access;
         Is_Default : Boolean;
         Command    : Unbounded_String;

      begin
         --  Prepare switches for including source directories
         for Index in Dirs'Range loop
            declare
               --  Use relative path because file name is used as a key in
               --  clangd cache, so this will guarantee that we have unique
               --  key-names
               Path : constant String :=
                 (+(Relative_Path (Dirs (Index), Project_Dir)));
            begin
               --  Remove last path separator (e.g. '/') because example in
               --  clangd documentation looks like:
               --  "command": "/usr/bin/clang++ -Irelative -c file.cc",
               --  so without it
               if Path (Path'First .. Path'Last - 1) /= "" then
                  Append
                    (Includes,
                     "-I" & Path (Path'First .. Path'Last - 1) & " ");
               end if;
            end;
         end loop;

         --  Process files and prepare information for clangd
         --  compilation database
         for File of Files.all loop
            declare
               use Remote;
               Language : constant String :=
                 File_Info'Class (Tree.Info_Set (File).First_Element).Language;
               Toolchain : Toolchains.Toolchain;
               Full_Name : Unbounded_String;
            begin
               --  We only want to generate entries for C and C++
               --  implementation files, not headers.

               if Language in "c" | "cpp" | "c++"
                 and then not Is_Header_File
                   (File            => File,
                    C_Spec_Suffix   => C_Spec_Suffix,
                    CPP_Spec_Suffix => CPP_Spec_Suffix)
               then
                  --  Retrieve compiler name for certain language if
                  --  did'nt done yet. For C and CPP we can have
                  --  different ones.
                  if not Compilers.Contains (Language) then
                     Toolchain := Self.Kernel.Get_Toolchains_Manager.
                       Get_Toolchain (P);

                     if Is_Base_Name (Toolchain, Language) then
                        Full_Name := To_Unbounded_String
                          (+GNATCOLL.VFS.Locate_On_Path
                             (+Get_Exe (Get_Compiler (Toolchain, Language)),
                              Get_Nickname (Build_Server)).Full_Name);
                     else
                        Full_Name := To_Unbounded_String
                          (Get_Exe (Get_Compiler (Toolchain, Language)));
                     end if;

                     Compilers.Insert (Language, To_String (Full_Name));

                     if Length (Drivers) /= 0 then
                        Append (Drivers, ",");
                     end if;
                     Append (Drivers, To_String (Full_Name));
                  end if;

                  Writer.Start_Object;

                  declare
                     Path : constant String :=
                       (+(Relative_Path (File, Project_Dir)));

                  begin
                     Writer.Key_Name
                       (VSS.Strings.To_Virtual_String ("directory"));
                     Writer.String_Value (Project_Dir_Name);

                     P.Switches
                       ("Compiler", File, Language, Switches, Is_Default);

                     Command := Compilers.Element (Language) & " " & Includes;

                     for Index in 1 .. Switches'Length loop
                        if Switches (Index) /= null
                          and then Switches (Index).all /= ""
                        then
                           Append (Command, Switches (Index).all & " ");
                        end if;
                     end loop;
                     Free (Switches);

                     --  If we are on a native Windows host, specify the target
                     --  to clang: otherwise it will try to use MSVC by
                     --  default.
                     if Target = "" and then Config.Host = Windows then
                        Append (Command, "--target=x86_64-pc-windows-gnu ");
                     end if;

                     Writer.Key_Name
                       (VSS.Strings.To_Virtual_String ("command"));
                     Writer.String_Value
                       (VSS.Strings.Conversions.To_Virtual_String
                          (Ada.Strings.Unbounded.To_String (Command & Path)));

                     Writer.Key_Name
                       (VSS.Strings.To_Virtual_String ("file"));
                     Writer.String_Value
                       (VSS.Strings.Conversions.To_Virtual_String (Path));
                  end;

                  Writer.End_Object;
               end if;
            end;
         end loop;

         GNATCOLL.VFS.Unchecked_Free (Files);
      end Process_Files;

   begin
      Dir := Tree.Root_Project.Artifacts_Dir / (+".clangd");

      if not Dir.Is_Directory then
         Make_Dir (Dir);
      end if;

      Stream.Open (Create_From_Dir (Dir, "compile_commands.json"));
      Writer.Set_Stream (Stream'Unchecked_Access);

      Writer.Start_Document;
      Writer.Start_Array;

      Iter := Tree.Root_Project.Start;
      loop
         P := Current (Iter);
         exit when P = No_Project;
         Process_Files (P);
         Next (Iter);
      end loop;

      Writer.End_Array;
      Writer.End_Document;
      Stream.Close;

      Set_Formatting (Tree.Root_Project);

      Self.Server_Arguments.Append
        ("--compile-commands-dir=" & Display_Dir_Name (Dir));
      Self.Server_Arguments.Append ("--offset-encoding=utf-8");
      Self.Server_Arguments.Append ("--pretty");

      --  Set logging to verbose if the GPS.LSP.CLANGD_SUPPORT.DIAGNOSTICS
      --  trace is enabled. Just log the errors othwerwise.
      Self.Server_Arguments.Append
        ("--log=" & (if Me.Is_Active then "verbose" else "error"));

      Self.Server_Arguments.Append ("--query-driver=" & To_String (Drivers));

   exception
      when E : others =>
         Me.Trace (E);
   end Prepare_Configuration_Settings;

   --------------------
   -- Set_Formatting --
   --------------------

   procedure Set_Formatting (Root_Project : Project_Type)
   is
      use type Ada.Containers.Count_Type;

      F       : Virtual_File;
      Changed : Boolean := False;
      --  Whether options are changed and the file should be rewrited

      Exists : array (Formatting_Options) of Boolean := (others => False);
      --  Used for detecting whether an option has a value

      Project_Config_Dir : GNATCOLL.VFS.Virtual_File;
      --  Configuration directory for the root project.

      New_Value : Unbounded_String;

   begin
      Current_Formatting_Options.Clear;
      Formatting_Config_Dir.Clear;

      Project_Config_Dir :=
        Greatest_Common_Path (Root_Project.Source_Dirs);

      if Project_Config_Dir = No_File then
         Project_Config_Dir := Root_Project.Project_Path.Dir;
      end if;

      Formatting_Config_Dir.Insert (Project_Config_Dir);

      --  Process all projects

      declare
         Iterator   : Project_Iterator := Root_Project.Start;
         Project    : Project_Type;
         Common_Dir : Virtual_File;
         Include    : Boolean;

      begin
         loop
            Project := Current (Iterator);

            exit when Project = No_Project;

            if not Project.Externally_Built
              and (Project.Has_Language ("c")
                   or Project.Has_Language ("cpp")
                   or Project.Has_Language ("c++"))
            then
               --  Process only project with C/C++ sources and not externally
               --  built.

               Common_Dir := Greatest_Common_Path (Project.Source_Dirs);

               if Common_Dir /= No_File then
                  Include    := True;

                  for D of Formatting_Config_Dir loop
                     if D = Greatest_Common_Path ((D, Common_Dir)) then
                        Include := False;

                        exit;
                     end if;
                  end loop;

                  if Include then
                     Formatting_Config_Dir.Insert (Common_Dir);
                  end if;
               end if;
            end if;

            Next (Iterator);
         end loop;
      end;

      F := Create_From_Dir (Project_Config_Dir, Clang_Format_File_Name);

      if F.Is_Regular_File then
         --  Loading old settings
         declare
            Items : VSS.String_Vectors.Virtual_String_Vector;
            S     : GNAT.Strings.String_Access :=  F.Read_File;
         begin
            Items := VSS.Strings.Conversions.To_Virtual_String
              (S.all).Split_Lines;
            Free (S);

            declare
               S        : Unbounded_String;
               Option   : Formatting_Options;
               Modified : Boolean;
               Stop     : Boolean := False;
            begin
               for Index in 1 .. Items.Length loop
                  S := To_Unbounded_String
                    (VSS.Strings.Conversions.To_UTF_8_String
                       (Items.Element (Index)));

                  if not Stop then
                     Check_Formatting_Option (S, None, Option, Modified);
                     Stop := Option = Language;
                     Exists (Option) := True;
                     Changed := Changed or else Modified;
                  end if;
                  Current_Formatting_Options.Append (S);
               end loop;
            end;
         end;
      end if;

      for Option in reverse Formatting_Options loop
         if not Exists (Option)
           and then Check_Formatting_Option (Option, "", New_Value)
         then
            Current_Formatting_Options.Prepend (New_Value);
            Changed := True;
         end if;
      end loop;

      if Changed or Formatting_Config_Dir.Length /= 1 then
         --  Rewrite formatting options when they has been changed or
         --  there are more then one configuration file present. Last
         --  is necessary to synchronize options for all projects.

         Write_Clang_Format_Files;
      end if;
   end Set_Formatting;

   ------------------------------
   -- Set_Standard_Errors_File --
   ------------------------------

   procedure Set_Standard_Errors_File
     (Kernel : not null access Kernel_Handle_Record'Class;
      Client : in out GPS.LSP_Clients.LSP_Client)
   is
      Now  : constant Ada.Calendar.Time := Clock;
      File : constant Virtual_File := Create_From_Dir
        (Kernel.Get_Log_Dir,
         +("clangd." &
             Image (Now, ISO_Date) & Image (Now, "T%H%M%S") &
             ".txt"));
   begin
      Client.Set_Standard_Errors_File (File);
   end Set_Standard_Errors_File;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : Kernel_Handle) is

      Manager : constant Preferences_Manager := Kernel.Get_Preferences;
      Path    : constant String := "Editor/C & C++:Formatting with clangd";
   begin
      BasedOnStyle_Preference := BasedOnStyle_Formatting_Preferences.Create
        (Manager,
         Path    => Path,
         Name    => "clangd-BasedOnStyle",
         Default => GNU,
         Doc     => "The style used for all options not specifically set.",
         Label   => "BasedOnStyle");

      ContinuationIndentWidth_Preference := Manager.Create
        (Path    => Path,
         Name    => "clangd-ContinuationIndentWidth",
         Minimum => 0,
         Maximum => 99,
         Default => 2,
         Doc     => "Indent width for line continuations.",
         Label   => "ContinuationIndentWidth");

      GPS.Kernel.Hooks.Preferences_Changed_Hook.Add (new On_Pref_Changed);
      GPS.Kernel.Hooks.Project_Changing_Hook.Add (new On_Project_Changing);
   end Register;

   ------------------------------
   -- Write_Clang_Format_Files --
   ------------------------------

   procedure Write_Clang_Format_Files is
   begin
      for D of Formatting_Config_Dir loop
         declare
            F  : constant Virtual_File :=
              Create_From_Dir (D, Clang_Format_File_Name);
            WF : Writable_File := Write_File (F);

         begin
            for Item of Current_Formatting_Options loop
               Write (WF, To_String (Item) & ASCII.LF);
            end loop;

            Close (WF);
         end;
      end loop;
   end Write_Clang_Format_Files;

end GPS.LSP_Client.Configurations.Clangd;
