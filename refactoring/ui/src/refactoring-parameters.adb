------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with Basic_Types;           use Basic_Types;
with GNATCOLL.Scripts;      use GNATCOLL.Scripts;
with GPS.Editors;           use GPS.Editors;
with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.Actions;    use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;   use GPS.Kernel.Contexts;
with GPS.Kernel.Scripts;    use GPS.Kernel.Scripts;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI; use GPS.Kernel.Modules.UI;
with Commands.Interactive;  use Commands, Commands.Interactive;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with String_Utils;          use String_Utils;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GPS.Intl;              use GPS.Intl;
with Refactoring.Performers; use Refactoring.Performers;
with Refactoring.Services;   use Refactoring.Services;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Language;               use Language;
with Language.Ada;           use Language.Ada;
with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;
with Ada_Semantic_Tree;      use Ada_Semantic_Tree;
with Xref;                   use Xref;

package body Refactoring.Parameters is
   Me : constant Trace_Handle := Create ("GPS.REFACTORING.PARAMS");

   Location_Cst               : aliased constant String := "location";

   type Name_Parameters_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Name_Parameters_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Name Parameters" menu

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands

   function Name_Parameters
     (Context : not null access Factory_Context_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class;
      Entity  : Root_Entity'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Line    : Integer;
      Column  : Visible_Column_Type) return Command_Return_Type;
   --  Name the parameters for the call to Entity at the given location.
   --  Project is used in the case of aggregate projects to identify the
   --  context for File.

   ---------------------
   -- Name_Parameters --
   ---------------------

   function Name_Parameters
     (Context : not null access Factory_Context_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class;
      Entity  : Root_Entity'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Line    : Integer;
      Column  : Visible_Column_Type) return Command_Return_Type
   is
      --  File needs to be open for get_chars to work, unfortunately
      View  : constant Editor_View'Class :=
        Context.Buffer_Factory.Get (File).Open;
      pragma Unreferenced (View);

      Chars : constant String := Get_Text
        (Kernel, File, Line, Column, 1_000);
      First, Last : Integer := Chars'First;
      Nest_Count  : Integer := 1;
      Result : Unbounded_String;

      Params : Parameter_Array := Xref.Parameters (Entity);
      Iter   : Integer := Params'First;

      procedure Add_Parameter_Name;
      --  Add the name of the parameter in Result, skipping any space currently
      --  pointed to by First.
      --  First should point to '(' or ',' in the string Chars. Any character
      --  preceding First and not yet copied to Result is copied first

      function Is_Dotted_Notation return Boolean;
      --  When using the dotted notation, we need to skip the first parameter.
      --  Since there is no explicit indication in the ALI file, we need to
      --  check what's before the "." just before the name (if there is a dot).
      --  If it is a tagged object and we have a primitive operation, we are
      --  using the dotted notation

      ------------------------
      -- Add_Parameter_Name --
      ------------------------

      procedure Add_Parameter_Name is
         Tmp : Natural;
      begin
         First := First + 1;
         Skip_Blanks (Chars, First);

         --  Do we have a named parameter already ?
         Tmp := First;
         Skip_Word   (Chars, Tmp);
         Skip_Blanks (Chars, Tmp);
         if Tmp + 1 <= Chars'Last and then Chars (Tmp .. Tmp + 1) = "=>" then
            --  No need to replace any more: There won't be any unnamed
            --  parameter afterward
            Iter := Integer'Last;

         else
            Result := Result & Chars (Last .. First - 1);
            Last := First;

            if Chars (First - 1) /= '(' then
               First := First - 1;
            end if;

            Append (Result,
                    Xref.Get_Name (Params (Iter).Parameter.all)
                    & " => ");

            Iter := Iter + 1;
         end if;
      end Add_Parameter_Name;

      ------------------------
      -- Is_Dotted_Notation --
      ------------------------

      function Is_Dotted_Notation return Boolean is
         use type Ada.Containers.Count_Type;

         S_File : constant Structured_File_Access := Get_Or_Create
           (Db   => Kernel.Get_Construct_Database,
            File => File);

         Offset : constant String_Index_Type :=
           To_String_Index (S_File, Line, Column);

         Expression : Parsed_Expression :=
           Parse_Expression_Backward
             (Buffer       => Get_Buffer (S_File),
              Start_Offset => Offset);

         Entity_Token : Token_Record;

         Tok_Line   : Integer;
         Tok_Column : Visible_Column_Type;
      begin
         --  We expect to have something like [id] [.] [call]. Otherwise, we're
         --  not on a prefixed notation

         if Expression.Tokens.Length < 3 then
            Free (Expression);
            return False;
         end if;

         Expression.Tokens.Delete_Last;
         Entity_Token := Expression.Tokens.Last_Element;

         if Entity_Token.Tok_Type /= Tok_Dot then
            Free (Expression);
            return False;
         end if;

         Expression.Tokens.Delete_Last;
         Entity_Token := Expression.Tokens.Last_Element;

         if Entity_Token.Tok_Type /= Tok_Identifier then
            Free (Expression);
            return False;
         end if;

         if Is_Primitive_Of (Entity)'Length /= 0 then
            To_Line_Column
              (S_File, Entity_Token.Token_First, Tok_Line, Tok_Column);

            --  The following will not handle correctly where the primitive
            --  operation is declared inside a subprogram, and we use the fully
            --  qualified name to access it. That should be pretty rare though,
            --  since primitive operations can only be overridden in such a
            --  context, and users are not likely to use full qualification.
            --  But we want to handle the case of factories (ie subprograms)
            --  that return an access type and we use the dotted notation to
            --  call a primitive op on the result.
            declare
               Entity_Before : constant Root_Entity'Class
                 := Kernel.Databases.Get_Entity
                   (Name => Get_Name (Expression, Entity_Token),
                    Loc  => (File   => File,
                             Project_Path => Project.Project_Path,
                             Line   => Tok_Line,
                             Column => Tok_Column));
            begin
               if Entity_Before /= No_Root_Entity
                 and then (Has_Methods (Entity_Before)
                           --  A subprogram that returns a primitive
                           or else Is_Subprogram (Entity_Before))
               then
                  Free (Expression);
                  return True;
               end if;
            end;
         end if;

         Free (Expression);
         return False;
      end Is_Dotted_Notation;

   begin
      Skip_Word   (Chars, First);
      Skip_Blanks (Chars, First);
      if First > Chars'Last
        or else Chars (First) /= '('
      then
         Trace (Me, "Doesn't appear to be a subprogram call");
         Free (Params);
         return Failure;
      end if;

      if Params'Length = 0 then
         Trace (Me, "No parameter for this subprogram");
         Free (Params);
         return Failure;
      end if;

      if Is_Dotted_Notation then
         Trace (Me, "Rename parameters: detected dotted notation");
         Iter := Iter + 1;
         if Params'Length = 1 then
            Free (Params);
            return Success;
         end if;
      end if;

      --  Now add all parameter names, but not if the name is already specified
      --  by the user

      Add_Parameter_Name;

      while Iter <= Params'Last and then First <= Chars'Last loop
         if Chars (First) = '(' then
            Nest_Count := Nest_Count + 1;
         elsif Chars (First) = ')' then
            Nest_Count := Nest_Count - 1;
            exit when Nest_Count = 0;
         elsif Chars (First) = ',' and then Nest_Count = 1 then
            Add_Parameter_Name;
         end if;

         First  := First + 1;
      end loop;

      Free (Params);

      Result := Result & Chars (Last .. First);
      if Insert_Text
        (Context, In_File => File, Line => Line, Column => Column,
         Text            => To_String (Result),
         Indent          => False,
         Replaced_Length => First - Chars'First + 1)
      then
         return Success;
      else
         return Failure;
      end if;
   end Name_Parameters;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Name_Parameters_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Entity : constant Root_Entity'Class := Get_Entity (Context.Context);
   begin
      if Entity /= No_Root_Entity
        and then Is_Subprogram (Entity)
      then
         return Name_Parameters
           (Kernel  => Get_Kernel (Context.Context),
            Context => Get_Kernel (Context.Context).Refactoring_Context,
            Entity  => Get_Entity (Context.Context),
            File    => File_Information (Context.Context),
            Project => Project_Information (Context.Context),
            Line    => Line_Information (Context.Context),
            Column  => Column_Information (Context.Context));
      else
         return Commands.Success;
      end if;
   end Execute;

   ----------------------------
   -- Entity_Command_Handler --
   ----------------------------

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "name_parameters" then
         Name_Parameters (Data, (1 => Location_Cst'Access));
         declare
            Entity   : constant Root_Entity'Class := Get_Data (Data, 1);
            Location : constant File_Location_Info := Get_Data (Data, 2);
            File     : constant Virtual_File := Get_Data (Get_File (Location));
         begin
            if Name_Parameters
              (Kernel  => Get_Kernel (Data),
               Context => Get_Kernel (Data).Refactoring_Context,
               Entity  => Entity,
               File    => File,
               Project => No_Project,  --  ??? unknown
               Line    => Get_Line (Location),
               Column  => Get_Column (Location)) /= Success
            then
               Set_Error_Msg (Data, -"Couldn't name parameters");
            end if;
         end;
      end if;
   end Entity_Command_Handler;

   --------------------------
   -- Register_Refactoring --
   --------------------------

   procedure Register_Refactoring
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Action
        (Kernel, "refactoring name parameters",
         Command      => new Name_Parameters_Command,
         Description  =>
           -"Use named parameters for the selected subprogram call",
         Filter       => Create (Language => "Ada")
         and Lookup_Filter (Kernel, "Entity"),
         Category     => -"Refactoring",
         For_Learning => True);
      Register_Contextual_Menu
        (Kernel,
         Label => "Refactoring/Name parameters",
         Action => "refactoring name parameters",
         Group  => Editing_Contextual_Group);

      Kernel.Scripts.Register_Command
        ("name_parameters", 1, 1, Entity_Command_Handler'Access,
         Get_Entity_Class (Kernel));
   end Register_Refactoring;

end Refactoring.Parameters;
