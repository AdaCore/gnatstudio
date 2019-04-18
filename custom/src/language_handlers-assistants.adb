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

with Ada.Characters.Handling;            use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;                  use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;
with GNATCOLL.Projects;                  use GNATCOLL.Projects;
with GNATCOLL.Scripts;                   use GNATCOLL.Scripts;
with GNATCOLL.Utils;                     use GNATCOLL.Utils;
with GNATCOLL.VFS;                       use GNATCOLL.VFS;
with GNATCOLL.Xref;                      use GNATCOLL.Xref;

with Aliases_Module;                     use Aliases_Module;
with Commands;                           use Commands;
with Commands.Interactive;               use Commands.Interactive;
with Default_Preferences;                use Default_Preferences;
with GPS.Editors;                        use GPS.Editors;
with GPS.Kernel.Actions;                 use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;                use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                   use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;                 use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;              use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;                 use GPS.Kernel.Project;
with GPS.Kernel.Scripts;                 use GPS.Kernel.Scripts;

package body Language_Handlers.Assistants is

   File_Template_Class_Name : constant String := "FileTemplate";
   --  The name of the Python class for file templates

   File_Templates_Contextual_Group : constant := -1;
   --  The contextual menu group used when creating the file templates actions
   --  contextual menus.

   Create_Impl_Pref         : Boolean_Preference;

   type File_Template_Type is tagged record
      Alias       : Alias_Type;
      Label       : Unbounded_String;
      Unit_Param  : Unbounded_String;
      Language    : Unbounded_String;
      Is_Impl     : Boolean;
      Impl_Alias  : Alias_Type := No_Alias;
      Post_Action : Subprogram_Type;
   end record;
   package File_Template_Lists is new Ada.Containers.Doubly_Linked_Lists
     (File_Template_Type, "=");

   function Get_Create_Impl_Option
     (File_Template : File_Template_Type) return Alias_Option_Type;

   type Language_Assistants_Module_Id_Record is new Module_ID_Record
   with record
      File_Templates : File_Template_Lists.List;
      --  The list of all registered file templates
   end record;
   type Language_Assistants_Module_Id is
     access all Language_Assistants_Module_Id_Record'Class;
   --  Language assistants module type

   Language_Assistants_Module : Language_Assistants_Module_Id;

   type Create_File_From_Template_Command is new Interactive_Command
     with record
      File_Template : File_Template_Type;
   end record;
   overriding function Execute
     (Command : access Create_File_From_Template_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Command used to create a file from a given file template.
   --
   --  When executed, this command displays a dialog asking the user to enter
   --  the values needed to expand the file template's alias and creates a file
   --  with the expanded alias.

   type On_Open_File_From_Template is new File_Hooks_Function with record
      File    : Virtual_File;
      Project : Project_Type;
   end record;
   overriding procedure Execute
     (Self   : On_Open_File_From_Template;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Hook function used to indent the editors associated with files created
   --  by registered file templates.

   procedure Register_File_Template
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Alias_Name      : String;
      Label           : String;
      Unit_Param      : String;
      Language        : String;
      Is_Impl         : Boolean;
      Impl_Alias_Name : String := "";
      Post_Action     : Subprogram_Type := null;
      Error_Msg       : out Unbounded_String);
   --  Register a new file template.
   --
   --  Alias_Name is used to retrieve the alias to expand when creating a file
   --  from this template.
   --
   --  Label is used for displaying purpose (e.g: name of the action and
   --  contextual menu).
   --
   --  Unit_Param denotes the alias parameter that should be used to retrieve
   --  the unit name and deduce the base name of the file to create.
   --
   --  Language is used to retrieve the right naming scheme when creating the
   --  file.
   --
   --  Is_Impl is used to know which file extension we should use.
   --
   --  If non-null, the Post_Action subprogram will be called after creating
   --  the file(s) of the given template.
   --
   --  Error_Msg is set to a non-null string if errors are detected while
   --  trying to register the file template (e.g: when the alias does not
   --  exist).

   procedure File_Template_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Script handler for the FileTemplate class

   ----------------------------
   -- Get_Create_Impl_Option --
   ----------------------------

   function Get_Create_Impl_Option
     (File_Template : File_Template_Type) return Alias_Option_Type is
   begin
      if not File_Template.Is_Impl
        and then File_Template.Impl_Alias /= No_Alias
      then
         return Create
           (Label         => "Create also the corresponding implementation "
            & "file",
            Default_Value => Create_Impl_Pref.Get_Pref);
      else
         return No_Option;
      end if;
   end Get_Create_Impl_Option;

   ----------------------------
   -- Register_File_Template --
   ----------------------------

   procedure Register_File_Template
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Alias_Name      : String;
      Label           : String;
      Unit_Param      : String;
      Language        : String;
      Is_Impl         : Boolean;
      Impl_Alias_Name : String := "";
      Post_Action     : Subprogram_Type := null;
      Error_Msg       : out Unbounded_String)
   is
      Alias      : constant Alias_Type := Get_Alias (Alias_Name);
      Impl_Alias : constant Alias_Type := Get_Alias (Impl_Alias_Name);
   begin
      Error_Msg := Null_Unbounded_String;

      --  Check if the alias exists

      if Alias = No_Alias then
         Error_Msg := To_Unbounded_String
           ("No alias registered for the name: "
            & Alias_Name & ASCII.LF);
      end if;

      --  If the optional Impl_Alias_Name has been specified, check that it
      --  exists.

      if Impl_Alias_Name /= "" and then Impl_Alias = No_Alias then
         Error_Msg := Error_Msg
           & To_Unbounded_String
           ("No alias registered for the name: "
            & Impl_Alias_Name & ASCII.LF);
      end if;

      --  If Alias and the optional Impl_Alias_Name exist, check that they have
      --  then same parameters.

      if Impl_Alias /= No_Alias
        and then not Has_Same_Parameters (Alias, Impl_Alias)
      then
         Error_Msg := Error_Msg
           & To_Unbounded_String
           (Impl_Alias_Name
            & " should have the same parameters as "
            & Alias_Name & ASCII.LF);
      end if;

      if Error_Msg /= "" then
         return;
      end if;

      declare
         File_Template     : File_Template_Type;
         Command           : Interactive_Command_Access;
         Command_Name      : constant String := "new " & To_Lower (Label);
         File_View_Filter  : constant Action_Filter :=
                               Lookup_Filter (Kernel, "File_View")
                                 or Create (Module => Explorer_Module_Name);
         Language_Filter   : constant Action_Filter :=
                               Create (Language => Language);
         Dir_Filter        : constant Action_Filter :=
                               Lookup_Filter (Kernel, "Dir_Filter");
      begin
         --  Add the file template in the list

         File_Template := (Alias       => Alias,
                           Label       => To_Unbounded_String (Label),
                           Unit_Param  => To_Unbounded_String (Unit_Param),
                           Language    => To_Unbounded_String (Language),
                           Is_Impl     => Is_Impl,
                           Impl_Alias  => Impl_Alias,
                           Post_Action => Post_Action);
         Language_Assistants_Module.File_Templates.Append
           (File_Template);

         --  Create the command that creates a file from this file template and
         --  its corresponding contextual menu.

         Command := new Create_File_From_Template_Command'
           (Root_Command with File_Template => File_Template);
         Register_Action
           (Kernel,
            Name        => Command_Name,
            Command     => Command,
            Description => "Create " & Label,
            Filter      =>
              File_View_Filter and Dir_Filter and Language_Filter);
         Register_Contextual_Menu
           (Kernel,
            Action => Command_Name,
            Label  => "New/" & Label,
            Group  => File_Templates_Contextual_Group);
      end;
   end Register_File_Template;

   ---------------------------
   -- File_Template_Handler --
   ---------------------------

   procedure File_Template_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = "register" then
         declare
            Kernel          : constant Kernel_Handle := Get_Kernel (Data);
            Alias_Name      : constant String := Data.Nth_Arg (1);
            Label           : constant String := Data.Nth_Arg (2);
            Unit_Param      : constant String := Data.Nth_Arg (3);
            Language        : constant String := Data.Nth_Arg (4);
            Is_Impl         : constant Boolean := Data.Nth_Arg (5);
            Impl_Alias_Name : constant String := Data.Nth_Arg (6, "");
            Post_Action     : constant Subprogram_Type :=
                                Data.Nth_Arg (7, null);
            Error_Msg       : aliased Unbounded_String;
         begin
            Register_File_Template
              (Kernel          => Kernel,
               Alias_Name      => Alias_Name,
               Label           => Label,
               Unit_Param      => Unit_Param,
               Language        => Language,
               Is_Impl         => Is_Impl,
               Impl_Alias_Name => Impl_Alias_Name,
               Post_Action     => Post_Action,
               Error_Msg       => Error_Msg);

            if Error_Msg /= Null_Unbounded_String then
               Data.Set_Error_Msg (To_String (Error_Msg));
            end if;
         end;
      end if;
   end File_Template_Handler;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Create_File_From_Template_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is

      Kernel               : constant Kernel_Handle :=
                               Get_Kernel (Context.Context);
      Dir                  : constant Virtual_File :=
                               Directory_Information (Context.Context);
      Project              : constant Project_Type :=
                               Project_Information (Context.Context);
      Body_Ext             : constant String := +Project.File_From_Unit
        (Unit_Name       => "",
         Part            => Unit_Body,
         Language        => To_String (Command.File_Template.Language),
         File_Must_Exist => False);
      Spec_Ext             : constant String := +Project.File_From_Unit
        (Unit_Name       => "",
         Part            => Unit_Spec,
         Language        => To_String (Command.File_Template.Language),
         File_Must_Exist => False);

      function Avoid_Extension_Filter
        (Text      : String;
         Error_Msg : out Unbounded_String) return Boolean;
      --  Filter preventing users to specify filenames instead of unit names

      ----------------------------
      -- Avoid_Extension_Filter --
      ----------------------------

      function Avoid_Extension_Filter
        (Text      : String;
         Error_Msg : out Unbounded_String) return Boolean is
      begin
         if Index (Text, Body_Ext) /= 0
           or else Index (Text, Spec_Ext) /= 0
         then
            Error_Msg := To_Unbounded_String
              ("You should specify the unit name, not the filename (e.g: "
               & " for an Ada main, enter ""Main"" and not ""main.adb"".");

            return False;
         end if;

         return True;
      end Avoid_Extension_Filter;

      Must_Reindent        : Boolean;
      Params_Substitutions : Alias_Parameter_Substitution_Map.Map :=
                               Alias_Parameter_Substitution_Map.Empty_Map;
      Option               : aliased Alias_Option_Type :=
                               Get_Create_Impl_Option (Command.File_Template);
      Cursor               : Integer;
      Expanded_Text        : constant String := Expand_Alias
        (Alias                => Command.File_Template.Alias,
         Kernel               => Kernel,
         Cursor               => Cursor,
         Must_Reindent        => Must_Reindent,
         Params_Substitutions => Params_Substitutions,
         Dialog_Title         =>
           "Create " & To_String (Command.File_Template.Label),
         Option               => Option'Unchecked_Access,
         Filter               => Avoid_Extension_Filter'Unrestricted_Access);
      Unit_Name            : constant String :=
                               (if Expanded_Text /= "" then
                                   Params_Substitutions
                                  (To_String
                                     (Command.File_Template.Unit_Param))
                                else
                                   "");
      Impl_File            : Virtual_File := No_File with Unreferenced;
      New_File             : Virtual_File := No_File;

      function Get_Base_Name (Part : Unit_Parts) return String;
      --  Return the base name of the file to create

      procedure Get_Line_And_Column_From_Cursor
        (Text   : String;
         Cursor : Integer;
         Line   : out Integer;
         Column : out Integer);
      --  Get the line and column from Cursor.
      --  This is needed in order to place the cursor at the right position.

      function Create_File_From_Expanded_Template
        (Part   : Unit_Parts;
         Text   : String;
         Cursor : Integer) return Virtual_File;
      --  Create a file for the given the given expanded template Text.
      --  Part is used to know the file extension.
      --  Cursor is used to place the cursor when opening the new file.

      procedure Execute_Post_Action
        (Project : Project_Type;
         File    : Virtual_File);
      --  Execute the post action, if any

      -------------------
      -- Get_Base_Name --
      -------------------

      function Get_Base_Name (Part : Unit_Parts) return String is
      begin
         return +Project.File_From_Unit
           (Unit_Name       => Unit_Name,
            Part            => Part,
            Language        => To_String (Command.File_Template.Language),
            File_Must_Exist => False);
      end Get_Base_Name;

      -------------------------------------
      -- Get_Line_And_Column_From_Cursor --
      -------------------------------------

      procedure Get_Line_And_Column_From_Cursor
        (Text   : String;
         Cursor : Integer;
         Line   : out Integer;
         Column : out Integer)
      is
         Lines : constant Unbounded_String_Array := GNATCOLL.Utils.Split
           (Text (Text'First .. Cursor),
            On               => ASCII.LF,
            Omit_Empty_Lines => False);
      begin
         Line := Lines'Length;
         Column := Length (Lines (Lines'Last)) + 1;
      end Get_Line_And_Column_From_Cursor;

      ----------------------------------------
      -- Create_File_From_Expanded_Template --
      ----------------------------------------

      function Create_File_From_Expanded_Template
        (Part   : Unit_Parts;
         Text   : String;
         Cursor : Integer) return Virtual_File
      is
         File   : Virtual_File;
         W_File : Writable_File;
         Line   : Integer := 1;
         Column : Integer := 1;
      begin
         --  Create the file from its basename and the directory information

         File := Create_From_Dir (Dir, +Get_Base_Name (Part));
         W_File := GNATCOLL.VFS.Write_File (File);

         if W_File = Invalid_File then
            Kernel.Insert
              ("Cannot create file " & File.Display_Full_Name,
               Mode => Error);
            return No_File;
         end if;

         --  Write the template expanded text in it

         GNATCOLL.VFS.Write (W_File, Text);
         GNATCOLL.VFS.Close (W_File);

         File_Saved_Hook.Run (Kernel, File);

         --  Recompute the view

         if Project /= No_Project then
            Recompute_View (Kernel);
         end if;

         --  Open the created file and place the cursor at the right position

         Get_Line_And_Column_From_Cursor
           (Text   => Text,
            Cursor => Cursor,
            Line   => Line,
            Column => Column);

         --  If the expanded text should be reindented, add our hook function
         --  to the File_Edited_Hook to indent the editor when it gets opened.

         if Must_Reindent then
            File_Edited_Hook.Add
              (new On_Open_File_From_Template'(Hook_Function with
               File    => File,
               Project => Project));
         end if;

         --  Open an editor for the newly created file

         Open_File_Action_Hook.Run
           (Kernel,
            File    => File,
            Project => Project,
            Line    => Line,
            Column  => Visible_Column (Column));

         return File;
      end Create_File_From_Expanded_Template;

      -------------------------
      -- Execute_Post_Action --
      -------------------------

      procedure Execute_Post_Action
        (Project : Project_Type;
         File    : Virtual_File) is
      begin
         if Command.File_Template.Post_Action = null
           or else File = No_File
         then
            return;
         end if;

         declare
            Script : constant Scripting_Language :=
                       Command.File_Template.Post_Action.Get_Script;
            Data   : Callback_Data'Class := Script.Create (2);
            Result : Boolean;
         begin
            Data.Set_Nth_Arg (1, Create_Project (Script, Project));
            Data.Set_Nth_Arg (2, Create_File (Script, File));

            Result := Command.File_Template.Post_Action.Execute (Data);
            Free (Data);

            if not Result then
               Kernel.Insert
                 ("Failed to execute post action '"
                  & Command.File_Template.Post_Action.Get_Name
                  & "' of '"
                  & To_String (Command.File_Template.Label)
                  & "' file template.");
            end if;
         end;
      end Execute_Post_Action;

   begin
      if Expanded_Text = "" then
         return Success;
      end if;

      --  If the 'Create implementation' option is present and enabled,
      --  also create the corresponding implentation file (e.g: package body
      --  file for Ada).

      if Option /= No_Option then
         Set_Pref
           (Pref    => Create_Impl_Pref,
            Manager => Kernel.Get_Preferences,
            Value   => Is_Enabled (Option));

         if Is_Enabled (Option) then
            declare
               Impl_Cursor        : Integer;
               Impl_Expanded_Text : constant String :=
                                      Expand_Alias_With_Values
                 (Alias                => Command.File_Template.Impl_Alias,
                  Kernel               => Kernel,
                  Params_Substitutions => Params_Substitutions,
                  Cursor               => Impl_Cursor);
            begin
               Impl_File := Create_File_From_Expanded_Template
                 (Part   => Unit_Body,
                  Text   => Impl_Expanded_Text,
                  Cursor => Impl_Cursor);
            end;
         end if;
      end if;

      New_File := Create_File_From_Expanded_Template
        (Part   =>
           (if Command.File_Template.Is_Impl then
                 Unit_Body
            else
               Unit_Spec),
         Text   => Expanded_Text,
         Cursor => Cursor);

      --  Execute the post action, if any
      Execute_Post_Action
        (Project => Project,
         File    => New_File);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Open_File_From_Template;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      function If_Matches
        (F : not null access Hook_Function'Class) return Boolean;

      ----------------
      -- If_Matches --
      ----------------

      function If_Matches
        (F : not null access Hook_Function'Class) return Boolean is
      begin
         if F.all in On_Open_File_From_Template'Class then
            declare
               Func : constant On_Open_File_From_Template :=
                            On_Open_File_From_Template (F.all);
            begin
               if Func.File = Self.File then
                  Kernel.Insert ("Hook for " & File.Display_Full_Name
                                 & " has been removed !");
               end if;
               return Func.File = Self.File;
            end;
         end if;

         return False;
      end If_Matches;

   begin
      --  Return directly if the file associated with the newly opened editor
      --  does not match with the one created.

      if File /= Self.File then
         return;
      end if;

      --  Indent the editor and remove this hook function

      declare
         Editor : constant Editor_Buffer'Class :=
                    Kernel.Get_Buffer_Factory.Get
                      (File,
                       Open_Buffer => False,
                       Open_View   => False);
      begin
         Editor.Indent;

         File_Edited_Hook.Remove (If_Matches'Access);
      end;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File_Template_Class : constant Class_Type :=
                              New_Class
                                (Kernel,
                                 Name => File_Template_Class_Name);
   begin
      Language_Assistants_Module := new Language_Assistants_Module_Id_Record;
      Register_Module
        (Module      => Module_ID (Language_Assistants_Module),
         Kernel      => Kernel,
         Module_Name => "Language_Assistants",
         Priority    => Default_Priority);

      Kernel.Scripts.Register_Command
        (Command       => "register",
         Params        => (1 => Param ("alias_name"),
                           2 => Param ("label"),
                           3 => Param ("unit_param"),
                           4 => Param ("language"),
                           5 => Param ("is_impl"),
                           6 => Param ("impl_alias_name", Optional => True),
                           7 => Param ("post_action", Optional => True)),
         Handler       => File_Template_Handler'Access,
         Class         => File_Template_Class,
         Static_Method => True);

      Create_Impl_Pref := Create
        (Get_Preferences (Kernel),
         Name    => "Create-Impl-Template",
         Label   => "Incremental search",
         Path    => ":File Templates",
         Doc     =>
           "Create also the corresponding implementation file when creating "
         & "a new specification file from a template.",
         Default => False);
   end Register_Module;

end Language_Handlers.Assistants;
