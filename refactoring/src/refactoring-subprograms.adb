-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
--                            AdaCore                                --
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

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Commands.Interactive;   use Commands, Commands.Interactive;
with Dynamic_Arrays;
with Entities.Queries;       use Entities, Entities.Queries;
with Glib;                   use Glib;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;     use GPS.Kernel.Scripts;
with GPS.Kernel;             use GPS.Kernel;
with Gtk.Box;                use Gtk.Box;
with Gtk.Check_Button;       use Gtk.Check_Button;
with Gtk.Dialog;             use Gtk.Dialog;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Label;              use Gtk.Label;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Widget;             use Gtk.Widget;
with Histories;              use Histories;
with Language;               use Language;
with Language_Handlers.GPS;  use Language_Handlers.GPS;
with Refactoring.Performers; use Refactoring.Performers;
with Traces;                 use Traces;
with VFS;                    use VFS;

package body Refactoring.Subprograms is

   Me : constant Debug_Handle := Create ("Refactor.Subprograms");

   type Extract_Method_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Extract_Method_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Extract Method" menu

   type Parameter_Description is record
      Parameter : Entity_Information;
      PType     : Parameter_Type;
   end record;

   package Parameter_Arrays is new Dynamic_Arrays (Parameter_Description);

   use Parameter_Arrays;
   use Entity_Information_Arrays;

   type Extract_Method_Options is record
      Use_In_Keyword    : Boolean;
      Use_Separate_Decl : Boolean;
   end record;
   --  The option to configure the output of the Extract Method refactoring

   procedure Generate_Extracted_Method
     (Kernel     : access Kernel_Handle_Record'Class;
      Name       : String;
      Params     : Parameter_Arrays.Instance;
      Local_Vars : Entity_Information_Arrays.Instance;
      File       : VFS.Virtual_File;
      Line_Start : Natural;
      Line_End   : Natural;
      Options    : Extract_Method_Options;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String);
   --  Generate the code of the new method

   function Extract_Method
     (Kernel              : access Kernel_Handle_Record'Class;
      File                : VFS.Virtual_File;
      Line_Start, Line_End : Integer;
      Method_Name         : String;
      Options             : Extract_Method_Options) return Command_Return_Type;
   --  Extract a method

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands

   procedure Insert_New_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      In_File     : VFS.Virtual_File;
      Before_Line : Integer;
      Options     : Extract_Method_Options;
      Method_Decl : String;
      Method_Body : String);
   --  Insert the new method decl and body in In_File, if possible before the
   --  line Before_Line.

   -------------------------------
   -- Generate_Extracted_Method --
   -------------------------------

   procedure Generate_Extracted_Method
     (Kernel     : access Kernel_Handle_Record'Class;
      Name       : String;
      Params     : Parameter_Arrays.Instance;
      Local_Vars : Entity_Information_Arrays.Instance;
      File       : VFS.Virtual_File;
      Line_Start : Natural;
      Line_End   : Natural;
      Options    : Extract_Method_Options;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String)
   is
      Out_Params_Count    : Natural := 0;
      In_Out_Params_Count : Natural := 0;
      In_Params_Count     : Natural := 0;
      Result, Decl     : Unbounded_String;
      Typ              : Entity_Information;
      Args             : Argument_List_Access;
      First_Out_Param  : Entity_Information;
   begin
      for P in Parameter_Arrays.First .. Last (Params) loop
         if Params.Table (P).PType = Out_Parameter then
            Out_Params_Count := Out_Params_Count + 1;
            First_Out_Param := Params.Table (P).Parameter;
         elsif Params.Table (P).PType = In_Out_Parameter then
            In_Out_Params_Count := In_Out_Params_Count + 1;
         else
            In_Params_Count := In_Params_Count + 1;
         end if;
      end loop;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Decl := To_Unbounded_String ("function ");
      else
         Decl := To_Unbounded_String ("procedure ");
      end if;

      Decl := Decl & Name;

      --  Do we have at least one parameter left ?

      if In_Params_Count + In_Out_Params_Count > 0
        or else Out_Params_Count > 1
      then
         Decl := Decl & ASCII.LF & "   (";
         for P in Parameter_Arrays.First .. Last (Params) loop

            --  Do not emit anything if there is a single out parameter, since
            --  we then use a function for the extracted method
            if Params.Table (P).PType /= Out_Parameter
              or else Out_Params_Count /= 1
              or else In_Out_Params_Count /= 0
            then
               Decl := Decl
                 & Get_Name (Params.Table (P).Parameter).all & " : ";
               Typ := Get_Type_Of (Params.Table (P).Parameter);
               if Typ = null then
                  Trace (Me, "Couldn't find name of parameter");
                  Method_Decl := Null_Unbounded_String;
                  Method_Body := Null_Unbounded_String;
                  return;
               end if;

               if Params.Table (P).PType = Out_Parameter then
                  Decl := Decl & "out ";
               elsif Params.Table (P).PType = In_Out_Parameter then
                  Decl := Decl & "in out ";
               elsif Options.Use_In_Keyword then
                  Decl := Decl & "in ";
               end if;

               Decl := Decl & Get_Name (Typ).all;
               if P /= Last (Params) then
                  Decl := Decl & ";" & ASCII.LF & "    ";
               end if;
            end if;
         end loop;
         Decl := Decl & ")";
      end if;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Typ := Get_Type_Of (First_Out_Param);
         Decl := Decl & " return " & Get_Name (Typ).all;
      end if;

      Result := Decl;
      Decl   := Decl & ";" & ASCII.LF;

      Result := Result & ASCII.LF & "is" & ASCII.LF;

      for L in Entity_Information_Arrays.First .. Last (Local_Vars) loop
         Result := Result
           & "   " & Get_Name (Local_Vars.Table (L)).all & " : ";
         Typ := Get_Type_Of (Local_Vars.Table (L));
         if Typ = null then
            Trace (Me, "Couldn't find name of local variable");
            Method_Decl := Null_Unbounded_String;
            Method_Body := Null_Unbounded_String;
            return;
         end if;

         Result := Result & Get_Name (Typ).all & ";" & ASCII.LF;
      end loop;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Typ := Get_Type_Of (First_Out_Param);
         Result := Result & "   " & Get_Name (First_Out_Param).all
           & " : " & Get_Name (Typ).all & ";" & ASCII.LF;
      end if;

      Result := Result & "begin" & ASCII.LF & "   ";

      for L in Line_Start .. Line_End loop
         Args := new Argument_List'
           (new String'(Full_Name (File).all),
            new String'(Integer'Image (L)),
            new String'(Integer'Image (1)));
         Result := Result
           & Execute_GPS_Shell_Command (Kernel, "Editor.get_chars", Args.all);
         Free (Args);
      end loop;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Result := Result & "   return "
           & Get_Name (First_Out_Param).all & ";" & ASCII.LF;
      end if;

      Result := Result & "end " & Name & ";" & ASCII.LF;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Method_Call := To_Unbounded_String
           (Get_Name (First_Out_Param).all & " := ");
      end if;

      Method_Call := Method_Call & Name;
      if In_Params_Count + In_Out_Params_Count > 0
        or else Out_Params_Count > 1
      then
         Method_Call := Method_Call & " (";

         for P in Parameter_Arrays.First .. Last (Params) loop
            if Params.Table (P).PType /= Out_Parameter
              or else Out_Params_Count /= 1
              or else In_Out_Params_Count /= 0
            then
               Method_Call := Method_Call
                 & Get_Name (Params.Table (P).Parameter).all;

               if P /= Last (Params) then
                  Method_Call := Method_Call & ", ";
               end if;
            end if;
         end loop;

         Method_Call := Method_Call & ")";
      end if;

      Method_Call := Method_Call & ";" & ASCII.LF;
      Method_Decl := Decl;
      Method_Body := Result;
   end Generate_Extracted_Method;

   -----------------------
   -- Insert_New_Method --
   -----------------------

   procedure Insert_New_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      In_File     : VFS.Virtual_File;
      Before_Line : Integer;
      Options     : Extract_Method_Options;
      Method_Decl : String;
      Method_Body : String)
   is
      Languages  : constant GPS_Language_Handler :=
        GPS_Language_Handler (Get_Language_Handler (Kernel));
      Handler    : constant LI_Handler :=
        Get_LI_Handler_From_File (Languages, In_File);
      Constructs : Construct_List;
      Line       : Integer := Before_Line;
      Decl_Line  : Integer := Integer'Last;
   begin
      Parse_File_Constructs (Handler, Languages, In_File, Constructs);
      Constructs.Current := Constructs.First;
      while Constructs.Current /= null loop
         if Constructs.Current.Category in Subprogram_Category then
            if Constructs.Current.Sloc_Start.Line < Decl_Line then
               Decl_Line := Constructs.Current.Sloc_Start.Line - 1;
            end if;

            if Constructs.Current.Sloc_Start.Line <= Line
              and then Constructs.Current.Sloc_End.Line > Line
            then
               Line := Constructs.Current.Sloc_Start.Line - 1;
            end if;
         end if;

         Constructs.Current := Constructs.Current.Next;
      end loop;

      Free (Constructs);

      --  Insert the body before the decl, so that if they are inserted at the
      --  same line, they occur with the decl first
      Insert_Text (Kernel, In_File, Line, 1, Method_Body, True);

      if Options.Use_Separate_Decl then
         Insert_Text (Kernel, In_File, Decl_Line, 1, Method_Decl, True);
      end if;
   end Insert_New_Method;

   --------------------
   -- Extract_Method --
   --------------------

   function Extract_Method
     (Kernel               : access Kernel_Handle_Record'Class;
      File                 : VFS.Virtual_File;
      Line_Start, Line_End : Integer;
      Method_Name          : String;
      Options              : Extract_Method_Options) return Command_Return_Type
   is
      Ref_Iter : Entity_Reference_Iterator;
      Iter     : Entity_Iterator;
      Entity, Caller   : Entity_Information;
      Ref      : Entity_Reference;
      Location : File_Location;
      Source   : constant Source_File := Get_Or_Create
        (Get_Database (Kernel), File);
      Is_Modified     : Boolean;
      Is_Read         : Boolean;
      Has_Ref_Before, Has_Ref_After : Boolean;
      Local_Vars      : Entity_Information_Arrays.Instance;
      Params          : Parameter_Arrays.Instance;
      Is_Global       : Boolean;
      Is_Param        : Boolean;
      Method_Decl, Method_Body, Method_Call : Unbounded_String;

   begin
      Find_All_Entities_In_File (Iter, Source);
      while not At_End (Iter) loop
         Entity := Get (Iter);

         Caller    := Get_Caller (Declaration_As_Reference (Entity));
         Is_Global := Caller = null
           or else not Is_Subprogram (Caller);

         if not Is_Global then
            Is_Modified     := False;
            Is_Read         := False;
            Has_Ref_Before  := False;
            Has_Ref_After   := False;
            Is_Param        := Is_Parameter (Entity);

            Find_All_References (Ref_Iter, Entity, In_File => Source);
            while not At_End (Ref_Iter) loop
               Ref := Get (Ref_Iter);
               Location := Get_Location (Ref);

               if Location.Line >= Line_Start
                 and then Location.Line <= Line_End
               then
                  if Is_Read_Reference (Get_Kind (Ref)) then
                     Is_Read := True;
                  end if;

                  if Is_Write_Reference (Get_Kind (Ref)) then
                     Is_Modified := True;
                  end if;
               elsif Get_Location (Ref) /= Get_Declaration_Of (Entity) then
                  if Location.Line < Line_Start then
                     Has_Ref_Before := True;
                  else
                     Has_Ref_After := True;
                  end if;
               end if;

               Next (Ref_Iter);
            end loop;
            Destroy (Ref_Iter);

            if not Has_Ref_Before then
               if Get_Initial_Value (Kernel, Entity) /= "" then
                  Has_Ref_Before := True;
               end if;
            end if;

            if not Is_Modified and then Is_Read then
               if Has_Ref_Before
                 or else Has_Ref_After
                 or else Is_Param
               then
                  Append (Params, (Parameter => Entity,
                                   PType     => In_Parameter));
               else
                  Append (Local_Vars, Entity);
               end if;
            elsif Is_Modified then
               if Has_Ref_Before then
                  if Has_Ref_After then
                     Append (Params, (Parameter => Entity,
                                      PType     => In_Out_Parameter));
                  else
                     Append (Params, (Parameter => Entity,
                                      PType     => In_Parameter));
                  end if;

               elsif Has_Ref_After then
                  Append (Params, (Parameter => Entity,
                                   PType     => Out_Parameter));
               else
                  Append (Local_Vars, Entity);
               end if;
            end if;
         end if;

         Next (Iter);
      end loop;
      Destroy (Iter);

      Generate_Extracted_Method
        (Kernel,
         Name        => Method_Name,
         Params      => Params,
         Local_Vars  => Local_Vars,
         File        => File,
         Line_Start  => Line_Start,
         Line_End    => Line_End,
         Options     => Options,
         Method_Decl => Method_Decl,
         Method_Body => Method_Body,
         Method_Call => Method_Call);

      Delete_Text
        (Kernel      => Kernel,
         In_File     => File,
         Line_Start  => Line_Start,
         Line_End    => Line_End);
      Insert_Text
        (Kernel     => Kernel,
         In_File    => File,
         Line       => Line_Start,
         Column     => 1,
         Text       => To_String (Method_Call),
         Indent     => True);

      Insert_New_Method
        (Kernel      => Kernel,
         In_File     => File,
         Before_Line => Line_Start,
         Options     => Options,
         Method_Decl => To_String (Method_Decl),
         Method_Body => To_String (Method_Body));

      return Success;
   end Extract_Method;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Extract_Method_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      File : constant VFS.Virtual_File := File_Information
        (File_Selection_Context_Access (Context.Context));
      Line_Start, Line_End : Integer;
      Options : Extract_Method_Options;

      Dialog : Gtk_Dialog;
      Ent    : Gtk_Entry;
      Button : Gtk_Widget;
      Label  : Gtk_Label;
      Check, Separate_Decl  : Gtk_Check_Button;

      Result : Command_Return_Type := Failure;
      pragma Unreferenced (Button);

   begin
      Gtk_New (Dialog,
               Title  => -"Extract Method",
               Parent => Get_Current_Window (Get_Kernel (Context.Context)),
               Flags  => Modal);
      Gtk_New (Label, -"Name of the new subprogram:");
      Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

      Gtk_New (Ent);
      Set_Text (Ent, -"New_Method");
      Select_Region (Ent, 0, -1);
      Set_Activates_Default (Ent, True);
      Pack_Start (Get_Vbox (Dialog), Ent, Expand => False);

      Gtk_New (Check, -"Use ""in"" keyword");
      Pack_Start (Get_Vbox (Dialog), Check, Expand => False);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Get_Kernel (Context.Context)).all,
         Key           => "Refactoring_Use_In_Keyword",
         Default_Value => False);
      Associate (Get_History (Get_Kernel (Context.Context)).all,
                "Refactoring_Use_In_Keyword", Check);

      Gtk_New (Separate_Decl, -"Create declaration for subprogram");
      Pack_Start (Get_Vbox (Dialog), Separate_Decl, Expand => False);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Get_Kernel (Context.Context)).all,
         Key           => "Refactoring_Create_Declaration",
         Default_Value => True);
      Associate (Get_History (Get_Kernel (Context.Context)).all,
                "Refactoring_Create_Declaration", Separate_Decl);

      Grab_Default (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Options := (Use_In_Keyword    => Get_Active (Check),
                     Use_Separate_Decl => Get_Active (Separate_Decl));

         Get_Area
           (File_Area_Context_Access (Context.Context), Line_Start, Line_End);
         Result := Extract_Method
           (Get_Kernel (Context.Context),
            File, Line_Start, Line_End, Get_Text (Ent), Options);
      end if;

      Destroy (Dialog);

      return Result;
   end Execute;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      File        : constant String  := Nth_Arg (Data, 1);
      Line_Start  : constant Integer := Nth_Arg (Data, 2);
      Line_End    : constant Integer := Nth_Arg (Data, 3);
      Method_Name : constant String  := Nth_Arg (Data, 4, "New_Method");
   begin
      if Extract_Method
        (Get_Kernel (Data), Create (File), Line_Start, Line_End, Method_Name,
         Options => (Use_In_Keyword => True, Use_Separate_Decl => True))
        /= Success
      then
         Set_Error_Msg (Data, "Couldn't extract method");
      end if;
   end Command_Handler;

   --------------------------
   -- Register_Refactoring --
   --------------------------

   procedure Register_Refactoring
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      C : constant Interactive_Command_Access := new Extract_Method_Command;
      Filter : Action_Filter;
   begin
      Filter := new Is_Area_Context;
      Register_Contextual_Menu
        (Kernel,
         Name  => "Extract Method",
         Label => "Refactoring/Extract method",
         Filter => Filter and Create (Module => "Source_Editor"),
         Action => C);

      if Active (Testsuite_Handle) then
         Register_Command
           (Kernel, "extract_method", 3, 4, Command_Handler'Access);
      end if;
   end Register_Refactoring;

end Refactoring.Subprograms;
