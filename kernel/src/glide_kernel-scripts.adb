-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Glib.Object;          use Glib.Object;
with Glide_Intl;           use Glide_Intl;
with Gtk.Enums;            use Gtk.Enums;
with Gtkada.Dialogs;       use Gtkada.Dialogs;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Main_Window;    use Glide_Main_Window;
with Src_Info.Queries;     use Src_Info, Src_Info.Queries;
with String_Hash;
with System;               use System;
with String_Utils;         use String_Utils;
with Projects;             use Projects;
with Projects.Registry;    use Projects.Registry;
with Projects.Editor;      use Projects.Editor;
with Types;                use Types;
with Traces;               use Traces;
with VFS;                  use VFS;

package body Glide_Kernel.Scripts is

   Me : constant Debug_Handle := Create ("Glide_Kernel.Scripts");

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Class_Instance_Record'Class, Class_Instance);

   type Scripting_Language_Data;
   type Scripting_Language_List is access Scripting_Language_Data;
   type Scripting_Language_Data is record
      Script : Scripting_Language;
      Next   : Scripting_Language_List;
   end record;

   procedure Free (Class : in out Class_Type);
   package Classes_Hash is new String_Hash (Class_Type, Free, No_Class);
   use Classes_Hash.String_Hash_Table;

   type Scripting_Data_Record is new Kernel_Scripting_Data_Record with record
      Scripting_Languages : Scripting_Language_List;
      Classes             : Classes_Hash.String_Hash_Table.HTable;
      Entity_Class        : Class_Type := No_Class;
      File_Class          : Class_Type := No_Class;
      Project_Class       : Class_Type := No_Class;
      File_Context_Class  : Class_Type := No_Class;
      File_Location_Class : Class_Type := No_Class;
      Entity_Context_Class        : Class_Type := No_Class;
   end record;
   type Scripting_Data is access all Scripting_Data_Record'Class;

   type Entity_Information_Access is access Entity_Information;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Entity_Information_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Information, Entity_Information_Access);
   procedure On_Destroy_Entity (Value : System.Address);
   pragma Convention (C, On_Destroy_Entity);

   type File_Info_Access is access all File_Info;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, File_Info_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (File_Info, File_Info_Access);
   procedure On_Destroy_File (Value : System.Address);
   pragma Convention (C, On_Destroy_File);

   type File_Location_Info_Access is access all File_Location_Info;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, File_Location_Info_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (File_Location_Info, File_Location_Info_Access);
   procedure On_Destroy_File_Location (Value : System.Address);
   pragma Convention (C, On_Destroy_File_Location);

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Selection_Context_Access);
   procedure On_Destroy_Context (Value : System.Address);
   pragma Convention (C, On_Destroy_Context);

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the default commands

   procedure Create_Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "Entity" command

   procedure Create_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "File" command

   procedure Create_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "Project" command

   procedure Create_Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "Location" command

   procedure Context_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for all context-related commands

   procedure Entity_Context_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for all entity_context-related commands

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class; File : File_Info);
   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Project  : Projects.Project_Type);
   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Location : File_Location_Info);
   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Context  : Selection_Context_Access);
   --  Set the data for an instance

   procedure Free (File : in out File_Info);
   --  Free the contents of File_Info

   Name_Cst       : aliased constant String := "name";
   Filename_Cst   : aliased constant String := "filename";
   File_Cst       : aliased constant String := "file";
   Line_Cst       : aliased constant String := "line";
   Col_Cst        : aliased constant String := "column";
   Shared_Lib_Cst : aliased constant String := "shared_lib";
   Module_Cst     : aliased constant String := "module";
   Msg_Cst        : aliased constant String := "msg";
   Project_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
   Insmod_Cmd_Parameters  : constant Cst_Argument_List :=
     (1 => Shared_Lib_Cst'Access, 2 => Module_Cst'Access);
   Entity_Cmd_Parameters   : constant Cst_Argument_List :=
     (Name_Cst'Access, File_Cst'Access, Line_Cst'Access, Col_Cst'Access);
   File_Cmd_Parameters     : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
   Dialog_Cmd_Parameters   : constant Cst_Argument_List :=
     (1 => Msg_Cst'Access);
   Open_Cmd_Parameters     : constant Cst_Argument_List :=
     (1 => Filename_Cst'Access);
   Location_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Filename_Cst'Access,
      2 => Line_Cst'Access,
      3 => Col_Cst'Access);

   ----------
   -- Free --
   ----------

   procedure Free (Class : in out Class_Type) is
   begin
      Free (Class.Name);
   end Free;

   ---------------------------------
   -- Register_Scripting_Language --
   ---------------------------------

   procedure Register_Scripting_Language
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Script  : access Scripting_Language_Record'Class) is
   begin
      Scripting_Data (Kernel.Scripts).Scripting_Languages :=
        new Scripting_Language_Data'
          (Script => Scripting_Language (Script),
           Next   => Scripting_Data (Kernel.Scripts).Scripting_Languages);
   end Register_Scripting_Language;

   -------------------------------
   -- Lookup_Scripting_Language --
   -------------------------------

   function Lookup_Scripting_Language
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name    : String) return Scripting_Language
   is
      Tmp : Scripting_Language_List :=
        Scripting_Data (Kernel.Scripts).Scripting_Languages;
   begin
      while Tmp /= null loop
         if Case_Insensitive_Equal (Get_Name (Tmp.Script), Name) then
            return Tmp.Script;
         end if;

         Tmp := Tmp.Next;
      end loop;

      return null;
   end Lookup_Scripting_Language;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command       : String;
      Params        : String  := "";
      Return_Value  : String  := "";
      Description   : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False)
   is
      Tmp : Scripting_Language_List :=
        Scripting_Data (Kernel.Scripts).Scripting_Languages;
   begin
      Assert (Me,
              Command /= Constructor_Method or else Class /= No_Class,
              "Constructors can only be specified for classes");
      Assert (Me, not Static_Method or else Class /= No_Class,
              "Static method can only be created for classes");
      Assert (Me,
              Params = "" or else Params (Params'First) = '(',
              "Invalid usage string for "
              & Command & ": must start with '('");

      while Tmp /= null loop
         Register_Command
           (Tmp.Script, Command, Params, Return_Value, Description,
            Minimum_Args, Maximum_Args, Handler, Class, Static_Method);
         Tmp := Tmp.Next;
      end loop;
   end Register_Command;

   ---------------
   -- New_Class --
   ---------------

   function New_Class
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name          : String;
      Description   : String := "";
      Base          : Class_Type := No_Class) return Class_Type
   is
      Tmp   : Scripting_Language_List :=
        Scripting_Data (Kernel.Scripts).Scripting_Languages;
      Class : Class_Type;

   begin
      Class := Get (Scripting_Data (Kernel.Scripts).Classes, Name);

      if Class = No_Class then
         while Tmp /= null loop
            Register_Class (Tmp.Script, Name, Description, Base);
            Tmp := Tmp.Next;
         end loop;

         Class := Class_Type'(Name => new String'(Name));
         Set (Scripting_Data (Kernel.Scripts).Classes, Name, Class);
      end if;

      return Class;
   end New_Class;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Class : Class_Type) return String is
   begin
      if Class.Name = null then
         return "";
      else
         return Class.Name.all;
      end if;
   end Get_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Instance : access Class_Instance_Record'Class) is
      Ins : Class_Instance := Class_Instance (Instance);
   begin
      Primitive_Free (Instance.all);
      Unchecked_Free (Ins);
   end Free;

   -----------------------
   -- On_Destroy_Entity --
   -----------------------

   procedure On_Destroy_Entity (Value : System.Address) is
      Ent : Entity_Information_Access := Convert (Value);
   begin
      Destroy (Ent.all);
      Unchecked_Free (Ent);
   end On_Destroy_Entity;

   ----------
   -- Free --
   ----------

   procedure Free (File : in out File_Info) is
      pragma Unreferenced (File);
   begin
      null;
   end Free;

   ---------------------
   -- On_Destroy_File --
   ---------------------

   procedure On_Destroy_File (Value : System.Address) is
      File : File_Info_Access := Convert (Value);
   begin
      Free (File.all);
      Unchecked_Free (File);
   end On_Destroy_File;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Entity   : Entity_Information)
   is
      Ent : constant Entity_Information_Access :=
        new Entity_Information'(Copy (Entity));
      Script : constant Scripting_Language := Get_Script (Instance);
   begin
      if not Is_Subclass
        (Script, Get_Class (Instance), Get_Entity_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance,
         Value      => Ent.all'Address,
         On_Destroy => On_Destroy_Entity'Access);
   end Set_Data;

   ------------------------------
   -- On_Destroy_File_Location --
   ------------------------------

   procedure On_Destroy_File_Location (Value : System.Address) is
      File : File_Location_Info_Access := Convert (Value);
   begin
      Free (File.File);
      Unchecked_Free (File);
   end On_Destroy_File_Location;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Location : File_Location_Info)
   is
      Loc : constant File_Location_Info_Access :=
        new File_Location_Info'(Location);
      Script : constant Scripting_Language := Get_Script (Instance);
   begin
      if not Is_Subclass
        (Script, Get_Class (Instance),
         Get_File_Location_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance,
         Value      => Loc.all'Address,
         On_Destroy => On_Destroy_File_Location'Access);
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return Entity_Information
   is
      Script : constant Scripting_Language := Get_Script (Instance);
      Ent : Entity_Information_Access;
   begin
      if not Is_Subclass
        (Script, Get_Class (Instance), Get_Entity_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      Ent := Convert (Get_Data (Instance));
      return Ent.all;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return File_Location_Info
   is
      Script : constant Scripting_Language := Get_Script (Instance);
      Loc : File_Location_Info_Access;
   begin
      if not Is_Subclass
        (Script, Get_Class (Instance),
         Get_File_Location_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      Loc := Convert (Get_Data (Instance));
      return Loc.all;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      File     : File_Info)
   is
      Ent    : File_Info_Access;
      Script : constant Scripting_Language := Get_Script (Instance);
      Kernel : constant Kernel_Handle := Get_Kernel (Script);
   begin
      if not Is_Subclass
        (Script, Get_Class (Instance), Get_File_Class (Kernel))
      then
         raise Invalid_Data;
      end if;

      Ent      := new File_Info;
      Ent.File := Get_File (File);

      Set_Data
        (Instance,
         Value      => Ent.all'Address,
         On_Destroy => On_Destroy_File'Access);
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return File_Info
   is
      Ent : File_Info_Access;
      Script : constant Scripting_Language := Get_Script (Instance);
   begin
      if not Is_Subclass
        (Script, Get_Class (Instance), Get_File_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      Ent := Convert (Get_Data (Instance));
      return Ent.all;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Project  : Project_Type)
   is
      Script : constant Scripting_Language := Get_Script (Instance);
   begin
      if not Is_Subclass
        (Script, Get_Class (Instance), Get_Project_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      Set_Data (Instance, Value => Integer (Name_Id'(Project_Name (Project))));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return Project_Type
   is
      Script : constant Scripting_Language := Get_Script (Instance);
   begin
      if not Is_Subclass
        (Script, Get_Class (Instance), Get_Project_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      return Get_Project_From_Name
        (Project_Registry (Get_Registry (Get_Kernel (Script))),
         Name_Id (Integer'(Get_Data (Instance))));
   end Get_Data;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = "insmod" then
         Name_Parameters (Data, Insmod_Cmd_Parameters);
         declare
            Shared  : constant String := Nth_Arg (Data, 1);
            Module  : constant String := Nth_Arg (Data, 2);
            Success : Boolean;
         begin
            Dynamic_Register_Module (Kernel, Shared, Module, Success);
            if Success then
               Set_Return_Value (Data, -"Module successfully loaded.");
            else
               Set_Return_Value (Data, -"Couldn't load module.");
            end if;
         end;

      elsif Command = "lsmod" then
         declare
            use type Module_List.List_Node;
            Current : Module_List.List_Node;
            List    : constant Module_List.List := List_Of_Modules (Kernel);
         begin
            Current := Module_List.First (List);

            Set_Return_Value_As_List (Data);
            while Current /= Module_List.Null_Node loop
               Set_Return_Value
                 (Data,
                  Module_Name (Module_List.Data (Current)));
               Current := Module_List.Next (Current);
            end loop;
         end;

      elsif Command = "exit" then
         Quit (Glide_Window (Get_Main_Window (Kernel)));

      elsif Command = "dialog" then
         Name_Parameters (Data, Dialog_Cmd_Parameters);
         declare
            Result : Message_Dialog_Buttons;
            pragma Unreferenced (Result);
         begin
            Result := Message_Dialog
              (Msg     => Nth_Arg (Data, 1),
               Buttons => Button_OK,
               Justification => Justify_Left,
               Parent  => Get_Main_Window (Kernel));
         end;

      elsif Command = "yes_no_dialog" then
         Name_Parameters (Data, Dialog_Cmd_Parameters);
         Set_Return_Value
           (Data, Message_Dialog
            (Msg           => Nth_Arg (Data, 1),
             Buttons       => Button_Yes + Button_No,
             Justification => Justify_Left,
             Dialog_Type   => Confirmation,
             Parent        => Get_Main_Window (Kernel)) = Button_Yes);
      end if;
   end Default_Command_Handler;

   -------------------------------------
   -- Create_Location_Command_Handler --
   -------------------------------------

   procedure Create_Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_File_Location_Class (Kernel));
      Location : File_Location_Info;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Location_Cmd_Parameters);

         declare
            File   : constant Class_Instance  :=
              Nth_Arg (Data, 2, Get_File_Class (Kernel));
            L      : constant Integer := Nth_Arg (Data, 3);
            C      : constant Integer := Nth_Arg (Data, 4);
         begin
            Set_Data (Instance, File_Location_Info'(File, L, C));
         end;

      elsif Command = "line" then
         Location := Get_Data (Instance);
         Set_Return_Value (Data, Get_Line (Location));

      elsif Command = "column" then
         Location := Get_Data (Instance);
         Set_Return_Value (Data, Get_Column (Location));

      elsif Command = "file" then
         Location := Get_Data (Instance);
         Set_Return_Value (Data, Get_File (Location));
      end if;
   end Create_Location_Command_Handler;

   -----------------------------------
   -- Create_Entity_Command_Handler --
   -----------------------------------

   procedure Create_Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Entity : Entity_Information;
      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_Entity_Class (Kernel));
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Entity_Cmd_Parameters);

         declare
            Name   : constant String  := Nth_Arg (Data, 2);
            File   : constant Class_Instance  :=
              Nth_Arg (Data, 3, Get_File_Class (Kernel), Default => null,
                       Allow_Null => True);
            L      : constant Integer := Nth_Arg (Data, 4, Default => 1);
            C      : constant Integer := Nth_Arg (Data, 5, Default => 1);
            Status : Find_Decl_Or_Body_Query_Status;
            Lib_Info : LI_File_Ptr;
            F        : File_Info;
         begin
            if File = null then
               Entity := Create_Predefined_Entity
                 (Name, (Unresolved_Entity, False, True, False));
               Set_Data (Instance, Entity);
               Destroy (Entity);
               return;
            end if;

            F := Get_Data (File);
            Lib_Info := Locate_From_Source_And_Complete (Kernel, Get_File (F));
            if Lib_Info = No_LI_File then
               Set_Error_Msg
                 (Data, -"Xref information not found for: """
                  & Full_Name (Get_File (F)).all & '"');
               return;
            end if;

            Find_Declaration_Or_Overloaded
              (Kernel      => Kernel,
               Lib_Info    => Lib_Info,
               File_Name   => Get_File (F),
               Entity_Name => Name,
               Line        => L,
               Column      => C,
               Entity      => Entity,
               Status      => Status);

            if Status /= Success and then Status /= Fuzzy_Match then
               Set_Error_Msg (Data, -"Entity not found");
               Destroy (Entity);
            else
               Set_Data (Instance, Entity);
               Destroy (Entity);
            end if;
         end;

      elsif Command = "name" then
         Entity := Get_Data (Instance);
         Set_Return_Value (Data, Get_Name (Entity));

      elsif Command = "decl_file" then
         Entity := Get_Data (Instance);
         if not Is_Predefined_Entity (Entity) then
            Set_Return_Value
              (Data,
               Create_File
                 (Get_Script (Data), Get_Declaration_File_Of (Entity)));
         end if;

      elsif Command = "decl_line" then
         Entity := Get_Data (Instance);
         Set_Return_Value (Data, Get_Declaration_Line_Of (Entity));

      elsif Command = "decl_column" then
         Entity := Get_Data (Instance);
         Set_Return_Value (Data, Get_Declaration_Column_Of (Entity));

      elsif Command = "body" then
         declare
            Status : Find_Decl_Or_Body_Query_Status;
            Lib_Info : LI_File_Ptr;
            Location : File_Location;
         begin
            Entity := Get_Data (Instance);
            Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_Declaration_File_Of (Entity));
            Find_Next_Body
              (Kernel,
               Lib_Info    => Lib_Info,
               File_Name   => Get_Declaration_File_Of (Entity),
               Entity_Name => Get_Name (Entity),
               Line        => Get_Declaration_Line_Of (Entity),
               Column      => Get_Declaration_Column_Of (Entity),
               Location    => Location,
               Status      => Status);

            if Status = Success then
               Set_Return_Value
                 (Data, Create_File_Location
                    (Get_Script (Data),
                     File   => Create_File
                       (Get_Script (Data), Get_File (Location)),
                     Line   => Get_Line (Location),
                     Column => Get_Column (Location)));
            else
               Set_Error_Msg (Data, -"Body not found for the entity");
            end if;
         end;
      end if;
   end Create_Entity_Command_Handler;

   ---------------------------------
   -- Create_File_Command_Handler --
   ---------------------------------

   procedure Create_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_File_Class (Kernel));
      Info     : File_Info;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, File_Cmd_Parameters);
         Info := (File => Create (Full_Filename => Nth_Arg (Data, 2)));
         Set_Data (Instance, Info);
         Free (Info);

      elsif Command = "name" then
         Info     := Get_Data (Instance);
         Set_Return_Value (Data, Full_Name (Info.File).all);

      elsif Command = "project" then
         Info     := Get_Data (Instance);
         Set_Return_Value
           (Data, Create_Project
            (Get_Script (Data),
             Get_Project_From_File
             (Registry          => Project_Registry (Get_Registry (Kernel)),
              Source_Filename   => Info.File,
              Root_If_Not_Found => True)));

      elsif Command = "other_file" then
         Info     := Get_Data (Instance);
         Set_Return_Value
           (Data, Create_File
            (Get_Script (Data), Other_File_Name (Kernel, Info.File)));
      end if;
   end Create_File_Command_Handler;

   ------------------------------------
   -- Create_Project_Command_Handler --
   ------------------------------------

   procedure Create_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      Instance : Class_Instance;
      Project  : Project_Type;
   begin
      if Command = "load" then
         Name_Parameters (Data, Open_Cmd_Parameters);
         Load_Project (Kernel, Nth_Arg (Data, 1));
         Set_Return_Value
           (Data, Create_Project (Get_Script (Data), Get_Project (Kernel)));

      elsif Command = "root" then
         Set_Return_Value
           (Data, Create_Project (Get_Script (Data), Get_Project (Kernel)));

      else
         Instance := Nth_Arg (Data, 1, Get_Project_Class (Kernel));

         if Command = Constructor_Method then
            Name_Parameters (Data, Project_Cmd_Parameters);
            Project  := Get_Project_From_Name
              (Project_Registry (Get_Registry (Kernel)),
               Get_String (Nth_Arg (Data, 2)));

            if Project = No_Project then
               Set_Error_Msg (Data, -"No such project: " & Nth_Arg (Data, 2));
            else
               Set_Data (Instance, Project);
            end if;

         elsif Command = "name" then
            Project := Get_Data (Instance);
            Set_Return_Value (Data, Project_Name (Project));

         elsif Command = "ancestor_deps" then
            declare
               Iter : Imported_Project_Iterator;
               P    : Project_Type;
            begin
               Project := Get_Data (Instance);
               Set_Return_Value_As_List (Data);
               Iter := Find_All_Projects_Importing
                 (Get_Project (Kernel), Project, Include_Self => True);

               loop
                  P := Current (Iter);
                  exit when P = No_Project;
                  Set_Return_Value
                    (Data, Create_Project (Get_Script (Data), P));
                  Next (Iter);
               end loop;
            end;
         end if;
      end if;
   end Create_Project_Command_Handler;

   ------------------------------------
   -- Entity_Context_Command_Handler --
   ------------------------------------

   procedure Entity_Context_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      Instance : constant Class_Instance := Nth_Arg
        (Data, 1, Get_Entity_Context_Class (Kernel));
      Entity   : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access'(Get_Data (Instance));
      L, C     : Integer := -1;
   begin
      if Command = "location" then
         if Has_Line_Information (Entity) then
            L := Line_Information (Entity);
         end if;

         if Has_Column_Information (Entity) then
            C := Column_Information (Entity);
         end if;

         if Has_File_Information (Entity) then
            Set_Return_Value
              (Data,
               Create_File_Location
                 (Get_Script (Data),
                  (Create_File (Get_Script (Data), File_Information (Entity))),
                  L,
                  C));
         else
            Set_Error_Msg
              (Data, -"No file information stored in the context");
         end if;

      elsif Command = "entity" then
         Set_Return_Value
           (Data, Create_Entity (Get_Script (Data), Get_Entity (Entity)));
      end if;
   end Entity_Context_Command_Handler;

   -----------------------------
   -- Context_Command_Handler --
   -----------------------------

   procedure Context_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      Instance : Class_Instance;
      File     : File_Selection_Context_Access;
      Context  : Selection_Context_Access;
      L, C     : Integer := -1;
   begin
      if Command = Constructor_Method then
         Set_Error_Msg (Data, -"Cannot create an instance of this class");

      elsif Command = "file" then
         Instance := Nth_Arg (Data, 1, Get_File_Context_Class (Kernel));
         File := File_Selection_Context_Access'(Get_Data (Instance));
         if Has_File_Information (File) then
            Set_Return_Value
              (Data, Create_File (Get_Script (Data), File_Information (File)));
         else
            Set_Error_Msg (Data, -"No file information stored in the context");
         end if;

      elsif Command = "project" then
         Instance := Nth_Arg (Data, 1, Get_File_Context_Class (Kernel));
         File := File_Selection_Context_Access'(Get_Data (Instance));
         if Has_Project_Information (File) then
            Set_Return_Value
              (Data,
               Create_Project (Get_Script (Data), Project_Information (File)));
         else
            Set_Error_Msg (Data, -"No project stored in the context");
         end if;

      elsif Command = "location" then
         Instance := Nth_Arg
           (Data, 1, Get_File_Context_Class (Kernel));
         File := File_Selection_Context_Access'(Get_Data (Instance));

         if Has_Line_Information (File) then
            L := Line_Information (File);
         end if;

         if Has_Column_Information (File) then
            C := Column_Information (File);
         end if;

         if Has_File_Information (File) then
            Set_Return_Value
              (Data,
               Create_File_Location
                 (Get_Script (Data),
                  (Create_File (Get_Script (Data), File_Information (File))),
                  L,
                  C));
         else
            Set_Error_Msg
              (Data, -"No file information stored in the context");
         end if;

      elsif Command = "current_context" then
         Context := Get_Current_Context (Kernel);
         if Context = null then
            Set_Error_Msg (Data, -"There is no current context");

         elsif Context.all in Entity_Selection_Context'Class then
            Set_Return_Value
             (Data, Create_Entity_Context
               (Get_Script (Data), Entity_Selection_Context_Access (Context)));

         elsif Context.all in File_Selection_Context'Class then
            Set_Return_Value
             (Data, Create_File_Context
              (Get_Script (Data), File_Selection_Context_Access (Context)));

         else
            Set_Error_Msg (Data, -"Unknown current context");
         end if;
      end if;
   end Context_Command_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Kernel.Scripts := new Scripting_Data_Record;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scripting_Language_Data, Scripting_Language_List);

      List : Scripting_Language_List :=
        Scripting_Data (Kernel.Scripts).Scripting_Languages;
      Tmp  : Scripting_Language_List;
   begin
      while List /= null loop
         Tmp := List.Next;
         Destroy (List.Script);
         Unchecked_Free (List);

         List := Tmp;
      end loop;

      Free (Scripting_Data (Kernel.Scripts).Entity_Class.Name);
      Free (Scripting_Data (Kernel.Scripts).File_Class.Name);
      Free (Scripting_Data (Kernel.Scripts).Project_Class.Name);
      Free (Scripting_Data (Kernel.Scripts).File_Context_Class.Name);
      Free (Scripting_Data (Kernel.Scripts).File_Location_Class.Name);
      Free (Scripting_Data (Kernel.Scripts).Entity_Context_Class.Name);

      --  ??? the following code generates a SEGV (double deallocation, or
      --  deallocation of static memory ?)
      --  Classes_Hash.String_Hash_Table.Reset
      --    (Scripting_Data (Kernel.Scripts).Classes);
   end Finalize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Script : access Scripting_Language_Record) is
      pragma Unreferenced (Script);
   begin
      null;
   end Destroy;

   --------------------------------------
   -- Register_Default_Script_Commands --
   --------------------------------------

   procedure Register_Default_Script_Commands
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Command
        (Kernel,
         Command      => "insmod",
         Params       => Parameter_Names_To_Usage (Insmod_Cmd_Parameters),
         Description  => -"Dynamically register from shared-lib a new module.",
         Minimum_Args => 2,
         Maximum_Args => 2,
         Handler      => Default_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => "lsmod",
         Return_Value => "list of modules",
         Description  => -"List modules currently loaded.",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Default_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => "exit",
         Description  =>
           -("Exit GPS. If there are unsaved changes, a dialog is first"
             & " displayed to ask whether these should be saved. If the"
             & " user cancels the operation through the dialog, GPS will not"
             & " exit."),
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Default_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => "dialog",
         Params       => Parameter_Names_To_Usage (Dialog_Cmd_Parameters),
         Description  =>
           -("Display a modal dialog to report information to a user. This"
             & " blocks the interpreter until the dialog is closed."),
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "yes_no_dialog",
         Params       => Parameter_Names_To_Usage (Dialog_Cmd_Parameters),
         Return_Value => "boolean",
         Description  =>
           -("Display a modal dialog to ask a question to the user. This"
             & " blocks the interpreter until the dialog is closed. The"
             & " dialog has two buttons Yes and No, and the selected button"
             & " is returned to the caller"),
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Default_Command_Handler'Access);


      Register_Command
        (Kernel,
         Command      => Constructor_Method,
         Params       => Parameter_Names_To_Usage (File_Cmd_Parameters),
         Return_Value => "file",
         Description  => -"Create a new file, from its name.",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "name",
         Return_Value => "string",
         Description  => -"Return the name of the file",
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "other_file",
         Return_Value => "file",
         Description  =>
           -("Return the name of the other file semantically associated with"
             & " this one. In Ada this is the spec or body of the same package"
             & " depending on the type of this file."),
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "project",
         Return_Value => "project",
         Description  =>
           -("Return the project to which file belongs. If file is not one"
             & " of the souces of the project, the root project is returned"),
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => Constructor_Method,
         Params       => Parameter_Names_To_Usage (Entity_Cmd_Parameters, 2),
         Return_Value => "entity",
         Description  =>
           -("Create a new entity, from any of its references. File must be"
             & " an instance of the File method, or omitted for predefined"
             & " entities of the language"),
         Minimum_Args => 1,
         Maximum_Args => 4,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "name",
         Return_Value => "string",
         Description  => -"Return the name of the entity",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "decl_file",
         Return_Value => "GPS.File",
         Description  =>
           -("Return the file in which the entity is declared. This file's"
             & " name is empty for predefined entities"),
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "decl_line",
         Return_Value => "integer",
         Description  => -("Return the line in decl_file() at which the"
                           & " entity is defined"),
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "decl_column",
         Return_Value => "integer",
         Description  => -("Return the column in decl_file() at which the"
                           & " entity is defined"),
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "body",
         Return_Value => "FileLocation",
         Description  =>
           -("Return the location of the body for this entity. This is the"
             & " place where the subprogram is implemented, or where the full"
             & " definition of a type is visible. For types which do not have"
             & " the notion of body, this returns the location of the"
             & " declaration"),
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => Constructor_Method,
         Params       => Parameter_Names_To_Usage (Location_Cmd_Parameters),
         Return_Value => "location",
         Description  => -"Create a new file location, from its position.",
         Minimum_Args => 3,
         Maximum_Args => 3,
         Class        => Get_File_Location_Class (Kernel),
         Handler      => Create_Location_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command       => "line",
         Return_Value  => "integer",
         Description   => -"Return the line of the location",
         Class         => Get_File_Location_Class (Kernel),
         Handler       => Create_Location_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command       => "column",
         Return_Value  => "integer",
         Description   => -"Return the column of the location",
         Class         => Get_File_Location_Class (Kernel),
         Handler       => Create_Location_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command       => "file",
         Return_Value  => "File",
         Description   => -"Return the file of the location",
         Class         => Get_File_Location_Class (Kernel),
         Handler       => Create_Location_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => Constructor_Method,
         Params       => Parameter_Names_To_Usage (Project_Cmd_Parameters),
         Return_Value => "project",
         Description  =>
           -("Create a project handle, from its name. The project must have"
             & " been loaded already."),
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command       => "root",
         Return_Value  => "project",
         Description   =>
           -("Return the root project of the currently loaded hierarchy"),
         Class         => Get_Project_Class (Kernel),
         Static_Method => True,
         Handler       => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "load",
         Params       => Parameter_Names_To_Usage (Open_Cmd_Parameters),
         Return_Value => "project",
         Description  =>
           -("Load a new project, which replaces the current root project, and"
             & " return a handle to it. All imported projects are also"
             & " loaded at the same time. If the project is not found, a"
             & " default project is loaded"),
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Static_Method => True,
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "name",
         Return_Value => "string",
         Description  => -"Return the name of the project",
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "ancestor_deps",
         Return_Value => "list",
         Description  =>
           -("Return the list of projects that might contain sources that"
             & " depend on the project's sources. When doing extensive"
             & " searches it isn't worth checking other projects. Project"
             & " itself is included in the list."),
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => Constructor_Method,
         Return_Value => "FileContext",
         Description  => -"Prevents creation of FileContext instances",
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "file",
         Return_Value => "File",
         Description  => -"Return the name of the file in the context",
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "project",
         Return_Value => "Project",
         Description  =>
           -("Return the project in the context, or the root project if none"
             & " was specified in the context"),
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => Constructor_Method,
         Return_Value => "EntityContext",
         Description  => -"Prevents creation of EntityContext instances",
         Class        => Get_Entity_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "entity",
         Return_Value => "Entity",
         Description  => -"Return the entity stored in the context",
         Class        => Get_Entity_Context_Class (Kernel),
         Handler      => Entity_Context_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "location",
         Return_Value => "FileLocation",
         Description  => -"Return the file location stored in the context",
         Class        => Get_Entity_Context_Class (Kernel),
         Handler      => Entity_Context_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => "current_context",
         Return_Value => "FileContext",
         Description  =>
           -("Returns the current context in GPS. This is the currently"
             & " selected file, line, column, project,... depending on"
             & " what window is currently active"),
         Handler      => Context_Command_Handler'Access);
   end Register_Default_Script_Commands;

   ----------------------
   -- Get_Entity_Class --
   ----------------------

   function Get_Entity_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      if Scripting_Data (Kernel.Scripts).Entity_Class = No_Class then
         Scripting_Data (Kernel.Scripts).Entity_Class := New_Class
           (Kernel,
            "Entity", "Represents an entity from the source, based on the"
            & " location of its declaration");
      end if;
      return Scripting_Data (Kernel.Scripts).Entity_Class;
   end Get_Entity_Class;

   --------------------
   -- Get_File_Class --
   --------------------

   function Get_File_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      if Scripting_Data (Kernel.Scripts).File_Class = No_Class then
         Scripting_Data (Kernel.Scripts).File_Class := New_Class
           (Kernel,
            "File", "Represents a source file of your application");
      end if;
      return Scripting_Data (Kernel.Scripts).File_Class;
   end Get_File_Class;

   -----------------------
   -- Get_Project_Class --
   -----------------------

   function Get_Project_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      if Scripting_Data (Kernel.Scripts).Project_Class = No_Class then
         Scripting_Data (Kernel.Scripts).Project_Class := New_Class
           (Kernel, "Project", "Represents a project file");
      end if;
      return Scripting_Data (Kernel.Scripts).Project_Class;
   end Get_Project_Class;

   -----------------------------
   -- Get_File_Location_Class --
   -----------------------------

   function Get_File_Location_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      if Scripting_Data (Kernel.Scripts).File_Location_Class = No_Class then
         Scripting_Data (Kernel.Scripts).File_Location_Class := New_Class
           (Kernel, "FileLocation", "Represents a location in a file");
      end if;
      return Scripting_Data (Kernel.Scripts).File_Location_Class;
   end Get_File_Location_Class;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List := No_Args) return String is
   begin
      return Execute_Command
        (Lookup_Scripting_Language (Kernel, GPS_Shell_Name), Command, Args);
   end Execute_GPS_Shell_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   procedure Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List := No_Args)
   is
      Str : constant String := Execute_Command
        (Lookup_Scripting_Language (Kernel, GPS_Shell_Name), Command, Args);
      pragma Unreferenced (Str);
   begin
      null;
   end Execute_GPS_Shell_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script  : access Scripting_Language_Record;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String
   is
      Cmd : constant String := Command & ' '
        & Argument_List_To_String (Args);
   begin
      Execute_Command
        (Scripting_Language (Script), Cmd, Display_In_Console => False);
      return "";
   end Execute_Command;

   --------------
   -- Get_File --
   --------------

   function Get_File (File : File_Info) return Virtual_File is
   begin
      return File.File;
   end Get_File;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : String)
      return String is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Integer)
      return Integer is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Boolean)
      return Boolean is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data    : Callback_Data;
      N       : Positive;
      Default : System.Address)
      return System.Address is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data    : Callback_Data;
      N       : Positive;
      Class   : Class_Type;
      Default : Class_Instance;
      Allow_Null : Boolean := False)
      return Class_Instance is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N, Class, Allow_Null);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   ------------------------------
   -- Parameter_Names_To_Usage --
   ------------------------------

   function Parameter_Names_To_Usage
     (Parameters            : Cst_Argument_List;
      Optional_Params_Count : Natural := 0) return String
   is
      Length : Natural := 0;
   begin
      for P in Parameters'Range loop
         Length := Length + Parameters (P)'Length + 2;
      end loop;

      Length := Length + Optional_Params_Count * 2;

      declare
         Usage : String (1 .. Length);
         Index : Natural := Usage'First + 1;
      begin
         Usage (Usage'First) := '(';
         for P in Parameters'Range loop
            if Parameters'Last - P < Optional_Params_Count then
               Usage (Index) := '[';
               Index := Index + 1;
            end if;

            Usage (Index .. Index + Parameters (P)'Length - 1) :=
              Parameters (P).all;
            Index := Index + Parameters (P)'Length;

            if Parameters'Last - P < Optional_Params_Count then
               Usage (Index) := ']';
               Index := Index + 1;
            end if;

            if P /= Parameters'Last then
               Usage (Index .. Index + 1) := ", ";
               Index := Index + 2;
            end if;
         end loop;

         Usage (Index .. Usage'Last) := ")";
         return Usage;
      end;
   end Parameter_Names_To_Usage;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Data : Callback_Data)
      return Glide_Kernel.Kernel_Handle is
   begin
      return Get_Kernel (Get_Script (Callback_Data'Class (Data)));
   end Get_Kernel;

   -------------------
   -- Create_Entity --
   -------------------

   function Create_Entity
     (Script : access Scripting_Language_Record'Class;
      Entity : Src_Info.Queries.Entity_Information) return Class_Instance
   is
      Instance : constant Class_Instance := New_Instance
        (Script, Get_Entity_Class (Get_Kernel (Script)));
   begin
      Set_Data (Instance, Entity);
      return Instance;
   end Create_Entity;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Script : access Scripting_Language_Record'Class;
      File   : Virtual_File) return Class_Instance
   is
      Instance : constant Class_Instance := New_Instance
        (Script, Get_File_Class (Get_Kernel (Script)));
      Info     : File_Info := (File => File);
   begin
      Set_Data (Instance, Info);
      Free (Info);
      return Instance;
   end Create_File;

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project
     (Script  : access Scripting_Language_Record'Class;
      Project : Project_Type) return Class_Instance
   is
      Instance : Class_Instance := null;
   begin
      if Project /= No_Project then
         Instance := New_Instance
           (Script, Get_Project_Class (Get_Kernel (Script)));
         Set_Data (Instance, Project);
      end if;
      return Instance;
   end Create_Project;

   --------------------------
   -- Create_File_Location --
   --------------------------

   function Create_File_Location
     (Script : access Scripting_Language_Record'Class;
      File   : Class_Instance;
      Line   : Natural;
      Column : Natural) return Class_Instance
   is
      Instance : constant Class_Instance := New_Instance
        (Script, Get_File_Location_Class (Get_Kernel (Script)));
      Info : constant File_Location_Info := (File, Line, Column);
   begin
      Set_Data (Instance, Info);
      return Instance;
   end Create_File_Location;

   --------------
   -- Get_File --
   --------------

   function Get_File (Location : File_Location_Info) return Class_Instance is
   begin
      return Location.File;
   end Get_File;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Location : File_Location_Info) return Integer is
   begin
      return Location.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column (Location : File_Location_Info) return Integer is
   begin
      return Location.Column;
   end Get_Column;

   ----------------------------
   -- Get_File_Context_Class --
   ----------------------------

   function Get_File_Context_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      if Scripting_Data (Kernel.Scripts).File_Context_Class = No_Class then
         Scripting_Data (Kernel.Scripts).File_Context_Class := New_Class
           (Kernel,
            "FileContext",
            "Represents an context that contains file information");
      end if;
      return Scripting_Data (Kernel.Scripts).File_Context_Class;
   end Get_File_Context_Class;

   ------------------------------
   -- Get_Entity_Context_Class --
   ------------------------------

   function Get_Entity_Context_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      if Scripting_Data (Kernel.Scripts).Entity_Context_Class = No_Class then
         Scripting_Data (Kernel.Scripts).Entity_Context_Class := New_Class
           (Kernel,
            "EntityContext",
            "Represents an context that contains entity information",
            Base => Get_File_Context_Class (Kernel));
      end if;
      return Scripting_Data (Kernel.Scripts).Entity_Context_Class;
   end Get_Entity_Context_Class;

   ------------------------
   -- On_Destroy_Context --
   ------------------------

   procedure On_Destroy_Context (Value : System.Address) is
      C : Selection_Context_Access := Convert (Value);
   begin
      Unref (C);
   end On_Destroy_Context;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Context  : Selection_Context_Access)
   is
      Script : constant Scripting_Language := Get_Script (Instance);
   begin
      if not Is_Subclass
        (Script,
         Get_Class (Instance),
         Get_File_Context_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      Ref (Context);
      Set_Data
        (Instance,
         Value      => Context.all'Address,
         On_Destroy => On_Destroy_Context'Access);
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return Glide_Kernel.Modules.File_Selection_Context_Access
   is
      Script  : constant Scripting_Language := Get_Script (Instance);
   begin
      if not Is_Subclass
        (Script,
         Get_Class (Instance),
         Get_File_Context_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      return File_Selection_Context_Access
        (Selection_Context_Access'(Convert (Get_Data (Instance))));
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return Glide_Kernel.Modules.Entity_Selection_Context_Access
   is
      Script  : constant Scripting_Language := Get_Script (Instance);
   begin
      if not Is_Subclass
        (Script,
         Get_Class (Instance),
         Get_Entity_Context_Class (Get_Kernel (Script)))
      then
         raise Invalid_Data;
      end if;

      return Entity_Selection_Context_Access
        (Selection_Context_Access'(Convert (Get_Data (Instance))));
   end Get_Data;

   -------------------------
   -- Create_File_Context --
   -------------------------

   function Create_File_Context
     (Script  : access Scripting_Language_Record'Class;
      Context : Glide_Kernel.Modules.File_Selection_Context_Access)
      return Class_Instance
   is
      Instance : constant Class_Instance := New_Instance
        (Script, Get_File_Context_Class (Get_Kernel (Script)));
   begin
      Set_Data (Instance, Selection_Context_Access (Context));
      return Instance;
   end Create_File_Context;

   ---------------------------
   -- Create_Entity_Context --
   ---------------------------

   function Create_Entity_Context
     (Script  : access Scripting_Language_Record'Class;
      Context : Glide_Kernel.Modules.Entity_Selection_Context_Access)
      return Class_Instance
   is
      Instance : constant Class_Instance := New_Instance
        (Script, Get_Entity_Context_Class (Get_Kernel (Script)));
   begin
      Set_Data (Instance, Selection_Context_Access (Context));
      return Instance;
   end Create_Entity_Context;

end Glide_Kernel.Scripts;
