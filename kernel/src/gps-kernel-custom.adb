-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                              AdaCore                              --
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

with Ada.Exceptions;            use Ada.Exceptions;
with System.Assertions;         use System.Assertions;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Glib.Xml_Int;              use Glib.Xml_Int;
with Commands.Custom;           use Commands.Custom;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Traces;                    use Traces;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Intl;                  use GPS.Intl;
with File_Utils;                use File_Utils;
with Remote_Servers;            use Remote_Servers;
with String_Hash;
with VFS;                       use VFS;
with XML_Parsers;

package body GPS.Kernel.Custom is

   Me            : constant Debug_Handle := Create ("Kernel.Custom");
   XML_Extension : constant String := ".xml";

   type Startup_File_Description is record
      Initialization : Node_Ptr;
      Load           : Boolean;
      Explicit       : Boolean;
   end record;
   No_Startup_File : constant Startup_File_Description :=
     (null, False, False);

   procedure Free (File : in out Startup_File_Description);
   --  Free the memory occupied by File

   package Startup_Files_Hash is new String_Hash
     (Data_Type      => Startup_File_Description,
      Free_Data      => Free,
      Null_Ptr       => No_Startup_File,
      Case_Sensitive => Is_Case_Sensitive (Server => GPS_Server));
   use Startup_Files_Hash.String_Hash_Table;

   Startup_Files : Startup_Files_Hash.String_Hash_Table.HTable;

   procedure Parse_Custom_Dir
     (Kernel    : access Kernel_Handle_Record'Class;
      Directory : String;
      Level     : Customization_Level;
      Default_Autoload : Boolean);
   --  Parse and process all the XML files in the directory. Only those files
   --  that should be automatically loaded according to ~/.gps/startup.xml and
   --  Default_Autoload are loaded.

   -------------------------
   -- Autoload_System_Dir --
   -------------------------

   function Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return String is
   begin
      return Format_Pathname (Get_System_Dir (Kernel), UNIX)
        & "share/gps/plug-ins/";
   end Autoload_System_Dir;

   ----------------------------
   -- No_Autoload_System_Dir --
   ----------------------------

   function No_Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return String is
   begin
      return Format_Pathname (Get_System_Dir (Kernel), UNIX)
        & "share/gps/library/";
   end No_Autoload_System_Dir;

   -----------------------
   -- Autoload_User_Dir --
   -----------------------

   function Autoload_User_Dir
     (Kernel : access Kernel_Handle_Record'Class) return String
   is
      Dir : constant String :=
        Format_Pathname (Get_Home_Dir (Kernel), UNIX) & "plug-ins/";
   begin
      if not Is_Directory (Dir) then
         Make_Dir (Dir);
      end if;

      return Dir;
   end Autoload_User_Dir;

   ---------------------
   -- Get_Custom_Path --
   ---------------------

   function Get_Custom_Path return String is
      Env : String_Access := Getenv ("GPS_CUSTOM_PATH");
      Result : constant String := Env.all;
   begin
      Free (Env);
      return Result;
   end Get_Custom_Path;

   ----------------------------------
   -- Execute_Customization_String --
   ----------------------------------

   procedure Execute_Customization_String
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      use type Module_List.List_Node;
      List    : constant Module_List.List := List_Of_Modules (Kernel);
      Current : Module_List.List_Node;
      Tmp     : Node_Ptr := Node;
      Tmp2    : Node_Ptr;

   begin
      --  Parse the nodes in the XML file one after the other, and for each
      --  traverse the list of modules for it.
      --  It is less efficient than passing the whole file to each module, but
      --  is necessary to properly handle themes:
      --  If we have    <action>...</action>
      --                <theme> ... ref to the action ... </theme>
      --  we need to be sure that the action has been created before processing
      --  the theme itself, and that would depend on the order in which the
      --  modules were registered (DA15-003)

      while Tmp /= null loop
         Tmp2 := Tmp.Next;
         Tmp.Next := null;

         Current := Module_List.First (List);
         while Current /= Module_List.Null_Node loop
            Customize (Module_List.Data (Current), File, Tmp, Level);
            Current := Module_List.Next (Current);
         end loop;

         Tmp.Next := Tmp2;
         Tmp := Tmp.Next;
      end loop;
   end Execute_Customization_String;

   ----------------------
   -- Parse_Custom_Dir --
   ----------------------

   procedure Parse_Custom_Dir
     (Kernel    : access Kernel_Handle_Record'Class;
      Directory : String;
      Level     : Customization_Level;
      Default_Autoload : Boolean)
   is
      Norm_Dir  : constant String := Name_As_Directory (Directory);
      File      : String (1 .. 1024);
      Last      : Natural;
      Dir       : Dir_Type;
      File_Node : Node_Ptr;
      Command   : Custom_Command_Access;

   begin
      if Is_Directory (Norm_Dir) then
         Open (Dir, Directory);
         loop
            Read (Dir, File, Last);
            exit when Last = 0;

            declare
               F     : constant Virtual_File :=
                 Create (Full_Filename => Norm_Dir & File (1 .. Last));
               Error : String_Access;
            begin
               if File_Extension (F) = XML_Extension
                 and then Is_Regular_File (F)
               then
                  if Load_File_At_Startup
                    (Kernel, F, Default => Default_Autoload)
                  then
                     Trace (Me, "Loading " & Full_Name (F).all);

                     XML_Parsers.Parse (Full_Name (F).all, File_Node, Error);

                     if File_Node = null then
                        Trace (Me, "Could not parse XML file: "
                               & Full_Name (F).all);
                        Insert (Kernel, Error.all,
                                Mode => GPS.Kernel.Console.Error);
                        Free (Error);
                     else
                        Execute_Customization_String
                          (Kernel, F, File_Node.Child, Level);
                        Free (File_Node);

                        Command := Initialization_Command (Kernel, F);
                        if Command /= null then
                           while Execute (Command, Null_Context) =
                             Execute_Again
                           loop
                              null;
                           end loop;
                           Destroy (Command_Access (Command));
                        end if;
                     end if;
                  end if;
               end if;

            exception
               when Assert_Failure =>
                  Console.Insert
                    (Kernel, -"Could not parse custom file "
                     & Full_Name (F).all,
                     Mode => GPS.Kernel.Console.Error);
            end;
         end loop;

         Close (Dir);
      end if;

   exception
      when Directory_Error =>
         null;
   end Parse_Custom_Dir;

   ---------------------------
   -- Load_All_Custom_Files --
   ---------------------------

   procedure Load_All_Custom_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Env_Path : constant String := Get_Custom_Path;
      Path     : Path_Iterator;
      N        : Node_Ptr;

   begin
      Kernel.Custom_Files_Loaded := True;

      --  Load the hard-coded customization strings first, so that anyone
      --  can override them

      if Kernel.Customization_Strings /= null then
         Trace (Me, "Executing customization strings previously registered");
         Execute_Customization_String
           (Kernel,
            No_File,
            Kernel.Customization_Strings,
            Hard_Coded);

         --  Can't call Free itself, since it doesn't free the siblings
         while Kernel.Customization_Strings /= null loop
            N := Kernel.Customization_Strings;
            Kernel.Customization_Strings := Kernel.Customization_Strings.Next;
            Free (N);
         end loop;
      end if;

      --  Load the system plug-ins directory first, so that its contents can
      --  be overriden locally by the user
      Parse_Custom_Dir
        (Kernel, Autoload_System_Dir (Kernel), System_Wide,
         Default_Autoload => True);
      Parse_Custom_Dir
        (Kernel, No_Autoload_System_Dir (Kernel), System_Wide,
         Default_Autoload => False);

      Path := Start (Env_Path);
      while not At_End (Env_Path, Path) loop
         if Current (Env_Path, Path) /= "" then
            Parse_Custom_Dir
              (Kernel, Current (Env_Path, Path), Project_Wide,
               Default_Autoload => True);
         end if;
         Path := Next (Env_Path, Path);
      end loop;

      Parse_Custom_Dir
        (Kernel, Autoload_User_Dir (Kernel), User_Specific,
         Default_Autoload => True);
   end Load_All_Custom_Files;

   ------------------------------
   -- Add_Customization_String --
   ------------------------------

   function Add_Customization_String
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Customization : UTF8_String;
      From_File     : String;
      Start_Line    : Positive := 1) return String
   is
      --  Add a valid prefix and toplevel node, since the string won't
      --  contain any.

      Node : Node_Ptr;
      N    : Node_Ptr;
      Err  : String_Access;

   begin
      --  Don't do this at declaration time, since we want to catch exceptions

      --  If the string appears to be a complete file, accept it as is
      if Customization'Length > 5
        and then Customization (Customization'First .. Customization'First + 4)
          = "<?xml"
      then
         XML_Parsers.Parse_Buffer
           (Buffer     => Customization,
            From_File  => From_File,
            Start_Line => Start_Line,
            Tree       => Node,
            Error      => Err);

      else
         --  else enclose it
         XML_Parsers.Parse_Buffer
           ("<?xml version=""1.0""?><GPS>" & Customization & "</GPS>",
            From_File  => From_File,
            Start_Line => Start_Line,
            Tree       => Node,
            Error      => Err);
      end if;

      --  If the custom files have already been loaded, this means that all
      --  modules have been registered and are read to listen to events. We
      --  can then inform them all. Otherwise, the need to append them to the
      --  list which will be executed later when all modules have been
      --  registered

      if Node /= null then
         if Kernel.Custom_Files_Loaded then
            begin
               Execute_Customization_String
                 (Kernel, Create (From_File), Node.Child, Hard_Coded);
               Free (Node);
            exception
               when others =>
                  return "Error while executing parse_xml()";
            end;

         else
            N := Kernel.Customization_Strings;
            if N = null then
               Kernel.Customization_Strings := Node.Child;

            else
               while N.Next /= null loop
                  N := N.Next;
               end loop;

               N.Next := Node.Child;
            end if;

            N := Node.Child;
            while N /= null loop
               N.Parent := null;
               N := N.Next;
            end loop;

            Node.Child := null;
            Free (Node);
         end if;
         return "";

      else
         --  Insert (Kernel, Err.all, Mode => Error);
         declare
            E : constant String := Err.all;
         begin
            Free (Err);
            return E;
         end;
      end if;

   exception
      when E : others =>
         --  This is purely internal error for programmers, no need for console

         Trace (Me, "Could not parse custom string " & Customization
                & ' ' & Exception_Message (E));
         return "Internal error";
   end Add_Customization_String;

   -----------------------------------
   -- Parse_List_Of_Startup_Scripts --
   -----------------------------------

   procedure Parse_List_Of_Startup_Scripts
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Err  : String_Access;
      Node, N : Node_Ptr;
   begin
      XML_Parsers.Parse
        (File => Format_Pathname (Get_Home_Dir (Kernel), UNIX) & "startup.xml",
         Tree => Node,
         Error => Err);

      if Node = null then
         Trace (Me, "Error while loading startup.xml: " & Err.all);
         Free (Err);

      else
         N := Node.Child;
         while N /= null loop
            if N.Tag.all = "startup" then
               begin
                  Set
                    (Startup_Files,
                     K => Get_Attribute (N, "file"),
                     E =>
                       (Initialization => N.Child,
                        Explicit => True,
                        Load    => Boolean'Value (Get_Attribute (N, "load"))));
                  --  So that when we free the tree this is kept
                  N.Child := null;
               exception
                  when Constraint_Error =>
                     null;
               end;
            end if;
            N := N.Next;
         end loop;
         Free (Node);
      end if;
   end Parse_List_Of_Startup_Scripts;

   --------------------------
   -- Load_File_At_Startup --
   --------------------------

   function Load_File_At_Startup
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : VFS.Virtual_File;
      Default : Boolean) return Boolean
   is
      pragma Unreferenced (Kernel);
      Startup : constant Startup_File_Description :=
        Get (Startup_Files, K => Base_Name (File));
   begin
      if Startup.Explicit then
         return Startup.Load;
      else
         return Default;
      end if;
   end Load_File_At_Startup;

   ----------------------------
   -- Initialization_Command --
   ----------------------------

   function Initialization_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : VFS.Virtual_File)
      return Commands.Custom.Custom_Command_Access
   is
      Custom : Custom_Command_Access;
      Startup : constant Startup_File_Description :=
        Get (Startup_Files, K => Base_Name (File));
   begin
      if Startup.Initialization /= null then
         Custom := new Custom_Command;
         Create
           (Item                 => Custom,
            Name                 => "Initialize " & Full_Name (File).all,
            Kernel               => Kernel_Handle (Kernel),
            Command              => Startup.Initialization,
            Show_Command         => False,
            Show_In_Task_Manager => False);
         return Custom;
      else
         return null;
      end if;
   end Initialization_Command;

   ----------
   -- Free --
   ----------

   procedure Free (File : in out Startup_File_Description) is
   begin
      Free (File.Initialization);
   end Free;

end GPS.Kernel.Custom;
