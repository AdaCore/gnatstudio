------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with System.Assertions;         use System.Assertions;

with GNAT.OS_Lib;               use GNAT.OS_Lib;

with XML_Utils;                 use XML_Utils;
with Commands.Custom;           use Commands.Custom;
with Traces;                    use Traces;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Intl;                  use GPS.Intl;
with String_Hash;

with GNATCOLL.VFS;              use GNATCOLL.VFS;

with XML_Parsers;

package body GPS.Kernel.Custom is

   Me            : constant Debug_Handle := Create ("Kernel.Custom");
   XML_Extension : constant Filesystem_String := ".xml";

   use Scripts_Hash.String_Hash_Table;

   type Scripts_Htable_Record is new Root_Table with record
      Table : Scripts_Hash.String_Hash_Table.Instance;
   end record;
   type Scripts_Htable_Access is access all Scripts_Htable_Record'Class;

   overriding procedure Reset (Table : access Scripts_Htable_Record);
   --  Reset the table

   procedure Parse_Custom_Dir
     (Kernel           : access Kernel_Handle_Record'Class;
      Directory        : Virtual_File;
      Level            : Customization_Level;
      Default_Autoload : Boolean);
   --  Parse and process all the XML files in the directory. Only those files
   --  that should be automatically loaded according to ~/.gps/startup.xml and
   --  Default_Autoload are loaded.

   -------------------------
   -- Autoload_System_Dir --
   -------------------------

   function Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File is
   begin
      return Create_From_Dir (Get_System_Dir (Kernel), "share/gps/plug-ins");
   end Autoload_System_Dir;

   ----------------------------
   -- No_Autoload_System_Dir --
   ----------------------------

   function No_Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File is
   begin
      return Create_From_Dir (Get_System_Dir (Kernel), "share/gps/library");
   end No_Autoload_System_Dir;

   -----------------------
   -- Autoload_User_Dir --
   -----------------------

   function Autoload_User_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File
   is
      Dir : constant Virtual_File :=
              Create_From_Dir (Get_Home_Dir (Kernel), "plug-ins");
   begin
      if not Is_Directory (Dir) then
         Make_Dir (Dir);
      end if;

      return Dir;
   end Autoload_User_Dir;

   ---------------------
   -- Get_Custom_Path --
   ---------------------

   function Get_Custom_Path return File_Array is
      Env : String_Access := Getenv ("GPS_CUSTOM_PATH");
      Result : constant Filesystem_String := +Env.all;
   begin
      Free (Env);

      return From_Path (Result);
   end Get_Custom_Path;

   ----------------------------------
   -- Execute_Customization_String --
   ----------------------------------

   procedure Execute_Customization_String
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
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
      Directory : Virtual_File;
      Level     : Customization_Level;
      Default_Autoload : Boolean)
   is
      Files : File_Array_Access;
      File_Node : Node_Ptr;
      Command   : Custom_Command_Access;

   begin
      if Is_Directory (Directory) then
         Files := Read_Dir (Directory, Files_Only);

         for J in Files'Range loop
            declare
               F     : Virtual_File renames Files (J);
               Error : String_Access;
            begin
               Trace (Me, "Should we load " & F.Display_Full_Name);
               if File_Extension (F) = XML_Extension
                 and then Is_Regular_File (F)
               then
                  if Load_File_At_Startup
                    (Kernel, F, Default => Default_Autoload)
                  then
                     Trace (Me, "Loading " & Display_Full_Name (F));

                     XML_Parsers.Parse (F, File_Node, Error);

                     if File_Node = null then
                        Trace (Me, "Could not parse XML file: "
                               & Display_Full_Name (F));
                        Insert (Kernel, Error.all,
                                Mode => GPS.Kernel.Console.Error);
                        Free (Error);
                     else
                        Execute_Customization_String
                          (Kernel, F, File_Node.Child, Level);
                        Free (File_Node);

                        Command := Initialization_Command (Kernel, F);
                        if Command /= null then
                           Launch_Background_Command
                             (Kernel,
                              Command    => Command,
                              Active     => True,
                              Show_Bar   => False,
                              Block_Exit => False);
                        end if;
                     end if;
                  end if;
               end if;

            exception
               when Assert_Failure =>
                  Console.Insert
                    (Kernel, -"Could not parse custom file "
                     & Display_Full_Name (F),
                     Mode => GPS.Kernel.Console.Error);
            end;
         end loop;

         Unchecked_Free (Files);
      end if;

   exception
      when VFS_Directory_Error =>
         null;
   end Parse_Custom_Dir;

   ------------------------------
   -- Load_System_Custom_Files --
   ------------------------------

   procedure Load_System_Custom_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Env_Path : constant File_Array := Get_Custom_Path;
      N        : Node_Ptr;

   begin
      Kernel.Custom_Files_Loaded := System_Level;

      --  Load the hard-coded customization strings first, so that anyone
      --  can override them.

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

      for J in Env_Path'Range loop
         if Env_Path (J) /= No_File then
            Trace (Me, "Loading XML file from "
                & Env_Path (J).Display_Full_Name);
            Parse_Custom_Dir
              (Kernel, Env_Path (J), Project_Wide,
               Default_Autoload => True);
         end if;
      end loop;
   end Load_System_Custom_Files;

   ----------------------------
   -- Load_User_Custom_Files --
   ----------------------------

   procedure Load_User_Custom_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      N : Node_Ptr;
   begin
      Kernel.Custom_Files_Loaded := User_Level;

      --  Load the hard-coded customization strings first, so that anyone
      --  can override them.

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

      Parse_Custom_Dir
        (Kernel, Autoload_User_Dir (Kernel), User_Specific,
         Default_Autoload => True);
   end Load_User_Custom_Files;

   ------------------------------
   -- Add_Customization_String --
   ------------------------------

   function Add_Customization_String
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Customization : XML_Utils.UTF8_String;
      From_File     : Filesystem_String;
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
      --  modules have been registered and are ready to listen to events. We
      --  can then inform them all. Otherwise, we need to append them to the
      --  list which will be executed later when all modules have been
      --  registered.

      if Node /= null then
         if Kernel.Custom_Files_Loaded = User_Level then
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

   --------------------------------
   -- Parse_Startup_Scripts_List --
   --------------------------------

   procedure Parse_Startup_Scripts_List
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Startup : constant Virtual_File :=
                  Create_From_Dir (Get_Home_Dir (Kernel), "startup.xml");
      Err     : String_Access;
      Node, N : Node_Ptr;
   begin
      Kernel.Startup_Scripts := new Scripts_Htable_Record;

      if Is_Regular_File (Startup) then
         XML_Parsers.Parse
           (File  => Startup,
            Tree  => Node,
            Error => Err);

         if Node = null then
            Trace (Me, "Error while loading startup.xml: " & Err.all);
            Console.Insert
              (Kernel,
               "Could not parse startup.xml: " & Err.all,
               Mode => Error);
            Free (Err);

         else
            N := Node.Child;
            while N /= null loop
               if N.Tag.all = "startup" then
                  begin
                     Set
                       (Scripts_Htable_Access
                          (Kernel.Startup_Scripts).Table,
                        K => Get_Attribute (N, "file"),
                        E => new Script_Description'
                          (Initialization => Deep_Copy (N.Child),
                           Explicit => True,
                           Load => Boolean'Value (Get_Attribute (N, "load")),
                           File => GNATCOLL.VFS.No_File));
                  exception
                     when Constraint_Error =>
                        null;
                  end;
               end if;
               N := N.Next;
            end loop;
            Free (Node);
         end if;
      else
         Trace (Me, "File not found: " & Startup.Display_Full_Name);
      end if;
   end Parse_Startup_Scripts_List;

   -------------------------------
   -- Save_Startup_Scripts_List --
   -------------------------------

   procedure Save_Startup_Scripts_List
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Startup : constant Virtual_File :=
                  Create_From_Dir (Get_Home_Dir (Kernel), "startup.xml");
      File, Child : Node_Ptr;
      Iter        : Scripts_Hash.String_Hash_Table.Cursor;
      Script      : Script_Description_Access;
      Success     : Boolean;

   begin
      File     := new Node;
      File.Tag := new String'("GPS");

      Get_First (Scripts_Htable_Access (Kernel.Startup_Scripts).Table, Iter);
      loop
         Script := Get_Element (Iter);
         exit when Script = null;

         if Script.Explicit then
            Child := new Node;
            Child.Tag := new String'("startup");
            Set_Attribute (Child, "load", Boolean'Image (Script.Load));
            Set_Attribute (Child, "file", Get_Key (Iter));
            if Script.Initialization /= null then
               Add_Child (Child, Deep_Copy (Script.Initialization),
                          Append => True);
               --  Append in case Initialization has several siblings
            end if;
            Add_Child (File, Child);
         end if;

         Get_Next (Scripts_Htable_Access (Kernel.Startup_Scripts).Table, Iter);
      end loop;

      Trace (Me, "Saving " & Startup.Display_Full_Name);
      Print (File, Startup, Success);
      Free (File);

      if not Success then
         Report_Preference_File_Error (Kernel, Startup);
      end if;
   end Save_Startup_Scripts_List;

   ------------------------------
   -- Get_First_Startup_Script --
   ------------------------------

   procedure Get_First_Startup_Script
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : out Script_Iterator) is
   begin
      Get_First (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                 Iter.Iter);
      Iter.Kernel := Kernel_Handle (Kernel);
   end Get_First_Startup_Script;

   ----------
   -- Next --
   ----------

   procedure Next  (Iter : in out Script_Iterator) is
   begin
      Get_Next (Scripts_Htable_Access (Iter.Kernel.Startup_Scripts).Table,
                Iter.Iter);
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Script_Iterator) return Boolean is
   begin
      return Get_Element (Iter.Iter) = null;
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (Iter : Script_Iterator) return Script_Description is
   begin
      return Get_Element (Iter.Iter).all;
   end Get;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script (Iter : Script_Iterator) return String is
   begin
      return Get_Key (Iter.Iter);
   end Get_Script;

   -------------------
   -- Get_Full_File --
   -------------------

   function Get_Full_File (Desc : Script_Description) return Virtual_File is
   begin
      return Desc.File;
   end Get_Full_File;

   --------------
   -- Get_Load --
   --------------

   function Get_Load (Desc : Script_Description) return Boolean is
   begin
      return Desc.Load;
   end Get_Load;

   ------------------
   -- Get_Explicit --
   ------------------

   function Get_Explicit (Desc : Script_Description) return Boolean is
   begin
      return Desc.Explicit;
   end Get_Explicit;

   --------------
   -- Get_Init --
   --------------

   function Get_Init (Descr : Script_Description) return Node_Ptr is
   begin
      return Descr.Initialization;
   end Get_Init;

   -----------------------------
   -- Override_Startup_Script --
   -----------------------------

   procedure Override_Startup_Script
     (Kernel         : access Kernel_Handle_Record'Class;
      Base_Name      : String;
      Load           : Boolean;
      Initialization : XML_Utils.Node_Ptr)
   is
      Startup : Script_Description_Access :=
                  Get (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                       K => Base_Name);
   begin
      if Startup /= null then
         Startup.Load           := Load;
         if Startup.Initialization /= Initialization then
            Free (Startup.Initialization);
         end if;

         Startup.Initialization := Initialization;
         Startup.Explicit       := True;
      else
         Startup := new Script_Description'
           (File           => GNATCOLL.VFS.No_File,
            Load           => Load,
            Explicit       => True,
            Initialization => Initialization);
         Set (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
              K => Base_Name,
              E => Startup);
      end if;
   end Override_Startup_Script;

   --------------------------
   -- Load_File_At_Startup --
   --------------------------

   function Load_File_At_Startup
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Default : Boolean) return Boolean
   is
      Startup : Script_Description_Access :=
                  Get (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                       K => +Base_Name (File));
   begin
      --  The base name would be "" for python module (ie subdirectories).

      if Base_Name (File) /= ""
         and then Startup /= null
      then
         if Startup.File /= GNATCOLL.VFS.No_File then
            Insert (Kernel,
                    -"There are several startup scripts with the same name: "
                    & Display_Full_Name (Startup.File)
                    & ASCII.LF
                    & (-"Not loading: ") & Display_Full_Name (File),
                    Mode => Error);
            return False;
         else
            Startup.File := File;
         end if;

      else
         Startup := new Script_Description'
           (File           => File,
            Load           => Default,
            Explicit       => False,
            Initialization => null);
         Set (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
              K => +Base_Name (File),
              E => Startup);
      end if;

      return Startup.Load;
   end Load_File_At_Startup;

   ----------------------------
   -- Initialization_Command --
   ----------------------------

   function Initialization_Command
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
      return Commands.Custom.Custom_Command_Access
   is
      Startup : constant Script_Description_Access :=
                  Get (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                       K => +Base_Name (File));
      Custom  : Custom_Command_Access;
   begin
      if Startup /= null and then Startup.Initialization /= null then
         Custom := new Custom_Command;
         Create
           (Item           => Custom,
            Name           => "Initialize " & Display_Full_Name (File),
            Kernel         => Kernel_Handle (Kernel),
            Default_Output => No_Output,
            Command        => Startup.Initialization);
         return Custom;
      else
         return null;
      end if;
   end Initialization_Command;

   ----------
   -- Free --
   ----------

   procedure Free (File : in out Script_Description_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Script_Description, Script_Description_Access);
   begin
      Free (File.Initialization);
      Unchecked_Free (File);
   end Free;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Table : access Scripts_Htable_Record) is
   begin
      Reset (Table.Table);
   end Reset;

end GPS.Kernel.Custom;
