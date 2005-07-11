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

with Glib.Xml_Int;              use Glib.Xml_Int;
with XML_Parsers;
with Traces;                    use Traces;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with System.Assertions;         use System.Assertions;
with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Intl;                  use GPS.Intl;
with File_Utils;                use File_Utils;
with VFS;                       use VFS;

package body GPS.Kernel.Custom is

   Me                       : constant Debug_Handle :=
     Create ("Kernel.Custom");
   XML_Extension            : constant String := ".xml";
   GPS_Custom_Path_External : constant String := "GPS_CUSTOM_PATH";

   procedure Parse_Custom_Dir
     (Kernel    : access Kernel_Handle_Record'Class;
      Directory : String;
      Level     : Customization_Level);
   --  Parse and process all the XML files in the directory

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
      Tmp  : Node_Ptr := Node;
      Tmp2 : Node_Ptr;

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
      Level     : Customization_Level)
   is
      Norm_Dir  : constant String := Name_As_Directory (Directory);
      File      : String (1 .. 1024);
      Last      : Natural;
      Dir       : Dir_Type;
      File_Node : Node_Ptr;

   begin
      if Is_Directory (Norm_Dir) then
         Open (Dir, Directory);
         loop
            Read (Dir, File, Last);
            exit when Last = 0;

            declare
               F : constant String := Norm_Dir & File (1 .. Last);
               Error : String_Access;
            begin
               if File_Extension (F) = XML_Extension
                 and then Is_Regular_File (F)
               then
                  Trace (Me, "Loading " & F);

                  XML_Parsers.Parse (F, File_Node, Error);

                  if File_Node = null then
                     Trace (Me, "Could not parse XML file: " & F);
                     Insert (Kernel, Error.all,
                             Mode => GPS.Kernel.Console.Error);
                     Free (Error);
                  else
                     Execute_Customization_String
                       (Kernel, Create (F), File_Node.Child, Level);
                     Free (File_Node);
                  end if;
               end if;

            exception
               when Assert_Failure =>
                  Console.Insert
                    (Kernel, -"Could not parse custom file " & F,
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
      System_Directory : constant String :=
        Get_System_Dir (Kernel) & "share/gps/plug-ins/";
      User_Directory   : constant String :=
        Get_Home_Dir (Kernel) & "plug-ins/";
      Old_User_Dir     : constant String :=
        Get_Home_Dir (Kernel) & "customize/";
      Old_System_Dir   : constant String :=
        Get_System_Dir (Kernel) & "share/gps/customize/";
      Env_Path     : String_Access := Getenv (GPS_Custom_Path_External);
      Path         : Path_Iterator;
      N            : Node_Ptr;

   begin
      Kernel.Custom_Files_Loaded := True;

      --  Load the hard-coded customization strings first, so that anyone
      --  can override them

      if Kernel.Customization_Strings /= null then
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

      --  For backward compatibility with GPS < 3.0.0, parse the custom
      --  directory

      Parse_Custom_Dir (Kernel, Old_System_Dir, System_Wide);

      --  Load the system plug-ins directory first, so that its contents can
      --  be overriden locally by the user
      Parse_Custom_Dir (Kernel, System_Directory, System_Wide);

      Path := Start (Env_Path.all);
      while not At_End (Env_Path.all, Path) loop
         if Current (Env_Path.all, Path) /= "" then
            Parse_Custom_Dir
              (Kernel, Current (Env_Path.all, Path), Project_Wide);
         end if;
         Path := Next (Env_Path.all, Path);
      end loop;

      Free (Env_Path);

      --  For backward compatibility with GPS < 3.0.0, parse the user custom
      --  directory
      Parse_Custom_Dir (Kernel, Old_User_Dir, User_Specific);

      Parse_Custom_Dir (Kernel, User_Directory, User_Specific);
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
      --  contain any

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
           ("<?xml version=""1.0""?><Root>" & Customization & "</Root>",
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
            Execute_Customization_String
              (Kernel, No_File, Node.Child, Hard_Coded);
            Free (Node);
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

end GPS.Kernel.Custom;

