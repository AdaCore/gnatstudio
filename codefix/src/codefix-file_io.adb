-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with OS_Utils; use OS_Utils;

package body Codefix.File_Io is

   ----------
   -- Free --
   ----------

   procedure Free (This : in out File_Interface) is
   begin
      Free (This.Content);
   end Free;

   ---------
   -- Get --
   ---------

   function Get
     (This   : File_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural) return String is
   begin
      return Data (Get_Line_Node (This, Cursor.Line)).all
        (Cursor.Col .. Cursor.Col + Len - 1);
   end Get;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This   : File_Interface;
      Cursor : Text_Cursor'Class) return String
   is
      Element : Dynamic_String;
   begin
      Element := Data (Get_Line_Node (This, Cursor.Line));
      return Element.all (Cursor.Col .. Element.all'Length);
   end Get_Line;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This      : in out File_Interface;
      Cursor    : Text_Cursor'Class;
      Len       : Natural;
      New_Value : String)
   is
      Element  : Dynamic_String;
   begin
      Element := Data (Get_Line_Node (This, Cursor.Line));
      Set_Data
       (Get_Line_Node (This, Cursor.Line),
        new String '(Element.all (1 .. Cursor.Col - 1) &
                     New_Value &
                     Element.all (Cursor.Col + Len .. Element.all'Length)));
   end Replace;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This        : in out File_Interface;
      Cursor      : Text_Cursor'Class;
      New_Line    : String) is
   begin
      if Cursor.Line = 0 then
         Prepend (This.Content, new String'(New_Line));
      else
         Append
           (This.Content,
            Get_Line_Node (This, Cursor.Line),
            new String'(New_Line));
      end if;
   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This   : in out File_Interface;
      Cursor : Text_Cursor'Class)
   is
      Delete_Node : List_Node;
   begin
      Delete_Node := Get_Line_Node (This, Cursor.Line);
      Remove_Nodes
        (This.Content,
         Prev (This.Content, Delete_Node),
         Delete_Node);
   end Delete_Line;

   -------------------
   -- Get_Line_Node --
   -------------------

   function Get_Line_Node
     (This : File_Interface;
      Line : Positive) return List_Str.List_Node
   is
      Current_Node : List_Str.List_Node;
   begin
      Current_Node := First (This.Content);

      for J in 1 .. Line - 1 loop
         Current_Node := Next (Current_Node);
      end loop;

      return Current_Node;
   end Get_Line_Node;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out File_Interface;
      Path : String)
   is
      File     : File_Type;
      Line_Red : Dynamic_String;
   begin
      Open (File, In_File, Path);

      while not End_Of_File (File) loop
         Line_Red := null; --  to not to erase the line red before
         Get_Line (File, Line_Red);
         Append (This.Content, Line_Red);
      end loop;

      Close (File);
   end Initialize;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (This : File_Interface) return Dynamic_String is
   begin
      return Dynamic_String (Read_File (Get_File_Name (This)));
   end Read_File;

   ------------
   -- Update --
   ------------

   procedure Update (This : File_Interface) is
      Current_Node : List_Str.List_Node := First (This.Content);
      Current_File : File_Type;
   begin
      Open (Current_File, Out_File, Get_File_Name (This));

      while Current_Node /= List_Str.Null_Node loop
         Put_Line (Current_File, Data (Current_Node).all);
         Current_Node := Next (Current_Node);
      end loop;

      Close (Current_File);
   end Update;

   ------------------------
   -- Get_Direct_Message --
   ------------------------

   procedure Get_Direct_Message
     (This    : in out Errors_File;
      Current : out Error_Message)
   is
      Buffer        : String (1 .. 255);
      Buffer_Length : Natural;
   begin
      Get_Line (This.File.all, Buffer, Buffer_Length);
      Initialize (Current, Buffer (1 .. Buffer_Length));

      if End_Of_File (This.File.all) then
         Close (This.File.all);
         This.Is_Open := False;
      end if;
   end Get_Direct_Message;

   ----------------------
   -- No_More_Messages --
   ----------------------

   function No_More_Messages (This : Errors_File) return Boolean is
   begin
      return not This.Is_Open or else End_Of_File (This.File.all);
   end No_More_Messages;

   ----------
   -- Open --
   ----------

   procedure Open (This : in out Errors_File; File_Name : String) is
   begin
      This.File := new File_Type;
      Open (This.File.all, In_File, File_Name);
      This.Is_Open := True;
   end Open;

end Codefix.File_Io;
