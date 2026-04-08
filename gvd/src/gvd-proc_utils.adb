------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2002-2026, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNAT.Expect;             use GNAT.Expect;

with GNATCOLL.Arg_Lists;      use GNATCOLL.Arg_Lists;
with GNATCOLL.Python;         use GNATCOLL.Python;
with GNATCOLL.Scripts;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;

with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Kernel.Remote;       use GPS.Kernel.Remote;
with Remote;                  use Remote;
with String_Utils;            use String_Utils;
with VSS.Strings.Conversions;

package body GVD.Proc_Utils is

   type Process_Record is record
      Descriptor : Process_Descriptor_Access;
      Index      : Natural := 0;
      --  Index of the start of the command
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Process_Record, Process_Handle);

   procedure Free is new Ada.Unchecked_Deallocation
     (Process_Descriptor'Class, Process_Descriptor_Access);

   ---------------------
   -- Close_Processes --
   ---------------------

   procedure Close_Processes (Handle : in out Process_Handle) is
   begin
      Close (Handle.Descriptor.all);
      Free (Handle.Descriptor);
      Free (Handle);
   end Close_Processes;

   ------------------
   -- Next_Process --
   ------------------

   procedure Next_Process
     (Handle  : Process_Handle;
      Info    : out Process_Info;
      Success : out Boolean)
   is
      Match : Expect_Match := 0;
   begin
      Success := False;
      Expect (Handle.Descriptor.all, Match, "\n");

      if Match = 1 then
         declare
            S     : constant String :=
                      Strip_CR (Expect_Out (Handle.Descriptor.all));
            Index : Integer := S'First;
         begin
            Skip_Blanks (S, Index);
            Skip_To_Char (S, Index, ' ');

            if Handle.Index /= 0 then
               Info :=
                 (Id_Len   => Index - S'First + 1,
                  Info_Len => S'Last - Handle.Index,
                  Id       => S (S'First .. Index),
                  Info     => S (Handle.Index .. S'Last - 1));
            else
               Info :=
                 (Id_Len   => Index - S'First + 1,
                  Info_Len => S'Last - Index - 1,
                  Id       => S (S'First .. Index),
                  Info     => S (Index + 1 .. S'Last - 1));
            end if;

         exception
            when Constraint_Error =>
               --  Parsing failed due to an unexpected ouput.
               --  Return a null string instead.
               Info :=
                 (Id_Len   => 0,
                  Info_Len => 0,
                  Id       => "",
                  Info     => "");
         end;

         Success := True;
      end if;

   exception
      when Process_Died => null;
   end Next_Process;

   --------------------
   -- Open_Processes --
   --------------------

   procedure Open_Processes
     (Handle : out Process_Handle;
      Kernel : Kernel_Handle)
   is
      CL      : Arg_List;
      Match   : Expect_Match := 0;
      Success : Boolean;
   begin
      Handle := new Process_Record;

      --  ??? Get_Pref is not fine here, as this can be a remote call
      CL := Parse_String (List_Processes.Get_Pref, Separate_Args);
      Spawn (Kernel,
             CL,
             Debug_Server,
             Handle.Descriptor,
             Success);
      if Success then
         --  Read header and discard it
         Expect (Handle.Descriptor.all, Match, "\n");

         if Match = 1 then
            --  Let's keep the column where the command starts
            declare
               S : constant String :=
                     Strip_CR (Expect_Out (Handle.Descriptor.all));
            begin
               --  GNU/Linux   TIME
               --  Solaris     TIME
               --  Cygwin     STIME
               Handle.Index := Index (S, "TIME ");
               if Handle.Index /= 0 then
                  Handle.Index := Handle.Index + 5;
               end if;
            end;
         end if;
      end if;

   exception
      when Process_Died => null;
   end Open_Processes;

   ----------------
   -- Py_PSUtils --
   ----------------

   function Py_PSUtils
     (Kernel : Kernel_Handle)
      return Py_Process_List.List
   is
      Script    : constant GNATCOLL.Scripts.Scripting_Language :=
        Kernel.Scripts.Lookup_Scripting_Language (Python_Name);
      Py_Import : PyObject;
      Py_List   : PyObject;
      Errors    : aliased Boolean;
      Res       : Py_Process_List.List;
   begin
      Py_Import := Run_Command
        (Python_Scripting (Script),
         "import psutil",
         Need_Output     => False,
         Console         => null,
         Show_Command    => False,
         Hide_Output     => True,
         Hide_Exceptions => False,
         Errors          => Errors'Unchecked_Access);

      Py_List := Run_Command
        (Python_Scripting (Script),
         "[p.info for p in psutil.process_iter(['pid', 'name'])]",
         Need_Output     => True,
         Console         => null,
         Show_Command    => False,
         Hide_Output     => True,
         Hide_Exceptions => False,
         Errors          => Errors'Unchecked_Access);

      for I in 0 .. PyObject_Size (Py_List) - 1 loop
         --  Element of the list are dictionnaries indexed by "pid" and "name"
         declare
            use VSS.Strings.Conversions;
            Py_Process : constant PyObject := PyObject_GetItem (Py_List, I);
            Id         : constant PyObject :=
              PyDict_GetItemString (Py_Process, "pid");
            Name       : constant PyObject :=
              PyDict_GetItemString (Py_Process, "name");
            --  All these PyObjects are borrowed and should not be decrefed
         begin
            Res.Append
              (Py_Process_Info'(
               Id   => To_Virtual_String (PyInt_AsLong (Id)'Image),
               Name => To_Virtual_String (PyString_AsString (Name))));
         end;
      end loop;

      --  We have the list of ID/Name, free the memory used by python now
      Py_XDECREF (Py_Import);
      Py_XDECREF (Py_List);

      return Res;
   end Py_PSUtils;

end GVD.Proc_Utils;
