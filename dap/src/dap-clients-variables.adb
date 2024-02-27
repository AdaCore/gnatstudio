------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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
with VSS.Strings.Conversions;

with GNATCOLL.Traces;              use GNATCOLL.Traces;

with VSS.Characters;
with VSS.Strings.Cursors.Iterators.Characters;
with VSS.Transformers.Casing;      use VSS.Transformers.Casing;

with GPS.Kernel.Hooks;
with GPS.Debuggers;                use GPS.Debuggers;

with DAP.Clients.Variables.Scopes;
with DAP.Clients.Variables.Variables;
with DAP.Clients.Variables.Evaluate;
with DAP.Clients.Variables.SetExpression;
with DAP.Clients.Variables.SetVariable;
with DAP.Views.Variables;
with DAP.Modules.Scripts;

package body DAP.Clients.Variables is

   Me : constant Trace_Handle := Create ("DAP.Clients.Variables", Off);

   type On_Debug_Process_Terminated is
     new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debug_Process_Terminated;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class);
   --  Called when the process has terminated

   type On_Debug_Location_Changed is
     new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debug_Location_Changed;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class);
   --  Called when the location of the debugger has changed

   type On_Debugger_State_Changed is
     new GPS.Kernel.Hooks.Debugger_States_Hooks_Function with null record;
   overriding procedure Execute
     (Self      : On_Debugger_State_Changed;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger  : access GPS.Debuggers.Base_Visual_Debugger'Class;
      New_State : GPS.Debuggers.Debugger_State);
   --  Called when the state of the debugger changes

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      GPS.Kernel.Hooks.Debugger_Process_Terminated_Hook.Add
        (new On_Debug_Process_Terminated);
      GPS.Kernel.Hooks.Debugger_Location_Changed_Hook.Add
        (new On_Debug_Location_Changed);
      GPS.Kernel.Hooks.Debugger_State_Changed_Hook.Add
        (new On_Debugger_State_Changed);
   end Register_Module;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debug_Process_Terminated;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self);
      Client : constant DAP_Client_Access :=
        DAP_Visual_Debugger_Access (Debugger).Client;
   begin
      if Client /= null
        and then Client.Get_Variables /= null
      then
         Client.Get_Variables.Clear;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debug_Location_Changed;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self);
      Client : constant DAP_Client_Access :=
        DAP_Visual_Debugger_Access (Debugger).Client;
   begin
      if Client /= null
        and then Client.Get_Variables /= null
      then
         Client.Get_Variables.Clear;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self      : On_Debugger_State_Changed;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger  : access GPS.Debuggers.Base_Visual_Debugger'Class;
      New_State : GPS.Debuggers.Debugger_State)
   is
      pragma Unreferenced (Self);
      Client : constant DAP_Client_Access :=
        DAP_Visual_Debugger_Access (Debugger).Client;
   begin
      if New_State = Debug_Busy
        and then Client /= null
        and then Client.Get_Variables /= null
      then
         Client.Get_Variables.Clear;
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Src : in out Request_Parameters) is
   begin
      case Src.Kind is
         when View =>
            if Src.Path /= Null_Gtk_Tree_Path then
               Path_Free (Src.Path);
            end if;

         when Python_API =>
            GNATCOLL.Scripts.Free (Src.On_Result);
            Src.On_Result := null;
            GNATCOLL.Scripts.Free (Src.On_Error);
            Src.On_Error := null;
            GNATCOLL.Scripts.Free (Src.On_Rejected);
            Src.On_Rejected := null;

         when Set_Variable =>
            if Src.Set_Path /= Null_Gtk_Tree_Path then
               Path_Free (Src.Set_Path);
            end if;
      end case;
   end Free;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Variables_Holder) is
   begin
      Self.Locals_Scope_Id    := 0;
      Self.Arguments_Scope_Id := 0;
      Self.Has_Scopes_Ids     := False;
      Self.Scopes.Clear;
   end Clear;

   ----------------
   -- Get_Scopes --
   ----------------

   function Get_Scopes
     (Self : in out Variables_Holder)
      return Variables_References_Trees.Tree is
   begin
      return Self.Scopes;
   end Get_Scopes;

   ------------------
   -- Get_Variable --
   ------------------

   procedure Get_Variable
     (Self   : in out Variables_Holder;
      Params : in out Request_Parameters) is
   begin
      if Self.Client.Is_Stopped then
         if not Self.Has_Scopes_Ids then
            DAP.Clients.Variables.Scopes.Send_Scopes_Request
              (Self.Client, Params);

         else
            Self.On_Variables_Response (Params);
         end if;

      else
         Self.On_Variable_Not_Found (Params);
      end if;
   end Get_Variable;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable
     (Self   : in out Variables_Holder;
      Params : Request_Parameters)
   is
      Cursor : Variables_References_Trees.Cursor := Self.Scopes.Root;
      Found  : Boolean;

      ----------------------
      -- Has_EvaluateName --
      ----------------------

      function Has_EvaluateName return Boolean;
      function Has_EvaluateName return Boolean is
      begin
         return Cursor /= Variables_References_Trees.No_Element
           and then not Element (Cursor).Data.evaluateName.Is_Empty;
      end Has_EvaluateName;

   begin
      if not Self.Client.Get_Capabilities.Is_Set then
         return;
      end if;

      Find_Name_Or_Parent (Params.Name, Cursor, Found);

      if Self.Client.Get_Capabilities.Value.supportsSetExpression
        and then
          (not Self.Client.Get_Capabilities.Value.supportsSetVariable
           or else Has_EvaluateName)
      then
         DAP.Clients.Variables.SetExpression.Send_Set_Expression_Request
           (Self.Client, Params);

      elsif Self.Client.Get_Capabilities.Value.supportsSetVariable then
         if Found then
            declare
               P      : Request_Parameters := Params;
               Parent : constant Variables_References_Trees.Cursor :=
                 Variables_References_Trees.Parent (Cursor);
            begin
               P.Name := Element (Cursor).Data.name;
               if Parent = Self.Scopes.Root then
                  DAP.Clients.Variables.SetVariable.Send_Set_Variable_Request
                    (Self.Client,
                     (if Element (Cursor).Kind = Locals
                      then Self.Locals_Scope_Id
                      else Self.Arguments_Scope_Id), P);

               else
                  DAP.Clients.Variables.SetVariable.Send_Set_Variable_Request
                    (Self.Client, Element (Parent).Data.variablesReference, P);
               end if;
            end;
         end if;
      end if;
   end Set_Variable;

   ----------------------
   -- On_Scopes_Result --
   ----------------------

   procedure On_Scopes_Result
     (Self   : in out Variables_Holder;
      Params : in out Request_Parameters)
   is
      Id : Integer;
   begin
      if Params.Item.Cmd.Is_Empty then
         --  It is not a command, use variable request. Request contents of
         --  Locals or Arguments if any.

         if Self.Arguments_Scope_Id /= 0 then
            Id := Self.Arguments_Scope_Id;
         else
            Id := Self.Locals_Scope_Id;
         end if;

         if Id /= 0 then
            DAP.Clients.Variables.Variables.Send_Variables_Request
              (Self.Client, Id, Params);
         else
            Free (Params);
         end if;

      else
         --  it is command, use evaluate request
         DAP.Clients.Variables.Evaluate.Send_Evaluate_Request
           (Self.Client, Params);
      end if;
   end On_Scopes_Result;

   ----------------
   -- Find_By_Id --
   ----------------

   function Find_By_Id
     (Self : Variables_Holder;
      Id   : Integer)
      return Variables_References_Trees.Cursor
   is
      ----------
      -- Find --
      ----------

      function Find
        (C : Variables_References_Trees.Cursor)
         return Variables_References_Trees.Cursor;

      function Find
        (C : Variables_References_Trees.Cursor)
         return Variables_References_Trees.Cursor
      is
         use type Ada.Containers.Count_Type;
         Current : Variables_References_Trees.Cursor := C;
         R       : Variables_References_Trees.Cursor;
      begin
         while Current /= Variables_References_Trees.No_Element loop
            if Element (Current).Data.variablesReference = Id then
               return Current;
            end if;
            Next_Sibling (Current);
         end loop;

         Current := C;
         while Current /= Variables_References_Trees.No_Element loop
            if Child_Count (Current) /= 0 then
               R := Find (First_Child (Current));
               if R /= Variables_References_Trees.No_Element then
                  return R;
               end if;
            end if;
            Next_Sibling (Current);
         end loop;

         return Variables_References_Trees.No_Element;
      end Find;

   begin
      if Id = Self.Locals_Scope_Id
        or else Id = Self.Arguments_Scope_Id
      then
         return Self.Scopes.Root;

      else
         return Find (First_Child (Self.Scopes.Root));
      end if;
   end Find_By_Id;

   ------------------
   -- Find_By_Name --
   ------------------

   procedure Find_By_Name
     (Name   : Virtual_String;
      Cursor : in out Variables_References_Trees.Cursor)
   is
      use type Ada.Containers.Count_Type;
      N : constant Virtual_String := To_Lowercase.Transform (Name);

   begin
      if Child_Count (Cursor) = 0 then
         return;
      end if;

      Cursor := First_Child (Cursor);
      while Cursor /= Variables_References_Trees.No_Element loop
         if To_Lowercase.Transform (Element (Cursor).Data.name) = N then
            return;
         end if;
         Next_Sibling (Cursor);
      end loop;
   end Find_By_Name;

   -------------------------
   -- Find_Name_Or_Parent --
   -------------------------

   procedure Find_Name_Or_Parent
     (Name   : Virtual_String;
      Cursor : in out Variables_References_Trees.Cursor;
      Found  : out Boolean)
   is
      use type Ada.Containers.Count_Type;
      use VSS.Strings.Cursors.Iterators.Characters;
      use VSS.Characters;

      procedure Find (N : Virtual_String);
      procedure Find (N : Virtual_String) is
         Pos   : VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
           N.Before_First_Character;
         Part  : Virtual_String;
         Dummy : Boolean;
      begin
         if Child_Count (Cursor) = 0 then
            return;
         end if;

         while Forward (Pos)
           and then Element (Pos) /= '.'
         loop
            Part.Append (Element (Pos));
         end loop;

         Cursor := First_Child (Cursor);
         while Cursor /= Variables_References_Trees.No_Element loop
            if To_Lowercase.Transform (Element (Cursor).Data.name) = Part then
               exit;
            end if;
            Next_Sibling (Cursor);
         end loop;

         if Cursor /= Variables_References_Trees.No_Element then
            if Has_Element (Pos) then
               --  the part of the name is found
               Dummy := Forward (Pos); --  skip '.'
               Find (N.Slice (Pos, N.At_Last_Character));
            else
               --  the last name part is found
               Found := True;
            end if;
         end if;
      end Find;

   begin
      Found := False;

      if Child_Count (Cursor) = 0 then
         Cursor := Variables_References_Trees.No_Element;
         return;
      end if;

      Find (To_Lowercase.Transform (Name));
   end Find_Name_Or_Parent;

   ---------------------------
   -- On_Variables_Response --
   ---------------------------

   procedure On_Variables_Response
     (Self   : in out Variables_Holder;
      Params : in out Request_Parameters)
   is
      use type Ada.Containers.Count_Type;
      C     : Variables_References_Trees.Cursor := Self.Scopes.Root;
      Found : Boolean;

   begin
      Trace (Me, "On_Variables_Response:" &
               VSS.Strings.Conversions.To_UTF_8_String
               (Params.Item.Varname));

      if Params.Item.Varname.Is_Empty then
         --  We have a command here
         Find_By_Name (Params.Item.Cmd, C);
         Found := C /= No_Element;

      else
         --  We have a variable here
         Trace (Me, "Find_Name_Or_Parent:" &
                  VSS.Strings.Conversions.To_UTF_8_String
                  (Params.Item.Varname));
         Find_Name_Or_Parent (Params.Item.Varname, C, Found);
      end if;

      if Found then
         --  we found the variable

         if not Params.Children then
            --  we need the variable itself, inform the view to add it
            case Params.Kind is
               when View =>
                  DAP.Views.Variables.On_Variable_Loaded
                    (Self.Client, Params, C);

               when Python_API =>
                  Trace (Me, "Found:" &
                           VSS.Strings.Conversions.To_UTF_8_String
                           (Full_Name (C)) & " " &
                           VSS.Strings.Conversions.To_UTF_8_String
                           (Element (C).Data.value));

                  DAP.Modules.Scripts.Create_Debugger_Variable_For_Callback
                    (Callback => Params.On_Result,
                     Client   => Self.Client,
                     Data     => (Full_Name (C), Element (C).Data));

               when Set_Variable =>
                  null;
            end case;

            --  we done with the variable, free parameters
            Free (Params);

         elsif Element (C).Data.variablesReference > 0 then
            --  we need the variable's children and they can be fetched

            if Child_Count (C) = 0 then
               --  we need to get them, prepare a request
               DAP.Clients.Variables.Variables.Send_Variables_Request
                 (Self.Client, Element (C).Data.variablesReference, Params);

            else
               --  we already have children, inform the caller
               case Params.Kind is
                  when View =>
                     if Element (First_Child (C)).Data /= Empty_Variable then
                        DAP.Views.Variables.On_Children_Loaded
                          (Self.Client, Params, C);
                     end if;

                  when Python_API =>
                     declare
                        Vector : DAP.Modules.Scripts.
                          Variable_Data_Vector.Vector;
                     begin
                        C := First_Child (C);
                        while C /= Variables_References_Trees.No_Element loop
                           if Element (C).Data /= Empty_Variable then
                              Vector.Append
                                (DAP.Modules.Scripts.Variable_Data'
                                   (Full_Name (C), Element (C).Data));
                           end if;
                           Next_Sibling (C);
                        end loop;

                        DAP.Modules.Scripts.
                          Create_Debugger_Variables_For_Callback
                            (Callback => Params.On_Result,
                             Client   => Self.Client,
                             Data     => Vector);
                     end;

                  when Set_Variable =>
                     null;
               end case;

               --  we are done with the variables, free the parameters
               Free (Params);
            end if;

         else
            --  we need children but the variable does not have them
            if Params.Kind = Python_API then
               DAP.Modules.Scripts.
                 Create_Debugger_Variables_For_Callback
                   (Callback => Params.On_Result,
                    Client   => Self.Client,
                    Data     => DAP.Modules.Scripts.
                      Variable_Data_Vector.Empty_Vector);
            end if;

            --  we are done with the variables, free the parameters
            Free (Params);
         end if;

      elsif C /= Variables_References_Trees.No_Element then
         --  we found the parent of the variable we are looking for.
         --  Send new request to "expand" it and get children

         if not Params.Item.Cmd.Is_Empty then
            --  Create request for command
            DAP.Clients.Variables.Evaluate.Send_Evaluate_Request
              (Self.Client, Params);

         else
            --  Create request for variable
            DAP.Clients.Variables.Variables.Send_Variables_Request
              (Self.Client, Element (C).Data.variablesReference, Params);
         end if;

      else
         --  We did not find anything
         Self.On_Variable_Not_Found (Params);
      end if;
   end On_Variables_Response;

   ---------------------------
   -- On_Variable_Not_Found --
   ---------------------------

   procedure On_Variable_Not_Found
     (Self   : in out Variables_Holder;
      Params : in out Request_Parameters) is
   begin
      case Params.Kind is
         when View =>
            --  inform the view
            DAP.Views.Variables.On_Variable_Not_Found (Self.Client, Params);

         when Python_API =>
            --  inform the Python side
            DAP.Modules.Scripts.Create_No_Debugger_Variable_For_Callback
              (Params);

         when Set_Variable =>
            null;
      end case;

      Free (Params);
   end On_Variable_Not_Found;

   ---------------------
   -- On_Variable_Set --
   ---------------------

   procedure On_Variable_Set
     (Self     : in out Variables_Holder;
      Params   : Request_Parameters;
      Variable : DAP.Tools.Variable) is
   begin
      DAP.Views.Variables.On_Variable_Set (Self.Client, Params, Variable);
   end On_Variable_Set;

   -------------
   -- Full_Name --
   ---------------

   function Full_Name
     (Cursor : Variables_References_Trees.Cursor)
      return VSS.Strings.Virtual_String
   is
      Result : VSS.Strings.Virtual_String;
      C      : Variables_References_Trees.Cursor := Cursor;
   begin
      while C /= Variables_References_Trees.No_Element
        and then not Is_Root (C)
      loop
         if Result /= "" then
            Result := Element (C).Data.name & "." & Result;
         else
            Result := Element (C).Data.name;
         end if;
         C := Parent (C);
      end loop;

      return Result;
   end Full_Name;

end DAP.Clients.Variables;
