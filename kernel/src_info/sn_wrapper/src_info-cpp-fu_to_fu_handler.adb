separate (Src_Info.CPP)


------------------------
--  Fu_To_Fu_Handler  --
------------------------

procedure Fu_To_Fu_Handler (Ref : TO_Table) is
   P            : Pair_Ptr;
   Fn           : FU_Table;
   Decl_Info    : E_Declaration_Info_List;
   Overloaded   : Boolean := False;
   Init         : Boolean := True;
   Kind         : E_Kind;
   Ref_Id       : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);

   function Find_Function (Fn : FU_Table) return E_Declaration_Info_List;
   --  searches for forward declaration. if no fwd decl found, searches for
   --  implementation. If nothing found throws Declaration_Not_Found
   --  TODO multiple forward declarations

   function Find_Function (Fn : FU_Table) return E_Declaration_Info_List is
      Decl_Info    : E_Declaration_Info_List;
      FD_Tab       : FD_Table;
   begin
      FD_Tab := Find
        (SN_Table (FD),
         Fn.Buffer (Fn.Name.First .. Fn.Name.Last));
      Decl_Info := Find_Declaration
        (File           => Global_LI_File,
         Symbol_Name    => Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
         Location       => FD_Tab.Start_Position);
      Free (FD_Tab);
      return Decl_Info;
   exception
      when DB_Error | Not_Found | Declaration_Not_Found =>
         Decl_Info := Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
            Location    => Fn.Start_Position);
         return Decl_Info;
   end Find_Function;

begin

   Info ("Fu_To_Fu_Handler: " & Ref_Id);

   Set_Cursor
     (SN_Table (FU),
      By_Key,
      Ref_Id,
      False);

   loop
      P := Get_Pair (SN_Table (FU), Next_By_Key);
      exit when P = null;
      Overloaded := not Init;
      if Init then
         Fn   := Parse_Pair (P.all);
         Init := False;
      end if;
      Free (P);
      exit when Overloaded;
   end loop;

   if Init then -- referred function not found
      Fail ("unable to find function " & Ref_Id);
      return;
   end if;

   if not Overloaded then
      --  If procedure
      --    defined in the current file => add reference
      --    defined in another file => add dep decl and reference it
      if Fn.Buffer (Fn.Return_Type.First .. Fn.Return_Type.Last)
            = "void" then
         Kind := Non_Generic_Function_Or_Operator;
      else
         Kind := Non_Generic_Procedure;
      end if;
      if Fn.Buffer (Fn.File_Name.First .. Fn.File_Name.Last)
            = Get_LI_Filename (Global_LI_File) then
         begin
            --  this is a function defined in the current file
            --  it may be either forward declared or implemented
            --  right away
            Decl_Info := Find_Function (Fn);
         exception
            when Declaration_Not_Found =>
               --  function is in the current file, but used before
               --  declaration. Create forward declaration
               Insert_Declaration
                 (Handler            => LI_Handler (Global_CPP_Handler),
                  File               => Global_LI_File,
                  List               => Global_LI_File_List,
                  Symbol_Name        =>
                     Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
                  Source_Filename    =>
                     Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location           => Ref.Position,
                  Kind               => Kind,
                  Scope              => Global_Scope,
                  Declaration_Info   => Decl_Info);
         end;
      else
         begin
            --  this function is defined somewhere else...
            Decl_Info := Find_Dependency_Declaration
              (File                 => Global_LI_File,
               Symbol_Name          => Fn.Buffer
                 (Fn.Name.First .. Fn.Name.Last),
               Filename             => Fn.Buffer
                 (Fn.File_Name.First .. Fn.File_Name.Last),
               Location             => Fn.Start_Position);
         exception
            when Declaration_Not_Found => -- insert dep decl
               Insert_Dependency_Declaration
                 (Handler            => LI_Handler (Global_CPP_Handler),
                  File               => Global_LI_File,
                  List               => Global_LI_File_List,
                  Symbol_Name        =>
                     Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
                  Source_Filename    =>
                     Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location           => Fn.Start_Position,
                  Kind               => Kind,
                  Scope              => Global_Scope,
                  Referred_Filename  =>
                     Fn.Buffer (Fn.File_Name.First .. Fn.File_Name.Last),
                  Declaration_Info   => Decl_Info);
         end;
      end if;
   else -- overloaded entity
      --  have we already declared it?
      begin
         Decl_Info := Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
            Kind        => Overloaded_Entity);
      exception
         when Declaration_Not_Found =>
            Decl_Info := new E_Declaration_Info_Node'
              (Value =>
                 (Declaration => No_Declaration,
                  References => null),
               Next => Global_LI_File.LI.Body_Info.Declarations);
            Decl_Info.Value.Declaration.Name :=
               new String'(Ref_Id);
            Decl_Info.Value.Declaration.Kind := Overloaded_Entity;
            Global_LI_File.LI.Body_Info.Declarations := Decl_Info;
      end;
   end if;
   Free (Fn);

   Insert_Reference
     (Decl_Info,
      Global_LI_File,
      Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Ref.Position,
      Reference);
exception
   when Not_Found  | DB_Error => -- ignore
      Fail ("unable to find function" & Ref_Id);
   return;
end Fu_To_Fu_Handler;

