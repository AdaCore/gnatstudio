separate (Src_Info.CPP)


------------------------
--  Fu_To_Mi_Handler  --
------------------------

procedure Fu_To_Mi_Handler (Ref : TO_Table) is
   P            : Pair_Ptr;
   Fn           : MI_Table;
   Decl_Info    : E_Declaration_Info_List;
   Overloaded   : Boolean := False;
   Init         : Boolean := True;
   Ptr          : E_Declaration_Info_List;
   Kind         : E_Kind;

   function Find_Method (Fn : MI_Table) return E_Declaration_Info_List;
   --  searches for forward declaration. if no fwd decl found, searches for
   --  implementation. If nothing found throws Declaration_Not_Found

   function Find_Method (Fn : MI_Table) return E_Declaration_Info_List is
      Decl_Info    : E_Declaration_Info_List;
      MD_Tab       : MD_Table;
   begin
      MD_Tab := Find
        (SN_Table (MD),
         Fn.Buffer (Fn.Name.First .. Fn.Name.Last));
      Decl_Info := Find_Declaration
        (File           => Global_LI_File,
         Symbol_Name    => Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
         Class_Name     => Fn.Buffer (Fn.Class.First .. Fn.Class.Last),
         Location       => MD_Tab.Start_Position);
      Free (MD_Tab);
      return Decl_Info;
   exception
      when DB_Error | Not_Found | Declaration_Not_Found =>
         Decl_Info := Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
            Class_Name  => Fn.Buffer (Fn.Class.First .. Fn.Class.Last),
            Location    => Fn.Start_Position);
         return Decl_Info;
   end Find_Method;

begin

   Info ("Fu_To_Mi_Handler: """
         & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
               Ref.Referred_Symbol_Name.Last)
         & """");

   Set_Cursor
     (SN_Table (MI),
      By_Key,
      Ref.Buffer (Ref.Referred_Class.First ..
                  Ref.Referred_Class.Last)
         & Field_Sep
         & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                       Ref.Referred_Symbol_Name.Last)
         & Field_Sep,
      False);

   loop
      P := Get_Pair (SN_Table (MI), Next_By_Key);
      exit when P = null;
      Overloaded := not Init;
      if Init then
         Fn   := Parse_Pair (P.all);
         Init := False;
      end if;
      Free (P);
      exit when Overloaded;
   end loop;

   if not Overloaded then
      --  If method
      --    defined in the current file => add reference
      --    defined in another file => add dep decl and reference it
      --  TODO templates
      if Fn.Buffer (Fn.Return_Type.First .. Fn.Return_Type.Last)
            = "void" then
         Kind := Non_Generic_Function_Or_Operator;
      else
         Kind := Non_Generic_Procedure;
      end if;
      if Fn.Buffer (Fn.File_Name.First .. Fn.File_Name.Last)
            = Get_LI_Filename (Global_LI_File) then
         begin
            --  this is a method defined in the current file
            --  it may be either forward declared or implemented
            --  right away
            Decl_Info := Find_Method (Fn);
         exception
            when Declaration_Not_Found =>
               --  method is in the current file, but used before
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
      Ptr := Global_LI_File.LI.Body_Info.Declarations;
      while Ptr /= null loop
         Decl_Info := Ptr;
         exit when Ptr.Value.Declaration.Kind = Overloaded_Entity
            and then Ptr.Value.Declaration.Name.all =
               Fn.Buffer (Fn.Name.First .. Fn.Name.Last);
         Ptr := Ptr.Next;
      end loop;
      if Ptr = null then -- no, we have not. Do it now
         Decl_Info := new E_Declaration_Info_Node'
           (Value =>
              (Declaration => No_Declaration,
               References => null),
            Next => Global_LI_File.LI.Body_Info.Declarations);
         Decl_Info.Value.Declaration.Name :=
            new String'(Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                                    Ref.Referred_Symbol_Name.Last));
         Decl_Info.Value.Declaration.Kind := Overloaded_Entity;
         Global_LI_File.LI.Body_Info.Declarations := Decl_Info;
      end if;
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
      Fail ("Method "
         & Ref.Buffer (Ref.Referred_Class.First ..
                       Ref.Referred_Symbol_Name.Last)
         & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                       Ref.Referred_Symbol_Name.Last)
         & " not found");
   return;
end Fu_To_Mi_Handler;

