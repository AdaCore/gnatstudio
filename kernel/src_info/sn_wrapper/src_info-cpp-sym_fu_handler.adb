separate (Src_Info.CPP)

--------------------
-- Sym_FU_Handler --
--------------------

procedure Sym_FU_Handler (Sym : FIL_Table)
is
   Decl_Info      : E_Declaration_Info_List := null;
   Target_Kind    : E_Kind;
   Sym_Type       : SN.String_Access :=
                  new String'(ASCII.NUL & ASCII.NUL & ASCII.NUL);
   tmp_int        : Integer;
   P              : Pair_Ptr;
   FU_Tab         : FU_Table;
   MI_Tab         : MI_Table;
   Start_Position : Point := Sym.Start_Position;
   Body_Position  : Point := Invalid_Point;
   End_Position   : Point;
   IsTemplate     : Boolean := False;

   Fu_Id          : constant String := Sym.Buffer
     (Sym.Identifier.First .. Sym.Identifier.Last);

begin
   Info ("Sym_FU_Hanlder: """
         & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "."
         & Fu_Id
         & """");

   if Sym.Symbol = MI then
      declare
         Class_Def    : CL_Table;
      begin
         MI_Tab := Find (SN_Table (MI),
             Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
             Fu_Id,
             Sym.Start_Position,
             Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last));
         begin -- check if this class is template
            Class_Def := Find
              (SN_Table (CL), Sym.Buffer (Sym.Class.First .. Sym.Class.Last));
            IsTemplate := Class_Def.Template_Parameters.First
               < Class_Def.Template_Parameters.Last;
            Free (Class_Def);
         exception
            when DB_Error | Not_Found =>
               null;
         end;
         if MI_Tab.Buffer (MI_Tab.Return_Type.First ..
                           MI_Tab.Return_Type.Last) = "void" then
            if IsTemplate then
               Target_Kind := Generic_Procedure;
            else
               Target_Kind := Non_Generic_Procedure;
            end if;
         else
            if IsTemplate then
               Target_Kind := Generic_Function_Or_Operator;
            else
               Target_Kind := Non_Generic_Function_Or_Operator;
            end if;
         end if;
         End_Position := MI_Tab.End_Position;
      exception
         when DB_Error | Not_Found =>
            Fail ("unable to find method "
                  & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "."
                  & Fu_Id);
            return;
      end;
   else
      begin
         FU_Tab := Find
           (SN_Table (FU),
            Fu_Id,
            Sym.Start_Position,
            Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last));
         if FU_Tab.Buffer (FU_Tab.Return_Type.First ..
                           FU_Tab.Return_Type.Last) = "void" then
            Target_Kind := Non_Generic_Procedure;
         else
            Target_Kind := Non_Generic_Function_Or_Operator;
         end if;
         End_Position := FU_Tab.End_Position;
      exception
         when DB_Error | Not_Found =>
            Fail ("unable to find function " & Fu_Id);
            return;
      end;
   end if;

   --  Detect forward declaration. If there are many declarations
   --  we should not try do interpret them, 'cause it may be
   --  overloading.
   --  If exist only one, Start_Position
   --  should point to it and we have to add Body_Entity reference
   --  Otherwise Start_Position should point directly to the body.
   --  We should also try to find GPS declaration created during
   --  FD processing and not create new declaration.
   if Sym.Symbol = MI then
      Decl_Info      := Find_First_Forward_Declaration (MI_Tab);
      if Decl_Info /= null then -- Body_Entity is inserted only w/ fwd decl
         Body_Position  := Sym.Start_Position;
      end if;
   else
      --  Try to find forward declaration
      Decl_Info      := Find_First_Forward_Declaration (FU_Tab);
      if Decl_Info /= null then -- Body_Entity is inserted only w/ fwd decl
         Body_Position  := Sym.Start_Position;
      end if;
   end if;

   if Decl_Info = null then
      Insert_Declaration
        (Handler               => LI_Handler (Global_CPP_Handler),
         File                  => Global_LI_File,
         List                  => Global_LI_File_List,
         Symbol_Name           => Fu_Id,
         Source_Filename       =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location              => Start_Position,
         Kind                  => Target_Kind,
         Scope                 => Global_Scope,
         End_Of_Scope_Location => End_Position,
         Declaration_Info      => Decl_Info);
   else
      Set_End_Of_Scope (Decl_Info, End_Position);
   end if;

   if Body_Position /= Invalid_Point then
      Insert_Reference
        (Decl_Info,
         Global_LI_File,
         Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Body_Position,
         Body_Entity);
   end if;


   --  Declaration inserted. Now we need to check the body for external
   --  references.

   tmp_int := 1;
   To_String (Sym.Symbol, Sym_Type, tmp_int);
   Set_Cursor
     (SN_Table (TO),
      Position => By_Key,
      Key => Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & Field_Sep &
             Fu_Id &
             Field_Sep & Sym_Type.all,
      Exact_Match => False);

   loop
         P := Get_Pair (SN_Table (TO), Next_By_Key);
         exit when P = null;

         declare
            --  FIXME:
            --  SN has only line number in .to table. So, to get exact
            --  position we are getting that line from source file and
            --  calling corresponding handler for EVERY
            --  occurrence of that symbol in that line.
            Ref      : TO_Table := Parse_Pair (P.all);
            Src_Line : String := File_Buffer.Get_Line (Ref.Position.Line);
            PRef     : TO_Table;  -- Ref with exact position
            P        : Natural := Src_Line'First; -- index to start from
            S        : String  :=
                  Ref.Buffer (Ref.Referred_Symbol_Name.First
                                    .. Ref.Referred_Symbol_Name.Last);
            Matches  : Match_Array (0 .. 0);
            Pat      : Pattern_Matcher := Compile ("\b" & S & "\b");
            Our_Ref  : Boolean;
         begin
            if Sym.Symbol = MI then
               Our_Ref := Cmp_Arg_Types
                 (Ref.Buffer,
                  MI_Tab.Buffer,
                  Ref.Caller_Argument_Types,
                  MI_Tab.Arg_Types);
            else
               Our_Ref := Cmp_Arg_Types
                 (Ref.Buffer,
                  FU_Tab.Buffer,
                  Ref.Caller_Argument_Types,
                  FU_Tab.Arg_Types);
            end if;

            if Our_Ref then
               loop
                  Match (Pat, Src_Line (P .. Src_Line'Last), Matches);
                  exit when Matches (0) = No_Match or Ref.Symbol = Undef;
                  P := Matches (0).Last + 1;
                  PRef := Ref;
                  --  conversion to column
                  PRef.Position.Column := Matches (0).First - Src_Line'First;
                  if (Fu_To_Handlers (Ref.Referred_Symbol) /= null) then
                     Fu_To_Handlers (Ref.Referred_Symbol)(PRef);
                  end if;
               end loop;
            end if;
            Free (Ref);
         end;

         Free (P);
   end loop;

   if Sym.Symbol = MI then
      Free (MI_Tab);
   else
      Free (FU_Tab);
   end if;

exception
   when DB_Error => null; -- non-existent table .to

end Sym_FU_Handler;
