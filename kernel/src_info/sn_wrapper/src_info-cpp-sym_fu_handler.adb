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
   MD_Tab         : MD_Table;
   Start_Position : Point := Sym.Start_Position;
   Body_Position  : Point := Invalid_Point;
   End_Position   : Point;

   function Find_First_Forward_Declaration
     (Fn : FU_Table) return E_Declaration_Info_List;
   --  Attempts to find the first forward declaration
   --  for the function. Returns null if not found or
   --  forward declaration is in another file which
   --  has not been yet processed

   function Find_First_Forward_Declaration
     (Fn : FU_Table) return E_Declaration_Info_List is
      P            : Pair_Ptr;
      FD_Tab       : FD_Table;
      FD_Tab_Tmp   : FD_Table;
      First_FD_Pos : Point := Invalid_Point;
   begin
      --  First we have to find the first forward declaration
      --  that corresponds to our function, that is prototypes
      --  should be the same
      --  ??? What should we do when forward declarations are
      --  found in different files, which to choose?
      --  So far we take the first one.
      --  ??? What should we do when forward declaration located
      --  in another file has not been processed yet?
      Set_Cursor
        (SN_Table (FD),
         By_Key,
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (SN_Table (FD), Next_By_Key);
         if P = null then -- no fwd decls at all
            return null;
         end if;
         FD_Tab := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         exit when Cmp_Prototypes
              (FD_Tab.Buffer,
               Fn.Buffer,
               FD_Tab.Arg_Types,
               Fn.Arg_Types,
               FD_Tab.Return_Type,
               Fn.Return_Type);
         Free (FD_Tab);
      end loop;

      --  now find the first declaration in the file
      Set_Cursor
        (SN_Table (FD),
         By_Key,
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) & Field_Sep,
         False);

      loop
         P := Get_Pair (SN_Table (FD), Next_By_Key);
         exit when P = null;
         FD_Tab_Tmp := Parse_Pair (P.all);
         Free (P);
         --  Update position of the first forward declaration
         if FD_Tab.Buffer (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last)
            = FD_Tab_Tmp.Buffer (FD_Tab_Tmp.File_Name.First ..
                                 FD_Tab_Tmp.File_Name.Last)
            and then Cmp_Prototypes
              (FD_Tab.Buffer,
               FD_Tab_Tmp.Buffer,
               FD_Tab.Arg_Types,
               FD_Tab_Tmp.Arg_Types,
               FD_Tab.Return_Type,
               FD_Tab_Tmp.Return_Type)
            and then ((First_FD_Pos = Invalid_Point)
            or else Less (FD_Tab_Tmp.Start_Position, First_FD_Pos)) then
            First_FD_Pos := FD_Tab_Tmp.Start_Position;
         end if;
         Free (FD_Tab_Tmp);
      end loop;

      pragma Assert (First_FD_Pos /= Invalid_Point);
      begin
         return Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Sym.Buffer
                             (Sym.Identifier.First .. Sym.Identifier.Last),
            Location    => First_FD_Pos);
      exception
         when Declaration_Not_Found =>
            return null;
      end;
   end Find_First_Forward_Declaration;

begin
   Info ("Sym_FU_Hanlder: """
         & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "."
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   if Sym.Symbol = MI then
      begin
         MI_Tab := Find (SN_Table (MI),
             Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
             Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
         if MI_Tab.Buffer (MI_Tab.Return_Type.First
                                 .. MI_Tab.Return_Type.Last) = "void" then
            Target_Kind := Non_Generic_Procedure;
         else
            Target_Kind := Non_Generic_Function_Or_Operator;
         end if;
         End_Position := MI_Tab.End_Position;
      exception
         when DB_Error | Not_Found =>
            Fail ("unable to find method " &
                  Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "." &
                  Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
      end;
   else
      begin
         FU_Tab := Find (SN_Table (FU),
             Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
         if FU_Tab.Buffer (FU_Tab.Return_Type.First
                                    .. FU_Tab.Return_Type.Last) = "void" then
            Target_Kind := Non_Generic_Procedure;
         else
            Target_Kind := Non_Generic_Function_Or_Operator;
         end if;
         End_Position := FU_Tab.End_Position;
      exception
         when DB_Error | Not_Found =>
            Fail ("unable to find function " &
                  Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
      end;
   end if;

   --  Detect forward declaration. If there are many declarations
   --  we should not try do interpret them, 'cause it may be
   --  overloading.
   --  If exist only one, Start_Position
   --  should point to it and we have to add Body_Entity reference
   --  Otherwise Start_Position should point directly to the body
   --  We should also try to find GPS declaration created during
   --  FD processing and not create new declaration
   if Sym.Symbol = MI then
      Free (MI_Tab);
      begin
         Set_Cursor
           (SN_Table (MD),
            By_Key,
            Sym.Buffer (Sym.Class.First .. Sym.Class.Last)
               & Field_Sep
               & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
               & Field_Sep,
            False);
         P := Get_Pair (SN_Table (MD), Next_By_Key);
         if P /= null then
            MD_Tab := Parse_Pair (P.all);
            Free (P);
            P := Get_Pair (SN_Table (MD), Next_By_Key);
            if P = null then -- not overloaded fwd declaration
               Body_Position  := Sym.Start_Position;
               Start_Position := MD_Tab.Start_Position;
               begin -- locate forward declaration
                  Decl_Info := Find_Declaration
                    (File        => Global_LI_File,
                     Symbol_Name => Sym.Buffer
                           (Sym.Identifier.First .. Sym.Identifier.Last),
                     Class_Name  => Sym.Buffer
                           (Sym.Class.First .. Sym.Class.Last),
                     Location    => MD_Tab.Start_Position);
               exception
                  when Declaration_Not_Found =>
                     null;
               end;
            else -- overloaded or many forward declarations
               Free (P);
            end if;
            Free (MD_Tab);
         end if;
      exception
         when DB_Error =>
            null;
      end;
   else
      --  Try to find forward declaration
      Decl_Info      := Find_First_Forward_Declaration (FU_Tab);
      Body_Position  := Sym.Start_Position;
      Start_Position.Line   := Decl_Info.Value.Declaration.Location.Line;
      Start_Position.Column := Decl_Info.Value.Declaration.Location.Column;
      Free (FU_Tab);
   end if;

   if Decl_Info = null then
      Insert_Declaration
        (Handler               => LI_Handler (Global_CPP_Handler),
         File                  => Global_LI_File,
         List                  => Global_LI_File_List,
         Symbol_Name           =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename       =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location              => Start_Position,
         Kind                  => Target_Kind,
         Scope                 => Global_Scope,
         End_Of_Scope_Location => End_Position,
         Declaration_Info      => Decl_Info);
   else
      Decl_Info.Value.Declaration.End_Of_Scope.Kind := End_Of_Body;
      Decl_Info.Value.Declaration.End_Of_Scope.Location.Line
         := End_Position.Line;
      Decl_Info.Value.Declaration.End_Of_Scope.Location.Column
         := End_Position.Column;
      Decl_Info.Value.Declaration.End_Of_Scope.Location.File :=
        (LI               => Global_LI_File,
         Part             => Unit_Body,
         Source_Filename  => new String'(Get_LI_Filename (Global_LI_File)));
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
             Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) &
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
         begin
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
            Free (Ref);
         end;

         Free (P);
   end loop;

exception
   when DB_Error => null; -- non-existent table .to

end Sym_FU_Handler;
