with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Diff_Utils;           use Diff_Utils;
with Basic_Types;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
package body Vdiff2_Command is
   use Diff_Occurrence_List;
   procedure Goto_Difference (Kernel : Kernel_Handle;
                              Link : Diff_Occurrence_Link);
   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Diff_Command_Access;
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Occurrence_List_Access;
      Action    : Handler_Action) is

   begin
      Item := new Diff_Command;
      Item.Kernel := Kernel;
      Item.List_Diff := List_Diff;
      Item.Action := Action;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Diff_Command)
      return Command_Return_Type
   is
      Context : constant Selection_Context_Access
        := Get_Current_Context (Command.Kernel);
      CurrNode : Diff_Occurrence_List.List_Node :=
        First (Command.List_Diff.all);
      Diff : Diff_Head_Access := new Diff_List_Head;
      Selected_File : String_Access;

   begin
      if Has_File_Information (File_Selection_Context_Access (Context))
        and then
          Has_Directory_Information (File_Selection_Context_Access (Context))
      then
         Selected_File := new String'
           (Directory_Information (File_Selection_Context_Access (Context)) &
            File_Information (File_Selection_Context_Access (Context)));
         while CurrNode /= Null_Node
         loop
            Diff.all := Data (CurrNode);
            exit when (Diff.File1.all = Selected_File.all
                       or else
                       Diff.File2.all = Selected_File.all);
            CurrNode := Next (CurrNode);
         end loop;
         if CurrNode /= Null_Node then
            Command.Action (Command.Kernel, Diff);
            Set_Data (CurrNode, Diff.all);
            Free (Selected_File);
            Free (Diff);
         end if;
      end if;
      return Commands.Success;
   exception
      when others =>
         return Failure;
   end Execute;

   ---------------------
   -- Next_Difference --
   ---------------------

   procedure Next_Difference (Kernel : Kernel_Handle;
                              Diff   : in out  Diff_Head_Access)
   is
      Link : Diff_Occurrence_Link;
   begin
      if Diff.Current_Diff.Next /= null then
         Diff.Current_Diff := Diff.Current_Diff.Next;
         Link := Diff.Current_Diff;
         if Link.Range1.Mark /= null
           and then Link.Range2.Mark /= null then
            Goto_Difference (Kernel, Link);
         end if;
      end if;
   end Next_Difference;

   --------------------
   -- Prev_Diference --
   --------------------

   procedure Prev_Difference (Kernel : Kernel_Handle;
                              Diff   : in out Diff_Head_Access)
   is
      Link : Diff_Occurrence_Link;
   begin
      if Diff.Current_Diff.Prev /= null then
         Diff.Current_Diff := Diff.Current_Diff.Prev;
         Link := Diff.Current_Diff;
         if Link.Range1.Mark /= null
           and then Link.Range2.Mark /= null then
            Goto_Difference (Kernel, Link);
         end if;
      end if;
   end Prev_Difference;

   ---------------------
   -- First_Diference --
   ---------------------

   procedure First_Difference (Kernel : Kernel_Handle;
                               Diff   : in out Diff_Head_Access)
   is
      Link : Diff_Occurrence_Link;
   begin
      Diff.Current_Diff := Diff.List;
      Link := Diff.Current_Diff;
      if Link.Range1.Mark /= null
        and then Link.Range2.Mark /= null then
         Goto_Difference (Kernel, Link);
      end if;
   end First_Difference;

   --------------------
   -- Last_Diference --
   --------------------

   procedure Last_Difference (Kernel : Kernel_Handle;
                              Diff   : in out Diff_Head_Access)
   is
      Link : Diff_Occurrence_Link := Diff.Current_Diff;
   begin
      while Link.Next /= null loop
         Link := Link.Next;
         --  ??? not optimized
      end loop;
      Diff.Current_Diff := Link;
      if Diff.Current_Diff.Range1.Mark /= null
        and then Diff.Current_Diff.Range2.Mark /= null then
         Goto_Difference (Kernel, Diff.Current_Diff);
      end if;
   end Last_Difference;

   --------------------
   -- Goto_Diference --
   --------------------

   procedure Goto_Difference (Kernel : Kernel_Handle;
                              Link : Diff_Occurrence_Link) is
      Args : Argument_List (1 .. 1);
   begin

      if Link.Range1.Mark /= null
        and then Link.Range2.Mark /= null then
         Args := (1 => new String'(Link.Range1.Mark.all));
         Execute_GPS_Shell_Command (Kernel, "goto_mark", Args);
         Basic_Types.Free (Args);
         Args := (1 => new String'(Link.Range2.Mark.all));
         Execute_GPS_Shell_Command (Kernel, "goto_mark", Args);
         Basic_Types.Free (Args);
      end if;
   end Goto_Difference;

end Vdiff2_Command;
