--  Abstract :
--
--  Advent of Code day 8
--
--  Copyright (C) 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
procedure Day_8_Handheld_Halting
is
   Verbose : constant Boolean := Ada.Command_Line.Argument_Count > 1;

   type Op_Code is (Acc, Jmp, Nop);

   type Instruction is record
      Op : Op_Code;
      Arg : Integer;
   end record;

   function Image (Item : in Instruction) return String
   is (Item.Op'Image & " " & Item.Arg'Image);

   package Instruction_Arrays is new Ada.Containers.Vectors (Natural, Instruction);

   Code : Instruction_Arrays.Vector;

   Input_File : File_Type;
begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   loop
      declare
         Line : constant String := Get_Line (Input_File);
      begin
         Code.Append ((Op => Op_Code'Value (Line (1 .. 3)), Arg => Integer'Value (Line (4 .. Line'Last))));
      end;
      exit when End_Of_File (Input_File);
   end loop;

   declare
      Accumulator  : Integer := 0;

      function Run return Boolean
      is
         Executed : array (Code.First_Index .. Code.Last_Index) of Boolean := (others => False);

         Address : Integer := Code.First_Index;
      begin
         Accumulator := 0;

         loop
            exit when Address > Code.Last_Index;
            exit when Executed (Address);

            if Verbose then
               Put_Line ("execute" & Address'Image & ": " & Image (Code (Address)));
            end if;

            Executed (Address) := True;
            case Code (Address).Op is
            when Acc =>
               Accumulator := @ + Code (Address).Arg;
               Address     := @ + 1;

            when Jmp =>
               Address := @ + Code (Address).Arg;

            when Nop =>
               Address := @ + 1;

            end case;
         end loop;

         return Address > Code.Last_Index;
      end Run;

      Fix_Count : Integer := 0;
      Succeed   : Boolean := False;

      function Try_Fix (Fix_Address : in Integer; Fix_Op : in Op_Code) return Boolean
      is
         Saved_Op : constant Op_Code := Code (Fix_Address).Op;
      begin
         if Verbose then
            Put_Line ("fixing address" & Fix_Address'Image & ": " & Image (Code (Fix_Address)));
         end if;

         Code (Fix_Address).Op := Fix_Op;
         Fix_Count := @ + 1;
         return Result : constant Boolean := Run do
            Code (Fix_Address).Op := Saved_Op;
         end return;
      end Try_Fix;

   begin
      --  Brute force; change each instruction, see if it fixes the problem
      for Fix_Address in Code.First_Index .. Code.Last_Index loop
         case Code (Fix_Address).Op is
         when Acc =>
            Succeed := False;

         when Jmp =>
            Succeed := Try_Fix (Fix_Address, Nop);

         when Nop =>
            Succeed := Try_Fix (Fix_Address, Jmp);
         end case;

         if Succeed then
            Put_Line ("fix_count:" & Fix_Count'Image);
            Put_Line ("accumulator:" & Accumulator'Image);
            exit;
         end if;
      end loop;
   end;

end Day_8_Handheld_Halting;
