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
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
procedure Day_10_Adapter_Array
is
   Verbose : constant Boolean := Ada.Command_Line.Argument_Count > 1;

   --  We need to sort the adaptors in increasing joltage value, so we use a list.
   type Joltage is range 0 .. Integer'Last;
   package Joltage_Lists is new Ada.Containers.Doubly_Linked_Lists (Joltage);
   package Sorting is new Joltage_Lists.Generic_Sorting;

   Data_List : Joltage_Lists.List;

   Input_File : File_Type;

   Count_Diffs : array (Joltage range 1 .. 3) of Integer := (others => 0);

begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   loop
      declare
         Line : constant String := Get_Line (Input_File);
      begin
         Data_List.Append (Joltage'Value (Line));
      end;
      exit when End_Of_File (Input_File);
   end loop;

   Sorting.Sort (Data_List);

   Part_1 :
   declare
      Last : Joltage := 0;
   begin
      for N of Data_List loop
         Count_Diffs (N - Last) := @ + 1;
         Last := N;
      end loop;

      --  Internal adapter always has a diff of 3
      Count_Diffs (3) := @ + 1;

      Put_Line ("1:" & Count_Diffs (1)'Image & " 2:" & Count_Diffs (2)'Image & " 3:" & Count_Diffs (3)'Image);

      Put_Line ("part 1 answer:" & Integer'Image (Count_Diffs (1) * Count_Diffs (3)));
   end Part_1;

   Part_2 :
   declare
      --  To find the number of acceptable arrangements for part 2, we check
      --  whether each adapter can be skipped; each skip gives a factor of
      --  2. But skipping too many consecutive adapters is not valid, so
      --  that subtracts some. That happens in the second test input, and we
      --  assume in the full input.
      --
      --  Consider the adaptors: 1 2 3 4 5 6. 2, 3, 4, 5 can each be skipped
      --  individually, and in pairs, but not three or four together. Thus
      --  the multiplier for n consecutive skips that all differ by 1 is n
      --  choose 2.
      --
      --  Consider the adaptors: 1 2 3 5 6; same as above, but 3 5 is not a
      --  valid skip pair. So for each consective skip list, we enumerate
      --  all possible combinations.

      --  We want an array for simple access, and include 0 and internal adaptors.
      type Extended_Data_Index is range 0 .. Integer'Last;
      subtype Data_Index is Extended_Data_Index range 1 .. Extended_Data_Index (Data_List.Length) + 2;
      Data : array (Data_Index) of Joltage;

      function Count_Skip_List (J, K : in Data_Index) return Integer
      is
         --  Data (J), Data (K) are in all combinations; adaptors between them
         --  may be skipped.
         subtype List_Index is Data_Index range 1 .. K - J + 1;
         type Adaptor_Array is array (List_Index) of Joltage;
         subtype Combo_Index is Integer range 1 .. 2 ** Natural ((K - J - 1));
         Combinations : array (Combo_Index) of Adaptor_Array := (others => (others => 0));
         Last_Adaptor : array (Combo_Index) of Extended_Data_Index := (others => 0);
         Last_Combo   : Combo_Index                                := 1;
      begin
         Combinations (1)(1) := Data (J);
         Last_Adaptor (1)    := 1;
         for I in J + 1 .. K loop
            for Combo in 1 .. Last_Combo loop
               --  Find new combination
               if I < K then
                  if Combinations (Combo)(Last_Adaptor (Combo)) + 3 >= Data (I + 1) then
                     --  Can skip adaptor I
                     Last_Combo := @ + 1;
                     Combinations (Last_Combo) := Combinations (Combo);
                     Last_Adaptor (Last_Combo) := Last_Adaptor (Combo);
                  end if;
               end if;

               --  Extend current combination if valid
               if Combinations (Combo)(Last_Adaptor (Combo)) + 3 >= Data (I) then
                  --  Valid combination
                  Last_Adaptor (Combo) := @ + 1;
                  Combinations (Combo)(Last_Adaptor (Combo)) := Data (I);
               end if;
            end loop;
         end loop;

         return Count : Integer := 0 do
            for Combo in 1 .. Last_Combo loop
               if Combinations (Combo)(Last_Adaptor (Combo)) = Data (K) then
                  Count := @ + 1;
               end if;
            end loop;

            if Verbose then
               Put_Line
                 ("skip list:" & J'Image & ":" & Data (J)'Image & " .." &
                    K'Image & ":" & Data (K)'Image &
                    "; factor:" & Count'Image);
            end if;
         end return;
      end Count_Skip_List;

      --  The total number of arrangements is larger than fits in 64 bits; use Emacs bignums
      package Integer_Lists is new Ada.Containers.Doubly_Linked_Lists (Integer);
      Factors : Integer_Lists.List;

      First : Data_Index := 1; --  first adaptor in a possible consecutive skip list.
   begin
      declare
         Next : Data_Index := 1;
      begin
         Data (Next) := 0;
         Next := @ + 1;
         for N of Data_List loop
            Data (Next) := N;
            Next := @ + 1;
         end loop;
         Data (Next) := Data (Next - 1) + 3;
      end;
      if Verbose then
         Put ("Data sorted:");
         for N of Data loop
            Put (N'Image);
         end loop;
         New_Line;
      end if;

      Outer_Loop :
      loop
         declare
            Prev : Data_Index := First;
            Last : Data_Index := Prev + 1;
            Next : Data_Index := Last + 1;
         begin
            Inner_Loop :
            loop
               exit Inner_Loop when Data (Prev) + 3 < Data (Next);
               exit Inner_Loop when Next = Data'Last;
               Prev := @ + 1;
               Last := Prev + 1;
               Next := Last + 1;
            end loop Inner_Loop;
            if First + 1 < Last then
               Factors.Append (Count_Skip_List (First, Last));
            end if;

            First := Last;
            exit Outer_Loop when Next = Data'Last;
         end;
      end loop Outer_Loop;

      for F of Factors loop
         Put (F'Image);
      end loop;
      New_Line;
   end Part_2;

exception
when E : others =>
   declare
      use Ada.Exceptions;
   begin
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end;
end Day_10_Adapter_Array;
