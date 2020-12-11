--  Abstract :
--
--  day 11
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
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
procedure Day_11_Seating_Systems_Part_2
is
   Input_Error : exception;
   Verbose    : constant Boolean :=
     (if Ada.Command_Line.Argument_Count >= 2 then Ada.Command_Line.Argument (2) = "1" else False);
   Max_Iterations : constant Integer :=
     (if Ada.Command_Line.Argument_Count >= 3 then Integer'Value (Ada.Command_Line.Argument (3)) else 0);
   Input_File : File_Type;
   Rows       : Integer; -- up/down
   Columns    : Integer; -- left/right
begin
   if Max_Iterations > 0 then
      Put_Line ("max_iterations" & Max_Iterations'Image);
   end if;

   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   --  Get seating rows, columns.
   declare
      Line : constant String := Get_Line (Input_File);
   begin
      Columns := Line'Length;
      Rows := 1;
   end;

   loop
      Skip_Line (Input_File);
      Rows := @ + 1;
      exit when End_Of_File (Input_File);
   end loop;

   declare
      type Seat_State is ('.', 'L', '#');

      type Seat_Array is array (1 .. Columns, 1 .. Rows) of Seat_State;
      Seats : Seat_Array;

      procedure Update_Seats (Changed : out Boolean)
      is
         New_Seats : Seat_Array := Seats;

         procedure Update (C, R : in Integer)
         is
            Neighbors : Integer := 0;

            function See_Seat (I, J : in Integer) return Seat_State
            is
               K : Integer := C;
               L : Integer := R;
            begin
               loop
                  K := K + I;
                  L := L + J;

                  if not (K in 1 .. Columns) or not (L in 1 .. Rows) then
                     return '.';
                  end if;
                  exit when Seats (K, L) in 'L' | '#';
               end loop;
               return Seats (K, L);
            end See_Seat;

         begin
            if Seats (C, R) = '.' then
               return;
            end if;

            for I in -1 .. +1 loop
               for J in -1 .. +1 loop
                  if not (I = 0 and J = 0) and See_Seat (I, J) = '#' then
                     Neighbors := Neighbors + 1;
                  end if;
               end loop;
            end loop;

            case Seats (C, R) is
            when '.' =>
               null;

            when 'L' =>
               if Neighbors = 0 then
                  New_Seats (C, R) := '#';
                  Changed := True;
               end if;

            when '#' =>
               if Neighbors >= 5 then
                  New_Seats (C, R) := 'L';
                  Changed := True;
               end if;
            end case;
         end Update;

      begin
         Changed := False;

         for C in 1 .. Columns loop
            for R in 1 .. Rows loop
               Update (C, R);
            end loop;
         end loop;

         if Changed then
            Seats := New_Seats;
         end if;
      end Update_Seats;

      function Count_Occupied return Integer
      is
         Result : Integer := 0;
      begin
         for C in 1 .. Columns loop
            for R in 1 .. Rows loop
               if Seats (C, R) = '#' then
                  Result := @ + 1;
               end if;
            end loop;
         end loop;
         return Result;
      end Count_Occupied;

      procedure Put_Seats
      is begin
         for R in 1 .. Rows loop
            for C in 1 .. Columns loop
               Put
                 (case Seats (C, R) is
                  when '.' => '.',
                  when 'L' => 'L',
                  when '#' => '#');
            end loop;
            New_Line;
         end loop;
      end Put_Seats;

   begin
      --  Read in seats
      Reset (Input_File);
      for R in 1 .. Rows loop
         declare
            Line : constant String := Get_Line (Input_File);
         begin
            for C in 1 .. Columns loop
               Seats (C, R) :=
                 (case Line (C) is
                  when '.' => '.',
                  when 'L' => 'L',
                  when others => raise Input_Error);
            end loop;
         end;
      end loop;

      --  Loop until stable
      declare
         Changed    : Boolean := False;
         Iterations : Integer := 0;
      begin
         loop
            Update_Seats (Changed);
            exit when not Changed;
            Iterations := @ + 1;
            if Verbose then
               New_Line;
               Put_Line ("Iteration:" & Iterations'Image);
               Put_Seats;
            end if;
            exit when Max_Iterations > 0 and Iterations >= Max_Iterations;
         end loop;
         if not Verbose then
            Put_Line ("Iterations:" & Iterations'Image);
         else
            New_Line;
         end if;
      end;

      Put_Line ("occupied seats" & Count_Occupied'Image);
   end;

exception
when E : others =>
   declare
      use Ada.Exceptions;
   begin
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end;
end Day_11_Seating_Systems_Part_2;
