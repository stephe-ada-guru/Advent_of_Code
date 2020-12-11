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
procedure Day_11_Seating_Systems
is
   Input_Error : exception;
   --  Verbose    : constant Boolean := Ada.Command_Line.Argument_Count > 1;
   Input_File : File_Type;
   Rows       : Integer; -- up/down
   Columns    : Integer; -- left/right
begin
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

      --  We add two rows and columns of floor around the seats to simplify
      --  the 'neighbors' logic.
      type Seat_Array is array (0 .. Columns + 1, 0 .. Rows + 1) of Seat_State;
      Seats : Seat_Array;

      procedure Update_Seats (Changed : out Boolean)
      is
         New_Seats : Seat_Array := Seats;

         procedure Update (C, R : in Integer)
         is
            Neighbors : Integer := 0;
         begin
            if Seats (C, R) = '.' then
               return;
            end if;

            for I in C - 1 .. C + 1 loop
               for J in R - 1 .. R + 1 loop
                  if not (I = C and J = R) and Seats (I, J) = '#' then
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
               if Neighbors >= 4 then
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
      for C in 0 .. Columns + 1 loop
         Seats (C, 0) := '.';
         Seats (C, Rows + 1) := '.';
      end loop;

      for R in 1 .. Rows loop
         Seats (0, R) := '.';
         Seats (Columns + 1, R) := '.';
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
         Changed : Boolean := False;
         Iterations : Integer := 1;
      begin
         loop
            Update_Seats (Changed);
            exit when not Changed;
            Iterations := @ + 1;
         end loop;

         Put_Line ("Iterations:" & Iterations'Image);
      end;

      Put_Line ("final:");
      Put_Seats;
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
end Day_11_Seating_Systems;
