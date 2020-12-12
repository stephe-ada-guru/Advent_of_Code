--  Abstract :
--
--  Advent of Code day 12
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
procedure Day_12_Rain_Risk
is
   Input_Error : exception;
   --  Verbose    : constant Boolean :=
   --    (if Ada.Command_Line.Argument_Count >= 2 then Ada.Command_Line.Argument (2) = "1" else False);
   Input_File : File_Type;

   type Position is record
      X : Integer; -- east positive
      Y : Integer; -- north positive
   end record;

   type Direction is (East, South, West, North);

   Dir : Direction := East;
   Pos : Position  := (0, 0);
begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));


   loop
      declare
         Line : constant String := Get_Line (Input_File);
         Value : constant Integer := Integer'Value (Line (2 .. Line'Last));
      begin
         case Line (1) is
         when 'E' =>
            Pos.X := @ + Value;

         when 'S' =>
            Pos.Y := @ - Value;

         when 'W' =>
            Pos.X := @ - Value;

         when 'N' =>
            Pos.Y := @ + Value;

         when 'R' =>
            --  Value is positive multiple of 90 degrees
            pragma Assert (Value > 0 and (Value / 90) *  90 = Value);
            for I in 1 .. Value / 90 loop
               case Dir is
               when East =>
                  Dir := South;

               when South =>
                  Dir := West;

               when West =>
                  Dir := North;

               when North =>
                  Dir := East;
               end case;
            end loop;

         when 'L' =>
            --  Value is positive multiple of 90 degrees
            pragma Assert (Value > 0 and (Value / 90) * 90 = Value);
            for I in 1 .. Value / 90 loop
               case Dir is
               when East =>
                  Dir := North;

               when South =>
                  Dir := East;

               when West =>
                  Dir := South;

               when North =>
                  Dir := West;
               end case;
            end loop;

         when 'F' =>
            case Dir is
            when East =>
               Pos.X := @ + Value;

            when South =>
               Pos.Y := @ - Value;

            when West =>
               Pos.X := @ - Value;

            when North =>
               Pos.Y := @ + Value;
            end case;

         when others =>
            raise Input_Error;
         end case;
      end;
      exit when End_Of_File (Input_File);
   end loop;

      Put_Line ("distance:" & Integer'Image (abs Pos.X + abs Pos.Y));
exception
when E : others =>
   declare
      use Ada.Exceptions;
   begin
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end;
end Day_12_Rain_Risk;
