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
procedure Day_9_Encoding_Error
is
   --  Verbose : constant Boolean := Ada.Command_Line.Argument_Count > 2;

   type Value_Type is range -2**63 .. 2**63 - 1;
   package Integer_Arrays is new Ada.Containers.Vectors (Natural, Value_Type);

   Preamble_Length : constant Integer := Integer'Value (Ada.Command_Line.Argument (2));
   Data : Integer_Arrays.Vector;

   Input_File : File_Type;

   function Is_Sum (I : in Integer) return Boolean
   is begin
      for J in I - Preamble_Length - 1 .. I - 1 loop
         for K in J + 1 .. I - 1 loop
            if Data (J) + Data (K) = Data (I) then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Is_Sum;

   Target_I : Integer;
   Target   : Value_Type;
begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   loop
      declare
         Line : constant String := Get_Line (Input_File);
      begin
         Data.Append (Value_Type'Value (Line));
      end;
      exit when End_Of_File (Input_File);
   end loop;

   for I in Preamble_Length + 1 .. Data.Last_Index loop
      if not Is_Sum (I) then
         Target_I := I;
         Target := Data (I);
         Put_Line ("not sum:" & I'Image & Value_Type'Image (Target));
         exit;
      end if;
   end loop;

   --  Find contiguous set of at least 2 numbers that add to Target
   declare
      function Sum (I, J : in Integer) return Value_Type
      is
         Result : Value_Type := 0;
      begin
         for K in I .. I + J loop
            Result := @ + Data (K);
         end loop;
         return Result;
      end Sum;

      function Encrypt_Weak (I, J : in Integer) return Value_Type
      is
         --  Numbers are not in order, so we need to find the min and max.
         Min : Value_Type := Value_Type'Last;
         Max : Value_Type := Value_Type'First;
      begin
         for K in I .. I + J loop
            if Data (K) < Min then
               Min := Data (K);
            end if;
            if Data (K) > Max then
               Max := Data (K);
            end if;
         end loop;
         return Min + Max;
      end Encrypt_Weak;

   begin
      for I in Data.First_Index .. Target_I - 2 loop
         for J in 0 .. Target_I - I - 1 loop
            if Sum (I, J) = Target then
               Put_Line ("solution at" & I'Image & " .." & Integer'Image (I + J));
               Put_Line ("encryption weakness" & Encrypt_Weak (I, J)'Image);
               return;
            end if;
         end loop;
      end loop;
   end;
end Day_9_Encoding_Error;
