-------------------------------------------------------------------------------
--  The contents of this file originates from the Stephane Carrez project
--  Ada Server Faces (util-encoders-sha1).
--
--  The contents has been altered by AdaHeads K/S. The changes are primarily
--  stylistic plus removal of code that isn't directly used to complete an
--  OpenID authentication process. Changes to the actual code (logic) are not
--  marked specifically. AdaHeads K/S does NOT claim copyright on this file.
--
--  The header from the original file is included here for copyright and
--  license information:
--
--  -----------------------------------------------------------------------
--  --  util-encoders-sha1 -- Compute SHA-1 hash
--  --  Copyright (C) 2011 Stephane Carrez
--  --  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  --
--  --  Licensed under the Apache License, Version 2.0 (the "License");
--  --  you may not use this file except in compliance with the License.
--  --  You may obtain a copy of the License at
--  --
--  --      http://www.apache.org/licenses/LICENSE-2.0
--  --
--  --  Unless required by applicable law or agreed to in writing, software
--  --  distributed under the License is distributed on an "AS IS" BASIS,
--  --  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
--  --  implied.
--  --  See the License for the specific language governing permissions and
--  --  limitations under the License.
--  -----------------------------------------------------------------------

with AWS.OpenID.Encoders.Base16;
with AWS.OpenID.Encoders.Base64;

package body AWS.OpenID.Encoders.SHA1 is

   use Ada.Streams;
   use Interfaces;

   Padding : constant String
     (1 .. 64) := (1 => Character'Val (16#80#), 2 .. 64 => ASCII.NUL);

   function F1
     (B, C, D : in Unsigned_32)
      return Unsigned_32
   is ((B and C) or ((not B) and D));

   function F2
     (B, C, D : in Unsigned_32)
      return Unsigned_32
   is (B xor C xor D);

   function F3
     (B, C, D : in Unsigned_32)
      return Unsigned_32
   is ((B and C) or (B and D) or (C and D));

   function F4
     (B, C, D : in Unsigned_32)
      return Unsigned_32
   is (B xor C xor D);

   function To_Unsigned_32
     (C3, C2, C1, C0 : in Character)
      return Unsigned_32
   is (Character'Pos (C3)
       or Shift_Left (Unsigned_32 (Character'Pos (C2)), 8)
       or Shift_Left (Unsigned_32 (Character'Pos (C1)), 16)
       or Shift_Left (Unsigned_32 (Character'Pos (C0)), 24));

   ---------------
   --  Compute  --
   ---------------

   procedure Compute
     (Ctx : in out Context)
   is
      W : W_Array renames Ctx.W;
      H : H_Array renames Ctx.H;

      V, A, B, C, D, E : Unsigned_32;
   begin
      --  Step b: For t = 16 to 79 let
      --          W(t) = S^1(W(t-3) XOR W(t-8) XOR W(t-14) XOR W(t-16)).
      for I in 16 .. 79 loop
         W (I) := Rotate_Left
           (W (I - 3) xor W (I - 8) xor W (I - 14) xor W (I - 16), 1);
      end loop;

      --  Step c: Let A = H0, B = H1, C = H2, D = H3, E = H4.
      A := H (0);
      B := H (1);
      C := H (2);
      D := H (3);
      E := H (4);

      --  Step d: For t = 0 to 79 do
      --          TEMP = S^5(A) + f(t;B,C,D) + E + W(t) + K(t);
      --          E = D;  D = C;  C = S^30(B);  B = A; A = TEMP;
      for I in 0 .. 19 loop
         V := Rotate_Left (A, 5) + F1 (B, C, D) + E + W (I) + 16#5A82_7999#;
         E := D;
         D := C;
         C := Rotate_Left (B, 30);
         B := A;
         A := V;
      end loop;

      for I in 20 .. 39 loop
         V := Rotate_Left (A, 5) + F2 (B, C, D) + E + W (I) + 16#6ED9_EBA1#;
         E := D;
         D := C;
         C := Rotate_Left (B, 30);
         B := A;
         A := V;
      end loop;

      for I in 40 .. 59 loop
         V := Rotate_Left (A, 5) + F3 (B, C, D) + E + W (I) + 16#8F1B_BCDC#;
         E := D;
         D := C;
         C := Rotate_Left (B, 30);
         B := A;
         A := V;
      end loop;

      for I in 60 .. 79 loop
         V := Rotate_Left (A, 5) + F4 (B, C, D) + E + W (I) + 16#CA62_C1D6#;
         E := D;
         D := C;
         C := Rotate_Left (B, 30);
         B := A;
         A := V;
      end loop;

      --  Step e: Let H0 = H0 + A, H1 = H1 + B, H2 = H2 + C, H3 = H3 + D,
      --  H4 = H4 + E.
      H (0) := H (0) + A;
      H (1) := H (1) + B;
      H (2) := H (2) + C;
      H (3) := H (3) + D;
      H (4) := H (4) + E;
   end Compute;

   --------------
   --  Finish  --
   --------------

   procedure Finish
     (E    : in out Context;
      Hash :    out Hash_Array)
   is
      C : constant Unsigned_64 := E.Count * 8;
      N : constant Natural := E.Pos * 4 + E.Pending_Pos + 1 + 8;
   begin
      --  Pad to 512-bit block with 0x80 and 64-bit bit count at the end.
      if N <= 64 then
         Update (E, Padding (1 .. 64 - N + 1));
      else
         Update (E, Padding (1 .. 128 - N + 1));
      end if;
      pragma Assert (E.Pending_Pos = 0);
      pragma Assert (E.Pos = 14);

      E.W (14) := Unsigned_32 (Shift_Right (C, 32));
      E.W (15) := Unsigned_32 (C and 16#0ffffffff#);
      Compute (E);

      Hash (Hash'First) :=
        Stream_Element (Shift_Right (E.H (0), 24) and 16#0FF#);
      Hash (Hash'First + 1) :=
        Stream_Element (Shift_Right (E.H (0), 16) and 16#0FF#);
      Hash (Hash'First + 2) :=
        Stream_Element (Shift_Right (E.H (0), 8) and 16#0FF#);
      Hash (Hash'First + 3) :=
        Stream_Element (E.H (0) and 16#0FF#);

      Hash (Hash'First + 4) :=
        Stream_Element (Shift_Right (E.H (1), 24) and 16#0FF#);
      Hash (Hash'First + 5) :=
        Stream_Element (Shift_Right (E.H (1), 16) and 16#0FF#);
      Hash (Hash'First + 6) :=
        Stream_Element (Shift_Right (E.H (1), 8) and 16#0FF#);
      Hash (Hash'First + 7) :=
        Stream_Element (E.H (1) and 16#0FF#);

      Hash (Hash'First + 8) :=
        Stream_Element (Shift_Right (E.H (2), 24) and 16#0FF#);
      Hash (Hash'First + 9) :=
        Stream_Element (Shift_Right (E.H (2), 16) and 16#0FF#);
      Hash (Hash'First + 10) :=
        Stream_Element (Shift_Right (E.H (2), 8) and 16#0FF#);
      Hash (Hash'First + 11) :=
        Stream_Element (E.H (2) and 16#0FF#);

      Hash (Hash'First + 12) :=
        Stream_Element (Shift_Right (E.H (3), 24) and 16#0FF#);
      Hash (Hash'First + 13) :=
        Stream_Element (Shift_Right (E.H (3), 16) and 16#0FF#);
      Hash (Hash'First + 14) :=
        Stream_Element (Shift_Right (E.H (3), 8) and 16#0FF#);
      Hash (Hash'First + 15) := Stream_Element (E.H (3) and 16#0FF#);

      Hash (Hash'First + 16) :=
        Stream_Element (Shift_Right (E.H (4), 24) and 16#0FF#);
      Hash (Hash'First + 17) :=
        Stream_Element (Shift_Right (E.H (4), 16) and 16#0FF#);
      Hash (Hash'First + 18) :=
        Stream_Element (Shift_Right (E.H (4), 8) and 16#0FF#);
      Hash (Hash'First + 19) := Stream_Element (E.H (4) and 16#0FF#);

      --  Re-initialize for the next hash.
      E.Initialize;
   end Finish;

   --------------
   --  Finish  --
   --------------

   procedure Finish
     (E    : in out Context;
      Hash :    out Digest)
   is
      Buf : Ada.Streams.Stream_Element_Array (1 .. Hash'Length);
      for Buf'Address use Hash'Address;
      pragma Import (Ada, Buf);

      B       : AWS.OpenID.Encoders.Base16.Encoder;
      Encoded : Ada.Streams.Stream_Element_Offset;
      H       : Hash_Array;
      Last    : Ada.Streams.Stream_Element_Offset;
   begin
      Finish (E, H);
      B.Transform (Data => H, Into => Buf, Last => Last, Encoded => Encoded);
   end Finish;

   --------------
   --  Finish  --
   --------------

   procedure Finish_Base64
     (E    : in out Context;
      Hash :    out Base64_Digest)
   is
      Buf : Ada.Streams.Stream_Element_Array (1 .. Hash'Length);
      for Buf'Address use Hash'Address;
      pragma Import (Ada, Buf);

      B       : AWS.OpenID.Encoders.Base64.Encoder;
      Encoded : Ada.Streams.Stream_Element_Offset;
      H       : Hash_Array;
      Last    : Ada.Streams.Stream_Element_Offset;
   begin
      Finish (E, H);
      B.Transform (Data => H, Into => Buf, Last => Last, Encoded => Encoded);
   end Finish_Base64;

   ------------------
   --  Initialize  --
   ------------------

   overriding
   procedure Initialize
     (E : in out Context)
   is
   begin
      E.Count := 0;
      E.Pending_Pos := 0;
      E.Pos   := 0;
      E.H (0) := 16#67452301#;
      E.H (1) := 16#EFCDAB89#;
      E.H (2) := 16#98BADCFE#;
      E.H (3) := 16#10325476#;
      E.H (4) := 16#C3D2E1F0#;
   end Initialize;

   -----------------
   --  Transform  --
   -----------------

   overriding
   procedure Transform
     (E       : in     Encoder;
      Data    : in     Ada.Streams.Stream_Element_Array;
      Into    :    out Ada.Streams.Stream_Element_Array;
      Last    :    out Ada.Streams.Stream_Element_Offset;
      Encoded :    out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (E);

      Hash        : Ada.Streams.Stream_Element_Array (0 .. 19);
      Hex_Encoder : AWS.OpenID.Encoders.Base16.Encoder;
      Sha_Encoder : Context;
   begin
      Update (Sha_Encoder, Data);
      Finish (Sha_Encoder, Hash);
      Hex_Encoder.Transform (Data    => Hash,
                             Into    => Into,
                             Last    => Last,
                             Encoded => Encoded);
      Encoded := Data'Last;
   end Transform;

   --------------
   --  Update  --
   --------------

   procedure Update
     (E : in out Context;
      S : in     Ada.Streams.Stream_Element_Array)
   is
      Buf : String (1 .. S'Length);
      for Buf'Address use S'Address;
      pragma Import (Ada, Buf);
   begin
      E.Update (Buf);
   end Update;

   --------------
   --  Update  --
   --------------

   procedure Update
     (E : in out Context;
      S : in String)
   is
      I : Natural := S'First;
      N : Natural := E.Pos;
   begin
      if S'Length = 0 then
         return;
      end if;
      E.Count := E.Count + S'Length;

      --  If we have pending characters, try to make a current word with the
      --  string. If the string is not wide enough, save it in the pending
      --  context array. We can save at most 3 bytes.
      case E.Pending_Pos is
         when 1 =>
            if S'Length >= 3 then
               E.W (N) := To_Unsigned_32 (S (I + 2),
                                          S (I + 1),
                                          S (I + 0),
                                          E.Pending (1));
               E.Pending_Pos := 0;
               N := N + 1;
               if N = 16 then
                  Compute (E);
                  N := 0;
               end if;
               I := S'First + 3;
            else
               E.Pending (2) := S (I + 0);
               if I + 1 >= S'Last then
                  E.Pending (3) := S (I + 1);
                  E.Pending_Pos := 3;
               else
                  E.Pending_Pos := 2;
               end if;
               return;
            end if;

         when 2 =>
            if S'Length >= 2 then
               E.W (N) := To_Unsigned_32 (S (I + 1),
                                          S (I + 0),
                                          E.Pending (2),
                                          E.Pending (1));
               E.Pending_Pos := 0;
               N := N + 1;
               if N = 16 then
                  Compute (E);
                  N := 0;
               end if;
               I := S'First + 2;
            else
               E.Pending (3) := S (I + 0);
               E.Pending_Pos := 3;
               return;
            end if;

         when 3 =>
            E.W (N) := To_Unsigned_32 (S (I + 0),
                                       E.Pending (3),
                                       E.Pending (2),
                                       E.Pending (1));
            E.Pending_Pos := 0;
            N := N + 1;
            if N = 16 then
               Compute (E);
               N := 0;
            end if;
            I := S'First + 1;

         when others =>
            I := S'First;
      end case;

      --  Fill the 32-bit word block array.  When we have a full 512-bit block,
      --  compute the hash pass.
      while I + 3 <= S'Last loop
         E.W (N) := To_Unsigned_32 (S (I + 3),
                                    S (I + 2),
                                    S (I + 1),
                                    S (I + 0));
         I := I + 4;
         N := N + 1;
         if N = 16 then
            Compute (E);
            N := 0;
         end if;
      end loop;
      E.Pos := N;

      --  Save what remains in the pending buffer.
      N := S'Last + 1 - I;
      if N > 0 then
         E.Pending (1 .. N) := S (I .. S'Last);
         E.Pending_Pos := N;
      end if;
   end Update;

end AWS.OpenID.Encoders.SHA1;
