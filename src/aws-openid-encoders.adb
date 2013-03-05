-------------------------------------------------------------------------------
--  The contents of this file originates from the Stephane Carrez project
--  Ada Server Faces (util-encoders).
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
--  --  util-encoders -- Encode/Decode streams and strings from one format to
--  --  another.
--  --  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Ada.Unchecked_Deallocation;

with AWS.OpenID.Encoders.Base16;
with AWS.OpenID.Encoders.Base64;
with AWS.OpenID.Encoders.SHA1;

package body AWS.OpenID.Encoders is

   MIN_BUFFER_SIZE : constant Ada.Streams.Stream_Element_Offset := 64;
   MAX_BUFFER_SIZE : constant Ada.Streams.Stream_Element_Offset := 2_048;

   function Best_Size
     (Length : Natural)
      return Ada.Streams.Stream_Element_Offset;
   --  Compute a good size for allocating a buffer on the stack

   -----------------
   --  Best_Size  --
   -----------------

   function Best_Size
     (Length : Natural)
      return Ada.Streams.Stream_Element_Offset
   is
   begin
      if Length < Natural (MIN_BUFFER_SIZE) then
         return MIN_BUFFER_SIZE;
      elsif Length > Natural (MAX_BUFFER_SIZE) then
         return MAX_BUFFER_SIZE;
      else
         return Ada.Streams.Stream_Element_Offset (((Length + 15) / 16) * 16);
      end if;
   end Best_Size;

   --------------
   --  Create  --
   --------------

   function Create
     (Format : Encoding_Format)
      return Encoder is
   begin
      case Format is
         when Base64_Encoding | Base64_URL_Encoding =>
            return E : Encoder do
               E.Encode := new AWS.OpenID.Encoders.Base64.Encoder;
               E.Decode := new AWS.OpenID.Encoders.Base64.Decoder;
            end return;
         when Base16_Encoding | HEX_Encoding =>
            return E : Encoder do
               E.Encode := new AWS.OpenID.Encoders.Base16.Encoder;
               E.Decode := new AWS.OpenID.Encoders.Base16.Decoder;
            end return;
         when SHA1_Encoding =>
            return E : Encoder do
               E.Encode := new AWS.OpenID.Encoders.SHA1.Encoder;
               E.Decode := new AWS.OpenID.Encoders.Base64.Decoder;
            end return;
      end case;
   end Create;

   --------------
   --  Decode  --
   --------------

   function Decode
     (E    : in Encoder;
      Data : in String)
      return String
   is
   begin
      return E.Decode.Transform (Data);
   end Decode;

   ----------------
   --  Finalize  --
   ----------------

   overriding
   procedure Finalize
     (E : in out Encoder)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Transformer'Class, Transformer_Access);
   begin
      Free (E.Encode);
      Free (E.Decode);
   end Finalize;

   -----------------
   --  Transform  --
   -----------------

   function Transform
     (E    : in Transformer'Class;
      Data : in String)
      return String
   is
      use Ada.Streams;
      use Ada.Strings.Unbounded;

      Buf_Size : constant Stream_Element_Offset := Best_Size (Data'Length);
      Buf      : Stream_Element_Array (1 .. Buf_Size);
      Res      : Stream_Element_Array (1 .. Buf_Size);
      Result   : Unbounded_String;
      Pos      : Natural := Data'First;
      Tmp      : String (1 .. Natural (Buf_Size));
   begin
      while Pos <= Data'Last loop
         declare
            Last_Encoded  : Stream_Element_Offset;
            First_Encoded : Stream_Element_Offset := 1;
            Last          : Stream_Element_Offset;
            Size          : Stream_Element_Offset;
            Next_Pos      : Natural;
         begin
            --  Fill the stream buffer with our input string
            Size := Stream_Element_Offset (Data'Last - Pos + 1);

            if Size > Buf'Length then
               Size := Buf'Length;
            end if;

            for I in 1 .. Size loop
               Buf (I) := Character'Pos (Data (Natural (I) + Pos - 1));
            end loop;

            Next_Pos := Pos + Natural (Size);

            --  Encode that buffer and put the result in out result string.
            loop
               E.Transform (Data    => Buf (First_Encoded .. Size),
                            Into    => Res,
                            Encoded => Last_Encoded,
                            Last    => Last);

               --  If the encoder generated nothing, move the position backward
               --  to take into account the remaining bytes not taken into
               --  account.
               if Last < 1 then
                  Next_Pos := Next_Pos - Natural (Size - First_Encoded + 1);
                  exit;
               end if;

               for I in 1 .. Last loop
                  Tmp (Natural (I)) := Character'Val (Res (I));
               end loop;

               Append (Result, Tmp (1 .. Natural (Last)));

               exit when Last_Encoded = Size;

               First_Encoded := Last_Encoded + 1;
            end loop;

            --  The encoder cannot encode the data
            if Pos = Next_Pos then
               raise Encoding_Error with "Encoding cannot proceed";
            end if;

            Pos := Next_Pos;
         end;
      end loop;

      return To_String (Result);
   end Transform;

end AWS.OpenID.Encoders;
