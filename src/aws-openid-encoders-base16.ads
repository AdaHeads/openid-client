-------------------------------------------------------------------------------
--  The contents of this file originates from the Stephane Carrez project
--  Ada Server Faces (util-encoders-base16).
--
--  The contents has been altered by AdaHeads K/S. The changes are primarily
--  stylistic plus removal of code that isn't directly used to complete an
--  OpenID authentication process. Changes to the actual code (logic) are not
--  marked specifically. AdaHeads K/S does NOT claim copyright on this file.
--
--  The header from the original file is included here for copyright and
--  license information:
--
--  ---------------------------------------------------------------------------
--  --  util-encoders-base16 -- Encode/Decode a stream in hexadecimal
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
--  ---------------------------------------------------------------------------

with Ada.Streams;

package AWS.OpenID.Encoders.Base16 is

   type Decoder is new Encoders.Transformer with private;
   type Encoder is new Encoders.Transformer with private;

   overriding
   procedure Transform
     (E       : in     Decoder;
      Data    : in     Ada.Streams.Stream_Element_Array;
      Into    :    out Ada.Streams.Stream_Element_Array;
      Last    :    out Ada.Streams.Stream_Element_Offset;
      Encoded :    out Ada.Streams.Stream_Element_Offset);
   --  Decodes the Data base16 input stream into the Into binary output stream.
   --
   --  If the transformer does not have enough room to write the result, it
   --  must return in Encoded the index of the last encoded position in the
   --  Data stream.
   --
   --  Last is the last valid position in the Into output stream.
   --
   --  The Encoding_Error exception is raised if the input stream cannot be
   --  transformed.

   overriding
   procedure Transform
     (E       : in     Encoder;
      Data    : in     Ada.Streams.Stream_Element_Array;
      Into    :    out Ada.Streams.Stream_Element_Array;
      Last    :    out Ada.Streams.Stream_Element_Offset;
      Encoded :    out Ada.Streams.Stream_Element_Offset);
   --  Encodes the Data base16 input stream into the Into binary output stream.
   --
   --  If the transformer does not have enough room to write the result, it
   --  must return in Encoded the index of the last encoded position in the
   --  Data stream.
   --
   --  Last is the last valid position in the Into output stream.
   --
   --  The Encoding_Error exception is raised if the input stream cannot be
   --  transformed.

private

   type Decoder is new Encoders.Transformer with null record;
   type Encoder is new Encoders.Transformer with null record;

   generic

      type Input_Char is (<>);
      type Output_Char is (<>);
      type Index is range <>;
      type Output_Index is range <>;
      type Input is array (Index range <>) of Input_Char;
      type Output is array (Output_Index range <>) of Output_Char;

   package Encoding is

      procedure Encode
        (From    : in     Input;
         Into    : in out Output;
         Last    :    out Output_Index;
         Encoded :    out Index);

      procedure Decode
        (From    : in     Input;
         Into    : in out Output;
         Last    :    out Output_Index;
         Encoded :    out Index);

   end Encoding;

end AWS.OpenID.Encoders.Base16;
