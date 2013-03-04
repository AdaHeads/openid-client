-----------------------------------------------------------------------
--  util-encoders-sha1 -- Compute SHA-1 hash
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Ada.Finalization;
with Ada.Streams;
with Interfaces;

package AWS.OpenID.Encoders.SHA1 is

   subtype Hash_Array is Ada.Streams.Stream_Element_Array (0 .. 19);

   subtype Digest is String (1 .. 40);

   subtype Base64_Digest is String (1 .. 28);

   type Context is limited private;

   procedure Finish
     (E    : in out Context;
      Hash :    out Hash_Array);
   --  Computes the SHA1 hash and returns the raw binary hash in Hash.

   procedure Finish
     (E    : in out Context;
      Hash :    out Digest);
   --  Computes the SHA1 hash and returns the hexadecimal hash in Hash.

   procedure Finish_Base64
     (E    : in out Context;
      Hash :    out Base64_Digest);
   --  Computes the SHA1 hash and returns the base64 hash in Hash.

   procedure Update
     (E : in out Context;
      S : in     String);
   --  Update the hash with the string.

   procedure Update
     (E : in out Context;
      S : in     Ada.Streams.Stream_Element_Array);
   --  Update the hash with the string.

   type Encoder is new AWS.OpenID.Encoders.Transformer with private;

   overriding
   procedure Transform
     (E       : in Encoder;
      Data    : in Ada.Streams.Stream_Element_Array;
      Into    : out Ada.Streams.Stream_Element_Array;
      Last    : out Ada.Streams.Stream_Element_Offset;
      Encoded : out Ada.Streams.Stream_Element_Offset);
   --  Transform the Data input stream into the Into output stream. If the
   --  transformer does not have enough room to write the result, it must
   --  return in Encoded the index of the last encoded position in the Data
   --  stream.
   --
   --  Last is the last valid position in the Into output stream.
   --
   --  The Encoding_Error exception is raised if the input stream cannot be
   --  transformed.

private

   use Interfaces;

   type Encoder is new AWS.OpenID.Encoders.Transformer with null record;

   type H_Array is array (0 .. 4) of Unsigned_32;
   type W_Array is array (0 .. 79) of Unsigned_32;

   type Context is new Ada.Finalization.Limited_Controlled with
      record
         W           : W_Array;
         H           : H_Array;
         Pos         : Natural;
         Count       : Unsigned_64;
         Pending     : String (1 .. 3);
         Pending_Pos : Natural;
      end record;

   --  Process the message block collected in the context.
   procedure Compute
     (Ctx : in out Context);

   overriding
   procedure Initialize
     (E : in out Context);
   --  Initialize the SHA-1 context.

end AWS.OpenID.Encoders.SHA1;
