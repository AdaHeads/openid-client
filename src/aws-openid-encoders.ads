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

with Ada.Streams;
with Ada.Finalization;
with Ada.Strings.Unbounded;

package AWS.OpenID.Encoders is

   Encoding_Error : exception;

   type Encoding_Format is (Base64_Encoding,
                            Base64_URL_Encoding,
                            Base16_Encoding,
                            HEX_Encoding,
                            SHA1_Encoding);

   type Encoder is tagged limited private;

   function Decode
     (E    : in Encoder;
      Data : in String)
      return String;
   --  Decodes Data using the transformation rules provided by the E encoder.
   --  Returns the decoded string.
   --
   --  Raises Encoding_Error exception if the source string cannot be decoded.
   --  Raises Not_Supported exception if the decoding is not supported.

   function Create
     (Format : Encoding_Format)
      return Encoder;
   --  Create an Encoder object for the Format encoding.

   type Transformer is limited interface;
   type Transformer_Access is access all Transformer'Class;

   procedure Transform
     (E       : in     Transformer;
      Data    : in     Ada.Streams.Stream_Element_Array;
      Into    :    out Ada.Streams.Stream_Element_Array;
      Last    :    out Ada.Streams.Stream_Element_Offset;
      Encoded :    out Ada.Streams.Stream_Element_Offset)
   is abstract;
   --  Transform the Data input stream into the Into output stream. If the
   --  transformer does not have enough room to write the result, it must
   --  return in Encoded the index of the last encoded position in the Data
   --  stream.
   --
   --  Last is the last valid position in the Into output stream.
   --
   --  The Encoding_Error exception is raised if the input stream cannot be
   --  transformed.

   function Transform
     (E    : in Transformer'Class;
      Data : in String)
      return String;
   --  Transform the Data input string using the transformation rules provided
   --  by the E transformer.
   --
   --  Returns the transformed string.
   --
   --  Raises the Encoding_Error exception if the source string cannot be
   --  transformed

   procedure Transform
     (E      : in Transformer;
      Data   : in String;
      Result : out Ada.Strings.Unbounded.Unbounded_String)
   is null;

private

   type Encoder is new Ada.Finalization.Limited_Controlled with
      record
         Encode : Transformer_Access := null;
         Decode : Transformer_Access := null;
      end record;

   overriding
   procedure Finalize
     (E : in out Encoder);
   --  Delete the transformers

end AWS.OpenID.Encoders;
