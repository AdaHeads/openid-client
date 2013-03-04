-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

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
