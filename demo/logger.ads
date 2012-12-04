package Logger is
   procedure Open (File_Name : in     String);
   procedure Close;

   procedure Info    (Message : in     String);
   procedure Debug   (Message : in     String);
   procedure Error   (Message : in     String);
   procedure Warning (Message : in     String);
end Logger;
