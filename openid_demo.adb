--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Ada.Text_IO;
with
  AWS.Log,
  AWS.Net.SSL,
  AWS.Server.Log;
with
  OpenID_Handler;

procedure OpenID_Demo is
   Secure, Plain : AWS.Server.HTTP;
begin
   Ada.Text_IO.Put_Line ("AWS " & AWS.Version);
   Ada.Text_IO.Put_Line ("Enter 'q' key to exit...");

   if AWS.Net.SSL.Is_Supported then
      AWS.Server.Start
        (Web_Server     => Secure,
         Name           => "OpenID_Demo",
         Max_Connection => 10,
         Port           => 443,
         Security       => True,
         Callback       => OpenID_Handler.Service'Access);
      AWS.Server.Log.Start (Web_Server      => Secure,
                            Filename_Prefix => "openid-secure",
                            Split_Mode      => AWS.Log.Daily);
      Ada.Text_IO.Put_Line ("Listening on port 443.");
   else
      AWS.Server.Start
        (Web_Server     => Plain,
         Name           => "OpenID_Demo",
         Max_Connection => 10,
         Port           => 80,
         Security       => True,
         Callback       => OpenID_Handler.Service'Access);
      AWS.Server.Log.Start (Web_Server      => Plain,
                            Filename_Prefix => "openid-plain");
      Ada.Text_IO.Put_Line ("Listening on port 80.");
   end if;

   AWS.Server.Wait (AWS.Server.Q_Key_Pressed);

   if AWS.Net.SSL.Is_Supported then
      AWS.Server.Shutdown (Secure);
   else
      AWS.Server.Shutdown (Plain);
   end if;
end OpenID_Demo;
