--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Ada.Command_Line,
  Ada.Text_IO;
with
  AWS.Log,
  AWS.Net.SSL,
  AWS.OpenID.State,
  AWS.Server,
  AWS.Server.Log,
  AWS.Server.Status,
  AWS.URL;
with
  OpenID_Handler;

procedure OpenID_Demo is
   Web_Server : AWS.Server.HTTP;
begin
   Ada.Text_IO.Put_Line ("AWS " & AWS.Version);
   Ada.Text_IO.Put_Line ("Enter 'q' key to exit...");

   if AWS.Net.SSL.Is_Supported then
      AWS.Server.Start
        (Web_Server     => Web_Server,
         Name           => "OpenID_Demo",
         Max_Connection => 10,
         Port           => AWS.URL.Default_HTTPS_Port,
         Security       => True,
         Session        => True,
         Callback       => OpenID_Handler.Service'Access);
   else
      Ada.Text_IO.Put_Line ("Compiled without support for secure HTTP.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   AWS.OpenID.State.Load (File_Name           => "openid_demo.state",
                          Suppress_Exceptions => True);
   AWS.Server.Log.Start (Web_Server      => Web_Server,
                         Filename_Prefix => "openid_demo",
                         Split_Mode      => AWS.Log.Daily);

   Ada.Text_IO.Put_Line ("Please visit <" &
                         AWS.Server.Status.Local_URL (Web_Server) & ">.");

   AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
   AWS.OpenID.State.Save (File_Name => "openid_demo.state");
   AWS.Server.Shutdown (Web_Server);
end OpenID_Demo;
