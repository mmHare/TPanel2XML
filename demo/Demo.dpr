program Demo;

uses
  Vcl.Forms,
  DemoForm in 'DemoForm.pas' {FormDemo},
  uComponentXmlBuilder in '..\src\uComponentXmlBuilder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.
