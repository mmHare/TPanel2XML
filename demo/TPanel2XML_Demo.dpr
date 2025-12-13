program TPanel2XML_Demo;

uses
  Vcl.Forms,
  PanelDemoForm in 'PanelDemoForm.pas' {FormDemo},
  uPanel2XML in '..\src\uPanel2XML.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.
