unit PanelDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Generics.Collections,
  Vcl.Samples.Spin;

type
  TXmlMarkers = (xmConfiguration, xmUsername, xmPassword, xmServer, xmPort);

  TFormDemo = class(TForm)
    pnlTop: TPanel;
    pnlMain: TPanel;
    btnSave: TButton;
    edtUserName: TEdit;
    sePort: TSpinEdit;
    lblUser: TLabel;
    lblPort: TLabel;
    lblServer: TLabel;
    edtServer: TEdit;
    edtPassword: TEdit;
    lblPassword: TLabel;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    FDictMarkers: TDictionary<Integer, string>;
  public
    { Public declarations }
  end;

var
  FormDemo: TFormDemo;

implementation

uses
  uPanel2XML, IOUtils;

{$R *.dfm}

procedure TFormDemo.btnSaveClick(Sender: TObject);
var
  Panel2Xml: TPanel2Xml;
  logLine: string;
begin
// action for saving xml file
  Panel2Xml := TPanel2Xml.Create(pnlMain, FDictMarkers);
  try
    try


      if Panel2Xml.SaveXml then ShowMessage('File saved.')
                           else ShowMessage('File was not saved.')
    except
      on E: Exception do
      begin
        logLine := Format('%s - %s\n', [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), E.Message]);
        TFile.AppendAllText('logs.txt', logLine);
      end;
    end;
  finally
    Panel2Xml.Free;
  end;
end;

procedure TFormDemo.FormCreate(Sender: TObject);
begin
  // tag number - xml element names
  FDictMarkers := TDictionary<Integer, string>.Create;
  FDictMarkers.Add(Ord(xmConfiguration), 'CONFIGURATION');
  FDictMarkers.Add(Ord(xmUsername), 'USERNAME');
  FDictMarkers.Add(Ord(xmPassword), 'PASSWORD');
  FDictMarkers.Add(Ord(xmServer), 'SERVER');
  FDictMarkers.Add(Ord(xmPort), 'PORT');

  // setting compontents tags - this will define related xml elements
  pnlMain.Tag     := Ord(xmConfiguration);
  edtUserName.Tag := Ord(xmUsername);
  edtPassword.Tag := Ord(xmPassword);
  edtServer.Tag   := Ord(xmServer);
  sePort.Tag      := Ord(xmPort);

  // set order in which elements will be saved
  edtUserName.TabOrder := 1;
  edtPassword.TabOrder := 2;
  edtServer.TabOrder   := 3;
  sePort.TabOrder      := 4;

end;

procedure TFormDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDictMarkers);
end;

end.
