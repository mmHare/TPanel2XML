unit PanelDemoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Generics.Collections,
  Vcl.Samples.Spin;

type
  TXmlElementNames = (xmConfiguration, xmDatabase1, xmDatabase2, xmUsername, xmPassword, xmServer, xmPort);

  TFormDemo = class(TForm)
    pnlTop: TPanel;
    pnlMain: TPanel;
    btnSave: TButton;
    edtUser1: TEdit;
    sePort1: TSpinEdit;
    lblUser1: TLabel;
    lblPort1: TLabel;
    lblServer1: TLabel;
    edtServer1: TEdit;
    edtPassword1: TEdit;
    lblPassword1: TLabel;
    pnlDatabase1: TPanel;
    pnlDatabase2: TPanel;
    lblPassword2: TLabel;
    lblPort2: TLabel;
    lblServer2: TLabel;
    lblUserName: TLabel;
    sePort2: TSpinEdit;
    edtPassword2: TEdit;
    edtServer2: TEdit;
    edtUser2: TEdit;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDictMarkers: TDictionary<Integer, string>;

    procedure SetDictMarkers;
    procedure SetTags;
    procedure SetTabOrder;
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
//  Panel2Xml := TPanel2Xml.Create(pnlMain, FDictMarkers);

  Panel2Xml := TPanel2Xml.Create(nil, nil);
  Panel2Xml.AssignPanel(pnlMain);
  Panel2Xml.AssignDictNodeNames(FDictMarkers);

  try
    try
      Panel2Xml.WithTabOrder := True; // optional ordering param (default True)

      if Panel2Xml.SaveXml then ShowMessage('File saved.')
                           else ShowMessage('File was not saved.');
    except
      on E: Exception do
      begin
        ShowMessage('Error occurred while saving the file.');
        logLine := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ' + E.Message + sLineBreak;
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

  SetDictMarkers;
  SetTags;
  SetTabOrder;
end;

procedure TFormDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDictMarkers);
end;

procedure TFormDemo.SetDictMarkers;
begin
  if not Assigned(FDictMarkers) then Exit;

  // defining realtion between tag numbers and node names
  FDictMarkers.Add(Ord(xmConfiguration), 'CONFIGURATION');
  FDictMarkers.Add(Ord(xmDatabase1), 'DATABASE_1');
  FDictMarkers.Add(Ord(xmDatabase2), 'DATABASE_2');
  FDictMarkers.Add(Ord(xmUsername), 'USERNAME');
  FDictMarkers.Add(Ord(xmPassword), 'PASSWORD');
  FDictMarkers.Add(Ord(xmServer), 'SERVER');
  FDictMarkers.Add(Ord(xmPort), 'PORT');
end;

procedure TFormDemo.SetTags;
begin
  // setting compontents tags - this will define related xml elements
  pnlMain.Tag := Ord(xmConfiguration);

  {$region 'pnlDatabase1'}
    pnlDatabase1.Tag := Ord(xmDatabase1);
    edtUser1.Tag     := Ord(xmUsername);
    edtPassword1.Tag := Ord(xmPassword);
    edtServer1.Tag   := Ord(xmServer);
    sePort1.Tag      := Ord(xmPort);
  {$endregion}
  {$region 'pnlDatabase2'}
    pnlDatabase2.Tag := Ord(xmDatabase2);
    edtUser2.Tag     := Ord(xmUsername);
    edtPassword2.Tag := Ord(xmPassword);
    edtServer2.Tag   := Ord(xmServer);
    sePort2.Tag      := Ord(xmPort);
  {$endregion}
end;

procedure TFormDemo.SetTabOrder;
begin
  //(OPTIONAL) set order in which elements will be saved
  pnlDatabase1.TabOrder := 0;
  {$region 'pnlDatabase1'}
    edtUser1.TabOrder     := 1;
    edtPassword1.TabOrder := 2;
    edtServer1.TabOrder   := 3;
    sePort1.TabOrder      := 4;
  {$endregion}

  pnlDatabase2.TabOrder := 1;
  {$region 'pnlDatabase2'}
    edtUser2.TabOrder     := 1;
    edtPassword2.TabOrder := 2;
    edtServer2.TabOrder   := 3;
    sePort2.TabOrder      := 4;
  {$endregion}
end;

end.
