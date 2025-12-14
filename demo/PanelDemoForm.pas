unit PanelDemoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Generics.Collections,
  Vcl.Samples.Spin, Vcl.CheckLst;

type
  TXmlElementNames = (xmConfiguration=1, xmDatabase1, xmDatabase2, xmGeneral,
                      xmActive, xmDbType, xmUsername, xmPassword, xmServer, xmPort,
                      xmDescription, xmUseOption);

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
    lblDatabase1: TLabel;
    cmbDbType1: TComboBox;
    lblDbType1: TLabel;
    chkActive1: TCheckBox;
    pnlDatabase2: TPanel;
    lblPassword2: TLabel;
    Label2: TLabel;
    lblServer2: TLabel;
    lblUser2: TLabel;
    lblDatabase2: TLabel;
    lblDbType2: TLabel;
    sePort2: TSpinEdit;
    edtPassword2: TEdit;
    edtServer2: TEdit;
    edtUser2: TEdit;
    cmbDbType2: TComboBox;
    chkActive2: TCheckBox;
    btnLoad: TButton;
    grpbxGeneral: TGroupBox;
    memoDescr: TMemo;
    rdgrpUseOption: TRadioGroup;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
  private
    FDictMarkers: TDictionary<Integer, string>;

    function StringEncode(pText: string) : string;
    function StringDecode(pText: string) : string;

    procedure SetDictMarkers;
    procedure SetTags;
    procedure SetTabOrder;
    procedure LoadConfig;
  public
    { Public declarations }
  end;

var
  FormDemo: TFormDemo;

implementation

uses
  uPanel2XML, IOUtils, NetEncoding;

{$R *.dfm}

function XorCipher(const S: string; Key: Byte = $AA): string;
var
  I: Integer;
begin
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    Result[I] := Char(Byte(S[I]) xor Key);
end;

{ TFormDemo }

procedure TFormDemo.btnLoadClick(Sender: TObject);
begin
  LoadConfig;
end;

procedure TFormDemo.btnSaveClick(Sender: TObject);
var
  Panel2Xml: TPanel2Xml;
  logLine: string;
begin
// action for saving xml file
  Panel2Xml := TPanel2Xml.Create(pnlMain, FDictMarkers);
  try
    try
      Panel2Xml.WithTabOrder := True; // optional ordering param (default True)
      Panel2Xml.BoolStrValue := True; // ckeckbox value format: False - 1/0; True - true/false (default False)
      Panel2Xml.OnEncodeText := StringEncode;

      {$ifdef debug}
        Panel2Xml.SaveXml('panel.xml');
      {$else}
        if Panel2Xml.SaveXml('panel.xml') then
          ShowMessage('File saved.')
        else
          ShowMessage('File was not saved.');
      {$endif}
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

  LoadConfig;
end;

procedure TFormDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDictMarkers);
end;

procedure TFormDemo.LoadConfig;
var
  Panel2Xml: TPanel2Xml;
  logLine: string;
begin
// action for loading xml file
  Panel2Xml := TPanel2Xml.Create(pnlMain, FDictMarkers);
  try
    try
      Panel2Xml.OnDecodeText := StringDecode;

      Panel2Xml.LoadXml('panel.xml');
    except
      on E: Exception do
      begin
        logLine := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ' + E.Message + sLineBreak;
        TFile.AppendAllText('logs.txt', logLine);
      end;
    end;
  finally
    Panel2Xml.Free;
  end;
end;

procedure TFormDemo.SetDictMarkers;
begin
  if not Assigned(FDictMarkers) then Exit;

  // defining realtion between tag numbers and node names
  FDictMarkers.Add(Ord(xmConfiguration), 'CONFIGURATION');
  FDictMarkers.Add(Ord(xmDatabase1), 'DATABASE_1');
  FDictMarkers.Add(Ord(xmDatabase2), 'DATABASE_2');
  FDictMarkers.Add(Ord(xmGeneral), 'GENERAL');
  FDictMarkers.Add(Ord(xmActive), 'ACTIVE');
  FDictMarkers.Add(Ord(xmDbType), 'DB_TYPE');
  FDictMarkers.Add(Ord(xmUsername), 'USERNAME');
  FDictMarkers.Add(Ord(xmPassword), 'PASSWORD');
  FDictMarkers.Add(Ord(xmServer), 'SERVER');
  FDictMarkers.Add(Ord(xmPort), 'PORT');
  FDictMarkers.Add(Ord(xmDescription), 'DESCRIPTION');
  FDictMarkers.Add(Ord(xmUseOption), 'USE_OPTION');
end;

procedure TFormDemo.SetTags;
begin
  // setting compontents tags - this will define related xml elements
  pnlMain.Tag := Ord(xmConfiguration);
  grpbxGeneral.Tag := Ord(xmGeneral);

  {$region 'pnlDatabase1'}
    pnlDatabase1.Tag := Ord(xmDatabase1);
    chkActive1.Tag   := Ord(xmActive);
    cmbDbType1.Tag   := Ord(xmDbType);
    edtUser1.Tag     := Ord(xmUsername);
    edtPassword1.Tag := Ord(xmPassword);
    edtServer1.Tag   := Ord(xmServer);
    sePort1.Tag      := Ord(xmPort);

    edtPassword1.PasswordChar := '*'; // if PasswordChar is set, TPanel2XmlManager will use assigned OnEncodeText, OnDecodeText functions for encryption
  {$endregion}

  {$region 'pnlDatabase2'}
    pnlDatabase2.Tag := Ord(xmDatabase2);
    chkActive2.Tag   := Ord(xmActive);
    cmbDbType2.Tag   := Ord(xmDbType);
    edtUser2.Tag     := Ord(xmUsername);
    edtPassword2.Tag := Ord(xmPassword);
    edtServer2.Tag   := Ord(xmServer);
    sePort2.Tag      := Ord(xmPort);

    edtPassword2.PasswordChar := '*';
  {$endregion}

  {$region 'grpbxGeneral'}
    memoDescr.Tag      := Ord(xmDescription);
    rdgrpUseOption.Tag := Ord(xmUseOption);
  {$endregion}
end;

function TFormDemo.StringDecode(pText: string): string;
begin
  Result := XorCipher(TNetEncoding.Base64.Decode(pText));
end;

function TFormDemo.StringEncode(pText: string): string;
begin
  Result := TNetEncoding.Base64.Encode(XorCipher(pText));
end;

procedure TFormDemo.SetTabOrder;
begin
  //(OPTIONAL) set order in which elements will be saved
  grpbxGeneral.TabOrder := 0;
  pnlDatabase1.TabOrder := 1;
  pnlDatabase2.TabOrder := 2;

  {$region 'pnlDatabase1'}
    chkActive1.TabOrder   := 0;
    cmbDbType1.TabOrder   := 1;
    edtUser1.TabOrder     := 2;
    edtPassword1.TabOrder := 3;
    edtServer1.TabOrder   := 4;
    sePort1.TabOrder      := 5;
  {$endregion}

  {$region 'pnlDatabase2'}
    chkActive2.TabOrder   := 0;
    cmbDbType2.TabOrder   := 1;
    edtUser2.TabOrder     := 2;
    edtPassword2.TabOrder := 3;
    edtServer2.TabOrder   := 4;
    sePort2.TabOrder      := 5;
  {$endregion}

  {$region 'grpbxGeneral'}
    memoDescr.TabOrder      := 0;
    rdgrpUseOption.TabOrder := 1;
  {$endregion}
end;

end.
