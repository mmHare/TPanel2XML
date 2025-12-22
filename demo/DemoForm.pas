unit DemoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Generics.Collections,
  Vcl.Samples.Spin, Vcl.CheckLst, Vcl.Mask;

type
  TXmlElementNames = (xmConfiguration=1, xmDatabase1, xmDatabase2, xmGeneral,
                      xmActive, xmDbType, xmUsername, xmPassword, xmServer, xmPort,
                      xmDescription, xmUseOption, xmCustomText, xmLabel);

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
    chkDefDescr: TCheckBox;
    medtCustom: TMaskEdit;
    cmbSaveOption: TComboBox;
    Label1: TLabel;
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

    procedure SaveXmlWithNamesOption;
    procedure SaveXmlWithTagOption;
  public
    { Public declarations }
  end;

var
  FormDemo: TFormDemo;

implementation

uses
  uComponentXmlBuilder, IOUtils, NetEncoding;

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
begin
  case cmbSaveOption.ItemIndex of
    0: SaveXmlWithNamesOption;
    1: SaveXmlWithTagOption;
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
  XmlBuilder: TComponentXmlBuilder;
  logLine, sValue: string;
begin
// action for loading xml file
  XmlBuilder := TComponentXmlBuilder.CreateWithTags(pnlMain, FDictMarkers);
  try
    try
      XmlBuilder.OnDecodeText := StringDecode;
      XmlBuilder.AddToReadList(cmbDbType2); // add controls which values should be put into readlist instead of loading to component itself
      XmlBuilder.AddToReadList(medtCustom); // TMaskEdit is not supported so it will not be loaded until read explicitly
      XmlBuilder.AddToReadList(lblDatabase1); // dummy component

      XmlBuilder.LoadXml('panel.xml');

      // components on custom list will not be updated with read values, their values need to be get individually
      sValue := XmlBuilder.GetComponentValue(cmbDbType2);  // returned value is always a string
      sValue := sValue + ' : ' + XmlBuilder.GetComponentValue(lblDatabase1);
      memoDescr.Text := sValue;
      medtCustom.Text := XmlBuilder.GetComponentValue(medtCustom);
    except
      on E: Exception do
      begin
        logLine := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ' + E.Message + sLineBreak;
        TFile.AppendAllText('logs.txt', logLine);
      end;
    end;
  finally
    XmlBuilder.Free;
  end;
end;

procedure TFormDemo.SaveXmlWithNamesOption;
var
  XmlBuilder: TComponentXmlBuilder;
  logLine: string;
begin
// action for saving xml file
  XmlBuilder := TComponentXmlBuilder.Create(pnlMain);
  try
    try
      XmlBuilder.WithTabOrder := True; // optional ordering param (default True)
      XmlBuilder.BoolStrValue := False; // ckeckbox value format: False - 1/0; True - true/false (default False)
      XmlBuilder.OnEncodeText := StringEncode;  // assign function to be used in encrypting password edits (if Edit's PasswordChar <> #0)

      // prepare binding list
      XmlBuilder.AddComponentBind(pnlMain, 'CONFIGURATION');
      XmlBuilder.AddComponentBind(grpbxGeneral, 'GENERAL');

      {$region 'pnlDatabase1'}
        XmlBuilder.AddComponentBind(pnlDatabase1, 'DATABASE_1');
        XmlBuilder.AddComponentBind(chkActive1, 'ACTIVE');
        XmlBuilder.AddComponentBind(cmbDbType1, 'DB_TYPE');
        XmlBuilder.AddComponentBind(edtUser1, 'USERNAME');
        XmlBuilder.AddComponentBind(edtPassword1, 'PASSWORD');
        XmlBuilder.AddComponentBind(edtServer1, 'SERVER');
        XmlBuilder.AddComponentBind(sePort1, 'PORT');

        edtPassword1.PasswordChar := '*'; // if PasswordChar is set, TPanel2XmlManager will use assigned OnEncodeText, OnDecodeText functions for encryption
      {$endregion}

      {$region 'pnlDatabase2'}
        XmlBuilder.AddComponentBind(pnlDatabase2, 'DATABASE_2');
        XmlBuilder.AddComponentBind(chkActive2, 'ACTIVE');
        XmlBuilder.AddComponentBind(cmbDbType2, 'DB_TYPE', cmbDbType2.Text); // works like combine with AddCustomComponentValue
        XmlBuilder.AddComponentBind(edtUser2, 'USERNAME');
        XmlBuilder.AddComponentBind(edtPassword2, 'PASSWORD');
        XmlBuilder.AddComponentBind(edtServer2, 'SERVER');
        XmlBuilder.AddComponentBind(sePort2, 'PORT');

        edtPassword2.PasswordChar := '*'; // if PasswordChar is set, TPanel2XmlManager will use assigned OnEncodeText, OnDecodeText functions for encryption
      {$endregion}

      {$region 'grpbxGeneral'}
        XmlBuilder.AddComponentBind(memoDescr, 'DESCRIPTION');
        if chkDefDescr.Checked then
          XmlBuilder.AddCustomComponentValue(memoDescr, 'This is default description.'); // overwrite value

        XmlBuilder.AddComponentBind(rdgrpUseOption, 'USE_OPTION');
        XmlBuilder.AddComponentBind(medtCustom, 'CUSTOM_TEXT', medtCustom.Text); // works like combine with AddCustomComponentValue
//        XmlBuilder.AddComponentBind(chkDefDescr, ''); // if component is not added, it will be ommitted
      {$endregion}


      // component custom value save (Text instead of ItemIndex)
//      XmlBuilder.AddCustomComponentValue(cmbDbType2, cmbDbType2.Text);
//      XmlBuilder.AddCustomComponentValue(medtCustom, medtCustom.Text); // TMaskEdit is not supported but can be custom saved


      XmlBuilder.AddComponentBind(lblDatabase1, 'LABEL', 'Static text');   // TLabel has no edit value but can be used with custom save as dummy for static text
//      XmlBuilder.AddCustomComponentValue(lblDatabase1, 'Static text'); // use dummy component for static text

      XmlBuilder.SaveXml('panel.xml');
    except
      on E: Exception do
      begin
        ShowMessage('Error occurred while saving the file.');
        logLine := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ' + E.Message + sLineBreak;
        TFile.AppendAllText('logs.txt', logLine);
      end;
    end;
  finally
    XmlBuilder.Free;
  end;
end;

procedure TFormDemo.SaveXmlWithTagOption;
var
  XmlBuilder: TComponentXmlBuilder;
  logLine: string;
begin
// action for saving xml file
  XmlBuilder := TComponentXmlBuilder.CreateWithTags(pnlMain, FDictMarkers);
  try
    try
      XmlBuilder.WithTabOrder := True; // optional ordering param (default True)
      XmlBuilder.BoolStrValue := True; // ckeckbox value format: False - 1/0; True - true/false (default False)
      XmlBuilder.OnEncodeText := StringEncode;

      // component custom value save (Text instead of ItemIndex)
      XmlBuilder.AddCustomComponentValue(cmbDbType2, cmbDbType2.Text);
      XmlBuilder.AddCustomComponentValue(medtCustom, medtCustom.Text); // TMaskEdit is not supported but can be custom saved

      if chkDefDescr.Checked then
        XmlBuilder.AddCustomComponentValue(memoDescr, 'This is default description.'); // overwrite value

      XmlBuilder.AddCustomComponentValue(lblDatabase1, 'Static text'); // use dummy component for static text

      XmlBuilder.SaveXml('panel.xml');
    except
      on E: Exception do
      begin
        ShowMessage('Error occurred while saving the file.');
        logLine := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ' + E.Message + sLineBreak;
        TFile.AppendAllText('logs.txt', logLine);
      end;
    end;
  finally
    XmlBuilder.Free;
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
  FDictMarkers.Add(Ord(xmCustomText), 'CUSTOM_TEXT');
  FDictMarkers.Add(Ord(xmLabel), 'LABEL');
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
    cmbDbType2.Tag   := Ord(xmDbType);  // this control will be custom saved but still needs binding with element name
    edtUser2.Tag     := Ord(xmUsername);
    edtPassword2.Tag := Ord(xmPassword);
    edtServer2.Tag   := Ord(xmServer);
    sePort2.Tag      := Ord(xmPort);

    edtPassword2.PasswordChar := '*';
  {$endregion}

  {$region 'grpbxGeneral'}
    memoDescr.Tag      := Ord(xmDescription);
    rdgrpUseOption.Tag := Ord(xmUseOption);
    medtCustom.Tag     := Ord(xmCustomText); // TMaskEdit is not supported but can be custom saved

    chkDefDescr.Tag := -1; // -1 is not in FDictMarkers so the control will not be saved
  {$endregion}

  lblDatabase1.Tag := Ord(xmLabel); // TLabel has no edit value but can be used with custom save as dummy for static text
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
    medtCustom.TabOrder     := 2;
  {$endregion}

  // lblDatabase1 - TLabel has no TabOrder property so it will not be ordered
  // if ordering is needed, use components with TabOrder
end;

end.
