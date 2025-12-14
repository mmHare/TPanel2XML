unit uPanel2XML;

interface

uses
  Vcl.Controls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  Generics.Collections, Generics.Defaults,
  Classes, Xml.XMLDoc, Xml.XMLIntf;

type
  TPanel2Xml = class
    private
      FPanel: TPanel;
      FDictNodeNames: TDictionary<Integer, string>;
      FWithTabOrder: Boolean;
      FBoolStrValue: Boolean;

      function GetValueAsString(AControl: TControl): string;
      function AddNodeFromComponent(const Parent: IXMLNode; AControl: TControl): IXMLNode;
    public
      property PanelComponent: TPanel read FPanel;
      property WithTabOrder: Boolean read FWithTabOrder write FWithTabOrder;
      property BoolStrValue: Boolean read FBoolStrValue write FBoolStrValue;

      procedure AssignPanel(APanel: TPanel);
      procedure AssignDictNodeNames(ADictNodeNames: TDictionary<Integer, string>);

      function SaveXml(AFileName: string): Boolean;

      destructor Destroy; override;
      constructor Create(APanel: TPanel; ADictNodeNames: TDictionary<Integer, string>); overload;
  end;

implementation

uses
  SysUtils;

{ TPanel2XmlManager }

function TPanel2Xml.AddNodeFromComponent(const Parent: IXMLNode; AControl: TControl): IXMLNode;
var
  List: TList<TControl>;
begin
  Result := Parent;
  if not FDictNodeNames.ContainsKey(AControl.Tag) then Exit; // ignore elements with no name assigned


  // TPanel is treated as new section -> recursive node creation
  if AControl is TPanel then begin
    Result := Parent.AddChild(FDictNodeNames[AControl.Tag]);

    List := TList<TControl>.Create;
    try
      if FWithTabOrder then begin
        //sort with TabOrder
        for var I := 0 to TPanel(AControl).ControlCount - 1 do
          List.Add(TPanel(AControl).Controls[I]);

        List.Sort(
          TComparer<TControl>.Construct(
            function(const A, B: TControl): Integer
            begin
              Result := TWinControl(A).TabOrder - TWinControl(B).TabOrder;
            end
          )
        );

        for var CtrlTmp in List do
          AddNodeFromComponent(Result, CtrlTmp);
      end
      else begin
        // order doesn't matter
        for var I := 0 to TPanel(AControl).ControlCount - 1 do begin
          if TPanel(AControl).Controls[I] is TLabel then Continue; // ignore labels
          AddNodeFromComponent(Result, TPanel(AControl).Controls[I]);
        end;
      end;
    finally
      List.Free;
    end;
  end else
  // save values of supported components
  if (AControl is TEdit) or
     (AControl is TSpinEdit) or
     (AControl is TComboBox) or
     (AControl is TCheckBox)
  then begin
    Result.AddChild(FDictNodeNames[AControl.Tag]).Text := GetValueAsString(AControl);
  end;
end;

function TPanel2Xml.GetValueAsString(AControl: TControl): string;
begin
  if AControl is TEdit then begin
    Result := TEdit(AControl).Text;
  end else
  if AControl is TSpinEdit then begin
    Result := FloatToStr(TSpinEdit(AControl).Value);
  end else
  if (AControl is TComboBox) then begin
    Result := IntToStr(TComboBox(AControl).ItemIndex);
  end else
  if (AControl is TCheckBox) then begin
    if TCheckBox(AControl).Checked then
    begin
      if FBoolStrValue then Result := 'true'
                       else Result := '1';
    end
    else
    begin
      if FBoolStrValue then Result := 'false'
                       else Result := '0';
    end;
  end;
end;

procedure TPanel2Xml.AssignDictNodeNames(ADictNodeNames: TDictionary<Integer, string>);
begin
  FDictNodeNames := ADictNodeNames;
end;

procedure TPanel2Xml.AssignPanel(APanel: TPanel);
begin
  FPanel := APanel;
end;

constructor TPanel2Xml.Create(APanel: TPanel; ADictNodeNames: TDictionary<Integer, string>);
begin
  inherited Create;

  FPanel := APanel;
  FDictNodeNames := ADictNodeNames;
  FWithTabOrder := True;
  FBoolStrValue := False;
end;

destructor TPanel2Xml.Destroy;
begin
  inherited;
end;

function TPanel2Xml.SaveXml(AFileName: string): Boolean;
  function ValidateData: Boolean;
  begin
    if AFileName = '' then
       raise Exception.Create('No file path provided.');
    if not Assigned(FPanel) then
      raise Exception.Create('TPanel was not assigned.');
    if not Assigned(FDictNodeNames) or (FDictNodeNames.Count = 0) then
      raise Exception.Create('Element names dictionary is empty.');
    if not FDictNodeNames.ContainsKey(FPanel.Tag) then
      raise Exception.Create('No name provided for root element.');
    Result := True;
  end;

var
  Doc: IXMLDocument;
  pnlNode: IXMLNode;
  List: TList<TControl>;
begin
  Result := False;
  List := TList<TControl>.Create;
  try
    try
      if not ValidateData then Exit;

      Doc := NewXMLDocument;
      Doc.Encoding := 'utf-8';
      Doc.Options := [doNodeAutoIndent];

      // create root element from FPanel
      pnlNode := Doc.AddChild(FDictNodeNames[FPanel.Tag]);

      if FWithTabOrder then begin
        //sort with TabOrder
        for var I := 0 to FPanel.ControlCount - 1 do
          List.Add(FPanel.Controls[I]);

        List.Sort(
          TComparer<TControl>.Construct(
            function(const A, B: TControl): Integer
            begin
              Result := TWinControl(A).TabOrder - TWinControl(B).TabOrder;
            end
          )
        );

        for var CtrlTmp in List do
          AddNodeFromComponent(pnlNode, CtrlTmp);
      end
      else begin
        // order doesn't matter
        for var I := 0 to FPanel.ControlCount - 1 do begin
          AddNodeFromComponent(pnlNode, FPanel.Controls[I]);
        end;
      end;

      Doc.SaveToFile(AFileName);
      Result := True;
    except
      on E: Exception do begin
        raise Exception.CreateFmt('TPanel2Xml: %s', [E.Message]);
      end;
    end;
  finally
    List.Free;
  end;
end;

end.
