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

      function GetText(Node: IXMLNode; const Name: string; Default: string): string;
      function GetInt(Node: IXMLNode; const Name: string; Default: Integer): Integer;
      function GetFloat(Node: IXMLNode; const Name: string; Default: Double): Double;
      function GetBool(Node: IXMLNode; const Name: string; Default: Boolean): Boolean;

      procedure GetOrderedList(ComponentList: TList<TControl>; APanel: TPanel);
      
      function ValidateData(AFileName: string): Boolean;
      
      function GetValueAsString(AControl: TControl): string;
      function AddNodeFromComponent(const Parent: IXMLNode; AControl: TControl): IXMLNode;

      procedure GetComponentValueFromNode(const Parent: IXMLNode; AControl: TControl);
      function GetChildByName(Parent: IXMLNode; const Name: string): IXMLNode;

    public
      property PanelComponent: TPanel read FPanel;
      property WithTabOrder: Boolean read FWithTabOrder write FWithTabOrder;
      property BoolStrValue: Boolean read FBoolStrValue write FBoolStrValue;

      procedure AssignPanel(APanel: TPanel);
      procedure AssignDictNodeNames(ADictNodeNames: TDictionary<Integer, string>);

      function SaveXml(AFileName: string): Boolean;
      function LoadXml(AFileName: string): Boolean;

      destructor Destroy; override;
      constructor Create(APanel: TPanel; ADictNodeNames: TDictionary<Integer, string>); overload;
  end;

implementation

uses
  SysUtils, StrUtils;

{ TPanel2XmlManager }

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

function TPanel2Xml.ValidateData(AFileName: string): Boolean;
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

function TPanel2Xml.SaveXml(AFileName: string): Boolean;
var
  XmlDoc: IXMLDocument;
  pnlNode: IXMLNode;
  controlList: TList<TControl>;
begin
  controlList := TList<TControl>.Create;
  try
    try
      if not ValidateData(AFileName) then Exit(False);

      XmlDoc := NewXMLDocument;
      XmlDoc.Encoding := 'utf-8';
      XmlDoc.Options := [doNodeAutoIndent];

      // create root element from FPanel
      pnlNode := XmlDoc.AddChild(FDictNodeNames[FPanel.Tag]);

      if FWithTabOrder then begin
        //sort with TabOrder
        GetOrderedList(controlList, FPanel);
        
        for var CtrlTmp in controlList do
          AddNodeFromComponent(pnlNode, CtrlTmp);
      end
      else begin
        // order doesn't matter
        for var I := 0 to FPanel.ControlCount - 1 do begin
          AddNodeFromComponent(pnlNode, FPanel.Controls[I]);
        end;
      end;

      XmlDoc.SaveToFile(AFileName);
      Result := True;
    except
      on E: Exception do begin
        raise Exception.CreateFmt('TPanel2Xml: %s', [E.Message]);
      end;
    end;
  finally
    controlList.Free;
  end;
end;

function TPanel2Xml.AddNodeFromComponent(const Parent: IXMLNode; AControl: TControl): IXMLNode;
var
  controlList: TList<TControl>;
begin
  Result := Parent;
  if not FDictNodeNames.ContainsKey(AControl.Tag) then Exit; // ignore elements with no name assigned


  // TPanel is treated as new section -> recursive node creation
  if AControl is TPanel then begin
    Result := Parent.AddChild(FDictNodeNames[AControl.Tag]);

    controlList := TList<TControl>.Create;
    try
      if FWithTabOrder then begin
        //sort with TabOrder
        GetOrderedList(controlList, TPanel(AControl));

        for var CtrlTmp in controlList do
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
      controlList.Free;
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
      Result := IfThen(FBoolStrValue, 'true', '1')
    else
      Result := IfThen(FBoolStrValue, 'false', '0');
  end;
end;

function TPanel2Xml.LoadXml(AFileName: string): Boolean;
var
  XmlDoc: IXMLDocument;
  RootNode: IXMLNode;
begin
  XmlDoc := TXMLDocument.Create(nil);
  try
    if not ValidateData(AFileName) then Exit(False);
    
    XmlDoc.LoadFromFile(AFileName);
    XmlDoc.Active := True;
    RootNode := XmlDoc.DocumentElement;

    if RootNode = nil then raise Exception.Create('XML document empty or invalid.');
    if RootNode.NodeName <> FDictNodeNames[FPanel.Tag] then
      raise Exception.Create('Incorrect XML root element.');
      
    for var I := 0 to FPanel.ControlCount - 1 do 
      GetComponentValueFromNode(RootNode, FPanel.Controls[I]);
      
    Result := True
  except
    on E: Exception do begin
      raise Exception.CreateFmt('TPanel2Xml: %s', [E.Message]);
    end;
  end;
end;

procedure TPanel2Xml.GetComponentValueFromNode(const Parent: IXMLNode; AControl: TControl);
var
  Node: IXMLNode;
begin
  // TPanel is treated as child nodes container -> recursive get node
  if AControl is TPanel then begin
    for var I := 0 to TPanel(AControl).ControlCount - 1 do begin
      if TPanel(AControl).Controls[I] is TLabel then Continue; // ignore labels
      Node := GetChildByName(Parent, FDictNodeNames[AControl.Tag]);
      if Assigned(Node) then        
        GetComponentValueFromNode(Node, TPanel(AControl).Controls[I]);
    end;
  end;

  // load values
  if (AControl is TEdit) then begin
    TEdit(AControl).Text := GetText(Parent, FDictNodeNames[AControl.Tag], '');   
  end;
  if (AControl is TSpinEdit) then begin
    TSpinEdit(AControl).Value := GetInt(Parent, FDictNodeNames[AControl.Tag], 0);
  end;
  if (AControl is TComboBox) then begin
    TComboBox(AControl).ItemIndex := GetInt(Parent, FDictNodeNames[AControl.Tag], 0);
  end;
  if (AControl is TCheckBox) then begin
    TCheckBox(AControl).Checked := GetBool(Parent, FDictNodeNames[AControl.Tag], False);
  end;
end;

procedure TPanel2Xml.GetOrderedList(ComponentList: TList<TControl>; APanel: TPanel);
begin
  try
    for var I := 0 to APanel.ControlCount - 1 do
      ComponentList.Add(APanel.Controls[I]);

    ComponentList.Sort(
      TComparer<TControl>.Construct(
        function(const A, B: TControl): Integer
        begin
          Result := TWinControl(A).TabOrder - TWinControl(B).TabOrder;
        end
      )
    );
  except
    raise;
  end;
end;

function TPanel2Xml.GetChildByName(Parent: IXMLNode; const Name: string): IXMLNode;
begin
  Result := nil;
  for var I := 0 to Parent.ChildNodes.Count - 1 do
    if (Parent.ChildNodes[I].NodeName = Name) then
      Exit(Parent.ChildNodes[I]);
end;

function TPanel2Xml.GetFloat(Node: IXMLNode; const Name: string; Default: Double): Double;
var
  Child: IXMLNode;
begin
  Child := GetChildByName(Node, Name);
  if Assigned(Child) then
    Result := Child.Text.ToDouble
  else
    Result := Default;
end;

function TPanel2Xml.GetInt(Node: IXMLNode; const Name: string; Default: Integer): Integer;
var
  Child: IXMLNode;
begin
  Child := GetChildByName(Node, Name);
  if Assigned(Child) then
    Result := Child.Text.ToInteger
  else
    Result := Default;
end;

function TPanel2Xml.GetText(Node: IXMLNode; const Name: string; Default: string): string;
var
  Child: IXMLNode;
begin
  Child := GetChildByName(Node, Name);
  if Assigned(Child) then
    Result := Child.Text
  else
    Result := Default;
end;

function TPanel2Xml.GetBool(Node: IXMLNode; const Name: string; Default: Boolean): Boolean;
var
  Child: IXMLNode;
begin
  Child := GetChildByName(Node, Name);
  if Assigned(Child) then
    Result := Child.Text.ToBoolean
  else
    Result := Default;
end;

end.
