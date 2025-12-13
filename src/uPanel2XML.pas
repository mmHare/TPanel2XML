unit uPanel2XML;

interface

uses
  Vcl.Controls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  Generics.Collections, Generics.Defaults,
  Classes, Xml.XMLDoc, Xml.XMLIntf;

const
  DEFAULT_FILENAME = 'panel.xml';

type
  TPanel2Xml = class
    private
      FFileName: string;
      FPanel: TPanel;
      FDictNodeNames: TDictionary<Integer, string>;
      FWithTabOrder: Boolean;

      function GetValueAsString(AControl: TControl): string;
      function AddNodeFromComponent(const Parent: IXMLNode; AControl: TControl): IXMLNode;
      function ValidateData: Boolean;
    public
      property FileName: string read FFileName write FFileName;
      property PanelComponent: TPanel read FPanel;
      property WithTabOrder: Boolean read FWithTabOrder write FWithTabOrder;

      procedure AssignPanel(APanel: TPanel);
      procedure AssignDictNodeNames(ADictNodeNames: TDictionary<Integer, string>);

      function SaveXml: Boolean;

      destructor Destroy; override;
      constructor Create(APanel: TPanel; ADictNodeNames: TDictionary<Integer, string>; AFileName: string=''); overload;
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
     (AControl is TSpinEdit) then begin
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

constructor TPanel2Xml.Create(APanel: TPanel; ADictNodeNames: TDictionary<Integer, string>; AFileName: string='');
begin
  inherited Create;

  FFileName := AFileName;
  if FFileName = '' then FFileName := DEFAULT_FILENAME;

  FPanel := APanel;
  FDictNodeNames := ADictNodeNames;
  FWithTabOrder := True;
end;

destructor TPanel2Xml.Destroy;
begin
  inherited;
end;

function TPanel2Xml.SaveXml: Boolean;
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

      Doc.SaveToFile(FFileName);
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

function TPanel2Xml.ValidateData: Boolean;
begin
  if not Assigned(FPanel) then
    raise Exception.Create('TPanel was not assigned.');
  if not Assigned(FDictNodeNames) or (FDictNodeNames.Count = 0) then
    raise Exception.Create('Element names dictionary is empty.');
  if not FDictNodeNames.ContainsKey(FPanel.Tag) then
    raise Exception.Create('No name provided for root element.');
  Result := True;
end;

end.
