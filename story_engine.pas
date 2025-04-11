unit story_engine;

{$mode ObjFPC}{$H+}
{*
 다음과 같은 Story 명세를 처리하는 엔진

 - [story]: 하나의 플롯 단위. id, text, choices, set 등의 키 포함
 - [choice:<id>]: 선택지 정의. text, next, set, require 포함
 - set: 상태값을 설정하거나 변경 (변수=값 형식, 정수/불리언 지원)
 - require: 해당 조건이 참일 때만 선택 가능 (변수=값 또는 변수!=값)
 - 상태값은 엔진 내 전역 딕셔너리로 저장/참조됨
 - 스토리는 id로 연결되어 진행됨

 ```
 # story 파일: sample.story

 [story]
 id = intro
 text = "당신은 어두운 동굴에서 깨어납니다. 바닥에 횃불이 보입니다."
 choices = pick_torch, leave_torch

 [choice:pick_torch]
 text = "횃불을 집는다."
 next = torch_taken
 set = has_torch=true

 [choice:leave_torch]
 text = "그냥 지나친다."
 next = no_torch

 [story]
 id = torch_taken
 text = "당신은 횃불을 집었습니다. 이제 주변을 볼 수 있습니다."
 choices = continue_cave

 [story]
 id = no_torch
 text = "당신은 어둠 속을 더듬으며 나아갑니다."
 choices = continue_cave
 set = visibility=low

 [choice:continue_cave]
 text = "앞으로 간다."
 next = cave_branch

 [story]
 id = cave_branch
 text = "갈림길이 나옵니다. 왼쪽과 오른쪽이 있습니다."
 choices = go_left, go_right

 [choice:go_left]
 text = "왼쪽으로 간다."
 next = encounter_slime
 require = has_torch=true

 [choice:go_right]
 text = "오른쪽으로 간다."
 next = encounter_trap

 [story]
 id = encounter_slime
 text = "슬라임이 나타났다!"
 # 전투 발생

 [story]
 id = encounter_trap
 text = "당신은 함정을 밟았습니다. 체력이 감소합니다."
 set = hp=-10

 ```
*}

interface

uses
  Classes, SysUtils, fgl;

type
  TStoryNode = class
    ID, Text: string;
    Choices: array of string;
    SetValues: TStringList;
  end;

  TChoice = class
    ID, Text, NextID: string;
    SetValues: TStringList;
    RequireValues: TStringList;
  end;

  TStoryNodeMap = specialize TFPGMap<string, TStoryNode>;
  TChoiceMap = specialize TFPGMap<string, TChoice>;

  { TStorySystems }

  TStorySystems = class
  private
    FStates: TStringList;
    FStoryNodes: TStoryNodeMap;
    FChoices: TChoiceMap;
    function EvaluateCondition(Require: TStringList): boolean;
    procedure ApplySetValues(SetVals: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run(const StartID: string);
    procedure ParseStoryFile(const Filename: string);
  end;

implementation

{ TStorySystems }

procedure TStorySystems.ParseStoryFile(const Filename: string);
var
  Lines: TStringList;
  Line: string;
  I, J: integer;
  Node: TStoryNode;
  Choice: TChoice;
begin
  Lines := TStringList.Create;
  Lines.LoadFromFile(Filename);

  i := 0;
  while I < Lines.Count do
  begin
    line := Trim(Lines[I]);
    if line = '' then
    begin
      Inc(i);
      Continue;
    end;

    if line.StartsWith('[story]') then
    begin
      Node := TStoryNode.Create;
      Node.SetValues := TStringList.Create;
      Inc(i);

      while (i < Lines.Count) and (not Lines[i].StartsWith('[')) do
      begin
        Line := Trim(Lines[i]);
        if line.StartsWith('id =') then
          Node.ID := Trim(Copy(line, 5, Length(line)))
        else if line.StartsWith('text =') then
          Node.Text := Trim(Copy(line, 7, Length(line)))
        else if line.StartsWith('choices =') then
        begin
          Node.Choices := Trim(Copy(line, 10, Length(line))).Split([',']);
          For J := 0 To Length(Node.Choices) -1 do
          begin
            Node.Choices[J] := Trim(Node.Choices[J]);
          end;
        end
        else if line.StartsWith('set =') then
          Node.SetValues.Add(Trim(Copy(line, 6, length(line))));
        Inc(i);
      end;
      FStoryNodes.Add(Node.ID, Node);
      Continue;
    end;

    if Line.StartsWith('[choice:') then
    begin
      Choice := TChoice.Create;
      Choice.ID := Copy(Line, 9, Length(line) - 9); // remove [choice:] and closing
      Choice.SetValues := TStringList.Create;
      Choice.RequireValues := TStringList.Create;
      Inc(i);

      while (i < Lines.Count) and (not Lines[i].StartsWith('[')) do
      begin
        Line := Trim(Lines[i]);
        if line.StartsWith('text =') then
          Choice.Text := Trim(Copy(line, 7, length(line)))
        else if line.StartsWith('next =') then
          Choice.NextID := Trim(Copy(line, 7, Length(line)))
        else if line.StartsWith('set =') then
          Choice.SetValues.Add(Trim(Copy(Line, 6, Length(line))))
        else if line.StartsWith('require =') then
          Choice.RequireValues.Add(Trim(Copy(line, 10, Length(line))));
        Inc(i);
      end;
      FChoices.Add(Choice.ID, Choice);
      Continue;
    end;
    Inc(i);
  end;

  Lines.Free;
end;

function TStorySystems.EvaluateCondition(Require: TStringList): boolean;
var
  i: integer;
  Key, Val: string;
begin
  Result := True;
  for i := 0 to Require.Count - 1 do
  begin
    Key := Require.Names[i];
    Val := Require.Values[Key];

    WriteLn(Format('%s => %s', [Key, Val]));
    if (FStates.Values[Key] <> Val) then
    begin
      Result := False;
      Exit;
    end;
  end;

end;

procedure TStorySystems.ApplySetValues(SetVals: TStringList);
var
  i: integer;
  Key, Val: string;
begin
  for i := 0 to SetVals.Count - 1 do
  begin
    Key := SetVals.Names[i];
    Val := SetVals.Values[Key];
    FStates.Values[Key] := Val;
  end;
end;

constructor TStorySystems.Create;
begin
  FStates := TStringList.Create;
  FStoryNodes := TStoryNodeMap.Create;
  FChoices := TChoiceMap.Create;
  FStoryNodes.Sorted := True;
  FChoices.Sorted := True;
  FStoryNodes.Duplicates := dupError;
  FChoices.Duplicates := dupError;
end;

destructor TStorySystems.Destroy;
var
  i: integer;
begin
  for i := 0 to FStoryNodes.Count - 1 do
    FStoryNodes.Data[i].Free;
  FStoryNodes.Free;

  for i := 0 to FChoices.Count - 1 do
    FChoices.Data[i].Free;
  FChoices.Free;

  FStates.Free;
  inherited Destroy;
end;

procedure TStorySystems.Run(const StartID: string);
var
  CurrentID: string;
  Node: TStoryNode;
  chIdx: integer;
  Input: string;
  ch: string;
  ChoiceObj, SelectedChoice: TChoice;

  i: integer;
begin
  CurrentId := StartId;
  repeat
    Node := FStoryNodes.KeyData[CurrentId];
    WriteLn(Node.Text);

    for chIdx := 0 to Node.SetValues.Count - 1 do
      WriteLn(Node.SetValues.Values[Node.SetValues.Names[chIdx]]);

    ApplySetValues(Node.SetValues);

    // 선택지 출력
    for chIdx := 0 to Length(Node.Choices) - 1 do
    begin
      ch := Node.Choices[chIdx];
      ChoiceObj := FChoices.KeyData[ch];

      if EvaluateCondition(ChoiceObj.RequireValues) then
        WriteLn(Format('%d) %s', [chIdx + 1, ChoiceObj.Text]))
      else
        WriteLn(Format('%d) (조건 불충족)', [chIdx + 1]));
    end;

    ReadLn(Input);
    chIdx := StrToIntDef(Input, 0) - 1;
    if (chIdx < 0) or (chIdx >= Length(Node.Choices)) then Break;

    SelectedChoice := FChoices.KeyData[Node.Choices[chIdx]];

    if not EvaluateCondition(SelectedChoice.RequireValues) then
    begin
      WriteLn('조건을 만족하지 않습니다.');
      Continue;
    end;

    ApplySetValues(SelectedChoice.SetValues);
    CurrentID := SelectedChoice.NextID;
  until CurrentID = '';

end;

end.
