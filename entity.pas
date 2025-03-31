unit entity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LogUtil, component, game_types;

type

  { TEntity }

  TEntity = class
  private
    FPosition: TPositionComponent;
    FAnimation: TAnimationComponent;
    FInput: TInputComponent;
    FMovement: TMovementComponent;
    FIsAlive: boolean;
    FCollide: TCollideComponent;
    FDepth: TDepthComponent;
    nid: integer;
    FZIndex: integer;
    FTag: string;
    FGroup: TStringList;

  public
    constructor Create;
    destructor Destroy; override;
    property position: TPositionComponent read FPosition write FPosition;
    property animation: TAnimationComponent read FAnimation write FAnimation;
    property IsLive: boolean read FIsAlive write FIsAlive;
    property input: TInputComponent read FInput write FInput;
    property movement: TMovementComponent read FMovement write FMovement;
    property collide: TCollideComponent read FCollide write FCollide;
    property depth: TDepthComponent read FDepth write FDepth;
    property id: integer read nid;
    property tag: string read FTag write FTag;
    property zindex: integer read FZIndex write FZIndex;
    property group: TStringList read FGroup write FGroup;
    function GetBoundigRect: RRect;
    function GetPrevBoundingRect: RRect;
    function setNid(AId: integer): integer;
    procedure AddToGroup(groupName: string);
    procedure RemoveFromGroup(groupName: string);
  end;



implementation

uses  physics_util;

constructor TEntity.Create;
begin
  FIsAlive := True;
  FGroup := TStringList.Create;
end;

destructor TEntity.Destroy;
begin
  if Assigned(FPosition) then
  begin
    LogDebug(Format('Remove Position Component : %s', [FPosition.id]));
    FreeAndNil(FPosition);
  end;

  if Assigned(FAnimation) then
  begin
    LogDebug(Format('Remove Animation Component : %s', [FAnimation.id]));
    FreeAndNil(FAnimation);
  end;

  if Assigned(FInput) then
  begin
    FInput.Free;
  end;

  if Assigned(FMovement) then
  begin
    FMovement.Free;
  end;

  if Assigned(FCollide) then
  begin
    FreeAndNil(FCollide);
  end;


  If Assigned(FDepth) Then
  begin
    FreeAndNil(FDepth);
  end;
  FreeAndNil(FGroup);

  LogDebug(Format('Entity Destoryed : %3d' , [nid] ));
end;

function TEntity.GetBoundigRect: RRect;
begin
  Result.RX := Self.position.X + Self.collide.BoundBox.RX;
  Result.RY := Self.position.Y + Self.collide.BoundBox.RY;
  Result.RW := Self.collide.BoundBox.RW;
  Result.RH := Self.collide.BoundBox.RH;
end;

function TEntity.GetPrevBoundingRect: RRect;
begin
  Result.RX := Self.position.PX + Self.collide.BoundBox.RX;
  Result.RY := Self.position.PY + Self.collide.BoundBox.RY;
  Result.RW := Self.collide.BoundBox.RW;
  Result.RH := Self.collide.BoundBox.RH;
end;

function TEntity.setNid(AId: integer): integer;
begin
  Self.nid := AId;
  Result := Self.nid;
end;

procedure TEntity.AddToGroup(groupName: string);
begin
  FGroup.Add(groupName);
end;

procedure TEntity.RemoveFromGroup(groupName: string);
var
  pos: integer;
begin
  if FGroup.Find(groupName, pos) then
    FGroup.Delete(pos);
end;

end.
