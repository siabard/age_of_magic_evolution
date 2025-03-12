unit entity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LogUtil, component, game_types;

type

  TEntity = class
  private
    FPosition: TPositionComponent;
    FAnimation: TAnimationComponent;
    FInput: TInputComponent;
    FMovement: TMovementComponent;
    FIsAlive: boolean;
    FCollide: TCollideComponent;
    nid: Integer;
    FTag: String;

  public
    constructor Create;
    destructor Destroy; override;
    property position: TPositionComponent read FPosition write FPosition;
    property animation: TAnimationComponent read FAnimation write FAnimation;
    property IsLive: boolean read FIsAlive write FIsAlive;
    property input: TInputComponent read FInput write FInput;
    property movement: TMovementComponent read FMovement write FMovement;
    property collide: TCollideComponent read FCollide write FCollide;
    property id: Integer read nid;
    property tag: String read FTag write FTag;
    function GetBoundigRect: RRect;
    function GetPrevBoundingRect: RRect;
    function setNid(AId: Integer): Integer;
  end;



implementation

uses  physics_util;

constructor TEntity.Create;
begin
  FIsAlive := True;
end;

destructor TEntity.Destroy;
begin
  if Assigned(FPosition) then
  begin
    LogDebug('Remove Position Component');
    FreeAndNil(FPosition);
  end;

  if Assigned(FAnimation) then
  begin
    LogDebug('Remove Animation Component');
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

  if Assigned(FCollide) Then
  begin
    FreeAndNil(FCollide);
  end;
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

function TEntity.setNid(AId: Integer): Integer;
begin
  Self.nid := AId;
  Result := Self.nid;
end;

end.
