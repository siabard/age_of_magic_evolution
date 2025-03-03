unit entity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LogUtil, component;

type

  TEntity = class
  private
    FPosition: TPositionComponent;
    FAnimation: TAnimationComponent;
    FInput: TInputComponent;
    FMovement: TMovementComponent;
    FIsAlive: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property position: TPositionComponent read FPosition write FPosition;
    property animation: TAnimationComponent read FAnimation write FAnimation;
    property IsLive: boolean read FIsAlive write FIsAlive;
    property input: TInputComponent read FInput write FInput;
    property movement: TMovementComponent read FMovement write FMovement;
  end;



implementation

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
end;


end.
